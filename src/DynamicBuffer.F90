#include "error_handling_macros.fh"
module PFL_DynamicBuffer
   use PFL_Exception
   use PFL_KeywordEnforcer
   implicit none
   private

   public :: DynamicBuffer
   public :: INTERNAL_FILE_EOR
   public :: INTERNAL_FILE_EOF

   type :: DynamicBuffer
      integer :: record_size = 16 ! default
      integer, private :: num_records = 1
#ifdef __GFORTRAN__
      character(len=:), allocatable :: buffer
#else
      character(len=:), allocatable :: buffer(:)
#endif      
   contains
      procedure :: grow_record_size
      procedure :: grow_num_records
      procedure :: allocate
      procedure :: concatenate
   end type DynamicBuffer

   logical, save :: initialized = .false.

   integer, protected :: INTERNAL_FILE_EOR
   integer, protected :: INTERNAL_FILE_EOF
   character(len=1), parameter :: EOT = achar(003)

   integer, parameter :: MAX_BUFFER_SIZE = 2**20 ! 1 MB
   
contains

   subroutine grow_record_size(this, unusable, rc)
      class (DynamicBuffer), intent(inout) :: this
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      if (this%record_size * this%num_records > MAX_BUFFER_SIZE/2) then
         _THROW('DynamicBuffer::grow_record_size() - exceeded maximum permitted buffer size.')
         _RETURN(_FAILURE,rc)
      end if
      this%record_size = this%record_size * 2
      call this%allocate()

      _RETURN(_SUCCESS,rc)
   end subroutine grow_record_size


   subroutine grow_num_records(this, unusable, rc)
      class (DynamicBuffer), intent(inout) :: this
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

#ifdef __GFORTRAN__
      _ASSERT(.false., 'DynamicBuffer::grow_num_records() - GFortran cannot support dynamically sized multiline records for internal files.', rc)
#else
      if (this%record_size * this%num_records > MAX_BUFFER_SIZE/2) then
         _ASSERT(.false., 'DynamicBuffer::grow_num_records() - exceeded maximum permitted num records.', rc)
      end if

      this%num_records = this%num_records * 2
      call this%allocate()
#endif

      _RETURN(_SUCCESS,rc)
   end subroutine grow_num_records


   subroutine allocate(this)
      use iso_c_binding, only: C_NULL_CHAR
      class (DynamicBuffer), intent(inout) :: this
      integer :: i

      if (.not. initialized) call initialize()

      if (allocated(this%buffer)) then
         deallocate(this%buffer)
      end if

#ifdef __GFORTRAN__
      allocate(character(len=this%record_size) :: this%buffer)
#else
      allocate(character(len=this%record_size) :: this%buffer(this%num_records))
      do i = 1, this%num_records
         this%buffer(i) = EOT
      end do
#endif

   end subroutine allocate


   function concatenate(this) result(string)
      use iso_c_binding, only: C_NULL_CHAR
      class (DynamicBuffer), intent(in) :: this
      character(len=:), allocatable :: string

      integer :: i, j, n, k

#ifdef __GFORTRAN__
      string = trim(this%buffer)
#else

      j = 0
      do i = 1, this%num_records
         if (this%buffer(i) /= C_NULL_CHAR) then
            if (this%buffer(i)(1:1) == EOT) then ! done
               exit
            endif
            n = len_trim(this%buffer(i))
            j = j + n + 1
         end if
      end do
      j = j - 1 ! no newline for last record

      allocate(character(len=j) :: string)
      
      j = 0
      do i = 1, this%num_records
         if (this%buffer(i)(1:1) == EOT) then ! done
            exit
         end if

         if (i > 1) then
            string(j+1:j+1) = new_line('a')
            j = j + 1
         end if

         n = len_trim(this%buffer(i))
         string(j+1:j+n) = this%buffer(i)(1:n)
         j = j + n
      end do
#endif

   end function concatenate

   ! This procedure dynamically determines the compiler-dependent
   ! values of the iostat return value of end-of-record and
   ! end-of-file.
   
   subroutine initialize()
      character(len=1) :: buffer(1)

      initialized = .true.

      write(buffer,'(i2)', iostat=INTERNAL_FILE_EOR) 1
      write(buffer,'(i1/)', iostat=INTERNAL_FILE_EOF) 1  ! no longer used
      
   end subroutine initialize

end module PFL_DynamicBuffer
