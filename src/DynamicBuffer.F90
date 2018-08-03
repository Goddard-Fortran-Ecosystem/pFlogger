module PFL_DynamicBuffer_mod
   use PFL_Exception_mod
   implicit none
   private

   public :: DynamicBuffer
   public :: INTERNAL_FILE_EOR
   public :: INTERNAL_FILE_EOF

#ifdef __GFORTRAN__
   integer, parameter :: RECORD_SIZE = 256
#endif

   type :: DynamicBuffer
      integer, private :: record_size = 80
      integer, private :: num_records = 1
#ifdef __GFORTRAN__
      character(len=RECORD_SIZE), allocatable :: buffer(:)
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

   integer, parameter :: MAX_BUFFER_SIZE = 2**20 ! 1 MB
   
contains

   subroutine grow_record_size(this)
      class (DynamicBuffer), intent(inout) :: this

      if (this%record_size * this%num_records > MAX_BUFFER_SIZE/2) then
         call throw('DynamicBuffer::grow_record_size() - exceeded maximum permitted buffer size.')
         return
      end if
      this%record_size = this%record_size * 2
      call this%allocate()

#ifdef __GFORTRAN__
      call throw('Compiler limitation for GFORTRAN. Try increasing RECORD_SIZE.')
#endif

   end subroutine grow_record_size


   subroutine grow_num_records(this)
      class (DynamicBuffer), intent(inout) :: this

      if (this%record_size * this%num_records > MAX_BUFFER_SIZE/2) then
         call throw('DynamicBuffer::grow_num_records() - exceeded maximum permitted num records.')
         return
      end if

      this%num_records = this%num_records * 2
      call this%allocate()

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
      allocate(this%buffer(this%num_records))
#else
      allocate(character(len=this%record_size) :: this%buffer(this%num_records))
#endif       

      do i = 1, this%num_records
         this%buffer(i) = C_NULL_CHAR
      end do

   end subroutine allocate

   function concatenate(this) result(string)
      use iso_c_binding, only: C_NULL_CHAR
      class (DynamicBuffer), intent(in) :: this
      character(len=:), allocatable :: string

      integer :: i, j, n, k
      character(len=1), parameter :: EOT = achar(003)

      j = 0
      do i = 1, this%num_records
         if (this%buffer(i) /= C_NULL_CHAR) then
            n = len_trim(this%buffer(i))
            if (this%buffer(i)(n:n) == EOT) n = n - 1
            j = j + n + 1
         end if
      end do
      j = j - 1

      allocate(character(len=j) :: string)
      
      j = 0
      i = 1
      n = len_trim(this%buffer(1))
      if (this%buffer(1)(n:n) == EOT) n = n - 1
      string(j+1:j+n) = this%buffer(1)(1:n)
      j = j + n

      do i = 2, this%num_records
         if (this%buffer(i) /= C_NULL_CHAR) then
            n = len_trim(this%buffer(i))
            if (this%buffer(i)(n:n) == EOT) n = n - 1
            string(j+1:j+1) = new_line('a')
            j = j + 1
            string(j+1:j+n) = this%buffer(i)(1:n)
            j = j + n 
         else
            exit
         end if
      end do


   end function concatenate

   subroutine initialize()
      character(len=1) :: buffer(1)

      initialized = .true.

      write(buffer,'(i2)', iostat=INTERNAL_FILE_EOR) 1
      write(buffer,'(i1/)', iostat=INTERNAL_FILE_EOF) 1
      
   end subroutine initialize

end module PFL_DynamicBuffer_mod
