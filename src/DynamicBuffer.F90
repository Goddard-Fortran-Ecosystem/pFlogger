module ASTG_DynamicBuffer_mod
   use ASTG_Exception_mod
   implicit none
   private

   public :: DynamicBuffer
   public :: INTERNAL_FILE_EOR
   public :: INTERNAL_FILE_EOF

#ifdef __GFORTRAN__
   integer, parameter :: RECORD_SIZE = 256
#endif

   type :: DynamicBuffer
      integer, private :: recordSize = 80
      integer, private :: numRecords = 1
#ifdef __GFORTRAN__
      character(len=RECORD_SIZE), allocatable :: buffer(:)
#else
      character(len=:), allocatable :: buffer(:)
#endif
   contains
      procedure :: growRecordSize
      procedure :: growNumRecords
      procedure :: allocate
      procedure :: concatenate
   end type DynamicBuffer

   logical, save :: initialized = .false.

   integer, protected :: INTERNAL_FILE_EOR
   integer, protected :: INTERNAL_FILE_EOF

   integer, parameter :: MAX_BUFFER_SIZE = 2**20 ! 1 MB
   
contains

   subroutine growRecordSize(this)
      class (DynamicBuffer), intent(inout) :: this

      if (this%recordSize * this%numRecords > MAX_BUFFER_SIZE/2) then
         call throw('DynamicBuffer::growRecordSize() - exceeded maximum permitted buffer size.')
         return
      end if
      this%recordSize = this%recordSize * 2
      call this%allocate()

#ifdef __GFORTRAN__
      call throw('Compiler limitation for GFORTRAN. Try increasing RECORD_SIZE.')
#endif

   end subroutine growRecordSize


   subroutine growNumRecords(this)
      class (DynamicBuffer), intent(inout) :: this

      if (this%recordSize * this%numRecords > MAX_BUFFER_SIZE/2) then
         call throw('DynamicBuffer::growNumRecords() - exceeded maximum permitted buffer size.')
         return
      end if

      this%numRecords = this%numRecords * 2
      call this%allocate()

   end subroutine growNumRecords

   subroutine allocate(this)
      use iso_c_binding, only: C_NULL_CHAR
      class (DynamicBuffer), intent(inout) :: this
      integer :: i

      if (.not. initialized) call initialize()

      if (allocated(this%buffer)) then
         deallocate(this%buffer)
      end if

#ifdef __GFORTRAN__
      allocate(this%buffer(this%numRecords))
#else
      allocate(character(len=this%recordSize) :: this%buffer(this%numRecords))
#endif       

      do i = 1, this%numRecords
         this%buffer(i) = C_NULL_CHAR
      end do

   end subroutine allocate

   function concatenate(this) result(string)
      use iso_c_binding, only: C_NULL_CHAR
      class (DynamicBuffer), intent(in) :: this
      character(len=:), allocatable :: string

      integer :: i

      i = 1
      string = trim(this%buffer(1))

      do i = 2, this%numRecords
         if (this%buffer(i) /= C_NULL_CHAR) then
            string = string // new_line('a') // trim(this%buffer(i))
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

end module ASTG_DynamicBuffer_mod
