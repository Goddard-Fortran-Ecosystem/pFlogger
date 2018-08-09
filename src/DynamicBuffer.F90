module PFL_DynamicBuffer_mod
   use PFL_Exception_mod
   implicit none
   private

   public :: DynamicBuffer
   public :: INTERNAL_FILE_EOR
   public :: INTERNAL_FILE_EOF

   type :: DynamicBuffer
      integer :: record_size = 16 ! default
      character(len=:), allocatable :: buffer
   contains
      procedure :: grow_record_size
      procedure :: allocate
   end type DynamicBuffer

   logical, save :: initialized = .false.

   integer, protected :: INTERNAL_FILE_EOR
   integer, protected :: INTERNAL_FILE_EOF

   integer, parameter :: MAX_BUFFER_SIZE = 2**20 ! 1 MB
   
contains

   subroutine grow_record_size(this)
      class (DynamicBuffer), intent(inout) :: this

      if (this%record_size > MAX_BUFFER_SIZE/2) then
         call throw(__FILE__,__LINE__,'DynamicBuffer::grow_record_size() - exceeded maximum permitted buffer size.')
         return
      end if
      this%record_size = this%record_size * 2
      call this%allocate()

   end subroutine grow_record_size


   subroutine allocate(this)
      use iso_c_binding, only: C_NULL_CHAR
      class (DynamicBuffer), intent(inout) :: this
      integer :: i

      if (.not. initialized) call initialize()

      if (allocated(this%buffer)) then
         deallocate(this%buffer)
      end if

      allocate(character(len=this%record_size) :: this%buffer)

   end subroutine allocate


   ! This procedure dynamically determines the compiler-dependent
   ! values of the iostat return value of end-of-record and
   ! end-of-file.
   
   subroutine initialize()
      character(len=1) :: buffer(1)

      initialized = .true.

      write(buffer,'(i2)', iostat=INTERNAL_FILE_EOR) 1
      write(buffer,'(i1/)', iostat=INTERNAL_FILE_EOF) 1  ! no longer used
      
   end subroutine initialize

end module PFL_DynamicBuffer_mod
