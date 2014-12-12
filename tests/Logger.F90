module Test_Logger_mod
   use ASTG_Logger_mod
   use MockHandler_mod
   use ASTG_SeverityLevels_mod
   use pfunit_mod
   implicit none

   public :: Test_Logger

@TestCase
   type, extends(TestCase) :: Test_Logger
      type (Logger) :: aLogger
   contains
      procedure :: setUp     ! overides generic
      procedure :: tearDown  ! overrides generic
   end type Test_Logger

   
contains

   
   subroutine setUp(this)
      class (Test_Logger), intent(inout) :: this
      type (MockHandler) :: handler

      call handler%setLevel(info)
      this%aLogger = Logger()
      call this%aLogger%addHandler(handler)
      
   end subroutine setUp

   
   subroutine tearDown(this)
      class (Test_Logger), intent(inout) :: this
      if (allocated(buffer)) deallocate(buffer)
   end subroutine tearDown

@test
   subroutine test_logBasic(this)
      class (Test_Logger), intent(inout) :: this

      call this%aLogger%setLevel(INFO)
      call this%aLogger%log(INFO, 'hello')

      ! buffer is in MockHandler_mod
      @assertEqual('INFO: hello', buffer)

   end subroutine test_logBasic

@test
   subroutine test_logBelowThreshold(this)
      class (Test_Logger), intent(inout) :: this
      type (MockHandler) :: handler

      call handler%setLevel(info)
      
      call this%aLogger%addHandler(handler)
      call this%aLogger%log(DEBUG, 'hello')

      @assertFalse(allocated(buffer))

   end subroutine test_logBelowThreshold

   
!!$ @Test
!!$   subroutine test_logfile(this)
!!$      use ASTG_AbstractHandler_mod
!!$      use FTL_AbstracthandlerPolyWrap_mod
!!$      use FTL_AbstracthandlerPolyWrapVector_mod
!!$      class (Test_Logger), intent(inout) :: this
!!$
!!$      integer :: unit
!!$      character(len=:), allocatable :: message
!!$      character(len=32) :: foundMessage
!!$
!!$      type (AbstractHandlerPolyWrapVectorIterator) :: iter
!!$
!!$      type (AbstractHandlerPolyWrapVector), pointer :: handlers
!!$      type (AbstractHandlerPolyWrap), pointer :: handlerWrap
!!$      class (AbstractHandler), pointer :: handler
!!$
!!$      message = 'hello'
!!$      call this%fileLogger%log(INFO, message)
!!$
!!$      handlers => this%fileLogger%getHandlers()
!!$
!!$      iter = handlers%begin()
!!$      do while (iter /= handlers%end())
!!$         handlerWrap => iter%get()
!!$         handler => handlerWrap%get()
!!$         call handler%close()
!!$         call iter%next()
!!$      end do
!!$
!!$      open(newunit=unit, file='logfile', status='old')
!!$      read(unit, '(a)') foundMessage
!!$      close(unit)
!!$
!!$      @assertEqual(message, foundMessage)
!!$      
!!$   end subroutine test_logfile
!!$   
    
end module Test_Logger_mod

