module MockHandler_mod
   ! Instances of this class inherit from the abstract handler. The idea of 
   ! the mock is to be able to control the data that is written to an output
   ! stream (STDOUT or a disk file) by collecting it in a buffer and then 
   ! test for the contents of the buffer's correctnes.
   use ASTG_AbstractHandler_mod
   use ASTG_LogRecord_mod
   implicit none
   private
   
   public :: MockHandler
   public :: MockBuffer

   type MockBuffer
      character(len=:), allocatable :: buffer
   end type MockBuffer

   type, extends (AbstractHandler) :: MockHandler
      type (MockBuffer), pointer :: buffer
   contains
      procedure :: emitMessage
      procedure :: close ! noop
      procedure :: flush => flushUnit
   end type MockHandler

   interface MockHandler
      module procedure newMockHandler
   end interface MockHandler

   
contains

   function newMockHandler(buffer) result(handler)
      type (MockHandler) :: handler
      type (MockBuffer), target :: buffer

      handler%buffer => buffer

   end function newMockHandler

   
   subroutine emitMessage(this, levelString, record)
      class (MockHandler), intent(inout) :: this
      character(len=*), intent(in) :: levelString
      type (LogRecord) :: record

      this%buffer%buffer = levelString // ': ' // record%getMessage()
      
   end subroutine emitMessage


   subroutine flushUnit(this)
      class (MockHandler), intent(inout) :: this
   end subroutine flushUnit


   subroutine close(this)
      class (MockHandler), intent(inout) :: this
   end subroutine close

end module MockHandler_mod
