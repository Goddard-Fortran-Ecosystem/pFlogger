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
      procedure :: equal
   end type MockHandler

   interface MockHandler
      module procedure newMockHandler
   end interface MockHandler

   
contains

   function newMockHandler(buffer, level) result(handler)
      type (MockHandler) :: handler
      type (MockBuffer), target, intent(in) :: buffer
      integer, optional, intent(in) :: level

      handler%buffer => buffer
      if (present(level)) call handler%setLevel(level)

   end function newMockHandler

   
   subroutine emitMessage(this, record)
      class (MockHandler), intent(inout) :: this
      type (LogRecord) :: record

      this%buffer%buffer = record%getMessage()
      
   end subroutine emitMessage


   subroutine flushUnit(this)
      class (MockHandler), intent(inout) :: this
   end subroutine flushUnit


   subroutine close(this)
      class (MockHandler), intent(inout) :: this
   end subroutine close


   logical function equal(a, b)
      class (MockHandler), intent(in) :: a
      class (AbstractHandler), intent(in) :: b

      select type (b)
      class is (MockHandler)
         equal = associated(a%buffer, b%buffer) .and. (a%getLevel() == b%getLevel())
      class default
         equal = .false.
      end select

   end function equal

   
end module MockHandler_mod
