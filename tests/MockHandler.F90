module MockHandler_mod
   ! Instances of this class inherit from the abstract handler. The idea of 
   ! the mock is to be able to control the data that is written to an output
   ! stream (STDOUT or a disk file) by collecting it in a buffer and then 
   ! test for the contents of the buffer's correctnes.
   use ASTG_AbstractHandler_mod
   implicit none
   private
   
   public :: MockHandler
   public :: buffer

   character(len=:), allocatable :: buffer

   type, extends (AbstractHandler) :: MockHandler
   contains
      procedure :: emitMessage
      procedure :: close ! noop
      procedure :: flush => flushUnit
   end type MockHandler

   
contains

   
   subroutine emitMessage(this, levelString, message)
      class (MockHandler), intent(inout) :: this
      character(len=*), intent(in) :: levelString
      character(len=*), intent(in) :: message

      buffer = levelString // ': ' // message
      
   end subroutine emitMessage


   subroutine flushUnit(this)
      class (MockHandler), intent(inout) :: this
   end subroutine flushUnit


   subroutine close(this)
      class (MockHandler), intent(inout) :: this
   end subroutine close

end module MockHandler_mod
