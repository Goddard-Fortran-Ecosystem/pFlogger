module MockHandler_mod
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
      class (MockHandler), intent(in) :: this
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
