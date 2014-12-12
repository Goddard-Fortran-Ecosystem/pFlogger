module ASTG_AbstractHandler_mod
   implicit none
   private

   public :: AbstractHandler

   type, abstract :: AbstractHandler
      private
      integer :: level
   contains
      procedure(emit), deferred :: emit
      procedure(close), deferred :: close
      procedure :: setLevel
      procedure :: getLevel
   end type AbstractHandler

   abstract interface

     subroutine emit(this, message)
        import AbstractHandler
        class(AbstractHandler), intent(in) :: this
        character(len=*), intent(in) :: message
     end subroutine emit


     subroutine close(this)
        import AbstractHandler
        class(AbstractHandler), intent(inout) :: this
     end subroutine close

   end interface

   enum, bind(c)
      enumerator :: &
           & UNSET_LOGGING_LEVEL = 0, &
           & DEBUG_LOGGING_LEVEL = 1, &
           & INFO_LOGGING_LEVEL = 2, &
           & WARNING_LOGGING_LEVEL = 3, &
           & ERROR_LOGGING_LEVEL = 4, &
           & CRITICAL_LOGGING_LEVEL = 5
   end enum


 contains


   subroutine setLevel(this, level)
     class (AbstractHandler), intent(inout) :: this
     integer, intent(in) :: level
     this%level = level
   end subroutine setLevel

   
   integer function getLevel(this)
     class (AbstractHandler), intent(in) :: this
     getLevel = this%level
   end function getLevel

 
end module ASTG_AbstractHandler_mod
