module ASTG_AbstractHandler_mod
   public :: AbstractHandler
   public newHandler

   type, abstract :: AbstractHandler
     private
     integer :: level
   contains
     procedure :: setLevel
     procedure :: getLevel
   end type AbstractHandler

   abstract interface
     subroutine print(this, string)
       import AbstractHandler
       class(AbstractHandler), intent(in) :: this
       character(len=*) :: string
     end subroutine print
   end interface
   
   integer, parameter :: UNSET_LOGGING_LEVEL = 0
   integer, parameter :: DEBUG_LOGGING_LEVEL = 1
   integer, parameter :: INFO_LOGGING_LEVEL = 2
   integer, parameter :: WARNING_LOGGING_LEVEL = 3
   integer, parameter :: ERROR_LOGGING_LEVEL = 4
   integer, parameter :: CRITICAL_LOGGING_LEVEL = 5

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
