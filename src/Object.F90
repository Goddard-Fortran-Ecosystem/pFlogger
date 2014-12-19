module ASTG_Object_mod
   implicit none
   private

   public :: Object
   
   type, abstract :: Object
      private
   contains
      procedure :: toString
   end type Object


contains

   ! Empty implementation - avoids forcing subclasses from implementing,
   ! but interface is _alway_ available.
   function toString(this) result(string)
      class (Object), intent(in) :: this
      character(len=:), allocatable :: string
   end function toString

end module ASTG_Object_mod
