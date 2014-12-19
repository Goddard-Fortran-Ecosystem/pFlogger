! This class is intended to minimally emulate the "Object" class in Java.
! Although the power in this Fortran after-thought is minimal, I find it
! useful for enforcing a small number of interfaces that I expect all
! (nontrivial) classes to support.

module FTL_Object_mod
   implicit none
   private

   public :: Object

   type, abstract :: Object
   contains
      procedure(toString), deferred :: toString
   end type Object

   abstract interface

      function toString(this) result(string)
         import Object
         class (Object), intent(in) :: this
         character(:), allocatable :: string
      end function toString

   end interface
   
end module FTL_Object_mod
