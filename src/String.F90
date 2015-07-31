!  This module provides a workaround for cases where logger must refer
!  to items through pointers to unlimited polymorphic entities.  GFortran 5.1
!  does not correctly keep length information in this scenario.

module ASTG_String_mod
   implicit none
   private

   public :: String
   
   type :: String
      character(len=:), allocatable :: str
   end type String

end module ASTG_String_mod
