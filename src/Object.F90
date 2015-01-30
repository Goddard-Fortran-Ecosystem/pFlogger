!------------------------------------------------------------------------------
! NASA/GSFC, CISTO, Code 606, Advanced Software Technology Group
!------------------------------------------------------------------------------
!
! MODULE: ASTG_Object_mod
!
! AUTHOR: ASTG staff
!
! DESCRIPTION:
!------------------------------------------------------------------------------
module ASTG_Object_mod
   implicit none
   private

   public :: Object
   
   type, abstract :: Object
      private
   contains
      procedure :: toString_self
      generic :: toString => toString_self
   end type Object


contains

   !---------------------------------------------------------------------------  
   ! FUNCTION: 
   ! toString_self
   !
   ! DESCRIPTION: 
   ! Empty implementation - avoids forcing subclasses from implementing,
   ! but interface is _alway_ available.
   !---------------------------------------------------------------------------
   function toString_self(this) result(string)
      class (Object), intent(in) :: this
      character(len=:), allocatable :: string
      string = ''
   end function toString_self

end module ASTG_Object_mod
