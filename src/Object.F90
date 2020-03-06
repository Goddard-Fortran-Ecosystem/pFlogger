!------------------------------------------------------------------------------
! NASA/GSFC, CISTO, Code 606, Advanced Software Technology Group
!------------------------------------------------------------------------------
!
!*MODULE: PFL_Object
!
!> @brief Parent class of all other classes.
!> @details
!> An "object" is the "class" that you inherit from to make a class. So:
!>     a class is-a object.
!> @note However, not all logger framework classes inherit from Object.
!
!> @author ASTG staff
!> @date 01 Jan 2015 - Initial Version  
!---------------------------------------------------------------------------
module PFL_Object
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

end module PFL_Object
