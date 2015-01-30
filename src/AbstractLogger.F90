!------------------------------------------------------------------------------
! NASA/GSFC, CISTO, Code 606, Advanced Software Technology Group
!------------------------------------------------------------------------------
!
! MODULE: ASTG_AbstractLogger_mod
!
! AUTHOR: ASTG staff
!
! DESCRIPTION:
! AbstractLogger class provides common code to use in Logger class.
!------------------------------------------------------------------------------
module ASTG_AbstractLogger_mod
   use ASTG_Filterer_mod
   implicit none
   private

   public :: AbstractLogger

   type, abstract, extends(Filterer) :: AbstractLogger
      private
      class (AbstractLogger), pointer :: parent => null()
   contains
      procedure :: setParent
      procedure :: getParent
   end type AbstractLogger


contains


   !---------------------------------------------------------------------------  
   ! ROUTINE: 
   ! setParent
   !
   ! DESCRIPTION: 
   ! Set parent for 'this' logger.
   !---------------------------------------------------------------------------
   subroutine setParent(this, parent)
      class (AbstractLogger), intent(inout) :: this
      class (AbstractLogger), target, intent(in) :: parent

      this%parent => parent

   end subroutine setParent

   
   !---------------------------------------------------------------------------  
   ! FUNCTION: 
   ! getParent
   !
   ! DESCRIPTION: 
   ! Get parent of "this" logger.
   !---------------------------------------------------------------------------
   function getParent(this) result(parent)
      class (AbstractLogger), pointer :: parent
      class (AbstractLogger), intent(in) :: this

      parent => this%parent

   end function getParent

end module ASTG_AbstractLogger_mod
