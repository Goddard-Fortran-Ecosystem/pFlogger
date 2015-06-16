!------------------------------------------------------------------------------
! NASA/GSFC, CISTO, Code 606, Advanced Software Technology Group
!------------------------------------------------------------------------------
!
! MODULE: ASTG_PlaceHolder_mod
!
!> @author 
!> ASTG staff
!
! DESCRIPTION:
!> @brief
!> Instances of this class are used to take the place of 'nodes' for which
!> no logger has been defined.
!
! REVISION HISTORY:
! 01 Jan 2015 - Initial Version
! TODO: Is this class necessary?
!------------------------------------------------------------------------------
module ASTG_Placeholder_mod
   use ASTG_CIStringAbstractLoggerPolyMap_mod, only: LoggerMap => Map
   use ASTG_AbstractLogger_mod
   implicit none
   private

   public :: Placeholder

   type, extends(AbstractLogger) :: Placeholder
      private
      type (LoggerMap) :: children
   contains
      procedure :: addChild
   end type Placeholder

   
   interface Placeholder
      module procedure newPlaceholder
   end interface Placeholder


contains

   
   !---------------------------------------------------------------------------  
   ! FUNCTION: 
   ! newPlaceholder
   !
   ! DESCRIPTION: 
   ! Initialize a placeholder.
   !---------------------------------------------------------------------------
   function newPlaceholder(level) result(p)
      type (Placeholder) :: p
      integer, intent(in) :: level !! NOT OPTIONAL !!

   end function newPlaceholder


   !---------------------------------------------------------------------------  
   ! FUNCTION: 
   ! addChild
   !
   ! DESCRIPTION: 
   ! Add specified logger as a child of this placeholder
   !---------------------------------------------------------------------------
   subroutine addChild(this, child)
      class (Placeholder), intent(inout) :: this
      class (AbstractLogger), intent(in) :: child

   end subroutine addChild

end module ASTG_Placeholder_mod
