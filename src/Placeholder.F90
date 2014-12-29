! Instances of this class are used to take the place of 'nodes' for which
! no logger has been defined.
module ASTG_Placeholder_mod
   use FTL_CIStringAbstractLoggerPolyUMap_mod
   use ASTG_AbstractLogger_mod
   implicit none
   private

   public :: Placeholder

   type, extends(AbstractLogger) :: Placeholder
      private
      type (CIStringAbstractLoggerPolyUMap) :: children
   contains
      procedure :: addChild
   end type Placeholder

   
   interface Placeholder
      module procedure :: newPlaceholder
   end interface Placeholder


contains

   
   function newPlaceholder(level) result(p)
      type (Placeholder) :: p
      integer, intent(in) :: level !! NOT OPTIONAL !!

   end function newPlaceholder


   ! Add specified logger as a child of this placeholder
   subroutine addChild(this, child)
      class (Placeholder), intent(inout) :: this
      class (AbstractLogger), intent(in) :: child

   end subroutine addChild

end module ASTG_Placeholder_mod
