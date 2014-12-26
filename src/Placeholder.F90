module ASTG_Placeholder_mod
   use FTL_CaseInsensitiveStringLoggerPolyUnorderedMap_mod
   use ASTG_AbstractLogger_mod
   implicit none
   private

   public :: Placeholder

   type, extends(AbstractLogger) :: Placeholder
      private
      type (CaseInsensitiveStringLoggerPolyUnorderedMap) :: children
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

      call p%setName('root')
      call p%setLevel(level)

   end function newPlaceholder


   subroutine addChild(this, child)
      class (Placeholder), intent(inout) :: this
      class (Logger), intent(in) :: child

   end subroutine addChild

end module ASTG_Placeholder_mod
