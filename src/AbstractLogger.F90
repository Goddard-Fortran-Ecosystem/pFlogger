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
   end type AbstractLogger


contains


   subroutine setParent(this, parent)
      class (AbstractLogger), intent(inout) :: this
      class (AbstractLogger), target, intent(in) :: parent

      this%parent => parent

   end subroutine setParent


end module ASTG_AbstractLogger_mod
