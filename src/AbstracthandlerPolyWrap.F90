

module FTL_AbstractHandlerPolyWrap_mod
      use ASTG_AbstractHandler_mod, only: AbstractHandler   
   implicit none
   private

   public AbstractHandlerPolyWrap

   type AbstractHandlerPolyWrap
      class(AbstractHandler), allocatable :: item
   contains
      procedure :: get

#ifdef __INTEL_COMPILER
      procedure :: copy
      generic :: assignment(=) => copy
#endif

   end type AbstractHandlerPolyWrap

   interface AbstractHandlerPolyWrap
      module procedure new_copy
   end interface AbstractHandlerPolyWrap


contains


   function new_copy(item) result(container)
      type (AbstractHandlerPolyWrap) :: container
      class(AbstractHandler), intent(in) :: item
      allocate(container%item, source=item)
   end function new_copy

   
   function get(this) result(item)
      class (AbstractHandlerPolyWrap), target, intent(in) :: this
      class(AbstractHandler), pointer :: item

      item => this%item

   end function get


#ifdef __INTEL_COMPILER
   subroutine copy(a, b)
      class(AbstractHandlerPolyWrap), intent(out) :: a
      class(AbstractHandlerPolyWrap), intent(in) :: b

      allocate(a%item, source=b%item)
   end subroutine copy
#endif


end module FTL_AbstractHandlerPolyWrap_mod

   
