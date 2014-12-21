

module FTL_FilterPolyWrap_mod
      use ASTG_Filter_mod, only: Filter   
   implicit none
   private

   public FilterPolyWrap

   type FilterPolyWrap
      class(Filter), allocatable :: item
   contains
      procedure :: get

#ifdef __INTEL_COMPILER
      procedure :: copy
      generic :: assignment(=) => copy
#endif

   end type FilterPolyWrap

   interface FilterPolyWrap
      module procedure new_copy
   end interface FilterPolyWrap


contains


   function new_copy(item) result(container)
      type (FilterPolyWrap) :: container
      class(Filter), intent(in) :: item
      allocate(container%item, source=item)
   end function new_copy

   
   function get(this) result(item)
      class (FilterPolyWrap), target, intent(in) :: this
      class(Filter), pointer :: item

      item => this%item

   end function get


#ifdef __INTEL_COMPILER
   subroutine copy(a, b)
      class(FilterPolyWrap), intent(out) :: a
      class(FilterPolyWrap), intent(in) :: b

      allocate(a%item, source=b%item)
   end subroutine copy
#endif


end module FTL_FilterPolyWrap_mod

   
