

module FTL_XWrap_mod
   
   implicit none
   private

   public XWrap

   type XWrap
      class(*), allocatable :: item
   contains
      procedure :: get

#ifdef __INTEL_COMPILER
      procedure :: copy
      generic :: assignment(=) => copy
#endif

   end type XWrap

   interface XWrap
      module procedure new_copy
   end interface XWrap


contains


   function new_copy(item) result(container)
      type (XWrap) :: container
      class(*), intent(in) :: item
      allocate(container%item, source=item)
   end function new_copy

   
   function get(this) result(item)
      class (XWrap), target, intent(in) :: this
      class(*), pointer :: item

      item => this%item

   end function get


#ifdef __INTEL_COMPILER
   subroutine copy(a, b)
      class(XWrap), intent(out) :: a
      class(XWrap), intent(in) :: b

      allocate(a%item, source=b%item)
   end subroutine copy
#endif


end module FTL_XWrap_mod

   
