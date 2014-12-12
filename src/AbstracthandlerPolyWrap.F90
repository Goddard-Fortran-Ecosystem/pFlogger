module FTL_AbstracthandlerPolyWrap_mod
   use ASTG_AbstractHandler_mod, only: Abstracthandler   
   implicit none
   private

   public AbstracthandlerPolyWrap

   type AbstracthandlerPolyWrap
      class(AbstractHandler), allocatable :: item
   contains
      procedure :: get
   end type AbstracthandlerPolyWrap

   interface AbstracthandlerPolyWrap
      module procedure new_copy
   end interface AbstracthandlerPolyWrap


contains


   function new_copy(item) result(container)
      type (AbstracthandlerPolyWrap) :: container
      class(AbstractHandler), intent(in) :: item
      
      allocate(container%item, source=item)
      
   end function new_copy

   
   function get(this) result(item)
      class (AbstracthandlerPolyWrap), target, intent(in) :: this
      class(AbstractHandler), pointer :: item

      item => this%item

   end function get


end module FTL_AbstracthandlerPolyWrap_mod

   
