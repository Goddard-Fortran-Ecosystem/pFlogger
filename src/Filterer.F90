module ASTG_Filterer_mod
   use ASTG_Filter_mod
   use ASTG_LogRecord_mod
   use FTL_FilterPolyWrapVector_mod
   implicit none
   private

   public :: Filterer

   type Filterer
      private
      type (FilterPolyWrapVector) :: filters
   contains
      procedure :: addFilter
      procedure :: filter => filter_ ! avoid name conflict with class Filter.
   end type Filterer


   interface Filterer
      module procedure newFilterer
   end interface Filterer


contains


   function newFilterer() result(f)
      type (Filterer) :: f
      f%filters = FilterPolyWrapVector()
   end function newFilterer


   subroutine addFilter(this, fltr)
      class (Filterer), intent(inout) :: this
      class (Filter), intent(in) :: fltr

      call this%filters%push_back(fltr)

   end subroutine addFilter

   logical function filter_(this, record)
      class (Filterer), intent(in) :: this
      class (LogRecord), intent(in) :: record
      
      filter_ = .true.

   end function filter_

end module ASTG_Filterer_mod
