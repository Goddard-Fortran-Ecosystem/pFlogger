module ASTG_Filterer_mod
   use ASTG_Object_mod
   use ASTG_Filter_mod
   use ASTG_LogRecord_mod
   use FTL_FilterPolyWrapVec_mod
   implicit none
   private

   public :: Filterer

   type, extends(Object) :: Filterer
      private
      type (FilterPolyWrapVec) :: filters
   contains
      procedure :: addFilter
      procedure :: filter => filter_ ! avoid name conflict with class Filter.
      procedure :: removeFilter
      procedure :: getFilters
   end type Filterer


   interface Filterer
      module procedure newFilterer
   end interface Filterer


contains


   function newFilterer() result(f)
      type (Filterer) :: f
      f%filters = FilterPolyWrapVec()
   end function newFilterer


   subroutine addFilter(this, fltr)
      class (Filterer), intent(inout) :: this
      class (Filter), intent(in) :: fltr

      type (FilterPolyWrapVecIter) :: iter

      iter = this%filters%begin()
      do while (iter /= this%filters%end())
         if (fltr == iter%get_alt()) then
            ! duplicate - nothing to do
            return
         end if
         call iter%next()
      end do

      call this%filters%push_back(fltr)

   end subroutine addFilter

   logical function filter_(this, record)
      class (Filterer), intent(in) :: this
      class (LogRecord), intent(inout) :: record

      type (FilterPolyWrapVecIter) :: iter
      class (Filter), pointer :: fPtr

      filter_ = .true. ! unless

      iter = this%filters%begin()
      do while (iter /= this%filters%end())
         fPtr => iter%get_alt()
         if (.not. fPtr%filter(record)) then
            filter_ = .false.
            exit
         end if
         call iter%next()
      end do

   end function filter_


   subroutine removeFilter(this, f)
      use ASTG_Exception_mod
      class(Filterer), intent(inout) :: this
      class (Filter), intent(in) :: f

      type (FilterPolyWrapVecIter) :: iter
      class (Filter), pointer :: fPtr

      iter = this%filters%begin()
      do while (iter /= this%filters%end())
         if (f == iter%get_alt()) then
            iter = this%filters%erase(iter)
            return
         end if
         call iter%next()
      end do
      
      ! Only can get here if handler not found
      call throw('Filterer::removeFilter() - no such filter.')

   end subroutine removeFilter


   function getFilters(this) result(filters)
      type (FilterPolyWrapVec), pointer :: filters
      class (Filterer), target, intent(in) :: this

      filters => this%filters

   end function getFilters


end module ASTG_Filterer_mod
