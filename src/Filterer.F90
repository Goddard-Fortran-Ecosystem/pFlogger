!------------------------------------------------------------------------------
! NASA/GSFC, CISTO, Code 606, Advanced Software Technology Group
!------------------------------------------------------------------------------
!
! MODULE: ASTG_Filterer_mod
!
! AUTHOR: ASTG staff
!
! DESCRIPTION:
! A base class for loggers and handlers that allows them to share common code.
! Note that a filterer uses filters. Main funtion is called filter which
! determines if a LogRecord is "loggable".
!------------------------------------------------------------------------------
module ASTG_Filterer_mod
   use ASTG_Object_mod
   use ASTG_AbstractFilter_mod
   use ASTG_LogRecord_mod
   use FTL_AbstractFilterPolyWrapVec_mod
   implicit none
   private

   public :: Filterer

   type, extends(Object) :: Filterer
      private
      type (AbstractFilterPolyWrapVec) :: filters
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


   !---------------------------------------------------------------------------  
   ! FUNCTION: 
   ! newFilterer
   !
   ! DESCRIPTION: 
   ! Initializes list of filters to an empty list.
   !---------------------------------------------------------------------------
   function newFilterer() result(f)
      type (Filterer) :: f
      f%filters = AbstractFilterPolyWrapVec()
   end function newFilterer

   
   !---------------------------------------------------------------------------  
   ! ROUTINE: 
   ! addFilter
   !
   ! DESCRIPTION: 
   ! Add a filter to 'this' handler.
   !---------------------------------------------------------------------------
   subroutine addFilter(this, fltr)
      class (Filterer), intent(inout) :: this
      class (AbstractFilter), intent(in) :: fltr

      type (AbstractFilterPolyWrapVecIter) :: iter

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


   !---------------------------------------------------------------------------  
   ! FUNCTION: 
   ! filter_
   !
   ! DESCRIPTION: 
   ! Determine if a record is loggable by consulting all the filters.
   ! The default is to allow the record to be logged unless otherwise
   ! specified by the filter.  Returns FALSE if a record, else TRUE.
   !---------------------------------------------------------------------------
   logical function filter_(this, record)
      class (Filterer), intent(in) :: this
      class (LogRecord), intent(inout) :: record

      type (AbstractFilterPolyWrapVecIter) :: iter
      class (AbstractFilter), pointer :: fPtr

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


   !---------------------------------------------------------------------------  
   ! ROUTINE: 
   ! removeFilter
   !
   ! DESCRIPTION: 
   ! Remove filter from 'this' handler.
   !---------------------------------------------------------------------------
   subroutine removeFilter(this, f)
      use ASTG_Exception_mod
      class(Filterer), intent(inout) :: this
      class (AbstractFilter), intent(in) :: f

      type (AbstractFilterPolyWrapVecIter) :: iter
      class (AbstractFilter), pointer :: fPtr

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


   !---------------------------------------------------------------------------  
   ! FUNCTION: 
   ! getFilters
   !
   ! DESCRIPTION: 
   ! Get list of "this" filters.
   !---------------------------------------------------------------------------
  function getFilters(this) result(filters)
      type (AbstractFilterPolyWrapVec), pointer :: filters
      class (Filterer), target, intent(in) :: this

      filters => this%filters

   end function getFilters


end module ASTG_Filterer_mod
