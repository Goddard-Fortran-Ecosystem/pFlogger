!------------------------------------------------------------------------------
! NASA/GSFC, CISTO, Code 606, Advanced Software Technology Group
!------------------------------------------------------------------------------
!
! MODULE: ASTG_Filterer_mod
!
!> @author 
!> ASTG staff
!
! DESCRIPTION:
!> @brief
!> A base class for loggers and handlers that allows them to share common code.
!> Note that a filterer uses filters. Main funtion is called filter which
!> determines if a LogRecord is "loggable".
!
! REVISION HISTORY:
! 01 Jan 2015 - Initial Version
!------------------------------------------------------------------------------
module ASTG_Filterer_mod
   use ASTG_Object_mod
   use ASTG_AbstractFilter_mod
   use ASTG_LogRecord_mod
   use ASTG_AbstractFilterPolyVector_mod, only: FilterVector => Vector
   use ASTG_AbstractFilterPolyVector_mod, only: FilterVectorIterator => VectorIterator
   implicit none
   private

   public :: Filterer

   type, extends(Object) :: Filterer
      private
      type (FilterVector) :: filters
   contains
      procedure :: addFilter
      procedure :: doFilter
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
      f%filters = FilterVector()
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

      type (FilterVectorIterator) :: iter

      iter = this%filters%begin()
      do while (iter /= this%filters%end())
         if (fltr == iter%get()) then
            ! duplicate - nothing to do
            return
         end if
         call iter%next()
      end do

      call this%filters%push_back(fltr)

   end subroutine addFilter


   !---------------------------------------------------------------------------  
   ! FUNCTION: 
   ! doFilter
   !
   ! DESCRIPTION: 
   ! Determine if a record is loggable by consulting all the filters.
   ! The default is to allow the record to be logged unless otherwise
   ! specified by the filter.  Returns FALSE if a record, else TRUE.
   !---------------------------------------------------------------------------
   logical function doFilter(this, record)
      class (Filterer), intent(in) :: this
      class (LogRecord), intent(inout) :: record

      type (FilterVectorIterator) :: iter
      class (AbstractFilter), pointer :: fPtr

      doFilter = .true. ! unless

      iter = this%filters%begin()
      do while (iter /= this%filters%end())
         fPtr => iter%get()
         if (.not. fPtr%doFilter(record)) then
            doFilter = .false.
            exit
         end if
         call iter%next()
      end do

   end function doFilter


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

      type (FilterVectorIterator) :: iter
      class (AbstractFilter), pointer :: fPtr

      integer :: i
      
      i = this%filters%get_index(f)
      if (i > 0) then
         call this%filters%erase(this%filters%begin() + i - 1)
      else
         ! Only can get here if filter not found
         call throw('Filterer::removeFilter() - no such filter.')
      end if

   end subroutine removeFilter


   !---------------------------------------------------------------------------  
   ! FUNCTION: 
   ! getFilters
   !
   ! DESCRIPTION: 
   ! Get list of "this" filters.
   !---------------------------------------------------------------------------
  function getFilters(this) result(filters)
      type (FilterVector), pointer :: filters
      class (Filterer), target, intent(in) :: this

      filters => this%filters

   end function getFilters


end module ASTG_Filterer_mod
