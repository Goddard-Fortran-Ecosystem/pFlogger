!------------------------------------------------------------------------------
! NASA/GSFC, CISTO, Code 606, Advanced Software Technology Group
!------------------------------------------------------------------------------
!
! MODULE: PFL_Filterer
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
#include "error_handling_macros.fh"
module PFL_Filterer
   use PFL_AbstractFilter, only: AbstractFilter
   use PFL_LogRecord, only: LogRecord
   use PFL_AbstractFilterPolyVector, only: FilterVector => Vector
   use PFL_AbstractFilterPolyVector, only: FilterVectorIterator => VectorIterator
   use PFL_Exception, only: throw
   implicit none
   private

   public :: Filterer

   type :: Filterer
      private
      type (FilterVector) :: filters
   contains
      procedure :: add_filter
      procedure :: do_filter
      procedure :: remove_filter
      procedure :: get_filters
   end type Filterer


   interface Filterer
      module procedure new_Filterer
   end interface Filterer


contains


   !---------------------------------------------------------------------------  
   ! FUNCTION: 
   ! new_Filterer
   !
   ! DESCRIPTION: 
   ! Initializes list of filters to an empty list.
   !---------------------------------------------------------------------------
   function new_Filterer() result(f)
      type (Filterer) :: f
      f%filters = FilterVector()
   end function new_Filterer

   
   !---------------------------------------------------------------------------  
   ! ROUTINE: 
   ! add_filter
   !
   ! DESCRIPTION: 
   ! Add a filter to 'this' handler.
   !---------------------------------------------------------------------------
   subroutine add_filter(this, fltr)
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

   end subroutine add_filter


   !---------------------------------------------------------------------------  
   ! FUNCTION: 
   ! do_filter
   !
   ! DESCRIPTION: 
   ! Determine if a record is loggable by consulting all the filters.
   ! The default is to allow the record to be logged unless otherwise
   ! specified by the filter.  Returns FALSE if a record, else TRUE.
   !---------------------------------------------------------------------------
   logical function do_filter(this, record)
      class (Filterer), intent(in) :: this
      class (LogRecord), intent(in) :: record

      type (FilterVectorIterator) :: iter
      class (AbstractFilter), pointer :: fPtr

      do_filter = .true. ! unless

      iter = this%filters%begin()
      do while (iter /= this%filters%end())
         fPtr => iter%get()
         if (.not. fPtr%do_filter(record)) then
            do_filter = .false.
            exit
         end if
         call iter%next()
      end do

   end function do_filter


   !---------------------------------------------------------------------------  
   ! ROUTINE: 
   ! remove_filter
   !
   ! DESCRIPTION: 
   ! Remove filter from 'this' handler.
   !---------------------------------------------------------------------------
   subroutine remove_filter(this, f, rc)
      class(Filterer), intent(inout) :: this
      class (AbstractFilter), intent(in) :: f
      integer, optional, intent(out) :: rc

      type (FilterVectorIterator) :: iter
      class (AbstractFilter), pointer :: fPtr

      integer :: i
      integer :: status
      
      i = this%filters%get_index(f)
      if (i > 0) then
         call this%filters%erase(this%filters%begin() + i - 1)
      else
         ! Only can get here if filter not found
         _ASSERT(.false., 'Filterer::remove_filter() - no such filter.', rc)
      end if
      _RETURN(_SUCCESS,rc)
   end subroutine remove_filter


   !---------------------------------------------------------------------------  
   ! FUNCTION: 
   ! get_filters
   !
   ! DESCRIPTION: 
   ! Get list of "this" filters.
   !---------------------------------------------------------------------------
  function get_filters(this) result(filters)
      type (FilterVector), pointer :: filters
      class (Filterer), target, intent(in) :: this

      filters => this%filters

   end function get_filters


end module PFL_Filterer
