! Perform arbitrary filtering of LogRecords.
! MpiFilter class only allows records which are below a certain point in the
! logger hierarchy. 
module ASTG_MpiFilter_mod
   use ASTG_Filter_mod, only: FilterType => Filter
   use FTL_CaseInsensitiveString_mod
   use ASTG_LogRecord_mod
   implicit none
   private

   public :: MpiFilter

   type, extends(FilterType) :: MpiFilter
      private
      integer :: communicator
      integer :: rank
      logical :: shouldFilter
   contains
      procedure :: filter => filter_ ! name conflict
      procedure :: setRank
      procedure :: getRank
   end type MpiFilter

   interface MpiFilter
      module procedure newMpiFilter
   end interface MpiFilter

#ifdef USE_MPI
   include 'mpif.h'
#endif

contains


   ! Initialize filter with the name of the Logger
   function newMpiFilter(name, communicator, rank) result(f)
      type (MpiFilter) :: f
      character(len=*), intent(in) :: name
      integer, intent(in) :: communicator
      integer, optional, intent(in) :: rank
      integer :: rank_, myRank
#ifdef USE_MPI
      integer :: ier
#endif      
      call f%setName(name)
      f%communicator = communicator

      if (present(rank)) then
         rank_ = rank
      else  
         rank_ = 0
      end if
      f%rank = rank_
      
      f%shouldFilter = .false.
#ifdef USE_MPI
      call MPI_Comm_rank(communicator, myRank, ier)
#else
      myRank = 0
#endif      
      if (myRank == rank_) f%shouldFilter = .true.
      
   end function newMpiFilter

   
   subroutine setRank(this, rank)
      class (MpiFilter) :: this
      integer, intent(in) :: rank

      this%rank = rank
      
   end subroutine setRank

   
   function getRank(this) result(rank)
      class (MpiFilter) :: this
      integer :: rank

      rank = this%rank
   end function getRank

   ! Determine if LogRecord can be logged
   logical function filter_(this, record)
      class (MpiFilter), intent(in) :: this
      class (LogRecord), intent(inout) :: record

      character(len=:), allocatable :: recordName
      integer :: n

      if (this%shouldFilter) then 
         recordName = record%getName()
         n = len(this%getName())
         if (this%getName() == recordName(1:n)) then
            filter_ = .true.  ! do emit
          else
            filter_ = .false. ! do NOT emit
          end if
      else
         filter_ = .false.
      end if
      
   end function filter_


end module ASTG_MpiFilter_mod
