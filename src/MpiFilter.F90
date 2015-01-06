! Perform arbitrary filtering of LogRecords.
! MpiFilter class only allows records which are below a certain point in the
! logger hierarchy. 
module ASTG_MpiFilter_mod
   use ASTG_AbstractFilter_mod
   use FTL_CaseInsensitiveString_mod
   use ASTG_LogRecord_mod
   implicit none
   private

   public :: MpiFilter

   type, extends(AbstractFilter) :: MpiFilter
      private
      integer :: communicator
      integer :: rank
      logical :: shouldFilter
   contains
      procedure :: filter !=> filter_ ! name conflict
      procedure :: equal
      procedure :: setRank
      procedure :: getRank
   end type MpiFilter

   interface MpiFilter
      module procedure newMpiFilter
   end interface MpiFilter

contains


   ! Initialize filter with the name of the Logger
   function newMpiFilter(communicator, rank) result(f)
      type (MpiFilter) :: f
      integer, intent(in) :: communicator
      integer, optional, intent(in) :: rank
      integer :: rank_, myRank
      integer :: ier

      f%communicator = communicator

      if (present(rank)) then
         rank_ = rank
      else  
         rank_ = 0
      end if
      f%rank = rank_
      
      f%shouldFilter = .false.
      call MPI_Comm_rank(communicator, myRank, ier)
      
      if (myRank == rank_) f%shouldFilter = .true.
      
   end function newMpiFilter

   
   ! Determine if LogRecord can be logged
   logical function filter(this, record)
      class (MpiFilter), intent(in) :: this
      class (LogRecord), intent(inout) :: record

      character(len=:), allocatable :: recordName
      integer :: n

      if (this%shouldFilter) then 
         filter = .true.  ! do emit
      else
         filter = .false.
      end if
      
   end function filter
   

   logical function equal(a, b)
      class(MpiFilter), intent(in) :: a
      class(AbstractFilter), intent(in) :: b

      select type (b)
      type is (MpiFilter)
         equal = (a%rank == b%rank)
      class default
         equal = .false.
      end select

   end function equal

   
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


end module ASTG_MpiFilter_mod
