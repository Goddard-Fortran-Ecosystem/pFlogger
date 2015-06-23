!------------------------------------------------------------------------------
! NASA/GSFC, CISTO, Code 606, Advanced Software Technology Group
!------------------------------------------------------------------------------
!
! MODULE: ASTG_MpiFilter_mod
!
!> @author 
!> ASTG staff
!
! DESCRIPTION:
!> @brief
!> Perform arbitrary filtering of LogRecords.
!> MpiFilter class only allows records which are below a certain point in the
!> logger hierarchy. 
!
! REVISION HISTORY:
! 01 Jan 2015 - Initial Version
!------------------------------------------------------------------------------
module ASTG_MpiFilter_mod
   use ASTG_AbstractFilter_mod
   use ASTG_CaseInsensitiveString_mod
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
      procedure :: doFilter
      procedure :: equal
      procedure :: setRank
      procedure :: getRank
   end type MpiFilter

   interface MpiFilter
      module procedure newMpiFilter
   end interface MpiFilter

contains


   !---------------------------------------------------------------------------  
   ! FUNCTION: 
   ! newFilter
   !
   ! DESCRIPTION: 
   ! Initialize filter with the MPI communicator and, optionally, its MPI rank.
   ! That is, only allow messages that have the specified rank. If rank is not
   ! specified then print to root rank.
   !---------------------------------------------------------------------------
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

   
   !---------------------------------------------------------------------------  
   ! FUNCTION: 
   ! Filter
   !
   ! DESCRIPTION: 
   ! Determine if LogRecord can be logged.
   ! Is the specified record to be logged? Returns FALSE for no, TRUE for
   ! yes.
   !---------------------------------------------------------------------------
   logical function doFilter(this, record)
      class (MpiFilter), intent(in) :: this
      class (LogRecord), intent(inout) :: record

      character(len=:), allocatable :: recordName
      integer :: n

      if (this%shouldFilter) then 
         doFilter = .true.  ! do emit
      else
         doFilter = .false.
      end if
      
   end function doFilter
   

   !---------------------------------------------------------------------------  
   ! FUNCTION: 
   ! equal
   !
   ! DESCRIPTION: 
   ! Overloads 'equal' operation for MPI filters.
   !---------------------------------------------------------------------------  
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

   
   !---------------------------------------------------------------------------  
   ! ROUTINE: 
   ! setRank
   !
   ! DESCRIPTION: 
   ! Set the MPI rank associated with this MPI filter.
   !---------------------------------------------------------------------------  
   subroutine setRank(this, rank)
      class (MpiFilter) :: this
      integer, intent(in) :: rank

      this%rank = rank
      
   end subroutine setRank

   
   !---------------------------------------------------------------------------  
   ! FUNCTION: 
   ! getRank
   !
   ! DESCRIPTION: 
   ! Get the MPI rank associated with this MPI filter.
   !---------------------------------------------------------------------------  
   function getRank(this) result(rank)
      class (MpiFilter) :: this
      integer :: rank

      rank = this%rank
   end function getRank


end module ASTG_MpiFilter_mod
