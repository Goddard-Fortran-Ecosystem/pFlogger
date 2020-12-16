!------------------------------------------------------------------------------
! NASA/GSFC, CISTO, Code 606, Advanced Software Technology Group
!------------------------------------------------------------------------------
!
!*MODULE: PFL_MpiFilter
!
!> @brief Perform arbitrary filtering of LogRecords.
!> @details
!> MpiFilter class only allows records which are below a certain point in the
!> logger hierarchy. 
!
!> @author ASTG staff
!> @date 01 Jan 2015 - Initial Version  
!---------------------------------------------------------------------------
module PFL_MpiFilter
   use PFL_AbstractFilter
   use PFL_LogRecord
   use mpi
   implicit none
   private

   public :: MpiFilter

   type, extends(AbstractFilter) :: MpiFilter
      private
      integer :: communicator
      integer :: root
      logical :: shouldFilter
   contains
      procedure :: do_filter
      procedure :: equal
      procedure :: set_root
      procedure :: get_root
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
   function newMpiFilter(communicator, root) result(f)
      type (MpiFilter) :: f
      integer, intent(in) :: communicator
      integer, optional, intent(in) :: root
      integer :: root_, myRank
      integer :: ier

      f%communicator = communicator

      if (present(root)) then
         root_ = root
      else  
         root_ = 0
      end if
      f%root = root_
      
      f%shouldFilter = .false.
      call MPI_Comm_rank(communicator, myRank, ier)
      
      if (myRank == root_) f%shouldFilter = .true.
      
   end function newMpiFilter

   
   !---------------------------------------------------------------------------  
   ! FUNCTION: 
   ! do_filter
   !
   ! DESCRIPTION: 
   ! Determine if LogRecord can be logged.
   ! Is the specified record to be logged? Returns FALSE for no, TRUE for
   ! yes.
   !---------------------------------------------------------------------------
   logical function do_filter(this, record)
      class (MpiFilter), intent(in) :: this
      class (LogRecord), intent(in) :: record

      if (this%shouldFilter) then 
         do_filter = .true.  ! do emit
      else
         do_filter = .false.
      end if
      
   end function do_filter
   

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
         equal = (a%root == b%root)
      class default
         equal = .false.
      end select

   end function equal

   
   !---------------------------------------------------------------------------  
   ! ROUTINE: 
   ! set_root
   !
   ! DESCRIPTION: 
   ! Set the MPI root rank associated with this MPI filter.
   !---------------------------------------------------------------------------  
   subroutine set_root(this, root)
      class (MpiFilter) :: this
      integer, intent(in) :: root

      this%root = root
      
   end subroutine set_root

   
   !---------------------------------------------------------------------------  
   ! FUNCTION: 
   ! get_root
   !
   ! DESCRIPTION: 
   ! Get the MPI root rank associated with this MPI filter.
   !---------------------------------------------------------------------------  
   function get_root(this) result(root)
      class (MpiFilter) :: this
      integer :: root

      root = this%root
   end function get_root


end module PFL_MpiFilter
