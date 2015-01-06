module MockMpi_mod
   use ASTG_MpiFilter_mod
   implicit none
   private

   public :: MockMpi
   public :: setMpiRank
   public :: MPI_Comm_rank
   public :: mocker
   
   type MockMpi
      integer :: rank
   end type MockMpi

   type (MockMpi), save :: mocker

contains

   subroutine setMpiRank(rank)
      integer, intent(in) :: rank
      type (MpiFilter) :: f
      
      mocker%rank = rank
      
   end subroutine setMpiRank

   subroutine MPI_Comm_rank(comm, rank, ier)
      integer, intent(in) :: comm
      integer, intent(out) :: rank
      integer, intent(inout) :: ier

      rank = mocker%rank
      ier = 0

   end subroutine MPI_Comm_rank

end module MockMpi_mod
