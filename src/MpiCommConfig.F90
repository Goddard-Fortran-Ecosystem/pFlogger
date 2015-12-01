module astg_MpiCommConfig_mod
   use mpi
   use astg_StringUnlimitedMap_mod, only: StringUnlimitedMap => map
   implicit none
   private

   public :: MpiCommConfig

   interface MpiCommConfig
      module procedure MPICommConfig_default_comm
      module procedure MPICommConfig_comm
            module procedure MPICommConfig_multi_comm
   end interface MpiCommConfig

   type Unusable
   end type Unusable
contains


   function MpiCommConfig_Default_comm(unused, rank_keyword, size_keyword) result(cfg)
      type (StringUnlimitedMap) :: cfg
      type (Unusable), optional, intent(in) :: unused
      character(len=*), optional, intent(in) :: rank_keyword
      character(len=*), optional, intent(in) :: size_keyword

      cfg = MpiCommConfig(MPI_COMM_WORLD, unused, rank_keyword, size_keyword)
      
   end function MpiCommConfig_Default_comm
   

   function MpiCommConfig_comm(mpi_communicator, unused, rank_keyword, size_keyword) result(cfg)
      integer, intent(in) :: mpi_communicator
      type (StringUnlimitedMap) :: cfg
      type (Unusable), optional, intent(in) :: unused
      character(len=*), optional, intent(in) :: rank_keyword
      character(len=*), optional, intent(in) :: size_keyword

      integer :: rank
      integer :: npes
      integer :: ierror

      call MPI_Comm_rank(mpi_communicator, rank, ierror)
      call MPI_Comm_size(mpi_communicator, npes, ierror)

      call cfg%insert(default(rank_keyword,'mpi_rank'), rank)
      call cfg%insert(default(size_keyword,'mpi_size'), npes)
      
   end function MpiCommConfig_comm


   function MpiCommConfig_multi_comm(mpi_communicators, unused, &
        & rank_prefix, size_prefix) result(cfg)
      use astg_FormatString_mod
      use astg_ArgListUtilities_mod
      use ASTG_UnlimitedVector_mod
            
      integer, intent(in) :: mpi_communicators(:)
      type (StringUnlimitedMap) :: cfg
      type (Unusable), optional, intent(in) :: unused
      character(len=*), optional, intent(in) :: rank_prefix
      character(len=*), optional, intent(in) :: size_prefix
      type (Vector) :: v

      integer :: rank
      integer :: npes
      integer :: ierror
      integer :: i
      character(len=:), allocatable :: suffix

      do i = 1, size(mpi_communicators)
         call MPI_Comm_rank(mpi_communicators(i), rank, ierror)
         call MPI_Comm_size(mpi_communicators(i), npes, ierror)

#ifndef __GFORTRAN__         
         suffix = FormatString('_%i0', makeArgVector(i))
#else
         v = makeArgVector(i)
         suffix = FormatString('_%i0', v)
#endif

         call cfg%insert(default(rank_prefix,'mpi_rank')//suffix, rank)
         call cfg%insert(default(size_prefix,'mpi_size')//suffix, npes)
      end do
      
   end function MpiCommConfig_multi_comm
   

   ! helper funnction for convenience with optional arguments that
   ! have default values.
   function default(x_optional, x_default) result(x)
      character(len=:), allocatable :: x
      character(len=*), optional, intent(in) :: x_optional
      character(len=*), intent(in) :: x_default
      
      if (present(x_optional)) then
         x = x_optional
      else
         x = x_default
      end if
      
   end function default

end module astg_MpiCommConfig_mod
