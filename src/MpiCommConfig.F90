#include "error_handling_macros.fh"
module PFL_MpiCommConfig_mod
   use mpi
   use PFL_StringUnlimitedMap_mod
   use PFL_KeywordEnforcer_mod
   implicit none
   private

   public :: MpiCommConfig

   interface MpiCommConfig
      module procedure MPICommConfig_default_comm
      module procedure MPICommConfig_comm
      module procedure MPICommConfig_multi_comm
   end interface MpiCommConfig

contains


   function MpiCommConfig_Default_comm(unusable, rank_keyword, size_keyword) result(m)
      type (Map) :: m
      class (KeywordEnforcer), optional, intent(in) :: unusable
      character(len=*), optional, intent(in) :: rank_keyword
      character(len=*), optional, intent(in) :: size_keyword

      _UNUSED_DUMMY(unusable)

      m = MpiCommConfig(MPI_COMM_WORLD, unusable, rank_keyword, size_keyword)
      
   end function MpiCommConfig_Default_comm
   

   function MpiCommConfig_comm(mpi_communicator, unusable, rank_keyword, size_keyword) result(m)
      integer, intent(in) :: mpi_communicator
      type (Map) :: m
      class (KeywordEnforcer), optional, intent(in) :: unusable
      character(len=*), optional, intent(in) :: rank_keyword
      character(len=*), optional, intent(in) :: size_keyword

      integer :: rank
      integer :: npes
      integer :: ierror

      _UNUSED_DUMMY(unusable)

      call MPI_Comm_rank(mpi_communicator, rank, ierror)
      call MPI_Comm_size(mpi_communicator, npes, ierror)

      call m%insert(default(rank_keyword,'mpi_rank'), rank)
      call m%insert(default(size_keyword,'mpi_size'), npes)
      
   end function MpiCommConfig_comm


   function MpiCommConfig_multi_comm(mpi_communicators, unusable, &
        & rank_prefix, size_prefix) result(m)
      use PFL_FormatString_mod
      use PFL_ArgListUtilities_mod
      use PFL_UnlimitedVector_mod
            
      integer, intent(in) :: mpi_communicators(:)
      type (Map) :: m
      class (KeywordEnforcer), optional, intent(in) :: unusable
      character(len=*), optional, intent(in) :: rank_prefix
      character(len=*), optional, intent(in) :: size_prefix
      type (Vector) :: v

      integer :: rank
      integer :: npes
      integer :: ierror
      integer :: i
      character(len=:), allocatable :: suffix

      _UNUSED_DUMMY(unusable)

      do i = 1, size(mpi_communicators)
         call MPI_Comm_rank(mpi_communicators(i), rank, ierror)
         call MPI_Comm_size(mpi_communicators(i), npes, ierror)

#ifndef __GFORTRAN__         
         suffix = FormatString('_%i0', make_arg_vector(i))
#else
         v = make_arg_vector(i)
         suffix = FormatString('_%i0', v)
#endif

         call m%insert(default(rank_prefix,'mpi_rank')//suffix, rank)
         call m%insert(default(size_prefix,'mpi_size')//suffix, npes)
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

end module PFL_MpiCommConfig_mod