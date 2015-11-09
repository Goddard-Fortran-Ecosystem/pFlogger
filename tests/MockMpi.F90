module mpi_base
   include 'mpif.h'
end module mpi_base

module MockMpi_mod
   use pfunit_mod
   implicit none
   private

   public :: MockMpi
   public :: mocker

   public :: set_mpi_rank
   public :: set_mpi_size
   public :: set_mpi_get
   public :: verify

   type MockMpi
      integer :: rank
      integer :: size

      integer :: call_count = 0
      logical :: mpi_recv_was_called
      logical, allocatable :: MPI_Get_buffer(:)
   contains
      procedure :: reset
   end type MockMpi

   type (MockMpi), save :: mocker

contains


   subroutine reset(this)
      class (MockMpi), intent(inout) :: this
      this%call_count = 0
      this%mpi_recv_was_called = .false.
   end subroutine reset


   subroutine set_mpi_rank(rank)
      integer, intent(in) :: rank

      mocker%rank = rank

   end subroutine set_mpi_rank


   subroutine set_mpi_size(size)
      integer, intent(in) :: size

      mocker%size = size

   end subroutine set_mpi_size


   subroutine set_MPI_Get(buffer)
      logical, intent(in) :: buffer(:)
      mocker%MPI_Get_buffer = buffer
   end subroutine set_MPI_Get

   subroutine verify()
      if (.not. mocker%mpi_recv_was_called) then
         call throw('Expected call to MPI_recv')
      end if

   end subroutine verify

end module MockMpi_mod



module mpi
   use mpi_base
   use MockMpi_mod
   
!!$   interface
!!$      subroutine MPI_Alloc_mem_cptr(size, info, baseptr, ierror)
!!$         use iso_c_binding, only: c_ptr
!!$         import MPI_ADDRESS_KIND
!!$         integer info, ierror
!!$         integer(kind=MPI_ADDRESS_KIND) size
!!$         type (c_ptr), intent(out) :: baseptr
!!$      end subroutine MPI_Alloc_mem_cptr
!!$
!!$   end interface

end module mpi

! Implicit interface for actual subroutines
subroutine MPI_Comm_rank(comm, rank, ierror)
   use MockMpi_mod
   use mpi_base
   integer, intent(in) :: comm
   integer, intent(out) :: rank
   integer, intent(inout) :: ierror

   rank = mocker%rank
   ierror = MPI_SUCCESS

   mocker%call_count = mocker%call_count + 1

end subroutine MPI_Comm_rank


! Implicit interface for actual subroutines
subroutine MPI_Comm_size(comm, size, ierror)
   use MockMpi_mod
   use mpi_base
   integer, intent(in) :: comm
   integer, intent(out) :: size
   integer, intent(inout) :: ierror

   size = mocker%size
   ierror = MPI_SUCCESS

   mocker%call_count = mocker%call_count + 1

end subroutine MPI_Comm_size

subroutine MPI_Win_create(base, size, disp_unit, info, comm, win, ierror)
   use MockMpi_mod
   use mpi_base
   type(*) :: base(*)
   integer(kind=MPI_ADDRESS_KIND) size
   integer disp_unit, info, comm, win, ierror

   ierror = MPI_SUCCESS
   mocker%call_count = mocker%call_count + 1

end subroutine MPI_Win_create

subroutine MPI_Win_free(win, ierror)
   use MockMpi_mod
   use mpi_base
   integer win, ierror
   ierror = MPI_SUCCESS
   mocker%call_count = mocker%call_count + 1
end subroutine MPI_Win_free

subroutine MPI_Win_lock(lock_type, rank, assert, win, ierror)
   use MockMpi_mod
   use mpi_base
   integer, intent(in) :: lock_type
   integer, intent(in) :: rank
   integer, intent(in) :: assert
   integer, intent(out) :: win
   integer, intent(out) :: ierror

   win = 0
   ierror = MPI_SUCCESS
   mocker%call_count = mocker%call_count + 1

end subroutine MPI_Win_lock


subroutine MPI_Win_unlock(rank, win, ierror)
   use MockMpi_mod
   use mpi_base
   integer, intent(in) :: rank
   integer, intent(in) :: win
   integer, intent(out) :: ierror

   ierror = MPI_SUCCESS
   mocker%call_count = mocker%call_count + 1

end subroutine MPI_Win_unlock

subroutine MPI_Get(origin_addr, origin_count, origin_datatype, target_rank, &
     & target_disp, target_count, target_datatype, win, ierror)
   use MockMpi_mod
   use mpi_base
   use iso_c_binding, only: c_ptr, c_loc, c_f_pointer
   type(*) :: origin_addr(*)
   integer(kind=MPI_ADDRESS_KIND) target_disp
   integer origin_count, origin_datatype, target_rank, &
        & target_count, target_datatype, win, ierror

   mocker%call_count = mocker%call_count + 1

   block
     type (c_ptr) :: loc
     logical, pointer :: buffer(:)
     loc = c_loc(origin_addr(1))
     call c_f_pointer(loc, buffer, [1])
     buffer = .true.
   end block
end subroutine MPI_Get

subroutine MPI_Put(origin_addr, origin_count, origin_datatype, target_rank, &
     & target_disp, target_count, target_datatype, win, ierror)
   use MockMpi_mod
   use mpi_base
   type(*) :: origin_addr(*)
   integer(kind=mpi_address_kind) target_disp
   integer origin_count, origin_datatype, target_rank, target_count, &
        & target_datatype, win, ierror

   mocker%call_count = mocker%call_count + 1

end subroutine MPI_Put

subroutine MPI_Recv(buf, count, datatype, source, tag, comm, status, ierror)
   use MockMpi_mod
   use mpi_base
   type(*) :: buf(*)
   integer    count, datatype, source, tag, comm
   integer    status(MPI_STATUS_SIZE), ierror

   mocker%call_count = mocker%call_count + 1
   mocker%mpi_recv_was_called = .true.

   ierror = MPI_SUCCESS
end subroutine MPI_Recv

subroutine MPI_Send(buf, count, datatype, dest, tag, comm, ierror)
   use MockMpi_mod
   use mpi_base
   type(*) :: buf(*)
   integer    count, datatype, dest, tag, comm, ierror

   mocker%call_count = mocker%call_count + 1
   ierror = MPI_SUCCESS

end subroutine MPI_Send


subroutine MPI_Alloc_mem_cptr(size, info, baseptr, ierror)
   use MockMpi_mod
   use iso_c_binding, only: c_ptr, c_loc
   use mpi_base
   use iso_fortran_env, only: INT8
   integer info, ierror
   integer(kind=MPI_ADDRESS_KIND) size
   type (c_ptr), intent(out) :: baseptr

   integer(kind=INT8), pointer :: buffer(:)

   allocate(buffer(size))
   baseptr = c_loc(buffer)
   
   mocker%call_count = mocker%call_count + 1
   ierror = MPI_SUCCESS
end subroutine MPI_Alloc_mem_cptr


subroutine MPI_Free_mem(base, ierror)
   use MockMpi_mod
   use mpi_base
   type(*) :: base(*)
   integer ierror

   mocker%call_count = mocker%call_count + 1
   ierror = MPI_SUCCESS

end subroutine MPI_Free_mem

! This one is just a stub for now
subroutine MPI_Comm_dup(comm, newcomm, ierror)
   use MockMpi_mod
   use mpi_base
   integer, intent(in) :: comm
   integer, intent(out) :: newcomm
   integer, intent(out) :: ierror

   newcomm = comm
   ierror = MPI_SUCCESS
   mocker%call_count = mocker%call_count + 1

end subroutine MPI_Comm_dup


subroutine MPI_Type_indexed(count, array_of_blocklengths, array_of_displacements, oldtype, newtype, ierror)
   use MockMpi_mod
   use mpi_base
   integer, intent(in) :: count
   integer, intent(in) :: array_of_blocklengths(*)
   integer, intent(in) :: array_of_displacements(*)
   integer, intent(in) :: oldtype
   integer, intent(out) :: newtype
   integer, intent(out) :: ierror

   newtype = oldtype
   ierror = MPI_SUCCESS
   mocker%call_count = mocker%call_count + 1

end subroutine MPI_Type_indexed

subroutine MPI_Type_commit(datatype, ierror)
   use MockMpi_mod
   use mpi_base
   integer, intent(in) :: datatype
   integer, intent(out) :: ierror

   ierror = MPI_SUCCESS
   mocker%call_count = mocker%call_count + 1

end subroutine MPI_Type_commit


subroutine MPI_Type_extent(datatype, extent, ierror)
   use MockMpi_mod
   use mpi_base
   integer, intent(in) :: datatype
   integer, intent(out) :: extent
   integer, intent(out) :: ierror

   extent = 1
   ierror = MPI_SUCCESS
   mocker%call_count = mocker%call_count + 1

end subroutine MPI_Type_extent


subroutine MPI_Type_free(datatype, ierror)
   use MockMpi_mod
   use mpi_base
   integer    datatype, ierror
   ierror = MPI_SUCCESS
   mocker%call_count = mocker%call_count + 1
end subroutine MPI_Type_free

!!$subroutine mpi_init(ierror)
!!$   use MockMpi_mod
!!$   use mpi_base
!!$   integer    datatype, ierror
!!$   ierror = MPI_SUCCESS
!!$   mocker%call_count = mocker%call_count + 1
!!$end subroutine mpi_init

