module mpi
   use, intrinsic :: iso_fortran_env, only: INT64
   use funit
   implicit none
   private

   public :: mocker

   public :: set_mpi_rank
   public :: set_mpi_size
   public :: verify

   public :: MPI_COMM_WORLD
   public :: MPI_ADDRESS_KIND
   public :: MPI_STATUS_SIZE
   public :: MPI_STATUS_IGNORE
   public :: MPI_LOGICAL
   public :: MPI_SUCCESS
   public :: MPI_INFO_NULL
   public :: MPI_ANY_SOURCE
   public :: MPI_LOCK_EXCLUSIVE

   public :: MPI_WIN_CREATE

   integer, parameter :: MPI_COMM_WORLD = 0
   integer, parameter :: MPI_ADDRESS_KIND = INT64
   integer, parameter :: MPI_STATUS_SIZE = 6
   integer, parameter :: MPI_STATUS_IGNORE(MPI_STATUS_SIZE) = reshape([0], shape=[MPI_STATUS_SIZE], pad=[0])
   integer, parameter :: MPI_LOGICAL = 9
   integer, parameter :: MPI_SUCCESS = 0
   integer, parameter :: MPI_INFO_NULL = 0
   integer, parameter :: MPI_ANY_SOURCE = -1
   integer, parameter :: MPI_LOCK_EXCLUSIVE = 1

   public :: MPI_Alloc_mem
   public :: MPI_Type_indexed
   public :: MPI_Type_Commit


   ! Because this interface is overloaded (in theory), it cannot
   ! be accessed through "include 'mpif.h'".
   ! As such, we can include it in the mock implementation.

   interface MPI_Alloc_mem
      subroutine MPI_Alloc_mem_cptr(size, info, baseptr, ierror)
         use iso_c_binding, only: c_ptr, c_loc
         use iso_fortran_env, only: INT8
         import MPI_ADDRESS_KIND
         integer info, ierror
         integer(kind=MPI_ADDRESS_KIND) size
         type (c_ptr), intent(out) :: baseptr
      end subroutine MPI_Alloc_mem_cptr
   end interface

   interface MPI_Type_indexed
      subroutine MPI_Type_indexed(count, array_of_blocklengths, &
         array_of_displacements, oldtype, newtype, ierror)
         integer, intent(in) :: count, array_of_blocklengths(*)
         integer, intent(in) :: array_of_displacements(*), oldtype
         integer, intent(out) :: newtype
         integer, intent(out) :: ierror
      end subroutine MPI_TYPE_INDEXED
   end interface MPI_Type_indexed

   interface MPI_Type_Commit
      subroutine MPI_Type_commit(datatype, ierror)
         !   use mpi_base
         integer, intent(in) :: datatype
         integer, intent(out) :: ierror
         
      end subroutine MPI_Type_commit
   end interface MPI_Type_Commit


   interface MPI_Win_create
      procedure :: win_create_1
!!$      procedure :: win_create_2
   end interface MPI_Win_create

   type MockMpi
      integer :: rank
      integer :: size

      integer :: call_count = 0
      integer :: mpi_get_call_count = 0
      logical :: mpi_recv_has_expectation
      logical :: mpi_recv_is_called
      logical :: mpi_recv_was_called
      logical :: mpi_send_has_expectation
      logical :: mpi_send_is_called
      logical :: mpi_send_was_called
      logical, allocatable :: MPI_Get_buffer(:)
      logical, allocatable :: MPI_Get_buffer2(:)
   contains
      procedure :: reset
   end type MockMpi

   type (MockMpi), save :: mocker

contains


   subroutine reset(this)
      class (MockMpi), intent(inout) :: this
      this%call_count = 0
      this%mpi_recv_has_expectation = .false.
      this%mpi_recv_was_called = .false.
      this%mpi_send_has_expectation = .false.
      this%mpi_send_was_called = .false.
      if (allocated(this%mpi_get_buffer)) deallocate(this%mpi_get_buffer)
      if (allocated(this%mpi_get_buffer2)) deallocate(this%mpi_get_buffer2)
      this%mpi_get_call_count = 0
   end subroutine reset

 
subroutine set_mpi_rank(rank)
      integer, intent(in) :: rank

      mocker%rank = rank

   end subroutine set_mpi_rank


   subroutine set_mpi_size(size)
      integer, intent(in) :: size

      mocker%size = size

   end subroutine set_mpi_size


   subroutine set_MPI_Get(buffer, nth)
      logical, intent(in) :: buffer(:)
      integer, optional, intent(in) :: nth
      if (present(nth)) then
         mocker%MPI_Get_buffer2 = buffer
      else
         mocker%MPI_Get_buffer = buffer
      end if
   end subroutine set_MPI_Get


   subroutine set_MPI_Recv(is_called)
      logical, intent(in) :: is_called
      mocker%mpi_recv_has_expectation = .true.
      mocker%mpi_recv_is_called  = is_called
   end subroutine set_MPI_Recv


   subroutine set_MPI_Send(is_called)
      logical, intent(in) :: is_called
      mocker%mpi_send_has_expectation = .true.
      mocker%mpi_send_is_called  = is_called
   end subroutine set_MPI_Send
   
   subroutine verify()
      if (mocker%mpi_recv_has_expectation) then
         if (mocker%mpi_recv_is_called .neqv. mocker%mpi_recv_was_called) then
            call throw('Mismatched call to MPI_recv')
         end if
      end if
      if (mocker%mpi_send_has_expectation) then
         if (mocker%mpi_send_was_called .neqv. mocker%mpi_send_was_called) then
            call throw('Mismatched call to MPI_send')
         end if
      end if

      call mocker%reset()
   end subroutine verify


   subroutine Win_create_1(base, size, disp_unit, info, comm, win, ierror)
#ifdef SUPPORT_FOR_ASSUMED_TYPE   
     type(*) :: base(*)
#else
     logical :: base(*)
#endif
     integer(kind=MPI_ADDRESS_KIND) size
     integer disp_unit, info, comm, win, ierror
     
     ierror = MPI_SUCCESS
     mocker%call_count = mocker%call_count + 1
     
   end subroutine Win_create_1

end module mpi

! Implicit interface for actual subroutines
subroutine MPI_Comm_rank(comm, rank, ierror)
   use mpi
   integer, intent(in) :: comm
   integer, intent(out) :: rank
   integer, intent(inout) :: ierror

   rank = mocker%rank
   ierror = MPI_SUCCESS

   mocker%call_count = mocker%call_count + 1

end subroutine MPI_Comm_rank


! Implicit interface for actual subroutines
subroutine MPI_Comm_size(comm, size, ierror)
   use mpi
   integer, intent(in) :: comm
   integer, intent(out) :: size
   integer, intent(inout) :: ierror

   size = mocker%size
   ierror = MPI_SUCCESS

   mocker%call_count = mocker%call_count + 1

end subroutine MPI_Comm_size

subroutine MPI_Win_free(win, ierror)
   use mpi
   integer win, ierror
   ierror = MPI_SUCCESS
   mocker%call_count = mocker%call_count + 1
end subroutine MPI_Win_free

subroutine MPI_Win_lock(lock_type, rank, assert, win, ierror)
   use mpi
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
   use mpi
   integer, intent(in) :: rank
   integer, intent(in) :: win
   integer, intent(out) :: ierror

   ierror = MPI_SUCCESS
   mocker%call_count = mocker%call_count + 1

end subroutine MPI_Win_unlock

subroutine MPI_Get(origin_addr, origin_count, origin_datatype, target_rank, &
     & target_disp, target_count, target_datatype, win, ierror)
   use mpi
   use iso_c_binding, only: c_ptr, c_loc, c_f_pointer
#ifdef SUPPORT_FOR_ASSUMED_TYPE   
   type(*) :: origin_addr(*)
#else
   logical :: origin_addr(*)
#endif
   integer(kind=MPI_ADDRESS_KIND) target_disp
   integer origin_count, origin_datatype, target_rank, &
        & target_count, target_datatype, win, ierror

   mocker%mpi_get_call_count = mocker%mpi_get_call_count + 1

   block
     type (c_ptr) :: loc
     logical, pointer :: buffer(:)
#ifdef SUPPORT_FOR_C_LOC_ASSUMED_SIZE
     loc = c_loc(origin_addr(1))
     call c_f_pointer(loc, buffer, [1])
     select case (mocker%mpi_get_call_count)
     case (1)
        buffer = mocker%mpi_get_buffer
     case (2)
        buffer = mocker%mpi_get_buffer2
     end select
#endif
   end block
end subroutine MPI_Get

subroutine MPI_Put(origin_addr, origin_count, origin_datatype, target_rank, &
     & target_disp, target_count, target_datatype, win, ierror)
   use mpi
#ifdef SUPPORT_FOR_ASSUMED_TYPE   
   type(*) :: origin_addr(*)
#else
   logical :: origin_addr(*)
#endif
   integer(kind=mpi_address_kind) target_disp
   integer origin_count, origin_datatype, target_rank, target_count, &
        & target_datatype, win, ierror

   mocker%call_count = mocker%call_count + 1

end subroutine MPI_Put

subroutine MPI_Recv(buf, count, datatype, source, tag, comm, status, ierror)
   use mpi
#ifdef SUPPORT_FOR_ASSUMED_TYPE   
   type(*) :: buf(*)
#else
   logical :: buf(*)
#endif
   integer    count, datatype, source, tag, comm
   integer    status(MPI_STATUS_SIZE), ierror

   mocker%call_count = mocker%call_count + 1
   mocker%mpi_recv_was_called = .true.

   ierror = MPI_SUCCESS
end subroutine MPI_Recv

subroutine MPI_Send(buf, count, datatype, dest, tag, comm, ierror)
   use mpi
#ifdef SUPPORT_FOR_ASSUMED_TYPE   
   type(*) :: buf(*)
#else
   logical :: buf(*)
#endif
   integer    count, datatype, dest, tag, comm, ierror

   mocker%call_count = mocker%call_count + 1
   ierror = MPI_SUCCESS

end subroutine MPI_Send

subroutine MPI_Alloc_mem(size, info, baseptr, ierror)
   use mpi, only: MPI_ADDRESS_KIND
   use iso_c_binding, only: c_ptr, c_loc
   use iso_fortran_env, only: INT8

   integer info, ierror
   integer(kind=MPI_ADDRESS_KIND) size
   type (c_ptr), intent(out) :: baseptr
   
   integer(kind=INT8), pointer :: buffer(:)
  
   call MPI_Alloc_mem_cptr(size, info, baseptr, ierror) 
   
end subroutine MPI_Alloc_mem

subroutine MPI_Alloc_mem_cptr(size, info, baseptr, ierror)
   use mpi, only: mocker, MPI_ADDRESS_KIND
   use iso_c_binding, only: c_ptr, c_loc
   use iso_fortran_env, only: INT8

   integer info, ierror
   integer(kind=MPI_ADDRESS_KIND) size
   type (c_ptr), intent(out) :: baseptr

   integer(kind=INT8), pointer :: buffer(:)

   allocate(buffer(size))
#ifdef SUPPORT_FOR_C_LOC_ASSUMED_SIZE
   baseptr = c_loc(buffer)
#endif

   mocker%call_count = mocker%call_count + 1
   ierror = MPI_SUCCESS
end subroutine MPI_Alloc_mem_cptr


! just a stub
subroutine MPI_Comm_free(comm, ierror)
   use mpi
   integer, intent(in) :: comm
   integer ierror

   ierror = MPI_SUCCESS

end subroutine MPI_Comm_free

subroutine MPI_Free_mem(base, ierror)
   use mpi
#ifdef SUPPORT_FOR_ASSUMED_TYPE   
   type(*) :: base(*)
#else
   logical :: base(*)
#endif
   integer ierror

   mocker%call_count = mocker%call_count + 1
   ierror = MPI_SUCCESS

end subroutine MPI_Free_mem

! This one is just a stub for now
subroutine MPI_Comm_dup(comm, newcomm, ierror)
   use mpi
   integer, intent(in) :: comm
   integer, intent(out) :: newcomm
   integer, intent(out) :: ierror

   newcomm = comm
   ierror = MPI_SUCCESS
   mocker%call_count = mocker%call_count + 1

end subroutine MPI_Comm_dup


subroutine MPI_Type_indexed(count, array_of_blocklengths, array_of_displacements, oldtype, newtype, ierror)
   use mpi, only: mocker
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
   use mpi, only: mocker
   integer, intent(in) :: datatype
   integer, intent(out) :: ierror

   ierror = MPI_SUCCESS
   mocker%call_count = mocker%call_count + 1

end subroutine MPI_Type_commit


subroutine MPI_Type_extent(datatype, extent, ierror)
   use mpi, only: mocker
   integer, intent(in) :: datatype
   integer, intent(out) :: extent
   integer, intent(out) :: ierror

   extent = 1
   ierror = MPI_SUCCESS
   mocker%call_count = mocker%call_count + 1

end subroutine MPI_Type_extent


subroutine MPI_Type_free(datatype, ierror)
   use mpi
   integer    datatype, ierror
   ierror = MPI_SUCCESS
   mocker%call_count = mocker%call_count + 1
end subroutine MPI_Type_free


