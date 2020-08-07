! Based upon
! http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.113.30&rep=rep1&type=pdf

module PFL_MpiLock
   use mpi
   use PFL_AbstractLock
   use iso_fortran_env, only: INT64
   use iso_c_binding, only: c_ptr, c_f_pointer
   implicit none
   private

   public :: MpiLock

   type, extends(AbstractLock) :: MpiLock
      integer :: comm
      integer :: npes
      integer :: rank
      integer :: window
      integer :: pe_locks_type
      type (c_ptr) :: locks_ptr
      logical, allocatable :: local_data(:)
      logical :: initialized
   contains
      procedure :: acquire
      procedure :: release
      procedure :: init
      procedure :: destroy
      procedure :: is_initialized
   end type MpiLock

   integer, parameter :: LOCK_TAG = 10

   interface MpiLock
      module procedure newMpiLock
   end interface MpiLock

contains


   !------------
   ! Constructor defers action until init() so that copies of an
   ! MpiLock object act independently.
   ! Otherwise, simple copies would all share the same Mpi window.
   !------------
   function newMpiLock(comm) result(lock)
      type (MpiLock) :: lock
      integer, intent(in) :: comm

      lock%comm = comm  ! will duplicate during init()
      lock%initialized = .false.
   end function newMpiLock


   subroutine init(this)
      class (MpiLock), intent(inout) :: this

      integer :: ierror
      integer :: sizeof_logical
      integer(kind=MPI_ADDRESS_KIND) :: sz
      integer :: old_comm

      if (this%is_initialized()) return
      
      old_comm = this%comm
      call MPI_Comm_dup(old_comm, this%comm, ierror)
      call MPI_Comm_rank(this%comm, this%rank, ierror)
      call MPI_Comm_size(this%comm, this%npes, ierror)

      ! This type is used to copy the status of locks on other PE's
      ! into a table that can be examined on the local process.
      block
        integer :: blklens(2)
        integer :: displs(2)
        blklens = [this%rank, this%npes - this%rank - 1]
        displs = [0, this%rank + 1]
        call MPI_Type_indexed(2, blklens, displs, MPI_LOGICAL, this%pe_locks_type, ierror);
        call MPI_Type_commit(this%pe_locks_type, ierror)
      end block

      ! Create windows
      if (this%rank == 0) then

         block
           logical, pointer :: scratchpad(:)
           integer :: sizeof_logical

           call MPI_Type_extent(MPI_LOGICAL, sizeof_logical, ierror)
           sz = this%npes * sizeof_logical
           call MPI_Alloc_mem(sz, MPI_INFO_NULL, this%locks_ptr, ierror)

           call c_f_pointer(this%locks_ptr, scratchpad, [this%npes])
           scratchpad = .false.

           call MPI_Win_create(scratchpad, sz, sizeof_logical, &
                & MPI_INFO_NULL, this%comm, this%window, ierror)
         end block

      else ! local window memory is size 0, but have to pass something
         block
           logical :: buffer(1)
           sz = 0
           call MPI_Win_create(buffer, sz, 1, MPI_INFO_NULL, this%comm, this%window, ierror)
         end block
      end if

      allocate(this%local_data(this%npes-1))
      this%initialized = .true.
    end subroutine init


   subroutine acquire(this)
      class (MpiLock), intent(inout) :: this

      integer :: ierror

      call MPI_Win_lock(MPI_LOCK_EXCLUSIVE, 0, 0, this%window, ierror)
      call MPI_Get(this%local_data, this%npes-1, MPI_LOGICAL, 0, &
           & 0_MPI_ADDRESS_KIND, 1, this%pe_locks_type, this%window, ierror)
      call MPI_Put(.true., 1, MPI_LOGICAL, 0, int(this%rank,kind=MPI_ADDRESS_KIND), &
           & 1, MPI_LOGICAL, this%window, ierror)

      call MPI_Win_unlock(0, this%window, ierror)

      ! Check other processes for holding the lock
      if (any(this%local_data)) then ! wait for signal from process with the lock
         block
           integer :: buffer ! unused
           call MPI_Recv(buffer, 0, MPI_LOGICAL, MPI_ANY_SOURCE, &
                & LOCK_TAG, this%comm, MPI_STATUS_IGNORE, ierror)
         end block
      end if

   end subroutine acquire



   subroutine release(this)
      class (MpiLock), intent(inout) :: this

      integer :: ierror

      call MPI_Win_lock(MPI_LOCK_EXCLUSIVE, 0, 0, this%window, ierror)
      call MPI_Get(this%local_data, this%npes-1, MPI_LOGICAL, 0, &
           & 0_MPI_ADDRESS_KIND, 1, this%pe_locks_type, this%window, ierror)
      call MPI_Put(.false., 1, MPI_LOGICAL, 0, int(this%rank,kind=MPI_ADDRESS_KIND), &
           & 1, MPI_LOGICAL, this%window, ierror)
      call MPI_Win_unlock(0, this%window, ierror)

      ! who needs the lock next (if anyone)?
      block
        integer :: p, next_rank, buffer
        p = this%rank
        next_rank = -1
        do p = this%rank+1, this%npes-1
           if (this%local_data(p)) then
              next_rank = p
              exit
           end if
        end do
        if (next_rank == -1) then
           do p = 1, this%rank
              if (this%local_data(p)) then
                 next_rank = p-1
                 exit
              end if
           end do
        end if
        
        if (next_rank /= -1) then
           call MPI_Send(buffer, 0, MPI_LOGICAL, next_rank, &
                & LOCK_TAG, this%comm, ierror)
        end if
      end block

   end subroutine release

   subroutine destroy(this)
      class (MpiLock), intent(inout) :: this

      logical, pointer :: scratchpad(:)
      integer :: ierror

      ! Release resources
      if (.not. this%is_initialized()) return
      call MPI_Type_free(this%pe_locks_type, ierror)
      call MPI_Win_free(this%window, ierror)

      if (this%rank == 0) then
         call c_f_pointer(this%locks_ptr, scratchpad, [this%npes])
         call MPI_Free_mem(scratchpad, ierror)
      end if

      this%initialized = .false.
      !W.J comment out. Does this comm belong to this lock? Maybe not
      !call MPI_Comm_free(this%comm, ierror)

   end subroutine destroy

   function is_initialized(this) result(init)
      class (MpiLock), intent(in) :: this
      logical :: init
      init = this%initialized
   end function

end module PFL_MpiLock
