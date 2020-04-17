! This program demonstrates the use of basic config
! which allows sidestepping YAML processing.


subroutine sub_A()
   use pflogger

   integer :: i
   class (Logger), pointer :: log
   class (Logger), pointer :: plog

   log => logging%get_logger('main.A')

   call log%debug('at line: %i3.3 in file: %a', __LINE__,__FILE__)
   call log%info('inside sub_A')

   call log%warning('empty procedure')
   call log%debug('at line: %i3.3 in file: %a', __LINE__,__FILE__)

end subroutine sub_A


subroutine sub_B()
   use pflogger

   integer :: i
   class (Logger), pointer :: log
   class (Logger), pointer :: plog

   log => logging%get_logger('main.B')

   call log%debug('at line: %i3.3 in file: %a', __LINE__,__FILE__)
   call log%info('inside sub_B')

   call log%error('this procedure is empty as well')
   call log%debug('at line: %i3.3 in file: %a', __LINE__,__FILE__)

end subroutine sub_B


program main
   use, intrinsic :: iso_fortran_env, only: ERROR_UNIT, OUTPUT_UNIT
   use pflogger
   use mpi
   implicit none

   integer :: ier
   class (Logger), pointer :: log
   integer :: status
   type(StreamHandler) :: stream
   type(FileHandler) :: fh
   type(HandlerVector) :: hv

   call example_init()

   call initialize() ! init logger
   stream = StreamHandler()
   call stream%set_level(DEBUG)

   fh = FileHandler('debug')
   call fh%set_lock(MpiLock(MPI_COMM_WORLD))
   call fh%set_level(DEBUG)
   call fh%set_formatter(MpiFormatter(MPI_COMM_WORLD))
   call hv%push_back(fh)

   fh = FileHandler('info')
   call fh%set_lock(MpiLock(MPI_COMM_WORLD))
   call fh%set_level(INFO)
   call hv%push_back(fh)

   fh = FileHandler('warn')
   call fh%set_lock(MpiLock(MPI_COMM_WORLD))
   call fh%set_level(WARNING)
   call hv%push_back(fh)

   call hv%push_back(stream)
   
   call logging%basic_config(handlers=hv, level=DEBUG, rc=status)
   if (status /= 0) error stop 'basic_config() failed'

   log => logging%get_logger('main')

   call log%debug('at line: %i3.3 in file: %a', __LINE__,__FILE__)
   call log%info('calling sub_A()')
   call sub_A()
   
   call log%debug('at line: %i3.3 in file: %a', __LINE__,__FILE__)
   call log%info('calling sub_B()')
   call sub_B()

   call log%debug('at line: %i3.3 in file: %a', __LINE__,__FILE__)


   call example_finalize()

contains

   ! The procedures below are simply to allow the example to link with
   ! an MPI build of the logging framework.  Users that are
   ! uninterested in MPI should _NOT_ need these procedures in their
   ! own code.

   subroutine example_init()

      integer :: ier
      call mpi_init(ier)

   end subroutine example_init

   subroutine example_finalize()


      integer :: ier
      call mpi_finalize(ier)

   end subroutine example_finalize
   
end program main



