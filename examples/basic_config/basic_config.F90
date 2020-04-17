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
   implicit none

   integer :: ier
   class (Logger), pointer :: log
   integer :: status
   type(StreamHandler) :: stream
   type(HandlerVector) :: hv

   call example_init()

   call initialize() ! init logger
   stream = StreamHandler()
   call stream%set_level(DEBUG)
   call logging%basic_config(stream=stream, level=DEBUG, rc=status)
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
#ifdef LOGGER_USE_MPI

      integer :: ier
      call mpi_init(ier)
#endif
   end subroutine example_init

   subroutine example_finalize()
#ifdef LOGGER_USE_MPI

      integer :: ier
      call mpi_finalize(ier)
#endif
   end subroutine example_finalize
   
end program main



