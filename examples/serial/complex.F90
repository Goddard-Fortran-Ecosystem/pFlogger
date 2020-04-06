subroutine sub_A()
   use pfl_String
   use pflogger

   integer :: i
   class (Logger), pointer :: log
   class (Logger), pointer :: plog

   log => logging%get_logger('main.A')

   call log%debug('at line: %i3.3 in file: %a', __LINE__,String(__FILE__))
   call log%info('inside sub_A')

   call log%warning('empty procedure')
   call log%debug('at line: %i3.3 in file: %a', __LINE__,String(__FILE__))

end subroutine sub_A


subroutine sub_B()
   use pfl_String
   use pflogger

   integer :: i
   class (Logger), pointer :: log
   class (Logger), pointer :: plog

   log => logging%get_logger('main.B')

   call log%debug('at line: %i3.3 in file: %a', __LINE__,String(__FILE__))
   call log%info('inside sub_B')

   call log%error('this procedure is empty as well')
   call log%debug('at line: %i3.3 in file: %a', __LINE__,String(__FILE__))

end subroutine sub_B


program main
   use pfl_String
   use pflogger
   implicit none

   integer :: ier
   class (Logger), pointer :: log
   integer :: rc

   call example_init()

   call initialize() ! init logger

   call logging%load_file('all_in_one.cfg')

   log => logging%get_logger('main')

   call log%debug('at line: %i3.3 in file: %a', __LINE__,string(__FILE__))
   call log%info('calling sub_A()')
   call sub_A()
   
   call log%debug('at line: %i3.3 in file: %a', __LINE__,string(__FILE__))
   call log%info('calling sub_B()')
   call sub_B()

   call log%debug('at line: %i3.3 in file: %a', __LINE__,string(__FILE__))


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



