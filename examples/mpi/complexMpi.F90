subroutine sub_A()
   use pflogger

   integer :: i
   class (Logger), pointer :: log
   class (Logger), pointer :: plog

   log => logging%get_logger('main.A')
   plog => logging%get_logger('parallel.A')

   call log%info('at line: %i3.3 in file: %a', __LINE__,__FILE__)
   call log%debug('inside sub_A')
   call plog%info('at line: %i3.3 in file: %a', __LINE__,__FILE__)
   call plog%debug('inside sub_A')

   call log%warning('empty procedure')
   call log%info('at line: %i3.3 in file: %a', __LINE__,__FILE__)

end subroutine sub_A


subroutine sub_B()
   use pflogger

   integer :: i
   class (Logger), pointer :: log
   class (Logger), pointer :: plog


   log => logging%get_logger('main.B')
   plog => logging%get_logger('parallel.B')

   call log%info('at line: %i3.3 in file: %a', __LINE__,__FILE__)
   call log%debug('inside sub_B')
   call plog%debug('inside sub_B')

   call log%error('this procedure is empty as well')
   call log%info('at line: %i3.3 in file: %a', __LINE__,__FILE__)

end subroutine sub_B


program main
   use pflogger
   implicit none

   integer :: ier
   class (Logger), pointer :: log
   integer :: rc
   integer :: rank

   call mpi_init(ier)
   block
     use mpi
     call mpi_comm_rank(MPI_COMM_WORLD, rank, rc)
   end block
   call initialize() ! init logger

   call logging%load_file('all_in_one.cfg')

   log => logging%get_logger('main')

   call log%info('at line: %i3.3 in file: %a', __LINE__,__FILE__)
   call sub_A()
   
   call log%info('at line: %i3.3 in file: %a', __LINE__,__FILE__)
   call sub_B()

   call log%info('at line: %i3.3 in file: %a', __LINE__,__FILE__)
   call mpi_finalize(ier)

end program main



