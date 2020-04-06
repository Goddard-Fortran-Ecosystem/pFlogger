subroutine sub_A()
   use PFL_String
   use pflogger

   integer :: i
   class (Logger), pointer :: log
   class (Logger), pointer :: plog

   log => logging%get_logger('main.A')
   plog => logging%get_logger('parallel.A')

   print*,__FILE__,__LINE__
   call log%info('at line: %i3.3 in file: %a', __LINE__,String(__FILE__))
   print*,__FILE__,__LINE__
   call log%debug('inside sub_A')
   print*,__FILE__,__LINE__
   call plog%info('at line: %i3.3 in file: %a', __LINE__,String(__FILE__))
   print*,__FILE__,__LINE__
   call plog%debug('inside sub_A')
   print*,__FILE__,__LINE__

   print*,__FILE__,__LINE__
   call log%warning('empty procedure')
   print*,__FILE__,__LINE__
   call log%info('at line: %i3.3 in file: %a', __LINE__,String(__FILE__))
   print*,__FILE__,__LINE__

end subroutine sub_A


subroutine sub_B()
   use PFL_String
   use pflogger

   integer :: i
   class (Logger), pointer :: log
   class (Logger), pointer :: plog


   log => logging%get_logger('main.B')
   plog => logging%get_logger('parallel.B')

   call log%info('at line: %i3.3 in file: %a', __LINE__,string(__FILE__))
   call log%debug('inside sub_B')
   call plog%debug('inside sub_B')

   call log%error('this procedure is empty as well')
   call log%info('at line: %i3.3 in file: %a', __LINE__,string(__FILE__))
end subroutine sub_B


program main
   use PFL_String
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

   print*,__FILE__,__LINE__
   call logging%load_file('all_in_one.cfg')
   print*,__FILE__,__LINE__,rank

   log => logging%get_logger('main')
   print*,__FILE__,__LINE__, rank

   call log%info('at line: %i3.3 in file: %a', __LINE__,string(__FILE__))
   print*,__FILE__,__LINE__, rank
   call sub_A()
   print*,__FILE__,__LINE__, rank
   
   call log%info('at line: %i3.3 in file: %a', __LINE__,string(__FILE__))
   print*,__FILE__,__LINE__, rank
   call sub_B()
   print*,__FILE__,__LINE__, rank

   call log%info('at line: %i3.3 in file: %a', __LINE__,string(__FILE__))
   print*,__FILE__,__LINE__, rank
   call mpi_finalize(ier)

end program main



