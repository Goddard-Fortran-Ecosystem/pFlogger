subroutine sub_A()
   use FTL_String_mod
   use pflogger

   integer :: i
   class (Logger), pointer :: log
   class (Logger), pointer :: plog

   log => logging%getLogger('main')
   plog => logging%getLogger('parallel')

   call log%info('at line: %i3.3 in file: %a20', __LINE__,String(__FILE__))
   call log%debug('inside sub_A')
   call plog%info('at line: %i3.3 in file: %a20', __LINE__,String(__FILE__))
   call plog%debug('inside sub_A')

   call log%warning('empty procedure')
   call log%info('at line: %i3.3 in file: %a20', __LINE__,String(__FILE__))

end subroutine sub_A


subroutine sub_B()
   use FTL_String_mod
   use pflogger

   integer :: i
   class (Logger), pointer :: log
   class (Logger), pointer :: plog


   log => logging%getLogger('main')
   plog => logging%getLogger('parallel')

   call log%info('at line: %i3.3 in file: %a20', __LINE__,string(__FILE__))
   call log%debug('inside sub_B')
   call plog%debug('inside sub_B')

   call log%error('this procedure is empty as well')
   call log%info('at line: %i3.3 in file: %a20', __LINE__,string(__FILE__))
end subroutine sub_B


program main
   use FTL_String_mod
   use pflogger
     use FTL_YAML_Parser_mod
     use FTL_Config_mod
   implicit none

   integer :: ier
   class (Logger), pointer :: log
   integer :: rc

   call mpi_init(ier)

   call initialize() ! init logger

   call logging%load_file('all_in_one.cfg')

   log => logging%getLogger('main')

   call log%info('at line: %i3.3 in file: %a20', __LINE__,string(__FILE__))
   call sub_A()
   
   call log%info('at line: %i3.3 in file: %a20', __LINE__,string(__FILE__))
   call sub_B()

   call log%info('at line: %i3.3 in file: %a20', __LINE__,string(__FILE__))
   call mpi_finalize(ier)

end program main



