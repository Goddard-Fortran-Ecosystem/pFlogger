program appMpi
   use ASTG_LoggerManager_mod
   use ASTG_MpiFileHandler_mod
   use ASTG_Logger_mod
   implicit none

   include 'mpif.h'

   type (LoggerManager) :: manager
   class (Logger), pointer :: lgr
   type (MpiFileHandler) :: handler

   integer :: ier

   call mpi_init(ier)

   ! instantiate manager with the root node of the logger hierarchy.
   manager = LoggerManager()

   
   lgr => manager%getLogger('A')
   handler = MpiFileHandler('log', MPI_COMM_WORLD)
   call lgr%addHandler(handler)


   call lgr%warning('hello')

   call mpi_finalize(ier)

end program appMpi

