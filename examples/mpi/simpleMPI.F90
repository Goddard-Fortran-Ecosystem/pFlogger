program simpleMpi
   use ASTG_RootLogger_mod
   use ASTG_SeverityLevels_mod
   use ASTG_LoggerManager_mod
   use ASTG_MpiFileHandler_mod
   use ASTG_Logger_mod
   implicit none

   include 'mpif.h'

   type (LoggerManager) :: manager
   class (Logger), pointer :: lgr
   type (MpiFileHandler) :: handler

   integer :: ier

   ! each rank will open and write to xxx.pe=rank file

   call mpi_init(ier)

   ! instantiate manager with the root node of the logger hierarchy.
   manager = LoggerManager(RootLogger(WARNING))
   lgr => manager%getLogger('A')

   ! each rank will open and write to log.pe=rank file
   ! for example, filess, log.pe=0,log.pe=1, ... 
   handler = MpiFileHandler('log', MPI_COMM_WORLD)
   call lgr%addHandler(handler)

   call lgr%warning('hello')
   call mpi_finalize(ier)

end program simpleMpi

