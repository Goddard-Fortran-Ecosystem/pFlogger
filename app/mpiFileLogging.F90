program mpiFileLogging
   use ASTG_Logger_mod
   use ASTG_MpiFileHandler_mod
   use ASTG_SeverityLevels_mod
   implicit none
   include 'mpif.h'
   
   type (Logger) :: log
   type (MpiFileHandler) :: fHandler
   integer :: ier, rank
   
   call mpi_init(ier)
   call mpi_comm_rank(mpi_comm_world, rank, ier)
   
   ! Create a Logger and give it a name.
   ! NOTE: Loggers have message levels to filter out messages
   ! and default is INFO.
   log = Logger('fileLog')

   ! Create a file handler, argument is fileName
   fHandler = MpiFileHandler('app.LOG', MPI_COMM_WORLD)

   ! Add this handler to logger so that logger can use it
   call log%addHandler(fHandler)
   
   ! Start logging!
   if (rank==0) print *,'---Logging to file app.LOG---'
   call log%warning('This is a warning message in a LOG file.')
   
   ! Done
   if (rank==0) print *,'---DONE---'

   call mpi_finalize(ier)
   
end program mpiFileLogging
