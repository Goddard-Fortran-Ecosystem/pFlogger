program multiLogging
   use ASTG_RootLogger_mod
   use ASTG_LoggerManager_mod
   use ASTG_Logger_mod
   use ASTG_Filter_mod
   use ASTG_MpiFilter_mod
   use ASTG_StreamHandler_mod
   use ASTG_FileHandler_mod
   use ASTG_MpiFileHandler_mod
   use ASTG_Formatter_mod
   use ASTG_SeverityLevels_mod
   use iso_fortran_env, only: OUTPUT_UNIT
   implicit none

   include 'mpif.h'
   integer :: ier
   type (MpiFilter) :: rootFilter
   type (MpiFileHandler) :: errFile
   type (StreamHandler) :: prtFile
   type (MpiFilter) :: debugFilter
   type (MpiFileHandler) :: debugFile
   type (LoggerManager) :: manager

   class (Logger), pointer :: loggerStd
   class (Logger), pointer :: loggerErr
   class (Logger), pointer :: loggerChem

   character(len=:), allocatable :: rundeck

   call mpi_init(ier)

   rundeck = 'em20'
   
   manager = LoggerManager(RootLogger(WARNING))
   loggerStd => manager%getLogger('modelE')
   rootFilter = MpiFilter(MPI_COMM_WORLD)
  
   ! print on screen
   prtFile = StreamHandler(OUTPUT_UNIT)
   call prtFile%setLevel(DEBUG)
   call prtFile%addFilter(rootFilter)
   call loggerStd%addHandler(prtFile)
   call loggerStd%warning('Warning on screen!')
  
   ! write error into  files
   errFile = MpiFileHandler(rundeck // '.ERR', MPI_COMM_WORLD)
   call errFile%setLevel(INFO)
   call errFile%addFilter(rootFilter)
   loggerErr => manager%getLogger('modelE.erro')
   call loggerErr%addHandler(errFile)
   call loggerErr%info('Info on a file!')

   ! write debug file into different files
   debugFilter = MpiFilter(MPI_COMM_WORLD)
   debugFile = MpiFileHandler(rundeck//'.Chem', MPI_COMM_WORLD, &
        suffixFormat='.%(i3.3)', &
        delay=.true.)
   call debugFile%setLevel(DEBUG)
   call debugFile%addFilter(debugFilter)
   loggerChem => manager%getLogger('modelE.chemistry')
   call loggerChem%setLevel(DEBUG)
   call loggerChem%addHandler(debugFile)
   call loggerChem%debug('Debug on mpi files!')
   call loggerChem%info('Info on debug mpi files!')

   call mpi_finalize(ier)


end program multiLogging
