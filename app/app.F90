program fileLogging

   use ASTG_LoggerManager_mod
   use ASTG_Logger_mod
   use ASTG_Filter_mod
   use ASTG_MpiFilter_mod
   use ASTG_StreamHandler_mod
   use ASTG_FileHandler_mod
   use ASTG_MpiFileHandler_mod
   use ASTG_Formatter_mod
   use ASTG_SeverityLevels_mod
   use ASTG_SeverityLevels_mod
   use iso_fortran_env, only: OUTPUT_UNIT
   implicit none

   include 'mpif.h'
   integer :: ier
   type (MpiFilter) :: rootFilter
   type (Filter) :: debugFilter
   type (FileHandler) :: errFile
   type (StreamHandler) :: prtFile
   type (MpiFileHandler) :: debugFile
   type (LoggerManager) :: manager
   class (Logger), pointer :: loggerStd
   class (Logger), pointer :: loggerErr
   class (Logger), pointer :: loggerChem

   character(len=:), allocatable :: rundeck

   call mpi_init(ier)

   rundeck = 'em20'
   
   manager = LoggerManager()
   rootFilter = MpiFilter(MPI_COMM_WORLD)

   prtFile = StreamHandler(OUTPUT_UNIT, level=DEBUG)
   call prtFile%addFilter(rootFilter)
   loggerStd => manager%getLogger('modelE')
   call loggerStd%addHandler(prtFile)
   call loggerStd%warning('Warning on screen!')
   
   errFile = FileHandler(rundeck // '.ERR', level=INFO)
   call errFile%addFilter(rootFilter)
   call errFile%setFormatter(Formatter('%(rank) %(level) %(line) %(file) %a'))
   
   loggerErr => manager%getLogger('modelE')
   call loggerErr%addHandler(errFile)
   call loggerErr%info('Info on file!')
   
   debugFilter = Filter('chemistry')
   debugFile = MpiFileHandler('debugChem', MPI_COMM_WORLD, &
        level=DEBUG, &
        suffixFormat='.%(i3.3)', &
        delay=.true.)
   call debugFile%addFilter(debugFilter)

   loggerChem => manager%getLogger('modelE.chemistry')
   call loggerChem%addHandler(debugFile)
   call loggerChem%debug('Debug on mpi files!')

   call mpi_finalize(ier)


end program fileLogging
