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
   class (Logger), pointer :: loggerPtr

   character(len=:), allocatable :: rundeck

   call mpi_init(ier)

   rundeck = 'em20'
   
   manager = LoggerManager()
   rootFilter = MpiFilter(MPI_COMM_WORLD)

   prtFile = StreamHandler(OUTPUT_UNIT, level=INFO)
   call prtFile%addFilter(rootFilter)
   
   errFile = FileHandler(rundeck // '.ERR', level=WARNING)
   call errFile%addFilter(rootFilter)
   call errFile%setFormatter(Formatter('%(rank) %(level) %(line) %(file) %a'))

   loggerPtr => manager%getLogger('modelE')
   call loggerPtr%addHandler(prtFile)
   call loggerPtr%addHandler(errFile)

   debugFilter = Filter('chemistry')
   debugFile = MpiFileHandler('debugChem.%i3.3', MPI_COMM_WORLD, &
        level=DEBUG, &
        delay=.true.)
   call debugFile%addFilter(Filter('chemistry'))

   loggerPtr => manager%getLogger('modelE.chemistry')
   call loggerPtr%addHandler(debugFile)

   call mpi_finalize(ier)


end program fileLogging
