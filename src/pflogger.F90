module pflogger
   use Pfl_Exception
   use PFL_LoggerManager
   use PFL_Logger
   use Pfl_String
   use PFL_RootLogger
   use PFL_AbstractHandler
   use PFL_StreamHandler
   use PFL_FileHandler
   use PFL_SeverityLevels
   use PFL_Formatter
   use PFL_FastFormatter
   use PFL_WrapArray
   use FTL_Config, only: Config
#ifdef _LOGGER_USE_MPI
#  ifdef SUPPORT_FOR_MPI_ALLOC_MEM_CPTR
   use PFL_MpiLock
   use PFL_MpiFilter
#  endif   
#endif
   use PFL_RotatingFileHandler
   implicit none
   private

   public :: initialize
   public :: finalize

   public :: logging
   public :: Logger
   public :: RootLogger
   public :: WrapArray

   public :: AbstractHandler
   public :: StreamHandler
   public :: FileHandler
   public :: RotatingFileHandler
   public :: newRotatingFileHandler

   public :: Formatter
   public :: FastFormatter

   public :: NOTSET
   public :: DEBUG
   public :: INFO
   public :: WARNING
   public :: ERROR
   public :: CRITICAL

   public :: level_to_name
   public :: name_to_level

   public :: Config

   public :: initialize_severity_levels
   public :: finalize_severity_levels

   public :: String
   
#ifdef _LOGGER_USE_MPI
#  ifdef SUPPORT_FOR_MPI_ALLOC_MEM_CPTR
   public :: MpiLock
   public :: MpiFilter
#  endif
#endif   

   public :: set_throw_fptr


contains

   subroutine initialize()

      call initialize_severity_levels()
      call initialize_logger_manager()

   end subroutine initialize
   

   subroutine finalize()

      call finalize_severity_levels()

   end subroutine finalize

end module pflogger
