module pflogger
   use Pfl_Exception
   use PFL_LoggerManager
   use PFL_AbstractHandlerPtrVector
   use PFL_AbstractHandlerPolyVector
   use PFL_Logger
   use PFL_RootLogger
   use PFL_AbstractHandler
   use PFL_StreamHandler
   use PFL_FileHandler
   use PFL_SeverityLevels
   use PFL_Formatter
   use PFL_FastFormatter
   use PFL_WrapArray
#ifdef _LOGGER_USE_MPI
#  ifdef SUPPORT_FOR_MPI_ALLOC_MEM_CPTR
   use PFL_MpiLock
   use PFL_MpiFilter
   use PFL_MpiFormatter
#  endif   
#endif
   use PFL_RotatingFileHandler
   use gFTL_StringUnlimitedMap
   implicit none
   private

   public :: initialize
   public :: finalize

   public :: logging
   public :: Logger
   public :: RootLogger
   public :: WrapArray

   public :: AbstractHandler
   public :: HandlerPtrVector, HandlerPtrVectorIterator
   public :: HandlerVector, HandlerVectorIterator
   public :: StreamHandler
   public :: FileHandler
   public :: RotatingFileHandler
   public :: newRotatingFileHandler

   public :: Formatter
   public :: newFormatter
   public :: FastFormatter

   public :: NOTSET
   public :: DEBUG
   public :: INFO
   public :: WARNING
   public :: ERROR
   public :: CRITICAL

   public :: level_to_name
   public :: name_to_level

   public :: initialize_severity_levels
   public :: finalize_severity_levels

#ifdef _LOGGER_USE_MPI
#  ifdef SUPPORT_FOR_MPI_ALLOC_MEM_CPTR
   public :: MpiLock
   public :: MpiFilter
   public :: MpiFormatter
#  endif
#endif   

   public :: set_throw_fptr
   public :: StringUnlimitedMap

contains

   subroutine initialize()

      call initialize_severity_levels()
      call initialize_logger_manager()
      call set_last_resort(StreamHandler())

   end subroutine initialize
   

   subroutine finalize()

      call finalize_severity_levels()

   end subroutine finalize

end module pflogger
