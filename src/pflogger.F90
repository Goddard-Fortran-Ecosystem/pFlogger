module pflogger
   use PFL_LoggerManager_mod
   use PFL_Logger_mod
   use PFL_RootLogger_mod
   use PFL_AbstractHandler_mod
   use PFL_StreamHandler_mod
   use PFL_FileHandler_mod
   use PFL_SeverityLevels_mod
   use PFL_Formatter_mod
   use PFL_FastFormatter_mod
   use PFL_WrapArray_mod
   use PFL_StringUnlimitedMap_mod, only: StringUnlimitedMap => map
#ifdef LOGGER_USE_MPI
#  ifdef SUPPORT_FOR_MPI_ALLOC_MEM_CPTR
   use PFL_MpiLock_mod
#  endif   
#endif
   use PFL_RotatingFileHandler_mod
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

   public :: StringUnlimitedMap

   public :: initialize_severity_levels
   public :: finalize_severity_levels

#ifdef LOGGER_USE_MPI
#  ifdef SUPPORT_FOR_MPI_ALLOC_MEM_CPTR
   public :: MpiLock
#  endif
#endif   



contains

   subroutine initialize(comm)
      use PFL_SeverityLevels_mod
      use PFL_RootLogger_mod
      integer, optional, intent(in) :: comm ! unused except with MPI

      call initialize_severity_levels()
      call initialize_logger_manager(comm)

   end subroutine initialize
   

   subroutine finalize()
      use PFL_SeverityLevels_mod

      call finalize_severity_levels()

   end subroutine finalize

end module pflogger
