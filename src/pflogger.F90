module pflogger_mod
   use astg_LoggerManager_mod
   use astg_Logger_mod
   use ASTG_RootLogger_mod
   use astg_AbstractHandler_mod
   use astg_StreamHandler_mod
   use astg_FileHandler_mod
   use astg_SeverityLevels_mod
   use astg_Formatter_mod
   use astg_FastFormatter_mod
   use astg_WrapArray_mod
   use astg_StringUnlimitedMap_mod, only: StringUnlimitedMap => map
#ifdef LOGGER_USE_MPI
   use astg_MpiLock_mod
#endif
   use astg_RotatingFileHandler_mod
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

   public :: levelToName
   public :: nameToLevel

   public :: StringUnlimitedMap

   public :: initialize_severity_levels
   public :: finalize_severity_levels

#ifdef LOGGER_USE_MPI
   public :: MpiLock
#endif



contains

   subroutine initialize(comm)
      use ASTG_SeverityLevels_mod
      use ASTG_RootLogger_mod
      integer, optional, intent(in) :: comm ! unused except with MPI

      call initialize_severity_levels()
      call initialize_logger_manager(comm)

   end subroutine initialize
   

   subroutine finalize()
      use ASTG_SeverityLevels_mod

      call finalize_severity_levels()

   end subroutine finalize

end module pflogger_mod
