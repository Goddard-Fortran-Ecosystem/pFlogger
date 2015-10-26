module pflogger_mod
   use astg_LoggerManager_mod
   use astg_Logger_mod
   use astg_AbstractHandler_mod
   use astg_StreamHandler_mod
   use astg_FileHandler_mod
   use astg_SeverityLevels_mod
   use astg_Formatter_mod
   use astg_FastFormatter_mod
   use astg_WrapArray_mod
   implicit none
   private

   public :: initialize
   public :: finalize

   public :: logging
   public :: Logger
   public :: WrapArray

   public :: AbstractHandler
   public :: StreamHandler
   public :: FileHandler

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

   public :: initialize_severity_levels
   public :: finalize_severity_levels


contains

   subroutine initialize()
      use ASTG_SeverityLevels_mod
      call initialize_severity_levels()
   end subroutine initialize

   subroutine finalize()
      use ASTG_SeverityLevels_mod
      call finalize_severity_levels()
   end subroutine finalize

end module pflogger_mod
