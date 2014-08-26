module PrivateLogger_mod
   implicit none
   private

   public Logger
   public newLogger
   public LoggerManager
   public newLoggerManager

   type Logger
      integer :: level
   end type Logger

   type LoggerManager
      type(Logger), allocatable :: Loggers(:)
   end type LoggerManager

   interface newLogger
      module procedure Logger_
   end interface newLogger

contains

   type(Logger) function Logger_()
      Logger_%level = 0
   end function Logger_ 

   function newLoggerManager() result(list)
      type (LoggerManager) :: list
      allocate(list%loggers(0))
   end function newLoggerManager

end module PrivateLogger_mod

module Logger_mod
   use PrivateLogger_mod
   implicit none
   private

   public Logger
   public newLogger
   public LoggerManager
   public newLoggerManager
   public :: initializeGlobalLoggerManager

   type (LoggerManager) :: globalLoggerManager ! private

contains

   subroutine initializeGlobalLoggerManager()
      globalLoggerManager = newLoggerManager()
   end subroutine initializeGlobalLoggerManager

end module Logger_mod


