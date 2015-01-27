! A logger instance represents a logging channel, i.e. a medium thorugh
! which information (logging events) about an application is conveyed.
! Associated with a logger instance is a set of handlers which dispatch
! logging events to specific destinations, e.g. STDOUT or a FILE
! A logger has associated with it a severity level. A looger looks at a
! message and ignores it if the message level is less severe than its own
! level (default is INFO).
module ASTG_Logger_mod
   use ASTG_Exception_mod
   use ASTG_AbstractLogger_mod
   use ASTG_AbstractHandler_mod, only: AbstractHandler
   use ASTG_StreamHandler_mod, only: StreamHandler
   use FTL_AbstracthandlerPolyWrap_mod
   use FTL_AbstracthandlerPolyWrapVec_mod
   use ASTG_SeverityLevels_mod, only: DEBUG_LEVEL => DEBUG
   use ASTG_SeverityLevels_mod, only: INFO_LEVEL => INFO
   use ASTG_SeverityLevels_mod, only: WARNING_LEVEL => WARNING
   use ASTG_SeverityLevels_mod, only: ERROR_LEVEL => ERROR
   use ASTG_SeverityLevels_mod, only: CRITICAL_LEVEL => critical
   use ASTG_LogRecord_mod
   implicit none
   private

   public :: Logger
   public :: newLogger

   type, extends(AbstractLogger) :: Logger
      private
      integer :: level
      character(len=:), allocatable :: name
      type (AbstractHandlerPolyWrapVec) :: handlers
   contains

      procedure :: setName
      procedure :: getName
      procedure :: isEnabledFor
      procedure :: log
      procedure :: debug
      procedure :: info
      procedure :: warning
      procedure :: error
      procedure :: critical
      procedure :: addHandler
      procedure :: removeHandler
      procedure :: getHandlers
      procedure :: setLevel
      procedure :: getLevel
      procedure :: log_
      procedure :: makeRecord
   end type Logger

   interface Logger
      module procedure newLogger
   end interface Logger

#define ARG_LIST arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9

contains

   
   ! Initialize the logger with an optional level
   function newLogger(name, level) result(alog)
      type (Logger) :: alog
      character(len=*), intent(in) :: name
      integer, optional, intent(in) :: level

      integer :: level_

      call alog%setName(name)
      aLog%name = name

      level_ = INFO_LEVEL
      if (present (level)) level_ = level
! TODO: Need to NOTSET when inheritance is working
      call aLog%setLevel(level_)

      alog%handlers = AbstractHandlerPolyWrapVec()
      
   end function newLogger


   subroutine setName(this, name)
      class (Logger), intent(inout) :: this
      character(len=*), intent(in) :: name

      this%name = name

   end subroutine setName


   function getName(this) result(name)
      character(len=:), allocatable :: name
      class (Logger), intent(in) :: this
      name = this%name
   end function getName

   
   subroutine addHandler(this, handler)
      class (Logger), intent(inout) :: this
      class (AbstractHandler), intent(in) :: handler
      
      type (AbstractHandlerPolyWrapVecIter) :: iter

      iter = this%handlers%begin()
      do while (iter /= this%handlers%end())
         if (handler == iter%get_alt()) then
            ! duplicate - nothing to do
            return
         end if
         call iter%next()
      end do

      call this%handlers%push_back(AbstractHandlerPolyWrap(handler))
      
   end subroutine addHandler


   subroutine removeHandler(this, handler)
      class (Logger), intent(inout) :: this
      class (AbstractHandler), intent(in) :: handler

      type (AbstractHandlerPolyWrapVecIter) :: iter


      iter = this%handlers%begin()
      do while (iter /= this%handlers%end())
         if (handler == iter%get_alt()) then
            iter = this%handlers%erase(iter)
            return
         end if
         call iter%next()
      end do

      ! Only can get here if handler not found
      call throw('Logger%removeHandler() called - logger has no such handler.')

   end subroutine removeHandler


   function makeRecord(this, level, message, args) result(record)
      use FTL_XWrapVec_mod
      class (Logger), intent(inout) :: this
      type (LogRecord) :: record
      integer, intent(in) :: level
      character(len=*), intent(in) :: message
      type (XWrapVec), optional, intent(in) :: args
      
      record = LogRecord(this%getName(), level, message, args=args)
      
   end function makeRecord

   logical function isEnabledFor(this, level)
      class (Logger), intent(in) :: this
      integer, intent(in) :: level

      isEnabledFor = (level >= this%level)
   end function isEnabledFor
   
   ! Logging routine that calls the appropriate handler and emits
   ! the logging event.
   ! The log method needs two parameters - a message and the severity level
   subroutine log_(this, level, message, ARG_LIST)
      use FTL_XWrapVec_mod
      use ASTG_ArgListUtilities_mod
      class (Logger), intent(inout) :: this
      character(len=*), intent(in) :: message
      integer, intent(in) :: level
      
      include 'recordOptArgs.inc'  
      type (AbstractHandlerPolyWrapVecIter) :: iter
      class (AbstractHandler), pointer :: handler
      type (LogRecord) :: record

      class (AbstractHandlerPolyWrap), pointer :: h
      type (XWrapVec) :: args

      ! Create LogRecord object from the message string and pass the LogRecord
      ! to its Handlers
      args = makeArgVector(ARG_LIST)
      record = this%makeRecord(level, message, args)
      
      iter = this%handlers%begin()
      do while (iter /= this%handlers%end())
         handler => iter%get_alt()
         call handler%handle(record)
         call iter%next()
      end do

   end subroutine log_

   
   ! Convenience methods follow - debug, info, warning, error, critical
   ! These methods are identical to the log method except that you don’t have
   ! to specify the level, because the level is implicit in the name.
   subroutine log(this, level, message, ARG_LIST)
      ! Log message with the integer severity 'INFO'.
      class (Logger), intent(inout) :: this
      character(len=*), intent(in) :: message
      integer, optional, intent(in) :: level
      
      include 'recordOptArgs.inc'  
      integer :: level_

      if (present(level)) then
        level_ = level
      else
        level_ = INFO_LEVEL
      end if

      if (this%isEnabledFor(level_)) call this%log_(level_, message)

   end subroutine log


   subroutine debug(this, message, ARG_LIST)
      ! Log message with the integer severity 'DEBUG'.
      class (Logger), intent(inout) :: this
      character(len=*), intent(in) :: message
      include 'recordOptArgs.inc'  

      if (this%isEnabledFor(DEBUG_LEVEL)) &
         call this%log_(DEBUG_LEVEL, message, ARG_LIST)

   end subroutine debug

   
   subroutine info(this, message, ARG_LIST)
      class (Logger), intent(inout) :: this
      character(len=*), intent(in) :: message
      include 'recordOptArgs.inc'  

      if (this%isEnabledFor(INFO_LEVEL)) &
           call this%log_(INFO_LEVEL, message, ARG_LIST)

   end subroutine info


   subroutine warning(this, message, ARG_LIST)
      class (Logger), intent(inout) :: this
      character(len=*), intent(in) :: message
      include 'recordOptArgs.inc'  
      
      if (this%isEnabledFor(WARNING_LEVEL)) &
           call this%log_(WARNING_LEVEL, message, ARG_LIST)

   end subroutine warning

   
   subroutine error(this, message, ARG_LIST)
      class (Logger), intent(inout) :: this
      character(len=*), intent(in) :: message
      include 'recordOptArgs.inc'  
      
      if (this%isEnabledFor(ERROR_LEVEL)) &
           call this%log_(ERROR_LEVEL, message, ARG_LIST)

   end subroutine error

   
   subroutine critical(this, message, ARG_LIST)
      class (Logger), intent(inout) :: this
      character(len=*), intent(in) :: message
      include 'recordOptArgs.inc'  
      
      if (this%isEnabledFor(CRITICAL_LEVEL)) &
           call this%log_(CRITICAL_LEVEL, message, ARG_LIST)

   end subroutine critical

   
   ! get handlers associated with this logger
   function getHandlers(this) result(handlers)
      class (Logger), target, intent(in) :: this
      type (AbstractHandlerPolyWrapVec), pointer :: handlers
      
      handlers => this%handlers
      
   end function getHandlers


   ! Set the logging level of this logger
   subroutine setLevel(this, level)
      class (Logger), intent(inout) :: this
      integer, intent(in) :: level      

      this%level = level
      
   end subroutine setLevel


   ! Get the logging level of this logger
   function getLevel(this) result(level)
      class (Logger), intent(inout) :: this
      integer :: level
      
      level = this%level
      
   end function getLevel

end module ASTG_Logger_mod


