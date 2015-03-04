!------------------------------------------------------------------------------
! NASA/GSFC, CISTO, Code 606, Advanced Software Technology Group
!------------------------------------------------------------------------------
!
! MODULE: ASTG_Logger_mod
!
!> @author 
!> ASTG staff
!
! DESCRIPTION: 
!> @brief
!> A logger instance represents a logging channel, i.e. a medium thorugh
!> which information (logging events) about an application is conveyed.
!> Associated with a logger instance is a set of handlers which dispatch
!> logging events to specific destinations, e.g. STDOUT or a FILE
!> A logger has associated with it a severity level. A looger looks at a
!> message and ignores it if the message level is less severe than its own
!> level (default is INFO).
!
! REVISION HISTORY:
! 01 Jan 2015 - Initial Version
!------------------------------------------------------------------------------
module ASTG_Logger_mod
   use FTL_AbstracthandlerPolyWrap_mod
   use FTL_AbstracthandlerPolyWrapVec_mod
   use ASTG_Exception_mod
   use ASTG_AbstractLogger_mod
   use ASTG_AbstractHandler_mod
   use ASTG_StreamHandler_mod
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

   
   !---------------------------------------------------------------------------  
   ! FUNCTION: 
   ! newLogger
   !
   ! DESCRIPTION: 
   ! This constructor instantiates a logger with a name and an optional level
   ! (default is INFO). Logger instance also has an (empty) vector of handlers
   ! associated with it.
   !---------------------------------------------------------------------------
   function newLogger(name, level) result(alog)
      type (Logger) :: alog
      character(len=*), intent(in) :: name
      integer, optional, intent(in) :: level

      integer :: level_
      character(len=:), allocatable :: name_

      name_ = name
      if (name=='') then
      ! Initialize the logger with the name "ROOT"  
        name_ = 'ROOT'
      end if  
      call alog%setName(name_)
      aLog%name = name_

      level_ = INFO_LEVEL
      if (present (level)) level_ = level
! TODO: Need to NOTSET when inheritance is working
      call aLog%setLevel(level_)

      alog%handlers = AbstractHandlerPolyWrapVec()
      
   end function newLogger


   !---------------------------------------------------------------------------  
   ! ROUTINE: 
   ! setName
   !
   ! DESCRIPTION: 
   ! Set the name for this logger.
   !---------------------------------------------------------------------------
   subroutine setName(this, name)
      class (Logger), intent(inout) :: this
      character(len=*), intent(in) :: name

      this%name = name

   end subroutine setName


   !---------------------------------------------------------------------------  
   ! FUNCTION: 
   ! getName
   !
   ! DESCRIPTION: 
   ! Get the name for this logger.
   !---------------------------------------------------------------------------
   function getName(this) result(name)
      character(len=:), allocatable :: name
      class (Logger), intent(in) :: this
      
      name = this%name
      
   end function getName

   
   !---------------------------------------------------------------------------  
   ! ROUTINE: 
   ! addHandler
   !
   ! DESCRIPTION: 
   ! Add the specified handler to this logger and increment handlers vector.
   !---------------------------------------------------------------------------
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
      ! increment
      call this%handlers%push_back(AbstractHandlerPolyWrap(handler))
      
   end subroutine addHandler


   !---------------------------------------------------------------------------  
   ! ROUTINE: 
   ! removeHandler
   !
   ! DESCRIPTION: 
   ! Remove the specified handler to this logger.
   !---------------------------------------------------------------------------
   subroutine removeHandler(this, handler)
      class (Logger), intent(inout) :: this
      class (AbstractHandler), intent(in) :: handler

      type (AbstractHandlerPolyWrapVecIter) :: iter

      iter = this%handlers%begin()
      do while (iter /= this%handlers%end())
         if (handler == iter%get_alt()) then
            ! remove hadler from handlers vector
            iter = this%handlers%erase(iter)
            return
         end if
         call iter%next()
      end do

      ! Only can get here if handler not found
      call throw('Logger%removeHandler() called - logger has no such handler.')

   end subroutine removeHandler


   !---------------------------------------------------------------------------  
   ! ROUTINE: 
   ! makeRecord
   !
   ! DESCRIPTION: 
   ! Create a logRecord
   !---------------------------------------------------------------------------
   function makeRecord(this, level, message, args) result(record)
      use FTL_XWrapVec_mod
      class (Logger), intent(inout) :: this
      type (LogRecord) :: record
      integer, intent(in) :: level
      character(len=*), intent(in) :: message
      type (XWrapVec), optional, intent(in) :: args

      character(len=:), allocatable :: name

      name = this%getName()
      call initLogRecord(record, name, level, message, args=args)
      
   end function makeRecord


   !---------------------------------------------------------------------------  
   ! FUNCTION: 
   ! isEnabledFor
   !
   ! DESCRIPTION: 
   ! Is this logger enabled for level 'level'?
   !---------------------------------------------------------------------------
   logical function isEnabledFor(this, level)
      class (Logger), intent(in) :: this
      integer, intent(in) :: level

      isEnabledFor = (level >= this%level)
      
   end function isEnabledFor
   
   !---------------------------------------------------------------------------  
   ! ROUTINE: 
   ! log_
   !
   ! DESCRIPTION: 
   ! Low level logging routine which creates a LogRecord and then calls the
   ! appropriate handler of this logger to handle the record.
   ! The log method needs two parameters - a message and the severity level
   !---------------------------------------------------------------------------
   subroutine log_(this, level, message, ARG_LIST)
      use FTL_XWrapVec_mod
      use ASTG_ArgListUtilities_mod
      class (Logger), intent(inout) :: this
      character(len=*), intent(in) :: message
      integer, intent(in) :: level
      include 'recordOptArgs.inc'
      
      type (AbstractHandlerPolyWrapVecIter) :: iter
      class (AbstractHandler), pointer :: handler
      type (XWrapVec) :: args

      args = makeArgVector(ARG_LIST)
      iter = this%handlers%begin()
      ! Create LogRecord object from the message string and pass the LogRecord
      ! to its Handlers
      do while (iter /= this%handlers%end())
         handler => iter%get_alt()
         call handler%handle(this%makeRecord(level, message, args))
         call iter%next()
      end do

   end subroutine log_

   
   !---------------------------------------------------------------------------  
   ! ROUTINE: 
   ! log
   !
   ! DESCRIPTION: 
   ! Log a message with default severity level or altenatively as specified
   ! in optional input argument 'level'.
   !---------------------------------------------------------------------------
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

      if (this%isEnabledFor(level_)) &
           call this%log_(level_, message, ARG_LIST)

   end subroutine log


   ! Convenience methods follow - debug, info, warning, error, critical
   ! These methods are identical to the log method except that you don’t have
   ! to specify the level, because the level is implicit in the name.
   
   !---------------------------------------------------------------------------  
   ! ROUTINE: 
   ! debug
   !
   ! DESCRIPTION: 
   ! Log a message with severity level DEBUG.
   !---------------------------------------------------------------------------
   subroutine debug(this, message, ARG_LIST)
      ! Log message with the integer severity 'DEBUG'.
      class (Logger), intent(inout) :: this
      character(len=*), intent(in) :: message
      include 'recordOptArgs.inc'  

      if (this%isEnabledFor(DEBUG_LEVEL)) &
         call this%log_(DEBUG_LEVEL, message, ARG_LIST)

   end subroutine debug

   
   !---------------------------------------------------------------------------  
   ! ROUTINE: 
   ! info
   !
   ! DESCRIPTION: 
   ! Log a message with severity level INFO.
   !---------------------------------------------------------------------------
   subroutine info(this, message, ARG_LIST)
      class (Logger), intent(inout) :: this
      character(len=*), intent(in) :: message
      include 'recordOptArgs.inc'  

      if (this%isEnabledFor(INFO_LEVEL)) &
           call this%log_(INFO_LEVEL, message, ARG_LIST)

   end subroutine info


   !---------------------------------------------------------------------------  
   ! ROUTINE: 
   ! warning
   !
   ! DESCRIPTION: 
   ! Log a message with severity level WARNING.
   !---------------------------------------------------------------------------
   subroutine warning(this, message, ARG_LIST)
      class (Logger), intent(inout) :: this
      character(len=*), intent(in) :: message
      include 'recordOptArgs.inc'  
      
      if (this%isEnabledFor(WARNING_LEVEL)) &
           call this%log_(WARNING_LEVEL, message, ARG_LIST)

   end subroutine warning

   
   !---------------------------------------------------------------------------  
   ! ROUTINE: 
   ! error
   !
   ! DESCRIPTION: 
   ! Log a message with severity level ERROR.
   !---------------------------------------------------------------------------
   subroutine error(this, message, ARG_LIST)
      class (Logger), intent(inout) :: this
      character(len=*), intent(in) :: message
      include 'recordOptArgs.inc'  
      
      if (this%isEnabledFor(ERROR_LEVEL)) &
           call this%log_(ERROR_LEVEL, message, ARG_LIST)

   end subroutine error

   
   !---------------------------------------------------------------------------  
   ! ROUTINE: 
   ! critical
   !
   ! DESCRIPTION: 
   ! Log a message with severity level CRITICAL.
   !---------------------------------------------------------------------------
   subroutine critical(this, message, ARG_LIST)
      class (Logger), intent(inout) :: this
      character(len=*), intent(in) :: message
      include 'recordOptArgs.inc'  
      
      if (this%isEnabledFor(CRITICAL_LEVEL)) &
           call this%log_(CRITICAL_LEVEL, message, ARG_LIST)

   end subroutine critical

   
   !---------------------------------------------------------------------------  
   ! ROUTINE: 
   ! getHandlers
   !
   ! DESCRIPTION: 
   ! Get handlers pointer associated with this logger.
   !---------------------------------------------------------------------------
   function getHandlers(this) result(handlers)
      class (Logger), target, intent(in) :: this
      type (AbstractHandlerPolyWrapVec), pointer :: handlers
      
      handlers => this%handlers
      
   end function getHandlers


   !---------------------------------------------------------------------------  
   ! ROUTINE: 
   ! setLevel
   !
   ! DESCRIPTION: 
   ! Set the logging level of this logger.
   !---------------------------------------------------------------------------
   subroutine setLevel(this, level)
      class (Logger), intent(inout) :: this
      integer, intent(in) :: level      

      this%level = level
      
   end subroutine setLevel


   !---------------------------------------------------------------------------  
   ! FUNCTION: 
   ! getLevel
   !
   ! DESCRIPTION: 
   ! Get the logging level of this logger.
   !---------------------------------------------------------------------------
   function getLevel(this) result(level)
      class (Logger), intent(inout) :: this
      integer :: level
      
      level = this%level
      
   end function getLevel

end module ASTG_Logger_mod


