! A logger instance represents a logging channel, i.e. a medium thorugh
! which information (logging events) about an application is conveyed.
! Associated with a logger instance is a set of handlers which dispatch
! logging events to specific destinations, e.g. STDOUT or a FILE
! A logger has associated with it a severity level. A looger looks at a
! message and ignores it if the message level is less severe than its own
! level (default is INFO).
module ASTG_Logger_mod
   use ASTG_Exception_mod
   use ASTG_Filterer_mod, only: Filterer
   use ASTG_AbstractHandler_mod, only: AbstractHandler
   use ASTG_StreamHandler_mod, only: StreamHandler
   use FTL_AbstracthandlerPolyWrap_mod
   use FTL_AbstracthandlerPolyWrapVector_mod
   use ASTG_LogRecord_mod
   
   implicit none
   private

   public :: Logger

   type, extends(Filterer) :: Logger
      private
      integer :: level
      character(len=:), allocatable :: name
      class (Logger), pointer :: parent => null()
      type (AbstractHandlerPolyWrapVector) :: handlers
   contains

      procedure :: getName
      procedure :: setParent
      procedure :: getParent
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
   end type Logger

   interface Logger
      module procedure newLogger
   end interface Logger

contains

   
   ! Initialize the logger with an optional level
   function newLogger(name, level) result(alog)
      use ASTG_SeverityLevels_mod, only: INFO_LEVEL => INFO
      type (Logger) :: alog
      character(len=*), intent(in) :: name
      integer, optional, intent(in) :: level

      integer :: level_

      aLog%name = name

      level_ = INFO_LEVEL
      if (present (level)) level_ = level
! TODO: Need to NOTSET when inheritance is working
      call aLog%setLevel(level_)

      alog%handlers = AbstractHandlerPolyWrapVector()
!!$      call alog%addHandler(StreamHandler())
      
   end function newLogger


   function getName(this) result(name)
      character(len=:), allocatable :: name
      class (Logger), intent(in) :: this
      name = this%name
   end function getName

   
   subroutine addHandler(this, handler)
      class (Logger), intent(inout) :: this
      class (AbstractHandler), intent(in) :: handler
      
      call this%handlers%push_back(AbstractHandlerPolyWrap(handler))
      
   end subroutine addHandler


   subroutine removeHandler(this, handler)
      class (Logger), intent(inout) :: this
      class (AbstractHandler), intent(in) :: handler

      type (AbstractHandlerPolyWrapVectorIterator) :: iter


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


   ! Logging routine that calls the appropriate handler and emits
   ! the logging event.
   ! The log method needs two parameters - a message and the severity level
   subroutine log(this, level, message)
      class (Logger), intent(inout) :: this
      integer, intent(in) :: level
      character(len=*), intent(in) :: message
      type (AbstractHandlerPolyWrapVectorIterator) :: iter
      class (AbstractHandler), pointer :: handler
      type (LogRecord) :: record

      class (AbstractHandlerPolyWrap), pointer :: h

      ! Create LogRecord object from the message string and pass the LogRecord
      ! to its Handlers
      record = LogRecord(this%name, level, message)
      
      iter = this%handlers%begin()
      do while (iter /= this%handlers%end())
         handler => iter%get_alt()
         call handler%handle(record)
         call iter%next()
      end do

   end subroutine log

   ! Convenience methods follow - debug, info, warning, error, critical
   ! These methods are identical to the log method except that you don’t have
   ! to specify the level, because the level is implicit in the name.
   subroutine debug(this, message)
      use ASTG_SeverityLevels_mod, only: DEBUG_LEVEL => DEBUG
      ! Log message with the integer severity 'DEBUG'.
      class (Logger), intent(inout) :: this
      character(len=*), intent(in) :: message

      call this%log(DEBUG_LEVEL, message)

   end subroutine debug

   
   subroutine info(this, message)
      use ASTG_SeverityLevels_mod, only: INFO_LEVEL => INFO
      class (Logger), intent(inout) :: this
      character(len=*), intent(in) :: message
      
      call this%log(INFO_LEVEL, message)

   end subroutine info


   subroutine warning(this, message)
      use ASTG_SeverityLevels_mod, only: WARNING_LEVEL => WARNING
      class (Logger), intent(inout) :: this
      character(len=*), intent(in) :: message
      
      call this%log(WARNING_LEVEL, message)

   end subroutine warning

   
   subroutine error(this, message)
      use ASTG_SeverityLevels_mod, only: ERROR_LEVEL => ERROR
      class (Logger), intent(inout) :: this
      character(len=*), intent(in) :: message
      
      call this%log(ERROR_LEVEL, message)

   end subroutine error

   
   subroutine critical(this, message)
      use ASTG_SeverityLevels_mod, only: CRITICAL_LEVEL => critical
      class (Logger), intent(inout) :: this
      character(len=*), intent(in) :: message
      
      call this%log(CRITICAL_LEVEL, message)

   end subroutine critical

   
   ! get handlers associated with this logger
   function getHandlers(this) result(handlers)
      class (Logger), target, intent(in) :: this
      type (AbstractHandlerPolyWrapVector), pointer :: handlers
      
      handlers => this%handlers
      
   end function getHandlers


   ! Set the logging level of this logger
   subroutine setLevel(this, level)
      class (Logger), intent(inout) :: this
      integer, intent(in) :: level
      

      this%level = level
      
   end subroutine setLevel


   subroutine setParent(this, parent)
      class (Logger), intent(inout) :: this
      class (Logger), target, intent(in) :: parent

      this%parent => parent

   end subroutine setParent


   function getParent(this) result(parent)
      class (Logger), pointer :: parent
      class (Logger), intent(in) :: this

      parent => this%parent

   end function getParent


end module ASTG_Logger_mod


