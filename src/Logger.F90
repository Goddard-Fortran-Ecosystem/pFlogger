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
      procedure :: log_
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
      procedure :: makeRecord
   end type Logger

   interface Logger
      module procedure newLogger
   end interface Logger

   ! This private type is used to force some arguments to be passed by keyword.
   type UnusableArgument
   end type UnusableArgument

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
!!$      call alog%addHandler(StreamHandler())
      
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


   function makeRecord(this, message, level, unusable, &
        arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9 &
      ) result(record)
      use FTL_XWrapVec_mod
      use FTL_CIStringXUMap_mod
      use ASTG_FormatParser_mod
      class (Logger), intent(inout) :: this
      type (LogRecord) :: record
      character(len=*), intent(in) :: message
      integer, intent(in) :: level
      type (UnusableArgument), optional :: unusable
      class(*), optional, intent(in) :: arg1
      class(*), optional, intent(in) :: arg2
      class(*), optional, intent(in) :: arg3
      class(*), optional, intent(in) :: arg4
      class(*), optional, intent(in) :: arg5
      class(*), optional, intent(in) :: arg6
      class(*), optional, intent(in) :: arg7
      class(*), optional, intent(in) :: arg8
      class(*), optional, intent(in) :: arg9
      type (XWrapVec) :: args
      type(FormatParser) :: parser
      character(len=:), allocatable :: str
      
      args = XWrapVec()
      if (present(arg1)) call args%push_back_alt(arg1)
      if (present(arg2)) call args%push_back_alt(arg2)
      if (present(arg3)) call args%push_back_alt(arg3)
      if (present(arg4)) call args%push_back_alt(arg4)
      if (present(arg5)) call args%push_back_alt(arg5)
      if (present(arg6)) call args%push_back_alt(arg6)
      if (present(arg7)) call args%push_back_alt(arg7)
      if (present(arg8)) call args%push_back_alt(arg8)
      if (present(arg9)) call args%push_back_alt(arg9)

      str = parser%format(message, args)
      record = LogRecord(this%getName(), level, str)
      
   end function makeRecord

   
   ! Logging routine that calls the appropriate handler and emits
   ! the logging event.
   ! The log method needs two parameters - a message and the severity level
   subroutine log_(this, message, level, unusable, &
        arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9 &
      )
      class (Logger), intent(inout) :: this
      character(len=*), intent(in) :: message
      integer, intent(in) :: level
      type (UnusableArgument), optional :: unusable
      class(*), optional, intent(in) :: arg1
      class(*), optional, intent(in) :: arg2
      class(*), optional, intent(in) :: arg3
      class(*), optional, intent(in) :: arg4
      class(*), optional, intent(in) :: arg5
      class(*), optional, intent(in) :: arg6
      class(*), optional, intent(in) :: arg7
      class(*), optional, intent(in) :: arg8
      class(*), optional, intent(in) :: arg9

      type (AbstractHandlerPolyWrapVecIter) :: iter
      class (AbstractHandler), pointer :: handler
      type (LogRecord) :: record

      class (AbstractHandlerPolyWrap), pointer :: h

      ! Create LogRecord object from the message string and pass the LogRecord
      ! to its Handlers
      record = this%makeRecord(message, level)
      
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
   subroutine log(this, message, level, unusable, &
        arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9 &
      )
      ! Log message with the integer severity 'INFO'.
      class (Logger), intent(inout) :: this
      character(len=*), intent(in) :: message
      integer, optional, intent(in) :: level
      type (UnusableArgument), optional :: unusable
      class(*), optional, intent(in) :: arg1
      class(*), optional, intent(in) :: arg2
      class(*), optional, intent(in) :: arg3
      class(*), optional, intent(in) :: arg4
      class(*), optional, intent(in) :: arg5
      class(*), optional, intent(in) :: arg6
      class(*), optional, intent(in) :: arg7
      class(*), optional, intent(in) :: arg8
      class(*), optional, intent(in) :: arg9
      integer :: level_

      if (present(level)) then
        level_ = level
      else
        level_ = INFO_LEVEL
      end if
      
      call this%log_(message, level_)

   end subroutine log


   subroutine debug(this, message)
      ! Log message with the integer severity 'DEBUG'.
      class (Logger), intent(inout) :: this
      character(len=*), intent(in) :: message

      call this%log_(message, DEBUG_LEVEL)

   end subroutine debug

   
   subroutine info(this, message)
      class (Logger), intent(inout) :: this
      character(len=*), intent(in) :: message
      
      call this%log_(message, INFO_LEVEL)

   end subroutine info


   subroutine warning(this, message)
      class (Logger), intent(inout) :: this
      character(len=*), intent(in) :: message
      
      call this%log_(message, WARNING_LEVEL)

   end subroutine warning

   
   subroutine error(this, message)
      class (Logger), intent(inout) :: this
      character(len=*), intent(in) :: message
      
      call this%log_(message, ERROR_LEVEL)

   end subroutine error

   
   subroutine critical(this, message)
      class (Logger), intent(inout) :: this
      character(len=*), intent(in) :: message
      
      call this%log_(message, CRITICAL_LEVEL)

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


end module ASTG_Logger_mod


