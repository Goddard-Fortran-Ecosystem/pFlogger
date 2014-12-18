module ASTG_Logger_mod
   ! A logger instance represents a logging channel, i.e. a medium thorugh
   ! which information (logging events) about an application is conveyed.
   ! Associated with a logger instance is a set of handlers which dispatch
   ! logging events to specific destinations, e.g. STDOUT or a FILE
   use ASTG_SeverityLevels_mod, only: DEBUG_LEVEL => DEBUG
   use ASTG_SeverityLevels_mod, only: INFO_LEVEL => INFO
   use ASTG_SeverityLevels_mod, only: WARNING_LEVEL => WARNING
   use ASTG_SeverityLevels_mod, only: ERROR_LEVEL => ERROR
   use ASTG_SeverityLevels_mod, only: CRITICAL_LEVEL => CRITICAL
   use ASTG_SeverityLevels_mod, only: NOTSET_LEVEL => NOTSET
   use ASTG_AbstractHandler_mod, only: AbstractHandler
   use ASTG_StreamHandler_mod, only: StreamHandler
   use FTL_AbstracthandlerPolyWrap_mod
   use FTL_AbstracthandlerPolyWrapVector_mod
   use ASTG_LogRecord_mod
   
   implicit none
   private

   public :: Logger

   type :: Logger
      private
      integer :: level
      character(len=:), allocatable :: name
      type (AbstractHandlerPolyWrapVector) :: handlers
   contains
      procedure :: getName
      procedure :: log
      procedure :: debug
      procedure :: info
      procedure :: warning
      procedure :: error
      procedure :: critical
      procedure :: notset
      procedure :: addHandler
      procedure :: removeHandler
      procedure :: getHandlers
      procedure :: setLevel
   end type Logger

   interface Logger
      module procedure newLogger
   end interface Logger

contains

   
   function newLogger(name, level) result(alog)
      ! Initialize the logger with an optional level
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
      call alog%addHandler(StreamHandler())
      
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

      call this%handlers%pop_back()
      
   end subroutine removeHandler


   subroutine log(this, level, message)
      ! Logging routine that calls the appropriate handler and emits
      ! the logging event
      class (Logger), intent(inout) :: this
      integer, intent(in) :: level
      character(len=*), intent(in) :: message
      type (AbstractHandlerPolyWrapVectorIterator) :: iter
      class (AbstractHandler), pointer :: handler
      type (LogRecord) :: record

      record = LogRecord(message)
      
      iter = this%handlers%begin()
      do while (iter /= this%handlers%end())
         handler => iter%get_alt()
         call handler%emit(level, record)
         call iter%next()
      end do

   end subroutine log

   
   subroutine debug(this, message)
      ! Log message with the integer severity 'DEBUG'.
      class (Logger), intent(inout) :: this
      character(len=*), intent(in) :: message

      call this%log(DEBUG_LEVEL, message)

   end subroutine debug

   
   subroutine info(this, message)
      ! Log message with the integer severity 'INFO'.
      class (Logger), intent(inout) :: this
      character(len=*), intent(in) :: message
      
      call this%log(INFO_LEVEL, message)

   end subroutine info


   subroutine warning(this, message)
      ! Log message with the integer severity 'WARNING'.
      class (Logger), intent(inout) :: this
      character(len=*), intent(in) :: message
      
      call this%log(WARNING_LEVEL, message)

   end subroutine warning

   
   subroutine error(this, message)
      ! Log message with the integer severity 'ERROR'.
      class (Logger), intent(inout) :: this
      character(len=*), intent(in) :: message
      
      call this%log(ERROR_LEVEL, message)

   end subroutine error

   
   subroutine critical(this, message)
      ! Log message with the integer severity 'CRITICAL'.
      class (Logger), intent(inout) :: this
      character(len=*), intent(in) :: message
      
      call this%log(CRITICAL_LEVEL, message)

   end subroutine critical

   
   subroutine notset(this, message)
      ! Log message with the integer severity 'NOTSET'.
      class (Logger), intent(inout) :: this
      character(len=*), intent(in) :: message
      
      call this%log(NOTSET_LEVEL, message)

   end subroutine notset

   
   function getHandlers(this) result(handlers)
      ! get handlers associated with this logger
      class (Logger), target, intent(in) :: this
      type (AbstractHandlerPolyWrapVector), pointer :: handlers
      
      handlers => this%handlers
      
   end function getHandlers


   subroutine setLevel(this, level)
      ! Set the logging level of this logger
      class (Logger), intent(inout) :: this
      integer, intent(in) :: level
      

      this%level = level
      
   end subroutine setLevel

end module ASTG_Logger_mod


