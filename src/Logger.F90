module ASTG_Logger_mod
   ! A logger instance represents a logging channel, i.e. a medium thorugh
   ! which information (logging events) about an application is conveyed.
   ! Associated with a logger instance is a set of handlers which dispatch
   ! logging events to specific destinations, e.g. STDOUT or a FILE
   use ASTG_SeverityLevels_mod, only: DEBUG_LEVEL=>DEBUG
   use ASTG_SeverityLevels_mod, only: INFO_LEVEL=>INFO
   use ASTG_SeverityLevels_mod, only: WARNING_LEVEL=>WARNING
   use ASTG_SeverityLevels_mod, only: ERROR_LEVEL=>ERROR
   use ASTG_SeverityLevels_mod, only: CRITICAL_LEVEL=>CRITICAL
   use ASTG_AbstractHandler_mod, only: AbstractHandler
   use ASTG_StreamHandler_mod, only: StreamHandler
   use FTL_AbstracthandlerPolyWrap_mod
   use FTL_AbstracthandlerPolyWrapVector_mod

   implicit none
   private

   public :: Logger

   type :: Logger
      private
      integer :: level
      type (AbstractHandlerPolyWrapVector) :: handlers
   contains
      procedure :: log
      procedure :: debug
      procedure :: addHandler
      procedure :: removeHandler
      procedure :: getHandlers
      procedure :: setLevel
   end type Logger

   interface Logger
      module procedure newLogger
   end interface Logger

contains

   
   function newLogger(level) result(alog)
      ! Initialize the logger with an optional level
      type (Logger) :: alog
      integer, optional, intent(in) :: level

      integer :: level_
      
      if (present (level)) then
        level_ = level
      else
        level_ = INFO_LEVEL
      end if

! TODO: Need to NOTSET when inheritance is working
      alog%level = level_
      alog%handlers = AbstractHandlerPolyWrapVector()
      call alog%addHandler(StreamHandler())
      
   end function newLogger

   
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
      ! Log message with the integer severity 'level'.
      class (Logger), intent(inout) :: this
      integer, intent(in) :: level
      character(len=*), intent(in) :: message
      
      type (AbstractHandlerPolyWrapVectorIterator) :: iter
      type (AbstractHandlerPolyWrap), pointer :: handlerWrap
      class (AbstractHandler), pointer :: handler

      iter = this%handlers%begin()
      do while (iter /= this%handlers%end())
         handlerWrap => iter%get()
         handler => handlerWrap%get()
         call handler%emit(level, message)
         call iter%next()
      end do

   end subroutine log

   subroutine log_(this, level, message)
      type(Logger), intent(inout) :: this
      integer, intent(in) :: level
      character(len=*), intent(in) :: message
      type (AbstractHandlerPolyWrapVectorIterator) :: iter
      type (AbstractHandlerPolyWrap), pointer :: handlerWrap
      class (AbstractHandler), pointer :: handler

      iter = this%handlers%begin()
      do while (iter /= this%handlers%end())
         handlerWrap => iter%get()
         handler => handlerWrap%get()
         call handler%emit(level, message)
         call iter%next()
      end do

   end subroutine log_

   subroutine debug(this, message)
      ! Log message with the integer severity 'DEBUG'.
      class (Logger), intent(inout) :: this
      character(len=*), intent(in) :: message

      call log_(this, DEBUG_LEVEL, message)

   end subroutine debug

   
   subroutine info(this, message)
      ! Log message with the integer severity 'INFO'.
      class (Logger), intent(inout) :: this
      character(len=*), intent(in) :: message
      
      call log_(this, INFO_LEVEL, message)

   end subroutine info


   subroutine warning(this, message)
      ! Log message with the integer severity 'WARNING'.
      class (Logger), intent(inout) :: this
      character(len=*), intent(in) :: message
      
      call log_(this, WARNING_LEVEL, message)

   end subroutine warning

   
   subroutine error(this, message)
      ! Log message with the integer severity 'ERROR'.
      class (Logger), intent(inout) :: this
      character(len=*), intent(in) :: message
      
      call log_(this, ERROR_LEVEL, message)

   end subroutine error

   
   subroutine critical(this, message)
      ! Log message with the integer severity 'CRITICAL'.
      class (Logger), intent(inout) :: this
      character(len=*), intent(in) :: message
      
      call log_(this, CRITICAL_LEVEL, message)

   end subroutine critical

   
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


