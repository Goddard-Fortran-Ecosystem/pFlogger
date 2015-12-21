!------------------------------------------------------------------------------
! NASA/GSFC, CISTO, Code 606, Advanced Software Technology Group
!------------------------------------------------------------------------------
!
!*MODULE: PFL_Logger_mod
!
!> @brief Logger classes and functions.
!> @details
!> A logger instance represents a logging channel, i.e. a medium thorugh
!> which information (logging events) about an application is conveyed.
!> Associated with a logger instance is a set of handlers which dispatch
!> logging events to specific destinations, e.g. STDOUT or a FILE
!> A logger has associated with it a severity level. A looger looks at a
!> message and ignores it if the message level is less severe than its own
!> level (default is INFO).
!
!> @author ASTG staff
!> @date 01 Jan 2015 - Initial Version
!------------------------------------------------------------------------------
module PFL_Logger_mod
   use PFL_AbstractHandlerPolyVector_mod, only: HandlerVector => Vector
   use PFL_AbstractHandlerPolyVector_mod, only: HandlerVectorIterator => VectorIterator
   use PFL_Exception_mod
   use PFL_AbstractLogger_mod
   use PFL_AbstractHandler_mod
   use PFL_StreamHandler_mod
   use PFL_SeverityLevels_mod, only: DEBUG_LEVEL => DEBUG
   use PFL_SeverityLevels_mod, only: INFO_LEVEL => INFO
   use PFL_SeverityLevels_mod, only: WARNING_LEVEL => WARNING
   use PFL_SeverityLevels_mod, only: ERROR_LEVEL => ERROR
   use PFL_SeverityLevels_mod, only: CRITICAL_LEVEL => critical
   use PFL_LogRecord_mod
   use PFL_StringUnlimitedMap_mod
   implicit none
   private

   public :: Logger
   public :: newLogger

   type, extends(AbstractLogger) :: Logger
      private
      integer :: level
      character(len=:), allocatable :: name
      type (HandlerVector) :: handlers
      class (Logger), pointer :: parent => null()
      logical :: propagate = .true.
   contains

      procedure :: set_name
      procedure :: get_name
      procedure :: isEnabledFor
      procedure :: log
      procedure :: debug
      procedure :: info
      procedure :: warning
      procedure :: error
      procedure :: critical
      procedure :: add_handler
      procedure :: remove_handler
      procedure :: get_handlers
      procedure :: set_level
      procedure :: get_level
      procedure :: log_
      procedure :: emit
      procedure :: make_record
      procedure :: set_parent
      procedure :: get_parent
      procedure :: set_propagate
      procedure :: get_propagate

   end type Logger

   interface Logger
      module procedure newLogger
   end interface Logger

#define ARG_LIST arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9


   type Unusable
   end type Unusable
contains

   
!---------------------------------------------------------------------------  
!*FUNCTION: newLogger
!
!> @brief Logger constructor

!> @details
!! This constructor instantiates a logger with a name and an optional level
!! (default is INFO). Logger instance also has an (empty) vector of handlers
!! associated with it.
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
      call alog%set_name(name_)
      aLog%name = name_

      level_ = INFO_LEVEL
      if (present (level)) level_ = level
! TODO: Need to NOTSET when inheritance is working
      call aLog%set_level(level_)

      alog%handlers = HandlerVector()
      
   end function newLogger


!---------------------------------------------------------------------------  
!*ROUTINE: set_name
!
!> @brief Set the name for this logger.
!---------------------------------------------------------------------------
   subroutine set_name(this, name)
      class (Logger), intent(inout) :: this
      character(len=*), intent(in) :: name

      this%name = name

   end subroutine set_name


!---------------------------------------------------------------------------  
!*FUNCTION: get_name
!
!> @brief Get the name for this logger.
!---------------------------------------------------------------------------
   function get_name(this) result(name)
      character(len=:), allocatable :: name
      class (Logger), intent(in) :: this
      
      name = this%name
      
   end function get_name

   
!---------------------------------------------------------------------------  
!*ROUTINE: add_handler
!
!> @brief Add the specified handler to this logger and increment handlers vector.
!---------------------------------------------------------------------------
   subroutine add_handler(this, handler)
      class (Logger), intent(inout) :: this
      class (AbstractHandler), intent(in) :: handler
      
      type (HandlerVectorIterator) :: iter

      iter = this%handlers%begin()
      do while (iter /= this%handlers%end())
         if (handler == iter%get()) then
            ! duplicate - nothing to do
            return
         end if
         call iter%next()
      end do
      ! increment
      call this%handlers%push_back(handler)
      
   end subroutine add_handler


!---------------------------------------------------------------------------  
!*ROUTINE: remove_handler
!
!> @brief Remove the specified handler to this logger.
!---------------------------------------------------------------------------
   subroutine remove_handler(this, handler)
      class (Logger), intent(inout) :: this
      class (AbstractHandler), intent(in) :: handler

      integer :: i
      
      i = this%handlers%get_index(handler)
      if (i > 0) then
         call this%handlers%erase(this%handlers%begin() + i - 1)
      else
         ! Only can get here if handler not found
         call throw('PFL::Logger%remove_handler() called - logger has no such handler.')
      end if

   end subroutine remove_handler


!---------------------------------------------------------------------------  
!*ROUTINE: make_record
!
!> @brief Create a logRecord
!---------------------------------------------------------------------------
   subroutine make_record(this, record, level, message, unused, args, extra)
      use PFL_UnlimitedVector_mod, only: UnlimitedVector => Vector
      class (Logger), intent(in) :: this
      type (LogRecord), intent(out) :: record
      integer, intent(in) :: level
      character(len=*), intent(in) :: message
      type(Unusable), optional, intent(in) :: unused
      type (UnlimitedVector), optional, intent(in) :: args
      type (map), optional, intent(in) :: extra

      character(len=:), allocatable :: name

      name = this%get_name()
      call initLogRecord(record, name, level, message, args=args, extra=extra)
      
   end subroutine make_record


!---------------------------------------------------------------------------  
!*FUNCTION: isEnabledFornewLogger
!
!> @brief Is this logger enabled for level 'level'?
!---------------------------------------------------------------------------
   logical function isEnabledFor(this, level)
      class (Logger), intent(in) :: this
      integer, intent(in) :: level

      isEnabledFor = (level >= this%level)
      
   end function isEnabledFor
   
!---------------------------------------------------------------------------  
!*ROUTINE: log_
!
!> @brief Low level logging routine.
!   
!> @details
!! Low level logging routine which creates a LogRecord and then calls the
!! appropriate handler of this logger to handle the record.
!! The log method needs two parameters - a message and the severity level
!---------------------------------------------------------------------------
   subroutine log_(this, level, message, ARG_LIST, unused, extra)
      use PFL_UnlimitedVector_mod, only: UnlimitedVector => Vector
      use PFL_ArgListUtilities_mod
      class (Logger), intent(inout) :: this
      character(len=*), intent(in) :: message
      integer, intent(in) :: level
      include 'recordOptArgs.inc'
      type(Unusable), optional, intent(in) :: unused
      type (map), optional, intent(in) :: extra
      
      type (UnlimitedVector) :: args
      type (LogRecord) :: record

      args = make_arg_vector(ARG_LIST)
      call this%make_record(record, level, message, args=args, extra=extra)

      if (this%do_filter(record)) then
         call this%emit(record)
      end if

   end subroutine log_

   subroutine emit(this, record)
      class (Logger), target, intent(in) :: this
      type (LogRecord), intent(inout) :: record

      type (HandlerVectorIterator) :: iter
      class (AbstractHandler), pointer :: h

      class (Logger), pointer :: current

      current => this
      do while (associated(current))
         iter = current%handlers%begin()
         do while (iter /= current%handlers%end())
            h => iter%get()
            call h%handle(record)
            call iter%next()
         end do

         if (current%propagate) then
            current => current%parent
         else
            exit
         end if
      end do
      
   end subroutine emit

   
!---------------------------------------------------------------------------  
!*ROUTINE: log
!
!> @brief Default logging routine.
!   
!> @details
!! Log a message with default severity level or altenatively as specified
!! in optional input argument 'level'.
!> @note
!! Convenience methods: debug, info, warning, error, critical
!! These methods are identical to the log method except that you don’t have
!! to specify the level, because the level is implicit in the name.  
!---------------------------------------------------------------------------
   subroutine log(this, level, message, ARG_LIST, unused, extra)
      ! Log message with the integer severity 'INFO'.
      class (Logger), intent(inout) :: this
      character(len=*), intent(in) :: message
      integer, optional, intent(in) :: level     
      include 'recordOptArgs.inc'
      type(Unusable), optional, intent(in) :: unused
      type (map), optional, intent(in) :: extra
      
      integer :: level_

      if (present(level)) then
        level_ = level
      else
        level_ = INFO_LEVEL
      end if

      if (this%isEnabledFor(level_)) &
           call this%log_(level_, message, ARG_LIST)

   end subroutine log


!---------------------------------------------------------------------------  
!*ROUTINE: debug
!
!> @brief Log a message with severity level DEBUG.
!---------------------------------------------------------------------------
   subroutine debug(this, message, ARG_LIST, unused, extra)
      ! Log message with the integer severity 'DEBUG'.
      class (Logger), intent(inout) :: this
      character(len=*), intent(in) :: message
      include 'recordOptArgs.inc'  
      type(Unusable), optional, intent(in) :: unused
      type (map), optional, intent(in) :: extra

      if (this%isEnabledFor(DEBUG_LEVEL)) &
         call this%log_(DEBUG_LEVEL, message, ARG_LIST, extra=extra)

   end subroutine debug

   
!---------------------------------------------------------------------------  
!*ROUTINE: info
!
!> @brief Log a message with severity level INFO.
!---------------------------------------------------------------------------
   subroutine info(this, message, ARG_LIST, unused, extra)
      class (Logger), intent(inout) :: this
      character(len=*), intent(in) :: message
      include 'recordOptArgs.inc'  
      type(Unusable), optional, intent(in) :: unused
      type (map), optional, intent(in) :: extra

      if (this%isEnabledFor(INFO_LEVEL)) &
           call this%log_(INFO_LEVEL, message, ARG_LIST, extra=extra)

   end subroutine info


!---------------------------------------------------------------------------  
!*ROUTINE: warning
!
!> @brief Log a message with severity level WARNING.
!---------------------------------------------------------------------------
   subroutine warning(this, message, ARG_LIST, unused, extra)
      class (Logger), intent(inout) :: this
      character(len=*), intent(in) :: message
      include 'recordOptArgs.inc'  
      type(Unusable), optional, intent(in) :: unused
      type (map), optional, intent(in) :: extra
      
      if (this%isEnabledFor(WARNING_LEVEL)) &
           call this%log_(WARNING_LEVEL, message, ARG_LIST, extra=extra)

   end subroutine warning

   
!---------------------------------------------------------------------------  
!*ROUTINE: error
!
!> @brief Log a message with severity level ERROR.
!---------------------------------------------------------------------------
   subroutine error(this, message, ARG_LIST, unused, extra)
      class (Logger), intent(inout) :: this
      character(len=*), intent(in) :: message
      include 'recordOptArgs.inc'  
      type(Unusable), optional, intent(in) :: unused
      type (map), optional, intent(in) :: extra
      
      if (this%isEnabledFor(ERROR_LEVEL)) &
           call this%log_(ERROR_LEVEL, message, ARG_LIST, extra=extra)

   end subroutine error

   
!---------------------------------------------------------------------------  
!*ROUTINE: critical
!
!> @brief Log a message with severity level CRITICAL.
!---------------------------------------------------------------------------
   subroutine critical(this, message, ARG_LIST, unused, extra)
      class (Logger), intent(inout) :: this
      character(len=*), intent(in) :: message
      include 'recordOptArgs.inc'  
      type(Unusable), optional, intent(in) :: unused
      type (map), optional, intent(in) :: extra
      
      if (this%isEnabledFor(CRITICAL_LEVEL)) &
           call this%log_(CRITICAL_LEVEL, message, ARG_LIST, extra=extra)

   end subroutine critical

   
!---------------------------------------------------------------------------  
!*ROUTINE: get_handlers
!
!> @brief Get handlers pointer associated with this logger.
!---------------------------------------------------------------------------
   function get_handlers(this) result(handlers)
      class (Logger), target, intent(in) :: this
      type (HandlerVector), pointer :: handlers
      
      handlers => this%handlers
      
   end function get_handlers


!---------------------------------------------------------------------------  
!*ROUTINE: set_level
!
!> @brief Set the logging level of this logger.
!---------------------------------------------------------------------------
   subroutine set_level(this, level)
      class (Logger), intent(inout) :: this
      integer, intent(in) :: level      

      this%level = level
      
   end subroutine set_level


!---------------------------------------------------------------------------  
!*FUNCTION: get_level
!
!> @brief Get the logging level of this logger.
!---------------------------------------------------------------------------
   function get_level(this) result(level)
      class (Logger), intent(inout) :: this
      integer :: level
      
      level = this%level
      
   end function get_level

!---------------------------------------------------------------------------  
!*ROUTINE: set_parent
!
!> @brief Set parent for 'this' logger.
!---------------------------------------------------------------------------
   subroutine set_parent(this, parent)
      class (Logger), intent(inout) :: this
      class (Logger), target, intent(in) :: parent

      this%parent => parent

   end subroutine set_parent

   
!---------------------------------------------------------------------------  
!*FUNCTION: get_parent
!
!> @brief Get parent of "this" logger.
!---------------------------------------------------------------------------
   function get_parent(this) result(parent)
      class (Logger), pointer :: parent
      class (Logger), intent(in) :: this

      parent => this%parent

   end function get_parent


!---------------------------------------------------------------------------  
!*ROUTINE: set_propagate
!
!> @brief Set propagate for 'this' logger.
!---------------------------------------------------------------------------
   subroutine set_propagate(this, propagate)
      class (Logger), intent(inout) :: this
      logical, intent(in) :: propagate

      this%propagate = propagate

   end subroutine set_propagate

   
!---------------------------------------------------------------------------  
!*FUNCTION: get_propagate
!
!> @brief Get propagate of "this" logger.
!---------------------------------------------------------------------------
   function get_propagate(this) result(propagate)
      logical :: propagate
      class (Logger), intent(in) :: this

      propagate = this%propagate

   end function get_propagate

   
end module PFL_Logger_mod


