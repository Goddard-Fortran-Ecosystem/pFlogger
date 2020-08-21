#include "error_handling_macros.fh"
!------------------------------------------------------------------------------
! NASA/GSFC, CISTO, Code 606, Advanced Software Technology Group
!------------------------------------------------------------------------------
!
!*MODULE: PFL_Logger
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
module PFL_Logger
   use gFTL_UnlimitedVector
   use gFTL_StringUnlimitedMap
   use PFL_AbstractHandlerPtrVector
   use PFL_Exception, only: throw
   use PFL_AbstractLogger
   use PFL_AbstractHandler
   use PFL_SeverityLevels, only: NOTSET
   use PFL_SeverityLevels, only: DEBUG_LEVEL => DEBUG
   use PFL_SeverityLevels, only: INFO_LEVEL => INFO
   use PFL_SeverityLevels, only: WARNING_LEVEL => WARNING
   use PFL_SeverityLevels, only: ERROR_LEVEL => ERROR
   use PFL_SeverityLevels, only: CRITICAL_LEVEL => critical
   use PFL_LogRecord
   use PFL_KeywordEnforcer
   implicit none
   private

   public :: Logger
   public :: newLogger
   public :: set_last_resort

   type, extends(AbstractLogger) :: Logger
      private
      integer :: level = NOTSET
      type (HandlerPtrVector) :: handlers
      class (Logger), pointer :: parent => null()
      logical :: propagate = .true.
      character(len=:), allocatable :: name
   contains
      procedure :: set_name
      procedure :: get_name
      procedure :: isEnabledFor
      procedure :: getEffectiveLevel
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
      !procedure :: free
   end type Logger

   interface Logger
      module procedure newLogger
   end interface Logger

#define ARG_LIST arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9

   ! Using a global for now.   Later intend to find a way to properly encapsulate this
   class(AbstractHandler), allocatable :: last_resort
   

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
      if (name == '') then
      ! Initialize the logger with the name "ROOT"  
        name_ = 'ROOT'
      end if  
      call alog%set_name(name_)

      level_ = NOTSET
      if (present (level)) level_ = level
      call aLog%set_level(level_)

      alog%handlers = HandlerPtrVector()
      
   end function newLogger


!---------------------------------------------------------------------------  
!*ROUTINE: set_name
!
!> @brief Set the name for this logger.
!---------------------------------------------------------------------------
   subroutine set_name(this, name)
      class (Logger), intent(inout) :: this
      character(len=*), intent(in) :: name

      allocate(this%name, source=name)

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
      class (AbstractHandler), target, intent(in) :: handler
      
      type (HandlerPtrVectorIterator) :: iter
      class (AbstractHandler), pointer :: hdlPtr

      iter = this%handlers%begin()
      do while (iter /= this%handlers%end())
         hdlPtr => iter%get()
         ! if duplicated - do nothing 
         if (associated(hdlPtr,handler)) return
         call iter%next()
      end do
      ! increment
      call this%handlers%push_back(handler)
      
   end subroutine add_handler

!   subroutine free(this)
!      class (Logger), intent(inout) :: this
!      class (AbstractHandler), pointer :: handler
!
!      type (HandlerPtrVectorIterator) :: iter
!
!      iter = this%handlers%begin()
!      do while (iter /= this%handlers%end())
!         handler => iter%get()
!         call handler%free()
!         call iter%next()
!      end do
!
!   end subroutine free


!---------------------------------------------------------------------------  
!*ROUTINE: remove_handler
!
!> @brief Remove the specified handler to this logger.
!---------------------------------------------------------------------------
   subroutine remove_handler(this, handler)
      class (Logger), intent(inout) :: this
      class (AbstractHandler), target, intent(in) :: handler

      class (AbstractHandler), pointer :: hdlerPtr
      integer :: i
      logical :: found
      type (HandlerPtrVectorIterator) :: iter 

      found = .false.
      iter = this%handlers%begin()
      do while (iter /= this%handlers%end())
         hdlerPtr=> iter%get()
         if (associated(hdlerPtr, handler)) then
           call this%handlers%erase(iter)
           found = .true.
           exit
        endif
        call iter%next()
      enddo

      !   ! Only can get here if handler not found
      if (.not. found) then
         call throw(__FILE__,__LINE__,'PFL::Logger%remove_handler() called - logger has no such handler.')
      end if

   end subroutine remove_handler


!---------------------------------------------------------------------------  
!*ROUTINE: make_record
!
!> @brief Create a logRecord
!---------------------------------------------------------------------------
   subroutine make_record(this, record, level, message, unusable, args, extra, line, file)
      class (Logger), intent(in) :: this
      type (LogRecord), intent(out) :: record
      integer, intent(in) :: level
      character(len=*), intent(in) :: message
      class (KeywordEnforcer), optional, intent(in) :: unusable
      type (UnlimitedVector), target, optional, intent(in) :: args
      type (StringUnlimitedMap), optional, target, intent(in) :: extra
      integer, optional, intent(in) :: line
      character(*), optional, intent(in) :: file

      character(len=:), allocatable :: name

      _UNUSED_DUMMY(unusable)
      
      name = this%get_name()
      call initLogRecord(record, name, level, message, args=args, extra=extra, line=line, file=file)
      
   end subroutine make_record


!---------------------------------------------------------------------------  
!*FUNCTION: isEnabledFornewLogger
!
!> @brief Is this logger enabled for level 'level'?
!---------------------------------------------------------------------------
   logical function isEnabledFor(this, level)
      class (Logger), intent(in) :: this
      integer, intent(in) :: level

      isEnabledFor = (level >= this%getEffectiveLevel())
      
   end function isEnabledFor
   
!---------------------------------------------------------------------------  
!*FUNCTION: getEffectiveLevelnewLogger
!
!> @brief Get the effective severity level of a logger.
!> @details
!! If a logger's actual level is NOTSET, it searches ancestors for
!! the first ancestor that has a level that is NOTSET.  The effective
!! level is the effective level of that ancestor.
!! If the search reaches ROOT, then the effective level is NOTSET.
!---------------------------------------------------------------------------
   integer function getEffectiveLevel(this) result(level)
      class (Logger), target, intent(in) :: this

      class (Logger), pointer :: lgr

      lgr => this
      do while (associated(lgr))
         if (lgr%level > NOTSET) then
            level = lgr%level
            return
         end if
         lgr => lgr%parent
      end do

      level = NOTSET
      
   end function getEffectiveLevel
   
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
   subroutine log_(this, level, message, ARG_LIST, unusable, extra, line, file, rc)
      use PFL_ArgListUtilities
      class (Logger), target, intent(inout) :: this
      character(len=*), intent(in) :: message
      integer, intent(in) :: level
      include 'recordOptArgs.inc'
      class (KeywordEnforcer), optional, intent(in) :: unusable
      type (StringUnlimitedMap), optional, target, intent(in) :: extra
      integer, optional, intent(in) :: line
      character(*), optional, intent(in) :: file
      integer, optional, intent(out) :: rc
      
      type (UnlimitedVector), target :: args
      type (LogRecord) :: record

      integer :: status
      
      args = make_arg_vector(ARG_LIST)
      call this%make_record(record, level, message, args=args, extra=extra, line=line, file=file)

      if (this%do_filter(record)) then
         call this%emit(record, rc=status)
         _VERIFY(status,'',rc)
      end if

      _RETURN(_SUCCESS,rc)
   end subroutine log_

   subroutine emit(this, record, unusable, rc)
      class (Logger), target, intent(in) :: this
      type (LogRecord), intent(inout) :: record
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      type (HandlerPtrVectorIterator) :: iter
      class (AbstractHandler), pointer :: h

      class (Logger), pointer :: current
      integer :: status
      integer :: count

      count = 0
      current => this
      do while (associated(current))
         iter = current%handlers%begin()
         do while (iter /= current%handlers%end())
            h => iter%get()
            call h%handle(record, rc=status)
            _VERIFY(status,'',rc)
            count = count + 1
            call iter%next()
         end do

         if (current%propagate) then
            current => current%parent
         else
            exit
         end if
      end do

      if (count == 0) then
         call last_resort%handle(record,rc=status)
         _VERIFY(status,'',rc)
      end if
         

      _RETURN(_SUCCESS,rc)
      
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
   subroutine log(this, level, message, ARG_LIST, unusable, extra, line, file, rc)
      ! Log message with the integer severity 'INFO'.
      class (Logger), intent(inout) :: this
      character(len=*), intent(in) :: message
      integer, intent(in) :: level     
      include 'recordOptArgs.inc'
      class(KeywordEnforcer), optional, intent(in) :: unusable
      type (StringUnlimitedMap), optional, target, intent(in) :: extra
      integer, optional, intent(in) :: line
      character(*), optional, intent(in) :: file
      integer, optional, intent(out) :: rc

      integer :: status

      if (this%isEnabledFor(level)) then
         call this%log_(level, message, ARG_LIST,rc=status)
         _VERIFY(status,'',rc)
      end if

      _RETURN(_SUCCESS,rc)

   end subroutine log


!---------------------------------------------------------------------------  
!*ROUTINE: debug
!
!> @brief Log a message with severity level DEBUG.
!---------------------------------------------------------------------------
   subroutine debug(this, message, ARG_LIST, unusable, extra, line, file, rc)
      ! Log message with the integer severity 'DEBUG'.
      class (Logger), target, intent(inout) :: this
      character(len=*), intent(in) :: message
      include 'recordOptArgs.inc'  
      class (KeywordEnforcer), optional, intent(in) :: unusable
      type (StringUnlimitedMap), optional, target, intent(in) :: extra
      integer, optional, intent(in) :: line
      character(*), optional, intent(in) :: file
      integer, optional, intent(out) :: rc

      integer :: status
      if (this%isEnabledFor(DEBUG_LEVEL)) then
         call this%log_(DEBUG_LEVEL, message, ARG_LIST, extra=extra, line=line, file=file, rc=status)
         _VERIFY(status,'',rc)
      end if
      _RETURN(_SUCCESS,rc)

   end subroutine debug

   
!---------------------------------------------------------------------------  
!*ROUTINE: info
!
!> @brief Log a message with severity level INFO.
!---------------------------------------------------------------------------
   subroutine info(this, message, ARG_LIST, unusable, extra, line, file, rc)
      ! Log message with the integer severity 'DEBUG'.
      class (Logger), target, intent(inout) :: this
      character(len=*), intent(in) :: message
      include 'recordOptArgs.inc'  
      class (KeywordEnforcer), optional, intent(in) :: unusable
      type (StringUnlimitedMap), optional, target, intent(in) :: extra
      integer, optional, intent(in) :: line
      character(*), optional, intent(in) :: file
      integer, optional, intent(out) :: rc

      integer :: status
      if (this%isEnabledFor(INFO_LEVEL)) then
         call this%log_(INFO_LEVEL, message, ARG_LIST, extra=extra, line=line, file=file, rc=status)
         _VERIFY(status,'',rc)
      end if
      _RETURN(_SUCCESS,rc)

   end subroutine info


!---------------------------------------------------------------------------  
!*ROUTINE: warning
!
!> @brief Log a message with severity level WARNING.
!---------------------------------------------------------------------------
   subroutine warning(this, message, ARG_LIST, unusable, extra, line, file, rc)
      ! Log message with the integer severity 'DEBUG'.
      class (Logger), target, intent(inout) :: this
      character(len=*), intent(in) :: message
      include 'recordOptArgs.inc'  
      class (KeywordEnforcer), optional, intent(in) :: unusable
      type (StringUnlimitedMap), optional, target, intent(in) :: extra
      integer, optional, intent(in) :: line
      character(*), optional, intent(in) :: file
      integer, optional, intent(out) :: rc
      
      integer :: status
      if (this%isEnabledFor(WARNING_LEVEL)) then
         call this%log_(WARNING_LEVEL, message, ARG_LIST, extra=extra, line=line, file=file, rc=status)
         _VERIFY(status,'',rc)
      end if
      _RETURN(_SUCCESS,rc)

   end subroutine warning

   
!---------------------------------------------------------------------------  
!*ROUTINE: error
!
!> @brief Log a message with severity level ERROR.
!---------------------------------------------------------------------------
   subroutine error(this, message, ARG_LIST, unusable, extra, line, file, rc)
      ! Log message with the integer severity 'DEBUG'.
      class (Logger), target, intent(inout) :: this
      character(len=*), intent(in) :: message
      include 'recordOptArgs.inc'  
      class (KeywordEnforcer), optional, intent(in) :: unusable
      type (StringUnlimitedMap), optional, target, intent(in) :: extra
      integer, optional, intent(in) :: line
      character(*), optional, intent(in) :: file
      integer, optional, intent(out) :: rc
      
      integer :: status
      if (this%isEnabledFor(ERROR_LEVEL)) then
         call this%log_(ERROR_LEVEL, message, ARG_LIST, extra=extra, line=line, file=file, rc=status)
         _VERIFY(status,'',rc)
      end if
      _RETURN(_SUCCESS,rc)


   end subroutine error

   
!---------------------------------------------------------------------------  
!*ROUTINE: critical
!
!> @brief Log a message with severity level CRITICAL.
!---------------------------------------------------------------------------
   subroutine critical(this, message, ARG_LIST, unusable, extra, line, file, rc)
      ! Log message with the integer severity 'DEBUG'.
      class (Logger), target, intent(inout) :: this
      character(len=*), intent(in) :: message
      include 'recordOptArgs.inc'  
      class (KeywordEnforcer), optional, intent(in) :: unusable
      type (StringUnlimitedMap), optional, target, intent(in) :: extra
      integer, optional, intent(in) :: line
      character(*), optional, intent(in) :: file
      integer, optional, intent(out) :: rc
      
      integer :: status
      if (this%isEnabledFor(CRITICAL_LEVEL)) then
         call this%log_(CRITICAL_LEVEL, message, ARG_LIST, extra=extra, line=line, file=file, rc=status)
         _VERIFY(status,'',rc)
      end if
      _RETURN(_SUCCESS,rc)

   end subroutine critical

   
!---------------------------------------------------------------------------  
!*ROUTINE: get_handlers
!
!> @brief Get handlers pointer associated with this logger.
!---------------------------------------------------------------------------
   function get_handlers(this) result(handlers)
      class (Logger), target, intent(in) :: this
      type (HandlerPtrVector), pointer :: handlers
      
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



   subroutine set_last_resort(h)
      class(AbstractHandler), intent(in) :: h

      allocate(last_resort, source=h)
      
   end subroutine set_last_resort

end module PFL_Logger


