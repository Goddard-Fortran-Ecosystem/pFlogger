#include "error_handling_macros.fh"
!------------------------------------------------------------------------------
! NASA/GSFC, CISTO, Code 606, Advanced Software Technology Group
!------------------------------------------------------------------------------
!
!* MODULE: PFL_AbstractHandler
!
!> @brief Abstract class for handlers. 
!> @details
!> This is a placeholder to define specific handler interfaces. 
!> Instances of this class define how logging events are dispatched to specific
!> destinations. Handlers are passed LogRecord objects which contain all the 
!> information we want to logging. The Handler also has a level. Once the 
!> Handler receives the LogRecord from the Logger, it will ignore any LogRecord 
!> that has a severity level that is smaller than its own level. Otherwise it 
!> passes the LogRecord to its Formatter.
!> Yes - each Handler has one Formatter. The Formatter formats the LogRecord
!> message to the desired format and sends the formatted text back to the
!> Handler. Finally the Handler receives the message as formatted text back
!> from the Formatter and emits it to your destination.
!
!> @author ASTG staff
!> @date 01 Jan 2015 - Initial Version
!------------------------------------------------------------------------------
module PFL_AbstractHandler
   use PFL_Filterer
   use PFL_SeverityLevels, only: NOTSET
   use PFL_LogRecord, only: LogRecord
   use PFL_Formatter, only: Formatter
   use PFL_KeywordEnforcer
   use PFL_Exception
   implicit none
   private

   public :: AbstractHandler
   public :: BASIC_FORMAT
   
   type, extends(Filterer), abstract :: AbstractHandler
!!$      private
      integer :: level = NOTSET ! default
      class (Formatter), allocatable :: fmt
   contains
      procedure(emit_message), deferred :: emit_message
      procedure :: handle
      procedure(close), deferred :: close
      procedure(flush), deferred :: flush
      procedure(free),  deferred :: free
      procedure :: set_formatter
      procedure :: format
      procedure :: set_level
      procedure :: get_level
      procedure(equal), deferred :: equal
      generic :: operator(==) => equal
      procedure :: notEqual
      generic :: operator(/=) => notEqual
   end type AbstractHandler

   abstract interface

      ! This version is intended to be implemented by subclasses
      subroutine emit_message(this, record, unusable, rc)
         use PFL_KeywordEnforcer
         import AbstractHandler
         import LogRecord
         class (AbstractHandler), intent(inout) :: this
         type (LogRecord), intent(in) :: record
         class (KeywordEnforcer), optional, intent(in) :: unusable
         integer, optional, intent(out) :: rc
      end subroutine emit_message

      ! This version is intended to be implemented by subclasses
      subroutine close(this)
         import AbstractHandler
         class(AbstractHandler), intent(inout) :: this
      end subroutine close

      ! This version is intended to be implemented by subclasses
      subroutine flush(this)
         ! Flushes Fortran unit currently open for output.
         import AbstractHandler
         class(AbstractHandler), intent(in) :: this
      end subroutine flush

      subroutine free(this, rc)
         import AbstractHandler
         class(AbstractHandler), intent(inout) :: this
         integer, optional, intent(out) :: rc
      end subroutine free

      logical function equal(a, b)
         import AbstractHandler
         class (AbstractHandler), intent(in) :: a
         class (AbstractHandler), intent(in) :: b
      end function equal


   end interface

   character(len=*), parameter :: BASIC_FORMAT = &
        '%(level_name)a~: %(name)a~: %(message)a'


contains

   
   !---------------------------------------------------------------------------  
   ! ROUTINE: 
   ! handle
   !
   ! DESCRIPTION: 
   ! Log a specified message with severity 'level'
   !---------------------------------------------------------------------------
   subroutine handle(this, record, unusable, rc)
      class(AbstractHandler), intent(inout) :: this
      type (LogRecord), intent(inout) :: record
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: level
      integer :: status

      level = record%get_level()
      if (level >= this%get_level()) then
         if (this%do_filter(record)) then
            call this%emit_message(record, rc=status)
            _VERIFY(status,'',rc)
         end if
      end if
      _RETURN(_SUCCESS,rc)
      
   end subroutine handle


   !---------------------------------------------------------------------------  
   ! FUNCTION: 
   ! format
   !
   ! DESCRIPTION: 
   ! Format a record using specified formatter.
   !---------------------------------------------------------------------------
   function format(this, record, unusable, rc) result(message)
      class(AbstractHandler), intent(in) :: this
      type(LogRecord), intent(in) :: record
      character(len=:), allocatable :: message
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status

      _UNUSED_DUMMY(unusable)

      if (allocated(this%fmt)) then
         message = this%fmt%format(record, rc=status)
         _VERIFY(status,'',rc)
      else
         block
           type (Formatter) :: fmtr
           fmtr = Formatter(BASIC_FORMAT)
           message = fmtr%format(record, rc=status)
           _VERIFY(status,'',rc)
         end block
      end if

      _RETURN(_SUCCESS,rc)
   end function format

   
   !---------------------------------------------------------------------------  
   ! ROUTINE: 
   ! set_formatter
   !
   ! DESCRIPTION: 
   ! Set the formatter for this handler.
   !---------------------------------------------------------------------------
   subroutine set_formatter(this, fmt)
      class (AbstractHandler), intent(inout) :: this
      class (Formatter), intent(in) :: fmt

      if (allocated(this%fmt)) deallocate(this%fmt)

      allocate(this%fmt, source=fmt)

   end subroutine set_formatter

   
   !---------------------------------------------------------------------------  
   ! ROUTINE: 
   ! set_level
   !
   ! DESCRIPTION: 
   ! Set the logging level of this handler.
   !---------------------------------------------------------------------------  
   subroutine set_level(this, level)
      class (AbstractHandler), intent(inout) :: this
      integer, intent(in) :: level
      
      this%level = level
     
   end subroutine set_level

   
   !---------------------------------------------------------------------------  
   ! FUNCTION: 
   ! get_level
   !
   ! DESCRIPTION: 
   ! Get the logging level of this handler.
   !---------------------------------------------------------------------------  
   integer function get_level(this)
      class (AbstractHandler), intent(in) :: this
      
      get_level = this%level
      
   end function get_level
 

   !---------------------------------------------------------------------------  
   ! FUNCTION: 
   ! notEqual
   !
   ! DESCRIPTION:
   ! Overloads 'not equal' operation for handlers.
   !---------------------------------------------------------------------------  
   logical function notEqual(a, b)
      class (AbstractHandler), intent(in) :: a
      class (AbstractHandler), intent(in) :: b
      
      notEqual = .not. (a == b)

   end function notEqual
   

end module PFL_AbstractHandler
