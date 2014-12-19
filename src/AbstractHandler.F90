! Abstract class for handlers. This is a placeholder to define specific
! handler interfaces. Instances of this class define how logging events 
! are dispatched to specific destinations. Handlers are passed LogRecord
! objects which contain all the information we want to logging.
! The Handler also has a level. Once the Handler receives the LogRecord
! from the Logger, it will ignore any LogRecord that has a severity level
! that is smaller than its own level. Otherwise it passes the LogRecord to
! its Formatter.
! Yes - each Handler has one Formatter. The Formatter formats the LogRecord
! message to the desired format and sends the formatted text back to the
! Handler. Finally the Handler receives the message as formatted text back
! from the Formatter and emits it to your destination.
module ASTG_AbstractHandler_mod
   use ASTG_SeverityLevels_mod, only: levelToString
   use ASTG_LogRecord_mod
   
   implicit none
   private

   public :: AbstractHandler
   
   type, abstract :: AbstractHandler
      private
      integer :: level
   contains
      procedure(emitMessage), deferred :: emitMessage
      procedure :: emit
      procedure(close), deferred :: close
      procedure(flush), deferred :: flush
      procedure :: setLevel
      procedure :: getLevel
   end type AbstractHandler

   abstract interface

      ! This version is intended to be implemented by subclasses
      subroutine emitMessage(this, levelString, record)
         import AbstractHandler
         import LogRecord
         class (AbstractHandler), intent(inout) :: this
         character(len=*), intent(in) :: levelString
         type (LogRecord) :: record
      end subroutine emitMessage

      ! This version is intended to be implemented by subclasses
      subroutine close(this)
         import AbstractHandler
         class(AbstractHandler), intent(inout) :: this
      end subroutine close

      ! This version is intended to be implemented by subclasses
      subroutine flush(this)
         ! Flushes Fortran unit currently open for output.
         import AbstractHandler
         class(AbstractHandler), intent(inout) :: this
      end subroutine flush

   end interface

   
contains

   
   ! Log a specified message with severity 'level'
   subroutine emit(this, level, record)
      class(AbstractHandler), intent(inout) :: this
      integer, intent(in) :: level
      type (LogRecord) :: record
      
      if (level >= this%getLevel()) then
        call this%emitMessage(levelToString(level), record)
      end if
      
   end subroutine emit

   
   ! Set the logging level of this handler
   subroutine setLevel(this, level)
      class (AbstractHandler), intent(inout) :: this
      integer, intent(in) :: level
      
      this%level = level
     
   end subroutine setLevel

   
   ! Get the logging level of this handler
   integer function getLevel(this)
      class (AbstractHandler), intent(in) :: this
      
      getLevel = this%level
      
   end function getLevel
 
end module ASTG_AbstractHandler_mod
