! A handler class which writes logging events to a stream, e.g. STDOUT
! Note that this class does not close the stream.
module ASTG_StreamHandler_mod
   use iso_fortran_env, only: output_unit
   use ASTG_SeverityLevels_mod, only: INFO
   use ASTG_AbstractHandler_mod, only: AbstractHandler
   use ASTG_LogRecord_mod
   
   implicit none
   private

   public :: StreamHandler

   type, extends(AbstractHandler) :: StreamHandler
      private
      integer :: unit = output_unit ! stdout
   contains
      procedure :: emitMessage
      procedure :: close ! noop
      procedure :: flush => flushUnit
   end type StreamHandler

   interface StreamHandler
      module procedure :: newStreamHandler
   end interface StreamHandler

   
contains

   
   ! Initialize the stream handler
   function newStreamHandler(unit, level) result(handler)
      type (StreamHandler) :: handler
      integer, optional, intent(in) :: unit
      integer, optional, intent(in) :: level
     
      integer :: level_

      if (present(unit)) handler%unit = unit

      if (present (level)) then
        level_ = level
      else
        level_ = INFO
      end if

      call handler%setLevel(level_)
      
   end function newStreamHandler

   
   ! Write a string to a stream. Level is specified in levelString
   subroutine emitMessage(this, levelString, record)
      class (StreamHandler), intent(inout) :: this
      character(len=*), intent(in) :: levelString
      type(LogRecord) :: record

      write(this%unit,'(a)') levelString // ': ' // record%getMessage()
      
   end subroutine emitMessage

   
   subroutine flushUnit(this)
      class (StreamHandler), intent(inout) :: this
      
      call flush(this%unit)
      
   end subroutine flushUnit

   
   subroutine close(this)
      class (StreamHandler), intent(inout) :: this
   end subroutine close

end module ASTG_StreamHandler_mod
