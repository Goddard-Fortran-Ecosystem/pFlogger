! A handler class which writes logging events to a stream, e.g. STDOUT
! Note that this class does not close the stream.
module ASTG_StreamHandler_mod
   use iso_fortran_env, only: OUTPUT_UNIT
   use ASTG_SeverityLevels_mod, only: INFO
   use ASTG_AbstractHandler_mod, only: AbstractHandler, BASIC_FORMAT
   use ASTG_LogRecord_mod
   use ASTG_Formatter_mod
   
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
      procedure :: equal
   end type StreamHandler

   interface StreamHandler
      module procedure newStreamHandler
   end interface StreamHandler

   
contains

   
   ! Initialize the stream handler
   function newStreamHandler(unit, level) result(handler)
      type (StreamHandler) :: handler
      integer, optional, intent(in) :: unit
      integer, optional, intent(in) :: level
     
      integer :: level_

      if (present(unit)) then
         handler%unit = unit
      else
         handler%unit = OUTPUT_UNIT
      end if

      if (present (level)) then
        level_ = level
      else
        level_ = INFO
      end if

      call handler%setLevel(level_)
      call handler%setFormatter(Formatter(BASIC_FORMAT))
      
   end function newStreamHandler

   
   ! Write a string to a stream. Level is specified in levelString
   subroutine emitMessage(this, record)
      class (StreamHandler), intent(inout) :: this
      type(LogRecord) :: record

      write(this%unit, '(a)') this%format(record)
      call this%flush()
     
   end subroutine emitMessage

   
   subroutine flushUnit(this)
      class (StreamHandler), intent(inout) :: this
      
      flush(this%unit)
      
   end subroutine flushUnit

   
   subroutine close(this)
      class (StreamHandler), intent(inout) :: this
   end subroutine close


   logical function equal(a, b)
      class (StreamHandler), intent(in) :: a
      class (AbstractHandler), intent(in) :: b

      select type (b)
      class is (StreamHandler)
         equal = (a%unit == b%unit) .and. (a%getLevel() == b%getLevel())
      class default
         equal = .false.
      end select

   end function equal


end module ASTG_StreamHandler_mod
