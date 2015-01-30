!------------------------------------------------------------------------------
! NASA/GSFC, CISTO, Code 606, Advanced Software Technology Group
!------------------------------------------------------------------------------
!
! MODULE: ASTG_StreamHandler_mod
!
!> @author 
!> ASTG staff
!
! DESCRIPTION:
!> @brief
!> A handler class which writes logging events to a stream, e.g. STDOUT
!> Note that this class does not close the stream.
!
! REVISION HISTORY:
! 01 Jan 2015 - Initial Version
!------------------------------------------------------------------------------
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

   
   !---------------------------------------------------------------------------  
   ! FUNCTION: 
   ! newStreamHandler
   !
   ! DESCRIPTION: 
   ! Instatiate a stream handler. If uit is not specified use OUTPUT_UNIT.
   ! OUTPUT_UNIT is defined by iso_fortran_env to be STDOUT (unit=6).
   !---------------------------------------------------------------------------
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

   
   !---------------------------------------------------------------------------  
   ! ROUTINE: 
   ! emitMessage
   !
   ! DESCRIPTION: 
   ! Write a formatted string to a stream.
   !---------------------------------------------------------------------------  
   subroutine emitMessage(this, record)
      class (StreamHandler), intent(inout) :: this
      type(LogRecord) :: record

      write(this%unit, '(a)') this%format(record)
      call this%flush()
     
   end subroutine emitMessage

   
   !---------------------------------------------------------------------------  
   ! ROUTINE: 
   ! flushUnit
   !
   ! DESCRIPTION: 
   ! Flushes unit currently open for output.
   !---------------------------------------------------------------------------  
   subroutine flushUnit(this)
      class (StreamHandler), intent(inout) :: this
      
      flush(this%unit)
      
   end subroutine flushUnit

   
   !---------------------------------------------------------------------------  
   ! ROUTINE: 
   ! close
   !
   ! DESCRIPTION: 
   ! A no-op routine.
   !---------------------------------------------------------------------------  
   subroutine close(this)
      class (StreamHandler), intent(inout) :: this
   end subroutine close


   !---------------------------------------------------------------------------  
   ! FUNCTION: 
   ! equal
   !
   ! DESCRIPTION: 
   ! Overloads 'equal' operation for stream handlers.
   !---------------------------------------------------------------------------  
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
