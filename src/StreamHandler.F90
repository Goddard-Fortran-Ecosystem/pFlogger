#include "error_handling_macros.fh"
!------------------------------------------------------------------------------
! NASA/GSFC, CISTO, Code 606, Advanced Software Technology Group
!------------------------------------------------------------------------------
!
!*MODULE: PFL_StreamHandler
!
!> @brief Write logging event to a stream.
!> @details
!> A handler class which writes logging events to a stream, e.g. STDOUT
!> Note that this class does not close the stream.
!
!> @author ASTG staff
!> @date 01 Jan 2015 - Initial Version  
!------------------------------------------------------------------------------
module PFL_StreamHandler
   use iso_fortran_env, only: ERROR_UNIT
   use PFL_AbstractHandler, only: AbstractHandler
   use PFL_LogRecord, only: LogRecord
   use PFL_KeywordEnforcer
   use PFL_Exception
   implicit none
   private

   public :: StreamHandler

   ! Fortran requires that INQUIRE return -1 for files that are
   ! not connected, but there is no name given to this value
   integer, parameter :: UNCONNECTED = -1

   type, extends(AbstractHandler) :: StreamHandler
      private
      integer :: unit = UNCONNECTED
   contains
      procedure :: get_unit
      procedure :: set_unit
      procedure :: emit_message
      procedure :: close ! noop
      procedure :: free ! noop
      procedure :: flush => flush_unit
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
   ! Instantiate a stream handler. If unit is not specified use ERROR_UNIT.
   ! ERROR_UNIT is defined by iso_fortran_env.
   !---------------------------------------------------------------------------
   function newStreamHandler(unit) result(handler)
      type (StreamHandler) :: handler
      integer, optional, intent(in) :: unit
     
      if (present(unit)) then
         handler%unit = unit
      else
         handler%unit = ERROR_UNIT
      end if

   end function newStreamHandler

   
   !---------------------------------------------------------------------------  
   ! ROUTINE: 
   ! get_unit
   !
   ! DESCRIPTION: 
   ! Return the io unit in use.  
   ! Forced to use this due to a bug in NAG 6.0 for INQUIRE combined with 
   ! NEWUNIT.
   !---------------------------------------------------------------------------  
   integer function get_unit(this) result(unit)
      class (StreamHandler), intent(in) :: this

      unit = this%unit
     
   end function get_unit


   subroutine set_unit(this, unit)
      class (StreamHandler), intent(inout) :: this
      integer, intent(in) :: unit

      this%unit = unit
     
   end subroutine set_unit
   
   !---------------------------------------------------------------------------  
   ! ROUTINE: 
   ! emit_message
   !
   ! DESCRIPTION: 
   ! Write a formatted string to a stream.
   !---------------------------------------------------------------------------  
   subroutine emit_message(this, record, unusable, rc)
      class (StreamHandler), intent(inout) :: this
      type(LogRecord), intent(in) :: record
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc
      
      integer :: status

      write(this%unit, '(a)') this%format(record,rc=status)
      _VERIFY(status,'',rc)
      call this%flush()
      _RETURN(_SUCCESS,rc)
     
   end subroutine emit_message

   
   !---------------------------------------------------------------------------  
   ! ROUTINE: 
   ! flush_unit
   !
   ! DESCRIPTION: 
   ! Flushes unit currently open for output.
   !---------------------------------------------------------------------------  
   subroutine flush_unit(this)
      class (StreamHandler), intent(in) :: this
      
      flush(this%unit)
      
   end subroutine flush_unit

   
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

   ! A no-op routine.
   !---------------------------------------------------------------------------  
   subroutine free(this, rc)
      class (StreamHandler), intent(inout) :: this
      integer, optional, intent(out) :: rc
   end subroutine free

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
         equal = (a%unit == b%unit) .and. (a%get_level() == b%get_level())
      class default
         equal = .false.
      end select

   end function equal


end module PFL_StreamHandler
