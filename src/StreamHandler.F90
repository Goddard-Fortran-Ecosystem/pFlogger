module ASTG_StreamHandler_mod
   ! A handler class which writes logging events to a stream, e.g. STDOUT
   ! Note that this class does not close the stream.
   use iso_fortran_env, only: output_unit
   use ASTG_SeverityLevels_mod, only: INFO
   use ASTG_AbstractHandler_mod, only: AbstractHandler
   implicit none
   private

   public :: StreamHandler

   type, extends(AbstractHandler) :: StreamHandler
      private
      integer :: unit = output_unit ! stdout
   contains
      procedure :: emitMessage
      procedure :: close ! noop
   end type StreamHandler

   interface StreamHandler
      module procedure :: newStreamHandler
   end interface StreamHandler

   
contains

   
   function newStreamHandler(unit, level) result(handler)
      ! Initialize the stream handler
      use iso_fortran_env, only: output_unit
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

   
   subroutine emitMessage(this, levelString, message)
      ! Write a string to a stream. Level is specified in levelString
      class (StreamHandler), intent(in) :: this
      character(len=*), intent(in) :: levelString
      character(len=*), intent(in) :: message

      write(this%unit,'(a)') levelString // ': ' // message
      
   end subroutine emitMessage

   
   subroutine close(this)
      class (StreamHandler), intent(inout) :: this
   end subroutine close

end module ASTG_StreamHandler_mod
