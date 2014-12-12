module ASTG_DefaultHandler_mod
   use iso_fortran_env, only: output_unit
   use ASTG_AbstractHandler_mod
   implicit none
   private

   public :: DefaultHandler

   type, extends(AbstractHandler) :: DefaultHandler
      private
      integer :: unit = output_unit ! stdout
   contains
      procedure :: emitMessage
      procedure :: close ! noop
   end type DefaultHandler

   interface DefaultHandler
      module procedure :: newDefaultHandler
   end interface DefaultHandler

   
contains

   
   function newDefaultHandler(testUnit, level) result(handler)
      use iso_fortran_env, only: output_unit
      type (DefaultHandler) :: handler
      integer, optional, intent(in) :: testUnit
      integer, optional, intent(in) :: level
     
      integer :: level_

      if (present(testUnit)) handler%unit = testUnit
      if (present (level)) then
        level_ = level
      else
        level_ = INFO
      end if
      call handler%setLevel(level_)
      
   end function newDefaultHandler

   
   subroutine emitMessage(this, message)
      class (DefaultHandler), intent(in) :: this
      character(len=*), intent(in) :: message

      write(this%unit,'(a)') message
      
   end subroutine emitMessage

   
   subroutine close(this)
      class (DefaultHandler), intent(inout) :: this
   end subroutine close

end module ASTG_DefaultHandler_mod
