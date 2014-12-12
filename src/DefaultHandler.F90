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
      procedure :: emit
      procedure :: close ! noop
   end type DefaultHandler


   interface DefaultHandler
      module procedure :: newDefaultHandler
   end interface DefaultHandler


contains

   function newDefaultHandler(testUnit) result(handler)
      use iso_fortran_env, only: output_unit
      type (DefaultHandler) :: handler
      integer, optional, intent(in) :: testUnit

      if (present(testUnit)) handler%unit = testUnit

   end function newDefaultHandler

   subroutine emit(this, message)
      class (DefaultHandler), intent(in) :: this
      character(len=*), intent(in) :: message

      write(this%unit,'(a)') message

   end subroutine emit


   subroutine close(this)
      class (DefaultHandler), intent(inout) :: this
   end subroutine close

end module ASTG_DefaultHandler_mod
