module ASTG_PrivateLogging_mod
   use ASTG_Logger_mod
   implicit none
   private

   public :: PrivateLogging

   type :: PrivateLogging
   contains
      procedure :: getLogger
   end type PrivateLogging

contains

   function getLogger(this, name) result(lgr)
      class (Logger), pointer :: lgr
      class (PrivateLogging), intent(in) :: this
      character(len=*), intent(in) :: name
      
      allocate(lgr, source=Logger(name))

   end function getLogger

end module ASTG_PrivateLogging_mod
