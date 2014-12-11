module ASTG_BasicLogger_mod
   use ASTG_AbstractLogger_mod, only: AbstractLogger
   use ASTG_StringUtilities_mod
   implicit none
   private

   public :: BasicLogger

   type, extends(AbstractLogger) :: BasicLogger
      private
   contains
      procedure :: toString_integer
   end type BasicLogger

   interface BasicLogger
      module procedure :: newBasicLogger
   end interface BasicLogger
      
contains


   function newBasicLogger(fileName) result(logger)
      type (BasicLogger) :: logger
      character(len=*), intent(in) :: fileName
      call logger%setFileName(fileName)
   end function newBasicLogger
   

   function toString_integer(this, i) result(string)
      character(len=:), allocatable :: string
      class (BasicLogger), intent(inout) :: this
      integer, intent(in) :: i

      string = toString(i)
      
   end function toString_integer


end module ASTG_BasicLogger_mod
