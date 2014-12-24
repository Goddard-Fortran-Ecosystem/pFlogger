module ASTG_RootLogger_mod
   use ASTG_Logger_mod
   implicit none
   private

   public :: RootLogger

   type, extends(Logger) :: RootLogger
   end type RootLogger

   
   interface RootLogger
      module procedure :: newRootLogger
   end interface RootLogger


contains

   
   function newRootLogger(level) result(lgr)
      type (RootLogger) :: lgr
      integer, intent(in) :: level !! NOT OPTIONAL !!

      call lgr%setName('root')
      call lgr%setLevel(level)

   end function newRootLogger

end module ASTG_RootLogger_mod
