!------------------------------------------------------------------------------
! NASA/GSFC, CISTO, Code 606, Advanced Software Technology Group
!------------------------------------------------------------------------------
!
!*MODULE: ASTG_RootLogger_mod
!
!> @brief Defines ROOT logger.
!> @details
!> Defines a unique -ROOT- logger in the logger hierarchy. It _MUST_ have a
!> level and is named 'root'.
!
!> @author ASTG staff
!> @date 01 Jan 2015 - Initial Version  
!------------------------------------------------------------------------------
module ASTG_RootLogger_mod
   use ASTG_Logger_mod
   implicit none
   private

   public :: RootLogger

   type, extends(Logger) :: RootLogger
   end type RootLogger

   interface RootLogger
      module procedure newRootLogger
   end interface RootLogger


contains

   
   !---------------------------------------------------------------------------  
   ! FUNCTION: 
   ! newRootLogger
   !
   ! DESCRIPTION: 
   ! Initialize the logger with the name "root".
   !---------------------------------------------------------------------------
   function newRootLogger(level) result(lgr)
      type (RootLogger) :: lgr
      integer, intent(in) :: level !! NOT OPTIONAL !!

      call lgr%setName('ROOT_LOGGER')
      call lgr%setLevel(level)

   end function newRootLogger

end module ASTG_RootLogger_mod
