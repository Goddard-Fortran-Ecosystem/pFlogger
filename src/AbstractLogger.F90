!------------------------------------------------------------------------------
! NASA/GSFC, CISTO, Code 606, Advanced Software Technology Group
!------------------------------------------------------------------------------
!
!*MODULE: PFL_AbstractLogger_mod
!
!> @brief AbstractLogger class provides common code to use in Logger class.
!
!> @author ASTG staff
!> @date 01 Jan 2015 - Initial Version
!------------------------------------------------------------------------------
module PFL_AbstractLogger_mod
   use PFL_Filterer_mod
   implicit none
   private

   public :: AbstractLogger

   type, abstract, extends(Filterer) :: AbstractLogger
   end type AbstractLogger

end module PFL_AbstractLogger_mod