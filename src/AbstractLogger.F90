!------------------------------------------------------------------------------
! NASA/GSFC, CISTO, Code 606, Advanced Software Technology Group
!------------------------------------------------------------------------------
!
!*MODULE: PFL_AbstractLogger
!
!> @brief AbstractLogger class provides common code to use in Logger class.
!
!> @author ASTG staff
!> @date 01 Jan 2015 - Initial Version
!------------------------------------------------------------------------------
module PFL_AbstractLogger
   use PFL_Filterer
   implicit none
   private

   public :: AbstractLogger

   type, abstract, extends(Filterer) :: AbstractLogger
   contains
     procedure :: free
   end type AbstractLogger

contains

! no op
    subroutine free(this)
       class(AbstractLogger), intent(inout) :: this
    end subroutine free

end module PFL_AbstractLogger
