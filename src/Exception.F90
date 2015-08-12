!------------------------------------------------------------------------------
! NASA/GSFC, CISTO, Code 606, Advanced Software Technology Group
!------------------------------------------------------------------------------
!
!*MODULE: ASTG_Exception_mod
!
!> @brief
!> This module contains a simple method to handle exceptions (runtime errors).
!
!> @author ASTG staff
!> @date 01 Jan 2015 - Initial Version
!------------------------------------------------------------------------------
module ASTG_Exception_mod
   implicit none
   private

   public :: throw
   public :: setThrowFunPtr

   procedure(throw), pointer :: throwFunPtr => null()

contains


!---------------------------------------------------------------------------  
!*ROUTINE: throw
!
!> @brief Throws exception with explanation for the error.
!---------------------------------------------------------------------------

   subroutine throw(message)
      character(len=*), intent(in) :: message

      if (.not. associated(throwFunPtr)) then
         throwFunPtr => printAndStop
      end if

      call throwFunPtr(message)
      
   end subroutine throw

   subroutine printAndStop(message)
      use iso_fortran_env, only: OUTPUT_UNIT
      character(len=*), intent(in) :: message

      write(OUTPUT_UNIT,*) message
      stop 1
      
   end subroutine printAndStop

   subroutine setThrowFunPtr(ptr)
      procedure(throw) :: ptr
      throwFunPtr => ptr
   end subroutine setThrowFunPtr

end module ASTG_Exception_mod
