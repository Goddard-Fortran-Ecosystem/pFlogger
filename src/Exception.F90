!------------------------------------------------------------------------------
! NASA/GSFC, CISTO, Code 606, Advanced Software Technology Group
!------------------------------------------------------------------------------
!
!*MODULE: PFL_Exception_mod
!
!> @brief
!> This module contains a simple method to handle exceptions (runtime errors).
!
!> @author ASTG staff
!> @date 01 Jan 2015 - Initial Version
!------------------------------------------------------------------------------
module PFL_Exception_mod
   implicit none
   private

   public :: throw
   public :: set_throw_fptr

   procedure(throw), pointer :: throw_fptr => null()

contains


!---------------------------------------------------------------------------  
!*ROUTINE: throw
!
!> @brief Throws exception with explanation for the error.
!---------------------------------------------------------------------------

   subroutine throw(file, line, message)
      character(len=*), intent(in) :: file
      integer, intent(in) :: line
      character(len=*), intent(in) :: message

      if (.not. associated(throw_fptr)) then
         throw_fptr => print_and_stop
      end if

      call throw_fptr(file, line, message)
      
   end subroutine throw

   subroutine print_and_stop(file, line, message)
      use iso_fortran_env, only: OUTPUT_UNIT
      character(len=*), intent(in) :: file
      integer, intent(in) :: line
      character(len=*), intent(in) :: message

      write(OUTPUT_UNIT,*) file, line
      write(OUTPUT_UNIT,*) message
      stop 1
      
   end subroutine print_and_stop

   subroutine set_throw_fptr(ptr)
      procedure(throw) :: ptr
      throw_fptr => ptr
   end subroutine set_throw_fptr

end module PFL_Exception_mod
