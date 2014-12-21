module ASTG_Exception_mod
   implicit none
   private

   public :: throw

contains

#ifdef USE_PFUNIT

   subroutine throw(message)
      use pFUnit_mod, only: pf_throw => throw
      character(len=*), intent(in) :: message

      call pf_throw(message)
      
   end subroutine throw

#else

   subroutine throw(message)
      use iso_fortran_env, only: OUTPUT_UNIT
      character(len=*), intent(in) :: message

      write(OUTPUT_UNIT,*) message
      stop
      
   end subroutine throw

#endif

end module ASTG_Exception_mod
