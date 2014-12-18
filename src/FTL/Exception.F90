module FTL_Exception_mod
#ifdef USE_PFUNIT
   use pfunit_mod, only : pf_throw => throw
#endif

   implicit none
   private

   public :: throw

contains
   
   subroutine throw(string)
      character(len=*), intent(in) :: string

#ifdef USE_PFUNIT
      call pf_throw(string)
#else
      write(*,*) string
      stop
#endif
   end subroutine throw

end module FTL_Exception_mod
