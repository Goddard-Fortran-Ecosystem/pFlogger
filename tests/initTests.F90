subroutine pfunit_throw(message)
   use pFUnit_mod, only: throw
   implicit none
   character(len=*), intent(in) :: message

   call throw(message)

end subroutine pfunit_throw

subroutine initTests()
   use pflogger
   use PFL_Exception_mod, only: setThrowFunPtr
   use FTL_Exception_mod, only: FTL_setThrowFunPtr => setThrowFunPtr
   implicit none
   external :: pfunit_throw

   call initialize()
   call setThrowFunPtr(pfunit_throw)
   call FTL_setThrowFunPtr(pfunit_throw)

end subroutine initTests
