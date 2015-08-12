subroutine pfunit_throw(message)
   use pFUnit_mod, only: throw
   implicit none
   character(len=*), intent(in) :: message

   call throw(message)

end subroutine pfunit_throw

subroutine initTests()
   use ASTG_Exception_mod, only: setThrowFunPtr
   implicit none
   external :: pfunit_throw

   call setThrowFunPtr(pfunit_throw)

end subroutine initTests
