subroutine pfunit_throw(message)
   use pFUnit_mod, only: throw
   implicit none
   character(len=*), intent(in) :: message

   call throw(message)

end subroutine pfunit_throw

subroutine init_tests()
   use pflogger
   use PFL_Exception_mod, only: set_throw_fptr
   implicit none
   external :: pfunit_throw

   call initialize()
   call set_throw_fptr(pfunit_throw)

end subroutine init_tests
