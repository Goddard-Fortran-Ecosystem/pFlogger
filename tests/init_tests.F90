subroutine pfunit_throw(file, line, message)
   use funit, only: throw, SourceLocation
   implicit none
   character(len=*), intent(in) :: file
   integer, intent(in) :: line
   character(len=*), intent(in) :: message

   call throw(message, location=SourceLocation(file,line))

end subroutine pfunit_throw

subroutine init_tests()
   use pflogger
   use PFL_Exception, only: set_throw_fptr
   implicit none
   external :: pfunit_throw

   call initialize()
   call set_throw_fptr(pfunit_throw)

end subroutine init_tests
