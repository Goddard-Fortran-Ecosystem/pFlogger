#include "error_handling_macros.fh"
module MockHandler_mod
   ! Instances of this class inherit from the abstract handler. The idea of 
   ! the mock is to be able to control the data that is written to an output
   ! stream (STDOUT or a disk file) by collecting it in a buffer and then 
   ! test for the contents of the buffer's correctnes.
   use PFL_AbstractHandler
   use PFL_LogRecord
   use PFL_KeywordEnforcer
   use PFL_Exception
   implicit none
   private
   
   public :: MockHandler
   public :: MockBuffer

   type MockBuffer
      character(len=:), allocatable :: buffer
   end type MockBuffer

   type, extends (AbstractHandler) :: MockHandler
      type (MockBuffer), pointer :: buffer => null()
   contains
      procedure :: emit_message
      procedure :: close ! noop
      procedure :: free ! noop
      procedure :: flush => flushUnit
      procedure :: equal
   end type MockHandler

   interface MockHandler
      module procedure newMockHandler
   end interface MockHandler

   
contains

   function newMockHandler(buffer, level) result(handler)
      use PFL_Formatter
      type (MockHandler) :: handler
      type (MockBuffer), target, intent(in) :: buffer
      integer, optional, intent(in) :: level

      handler%buffer => buffer
      call handler%set_formatter(Formatter('%(message)a'))
      if (present(level)) call handler%set_level(level)

   end function newMockHandler

   
   subroutine emit_message(this, record, unusable, rc)
      class (MockHandler), intent(inout) :: this
      type (LogRecord), intent(in) :: record
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status

      this%buffer%buffer = this%format(record,rc=status)
      _VERIFY(status,'',rc)

      _RETURN(_SUCCESS,rc)
      
   end subroutine emit_message


   subroutine flushUnit(this)
      class (MockHandler), intent(in) :: this
   end subroutine flushUnit


   subroutine close(this)
      class (MockHandler), intent(inout) :: this
   end subroutine close

   subroutine free(this, rc)
      class(MockHandler), intent(inout) :: this
      integer, optional, intent(out) :: rc
   end subroutine free

   logical function equal(a, b)
      class (MockHandler), intent(in) :: a
      class (AbstractHandler), intent(in) :: b

      select type (b)
      class is (MockHandler)
         equal = associated(a%buffer, b%buffer) .and. (a%get_level() == b%get_level())
      class default
         equal = .false.
      end select

   end function equal

   
end module MockHandler_Mod
