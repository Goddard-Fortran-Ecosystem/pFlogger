! Formatter instances are used to convert a LogRecord to text.
module ASTG_Formatter_mod
   use ASTG_Object_mod
   use ASTG_LogRecord_mod
   implicit none
   private

   public :: Formatter

   type, extends(Object) :: Formatter
      private
      type (LogRecord) :: record
   contains
      procedure :: getMessage
   end type Formatter

   interface Formatter
      module procedure :: newFormatter
   end interface Formatter

   
contains

   
   ! Initialize a formatter with a string which makes use of
   ! knowledge of the LogRecord attributes
   ! Default value is "message", else use optional arguments
   function newFormatter(message, opt1, opt2) result(rec)
      character(len=*), intent(in) :: message
      class(*), optional, intent(in) :: opt1
      class(*), optional, intent(in) :: opt2
      type (Formatter) :: rec
      character(len=:), allocatable :: str
      
      str = ''
      if (present(opt1))  then
        str = str // handle_(opt1)
      end if
      if (present(opt2))  then
        str = str // handle_(opt2)
      end if
      
      rec%message = message // str
      
   end function newFormatter

   
   ! This function operates on different input data types and returns a string
   function handle_(arg) result(str)
      class (*), optional, intent(in) :: arg
      character(len=:), allocatable :: str
      character(len=80) :: buffer

      if (.not. present(arg)) then
         str = ''
         return
      end if

      select type (arg)
      type is (integer(int32))
         write(buffer,'(i0)') arg
         str = trim(buffer)
      type is (real(real32))
         write(buffer,'(g20.14)') arg
         str = trim(buffer)
      type is (real(real64))
         write(buffer,'(g20.14)') arg
         str = trim(buffer)
      type is (character(len=*))
         str = trim(arg)
      type is (logical)
         write(buffer,'(L1)') arg
         str = trim(buffer)
      type is (integer(int64))
      type is (real(real128))
      type is (complex(real32))
         write(buffer,'(2g20.14)') real(arg),aimag(arg)
         str = trim(buffer)
      type is (complex(real64))
      type is (complex(real128))

      class default ! user defined
         str = 'unsupported'
      end select

   end function handle_

   
   ! return the message for this LogRecord.
   function getMessage(this) result(message)
      class (LogRecord), intent(in) :: this
      character(len=:), allocatable :: message
      
      message = this%record%getMessage()
      
   end function getMessage

   
end module ASTG_Formatter_mod
