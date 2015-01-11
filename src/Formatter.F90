! Formatter instances are used to convert a LogRecord to text.
module ASTG_Formatter_mod
   use ASTG_Object_mod
   use ASTG_LogRecord_mod
   implicit none
   private

   public :: Formatter
   public :: getMessage

   type, extends(Object) :: Formatter
      private
      character(len=:), allocatable :: fmt
   contains
      procedure :: format
      procedure :: toString_unlimitedPoly
      generic :: toString => toString_unlimitedPoly
      procedure :: toStringOther
   end type Formatter

   interface Formatter
      module procedure newFormatter
   end interface Formatter
   
   
contains

   
   ! Initialize a formatter with a string which makes use of
   ! knowledge of the LogRecord attributes
   ! Default value is "message", else use optional arguments
   function newFormatter(fmt) result(f)
      type (Formatter) :: f
      character(len=*), optional, intent(in) :: fmt
      
      if (present(fmt)) then
         f%fmt = fmt
      else
         f%fmt = '%(message::a)'
      end if
      
   end function newFormatter


   function format(this, record) result(logMessage)
      use ASTG_FormatParser_mod
      use FTL_String_mod
      use FTL_CIStringXUMap_mod
      character(len=:), allocatable :: logMessage
      class (Formatter), intent(in) :: this
      class (LogRecord), intent(in) :: record

      type (FormatParser) :: parser
      type (CIStringXUMap) :: extra
      type (CIStringXUMapIter) :: iter

      extra = record%extra

      logMessage = record%getMessage()
      iter = extra%emplace('message', String(logMessage))

      logMessage = parser%format(this%fmt, extra=extra)
      
   end function format


   ! This function operates on different input data types and returns a string
   function toString_unlimitedPoly(this, arg) result(str)
      use FTL_StringUtilities_mod
      use iso_fortran_env
      class (Formatter), intent(in) :: this
      class (*), intent(in) :: arg

      character(len=80) :: buffer
      character(len=:), allocatable :: str
       
      select type (p => arg)

      type is (integer(int32))
         str = toString(p)
      type is (integer(int64))
         str = toString(p)

      type is (real(real32))
         str = toString(p)
      type is (real(real64))
         str = toString(p)

      type is (complex(real32))
         str = toString(p)
      type is (complex(real64))
         str = toString(p)

      type is (character(len=*))
         str = toString(p)

      type is (logical)
         str = toString(p)

      class default ! user defined
         str = this%toStringOther(arg) ! allow subclasses to provid extensions
      end select

   end function toString_unlimitedPoly


   function toStringOther(this, arg) result(str)
      use ASTG_Exception_mod
      character(len=:), allocatable :: str
      class (Formatter), intent(in) :: this
      class (*), intent(in) :: arg

      str = ''
      call throw('Logger::toString_other() not implemented.')
      

   end function toStringOther

   
   ! return the message for this LogRecord.
   function getMessage(this) result(message)
      class (LogRecord), intent(in) :: this
      character(len=:), allocatable :: message
      
!!$      message = this%record%getMessage()
      
   end function getMessage

   
end module ASTG_Formatter_mod
