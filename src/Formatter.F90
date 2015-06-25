!------------------------------------------------------------------------------
! NASA/GSFC, CISTO, Code 606, Advanced Software Technology Group
!------------------------------------------------------------------------------
!
! MODULE: ASTG_Formatter_mod
!
!> @author 
!> ASTG staff
!
! DESCRIPTION: 
!> @brief
!> Formatter instances are used to convert a LogRecord to text.
!> Formatters need to know how a LogRecord is constructed. They are
!> responsible for converting a LogRecord to (usually) a string.
!> The Formatter can be initialized with a format string which makes use of
!> knowledge of the LogRecord attributes - e.g. the default value mentioned
!> above makes use of the fact that the user's message and arguments are
!> preformatted into a LogRecord's message attribute. Currently, the useful
!> attributes in a LogRecord are described by:
!>
!> %{name)s            Name of the logger
!> %{levelname}s       Text logging level for the message ("DEBUG", "INFO",
!>                        "WARNING", "ERROR", "CRITICAL")
!> %{asctime}s         Textual time when the LogRecord was created
!> %{message}s         The result of record.getMessage(), computed just as
!>                        the record is emitted
!
! REVISION HISTORY:
! 01 Jan 2015 - Initial Version
!------------------------------------------------------------------------------
module ASTG_Formatter_mod
   use ASTG_Object_mod
   use ASTG_LogRecord_mod
   implicit none
   private

   public :: Formatter

   type, extends(Object) :: Formatter
      private
      integer :: placeholder
      character(len=:), allocatable :: fmt
      character(len=:), allocatable :: datefmt
   contains
      procedure :: format
      procedure :: formatTime
      procedure :: toString_unlimitedPoly
      generic :: toString => toString_unlimitedPoly
      procedure :: toStringOther
      procedure :: usesTime
   end type Formatter

   interface Formatter
      module procedure newFormatter
   end interface Formatter
   
   
contains

 
   !---------------------------------------------------------------------------  
   ! FUNCTION: 
   ! newFormatter
   !
   ! DESCRIPTION: 
   ! Initialize a formatter with specified format strings or a default as
   ! described above. Allow for specialized date formatting using the optional
   ! datefmt argument.
   !---------------------------------------------------------------------------
   function newFormatter(fmt, datefmt) result(f)
      type (Formatter) :: f
      character(len=*), optional, intent(in) :: fmt
      character(len=*), optional, intent(in) :: datefmt
      
      if (present(fmt)) then
         f%fmt = fmt
      else
         f%fmt = '%{message,a}'
      end if
       
      if (present(datefmt)) then
         f%datefmt = datefmt
      else
         f%datefmt = ''
      end if
      
   end function newFormatter


   !---------------------------------------------------------------------------  
   ! FUNCTION: 
   ! formatTime
   !
   ! DESCRIPTION: 
   ! Return the creation time of the specified record as formatted text.
   !---------------------------------------------------------------------------
   function formatTime(this, record, datefmt) result(logMessage)
      use ASTG_NewFormatParser_mod, only: formatArgs
      use ASTG_CIStringUnlimitedMap_mod, only: CIStringUnlimitedMap => Map

      character(len=:), allocatable :: logMessage
      class (Formatter), intent(in) :: this
      class (LogRecord), intent(in) :: record
      character(len=*), optional, intent(in) :: datefmt

      type (CIStringUnlimitedMap) :: extra
      
      call extra%deepCopy(record%extra)
      if (present(datefmt)) then
         logMessage = formatArgs(datefmt, extra=extra)
      else
         logMessage = datefmt
      end if

   end function formatTime

   
   !---------------------------------------------------------------------------  
   ! FUNCTION: 
   ! format
   !
   ! DESCRIPTION: 
   ! Format the specified record as text - this calls FormatParser's format.
   ! The record's attribute dictionary is used as the operand to a string
   ! formatting operation which yields the returned string. If the formatting 
   ! string uses the time (as determined by a call to usesTime(), formatTime()
   ! is called to format the event time. The formatting of the dictionary is
   ! then performed by the format parser. For example, for a record message
   ! containing "hello" format returns "INFO: logName: Hello".
   !---------------------------------------------------------------------------
   function format(this, record) result(logMessage)
      use ASTG_NewFormatParser_mod, only: formatArgs
      use ASTG_CIStringUnlimitedMap_mod, only: CIStringUnlimitedMap => Map
      use ASTG_CIStringUnlimitedMap_mod, only: CIStringUnlimitedMapIterator => MapIterator

      character(len=:), allocatable :: logMessage
      character(len=:), allocatable :: asctime
      class (Formatter), intent(in) :: this
      class (LogRecord), intent(in) :: record

      type (CIStringUnlimitedMap) :: extra
      type (CIStringUnlimitedMapIterator) :: extraIter
      character(len=:), allocatable :: msg

      call extra%deepCopy(record%extra)
      msg = record%getMessage()

      call extra%insert('message', msg)
      if(this%usesTime()) then
         asctime = this%formatTime(record, datefmt=this%datefmt)
         call extra%insert('asctime', asctime)
      end if
      logMessage = formatArgs(this%fmt, extra=extra)
    
   end function format


   !---------------------------------------------------------------------------  
   ! FUNCTION: 
   ! usesTime
   !
   ! DESCRIPTION: 
   ! Check if the format uses the creation time of the record.
   !---------------------------------------------------------------------------
   logical function usesTime(this)
      class (Formatter), intent(in) :: this
      usesTime = (index(this%fmt,'%{asctime') > 0)
   end function usesTime

   
   !---------------------------------------------------------------------------  
   ! FUNCTION: 
   ! toString_unlimitedPoly
   !
   ! DESCRIPTION: 
   ! This function operates on different input data types and returns a string.
   !---------------------------------------------------------------------------
   function toString_unlimitedPoly(this, arg) result(str)
      use ASTG_StringUtilities_mod
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


   !---------------------------------------------------------------------------  
   ! FUNCTION: 
   ! toStringOther
   !
   ! DESCRIPTION: 
   ! Exception handler for unimplemented unlimited polymorphic variables.
   !---------------------------------------------------------------------------
   function toStringOther(this, arg) result(str)
      use ASTG_Exception_mod
      character(len=:), allocatable :: str
      class (Formatter), intent(in) :: this
      class (*), intent(in) :: arg

      str = ''
      call throw('Logger::toString_other() not implemented.')
      

   end function toStringOther

   
end module ASTG_Formatter_mod
