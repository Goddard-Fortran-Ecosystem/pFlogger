!------------------------------------------------------------------------------
! NASA/GSFC, CISTO, Code 606, Advanced Software Technology Group
!------------------------------------------------------------------------------
!
! MODULE: ASTG_FastFormatter_mod
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
!> %(name)a            Name of the logger
!> %(levelname)a       Text logging level for the message ("DEBUG", "INFO",
!>                        "WARNING", "ERROR", "CRITICAL")
!> %(asctime)a         Textual time when the LogRecord was created
!> %(message)a         The result of record.getMessage(), computed just as
!>                        the record is emitted
!
! REVISION HISTORY:
! 01 Jan 2015 - Initial Version
!------------------------------------------------------------------------------
module ASTG_FastFormatter_mod
   use ASTG_Object_mod
   use ASTG_LogRecord_mod
   use ASTG_Formatter_mod
   implicit none
   private

   public :: FastFormatter

   type, extends(Formatter) :: FastFormatter
      private
   contains
      procedure :: format
   end type FastFormatter

   interface FastFormatter
      module procedure newFormatter
   end interface FastFormatter
   
   
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
   function newFormatter() result(f)
      type (FastFormatter) :: f
      
   end function newFormatter


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
      use ASTG_FormatString_mod
      use ASTG_CIStringUnlimitedMap_mod, only: CIStringUnlimitedMap => Map
      use ASTG_CIStringUnlimitedMap_mod, only: CIStringUnlimitedMapIterator => MapIterator
      use ASTG_String_mod

      character(len=:), allocatable :: logMessage
      class (FastFormatter), intent(in) :: this
      class (LogRecord), intent(in) :: record

      logMessage = record%messageFormat

   end function format

   
end module ASTG_FastFormatter_mod
