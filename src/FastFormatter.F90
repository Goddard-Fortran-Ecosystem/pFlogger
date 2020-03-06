#include "error_handling_macros.fh"
!------------------------------------------------------------------------------
! NASA/GSFC, CISTO, Code 606, Advanced Software Technology Group
!------------------------------------------------------------------------------
!
! MODULE: PFL_FastFormatter
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
!> %(level_name)a       Text logging level for the message ("DEBUG", "INFO",
!>                        "WARNING", "ERROR", "CRITICAL")
!> %(asctime)a         Textual time when the LogRecord was created
!> %(message)a         The result of record.getMessage(), computed just as
!>                        the record is emitted
!
! REVISION HISTORY:
! 01 Jan 2015 - Initial Version
!------------------------------------------------------------------------------
module PFL_FastFormatter
   use PFL_Object
   use PFL_LogRecord
   use PFL_Formatter
   use PFL_KeywordEnforcer
   implicit none
   private

   public :: FastFormatter

   type, extends(Formatter) :: FastFormatter
      private
   contains
      procedure :: format
   end type FastFormatter

   interface FastFormatter
      module procedure new_FastFormatter
   end interface FastFormatter
   
   
contains

 
   !---------------------------------------------------------------------------  
   ! FUNCTION: 
   ! new_FastFormatter
   !
   ! DESCRIPTION: 
   ! Initialize a formatter with specified format strings or a default as
   ! described above. Allow for specialized date formatting using the optional
   ! datefmt argument.
   !---------------------------------------------------------------------------
   function new_FastFormatter() result(f)
      type (FastFormatter) :: f
      
   end function new_FastFormatter


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
   function format(this, record, unusable, rc) result(logMessage)
      use PFL_FormatString
      character(len=:), allocatable :: logMessage
      class (FastFormatter), intent(in) :: this
      class (LogRecord), intent(in) :: record
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      _UNUSED_DUMMY(unusable)
      
      logMessage = record%message_format
      _RETURN(_SUCCESS,rc)

   end function format

   
end module PFL_FastFormatter
