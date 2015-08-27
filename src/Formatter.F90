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
module ASTG_Formatter_mod
   use ASTG_Object_mod
   use ASTG_LogRecord_mod
   use ASTG_FormatParser_mod
   implicit none
   private

   public :: Formatter

   type, extends(Object) :: Formatter
      private
      character(len=:), allocatable :: fmt
      character(len=:), allocatable :: datefmt
      type (FormatParser) :: p
   contains
      procedure :: format
      procedure :: formatTime
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
         f%fmt = '%(message)a'
      end if
       
      if (present(datefmt)) then
         f%datefmt = datefmt
      end if

      call f%p%parse(f%fmt)
      
   end function newFormatter


   !---------------------------------------------------------------------------  
   ! FUNCTION: 
   ! formatTime
   !
   ! DESCRIPTION: 
   ! Return the creation time of the specified record as formatted text.
   !---------------------------------------------------------------------------
   function formatTime(this, record, datefmt) result(asctime)
      use ASTG_FormatString_mod
      use ASTG_CIStringUnlimitedMap_mod, only: CIStringUnlimitedMap => Map

      character(len=:), allocatable :: asctime
      class (Formatter), intent(in) :: this
      class (LogRecord), intent(in) :: record
      character(len=*), optional, intent(in) :: datefmt

      type (CIStringUnlimitedMap) :: extra
      character(len=:), allocatable :: datefmt_

      associate (t => record%time_fields)
        call extra%insert('Y', t(1))
        call extra%insert('M', t(2))
        call extra%insert('D', t(3))
        call extra%insert('HH', t(5))
        call extra%insert('MM', t(6))
        call extra%insert('SS', t(7))
        call extra%insert('MS', t(8))
      end associate

      if (present(datefmt)) then
         dateFmt_ = datefmt
      else
         dateFmt_ = '%(Y)i4.4~-%(M)i2.2~-%(D)i2.2 ' // &
              & '%(HH)i2.2~:%(MM)i2.2~:%(SS)i2.2~.%(MS)i3.3'
      end if
         
      asctime = FormatString(dateFmt_, extra)

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
      use ASTG_FormatString_mod
      use ASTG_CIStringUnlimitedMap_mod, only: CIStringUnlimitedMap => Map
      use ASTG_CIStringUnlimitedMap_mod, only: CIStringUnlimitedMapIterator => MapIterator
      use ASTG_String_mod

      character(len=:), allocatable :: logMessage
      class (Formatter), intent(in) :: this
      class (LogRecord), intent(in) :: record

      character(len=:), allocatable :: asctime
      type (CIStringUnlimitedMap) :: extra
      type (CIStringUnlimitedMapIterator) :: extraIter
      character(len=:), allocatable :: msg

      call extra%deepCopy(record%extra)
      msg = record%getMessage()

#ifdef __GFORTRAN__
      call extra%insert('message', String(msg))
#else
      call extra%insert('message', msg)
#endif
      if(this%usesTime()) then
         if (allocated(this%datefmt)) then
            asctime = this%formatTime(record, datefmt=this%datefmt)
         else
            asctime = this%formatTime(record)
         end if
#ifdef __GFORTRAN__
         call extra%insert('asctime', String(asctime))
#else
         call extra%insert('asctime', asctime)
#endif
      end if

      logMessage = FormatString(this%p, extra)
    
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
      usesTime = (index(this%fmt,'%(asctime)') > 0)
   end function usesTime

end module ASTG_Formatter_mod
