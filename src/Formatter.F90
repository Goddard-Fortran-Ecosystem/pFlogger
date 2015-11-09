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
   public :: get_sim_time
      

   type, extends(Object) :: Formatter
      private
      character(len=:), allocatable :: fmt
      character(len=:), allocatable :: datefmt
!!$      type (FormatParser) :: p
      logical :: uses_asc_time
      logical :: uses_sim_time
   contains
      procedure :: format
      procedure :: formatTime
      procedure :: usesAscTime
      procedure :: usesSimTime
   end type Formatter

   interface Formatter
      module procedure newFormatter
   end interface Formatter
   
   character(len=*), parameter :: DEFAULT_DATE_FMT = &
        & '%(Y)i4.4~-%(M)i2.2~-%(D)i2.2 %(HH)i2.2~:%(MM)i2.2~:%(SS)i2.2~.%(MS)i3.3'


   abstract interface
      subroutine get_time(dict)
         use ASTG_StringUnlimitedMap_mod
         type (Map), intent(out) :: dict
      end subroutine get_time
   end interface

   procedure(get_time), pointer :: get_sim_time ! => null()

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

      f%uses_asc_time = f%usesAscTime()
      f%uses_sim_time = f%usesSimTime()

!!$      call f%p%parse(f%fmt)
      
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
      use ASTG_StringUnlimitedMap_mod, only: StringUnlimitedMap => Map

      character(len=:), allocatable :: asctime
      class (Formatter), intent(in) :: this
      class (LogRecord), intent(in) :: record
      character(len=*), optional, intent(in) :: datefmt

      type (StringUnlimitedMap) :: extra
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
         dateFmt_ = DEFAULT_DATE_FMT
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
      use FTL
      use ASTG_FormatString_mod
      use ASTG_StringUnlimitedMap_mod, only: StringUnlimitedMap => Map
      use ASTG_StringUnlimitedMap_mod, only: StringUnlimitedMapIterator => MapIterator

      character(len=:), allocatable :: logMessage
      class (Formatter), intent(in) :: this
      class (LogRecord), intent(in) :: record

      character(len=:), allocatable :: asctime
      character(len=:), allocatable :: simtime
      type (StringUnlimitedMap) :: extra
      type (StringUnlimitedMapIterator) :: extraIter
      character(len=:), allocatable :: msg

      call extra%deepCopy(record%extra)
!!$      extra = record%extra
      msg = record%getMessage()

#ifdef __GFORTRAN__
      call extra%insert('message', String(msg))
#else
      call extra%insert('message', msg)
#endif
      if(this%uses_asc_time) then
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
      if(this%uses_sim_time) then
         block
           type (StringUnlimitedMap) :: dict
           if (allocated(this%datefmt)) then

              call get_sim_time(dict)
              simtime = FormatString(this%datefmt, dict)
           else
              simtime = FormatString(DEFAULT_DATE_FMT, dict)
           end if
         end block

         
#ifdef __GFORTRAN__
         call extra%insert('simtime', String(simtime))
#else
         call extra%insert('simtime', simtime)
#endif
      end if

!!$      logMessage = FormatString(this%p, extra)
      logMessage = FormatString(this%fmt, extra)
    
   end function format



   !---------------------------------------------------------------------------  
   ! FUNCTION: 
   ! usesTime
   !
   ! DESCRIPTION: 
   ! Check if the format uses the creation time of the record.
   !---------------------------------------------------------------------------
   logical function usesAscTime(this)
      class (Formatter), intent(in) :: this
      usesAscTime = (index(this%fmt,'%(asctime)') > 0)
   end function usesAscTime

   !---------------------------------------------------------------------------  
   ! FUNCTION: 
   ! usesSimTime
   !
   ! DESCRIPTION: 
   ! Check if the format uses the creation time of the record.
   !---------------------------------------------------------------------------
   logical function usesSimTime(this)
      class (Formatter), intent(in) :: this
      usesSimTime = (index(this%fmt,'%(simtime)') > 0)
   end function usesSimTime

end module ASTG_Formatter_mod
