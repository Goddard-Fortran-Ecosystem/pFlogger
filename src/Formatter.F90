!------------------------------------------------------------------------------
! NASA/GSFC, CISTO, Code 606, Advanced Software Technology Group
!------------------------------------------------------------------------------
!
! MODULE: PFL_Formatter
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
!> %(short_name)a      Short name of the logger
!> %(level_number)i    Integer severity level of the message. (DEBUG, INFO, ...)
!> %(level_name)a      Text logging level for the message ("DEBUG", "INFO",
!>                        "WARNING", "ERROR", "CRITICAL")
!> %(asctime)a         Textual time when the LogRecord was created
!> %(message)a         The result of record%get_message(), computed just as
!>                        the record is emitted
!> %(line)i            Integer line number of recorded in the record.
!> %(file)a            Text file recorded in the record.
!> %(basename)a        Text basename of file recorded in the record.
!
! REVISION HISTORY:
! 01 Jan 2015 - Initial Version
!------------------------------------------------------------------------------
#include "error_handling_macros.fh"
module PFL_Formatter
   use PFL_Object
   use PFL_LogRecord
   use PFL_FormatParser
   use gFTL_StringUnlimitedMap
   use PFL_KeywordEnforcer
   use PFL_Exception
   implicit none
   private

   public :: Formatter
   public :: newFormatter
   public :: get_sim_time

   type, extends(Object) :: Formatter
!!$      private
      character(len=:), allocatable :: fmt
      character(len=:), allocatable :: datefmt
      type (FormatParser) :: p
      logical :: fmt_uses_message
      logical :: fmt_uses_name
      logical :: fmt_uses_short_name
      logical :: fmt_uses_level_number
      logical :: fmt_uses_level_name
      logical :: fmt_uses_ascTime
      logical :: fmt_uses_simTime
      logical :: fmt_uses_line
      logical :: fmt_uses_file
      logical :: fmt_uses_basename
   contains
      procedure :: format
      procedure :: format_time
      procedure :: uses_keyword
      procedure, private :: fill_extra_keywords
   end type Formatter

   interface Formatter
      module procedure newFormatter
   end interface Formatter
   
   character(len=*), parameter :: DEFAULT_DATE_FMT = &
        & '%(Y)i4.4~-%(M)i2.2~-%(D)i2.2 %(HH)i2.2~:%(MM)i2.2~:%(SS)i2.2~.%(MS)i3.3'


   abstract interface
      subroutine get_time(dict)
         import StringUnlimitedMap
         type (StringUnlimitedMap), intent(out) :: dict
      end subroutine get_time
   end interface

   procedure(get_time), pointer :: get_sim_time ! => null()

   ! Private type - to force keyword usage for optional arguments
   type :: Unusable
   end type Unusable

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
   function newFormatter(fmt, unused, datefmt, extra) result(f)
      type (Formatter) :: f
      character(len=*), optional, intent(in) :: fmt
      type (Unusable), optional, intent(in) :: unused
      character(len=*), optional, intent(in) :: datefmt
      type (StringUnlimitedMap), optional, intent(in) :: extra

      if (present(fmt)) then
         f%fmt = fmt
      else
         f%fmt = '%(message)a'
      end if

      ! Allow for Python alias:
      if (f%uses_keyword('levelName')) then
         f%fmt = replace(f%fmt, '%(levelName)', '%(level_name)')
      end if

      if (present(datefmt)) then
         f%datefmt = datefmt
      end if

      f%fmt_uses_message = f%uses_keyword('message')
      f%fmt_uses_name = f%uses_keyword('name')
      f%fmt_uses_short_name = f%uses_keyword('short_name')
      f%fmt_uses_level_number = f%uses_keyword('level_number')
      f%fmt_uses_level_name = f%uses_keyword('level_name')

      f%fmt_uses_ascTime = f%uses_keyword('asctime')
      f%fmt_uses_simTime = f%uses_keyword('simtime')
      f%fmt_uses_line = f%uses_keyword('line')
      f%fmt_uses_file = f%uses_keyword('file')
      f%fmt_uses_basename = f%uses_keyword('basename')

      call f%p%parse(f%fmt)

      if (present(extra)) then
         call f%fill_extra_keywords(extra)
      end if

   end function newFormatter


   !---------------------------------------------------------------------------  
   ! FUNCTION: 
   ! format_time
   !
   ! DESCRIPTION: 
   ! Return the creation time of the specified record as formatted text.
   !---------------------------------------------------------------------------
   function format_time(this, record, datefmt) result(asctime)
      use PFL_FormatString

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

   end function format_time

   
   !---------------------------------------------------------------------------  
   ! FUNCTION: 
   ! format
   !
   ! DESCRIPTION: Format the specified record as text - this calls
   ! FormatParser's format.  The record's attribute dictionary is used
   ! as the operand to a string formatting operation which yields the
   ! returned string. If the formatting string uses the time (as
   ! determined by a call to uses_ascTime() or uses_simTime()),
   ! format_time() is called to format the event time. The formatting
   ! of the dictionary is then performed by the format parser. For
   ! example, for a record message containing "hello" format returns
   ! "INFO: logName: Hello".
   ! ---------------------------------------------------------------------------
   function format(this, record, unusable, rc) result(logMessage)
      use PFL_FormatString
      use gFTL_StringUnlimitedMap
      use yafyaml, only: String
      character(len=:), allocatable :: logMessage
      class (Formatter), intent(in) :: this
      class (LogRecord), intent(in) :: record
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      character(len=:), allocatable :: asctime
      character(len=:), allocatable :: simtime
      type (StringUnlimitedMap) :: extra
      type (StringUnlimitedMapIterator) :: extraIter
      character(len=:), allocatable :: msg
      type (StringUnlimitedMap) :: dict

      integer :: status
      
      if (associated(record%extra)) then
         call extra%deepCopy(record%extra)
      end if

      if (this%fmt_uses_message) then
         msg = record%get_message(rc=status)
         _VERIFY(status,'',rc)
         call extra%insert('message', String(msg))
      end if

      if (this%fmt_uses_name) then
         block
           character(len=:), allocatable :: name
           name = record%get_name()
           call extra%insert('name', String(name))
         end block
      end if
      
      if (this%fmt_uses_short_name) then
         block
           character(len=:), allocatable :: name, short_name
           name = record%get_name()
           short_name = name(scan(name, '.', back=.true.)+1:)
           call extra%insert('short_name', String(short_name))
         end block
      end if
      
      if (this%fmt_uses_level_number) then
         call extra%insert('level_number', record%get_level())
      end if

      if (this%fmt_uses_level_name) then
         block
           character(len=:), allocatable :: name
           name = record%get_level_name()
         call extra%insert('level_name', String(name))
       end block
      end if

      if(this%fmt_uses_ascTime) then
         if (allocated(this%datefmt)) then
            asctime = this%format_time(record, datefmt=this%datefmt)
         else
            asctime = this%format_time(record)
         end if
         call extra%insert('asctime', String(asctime))
      end if

      if(this%fmt_uses_simTime) then
         if (allocated(this%datefmt)) then
            call get_sim_time(dict)
            simtime = FormatString(this%datefmt, dict)
         else
            simtime = FormatString(DEFAULT_DATE_FMT, dict)
         end if
         
         call extra%insert('simtime', String(simtime))
      end if

      if (this%fmt_uses_line) then
         call extra%insert('line', record%get_line())
      end if

      if (this%fmt_uses_file) then
         call extra%insert('file', String(record%get_file()))
      end if

      if (this%fmt_uses_basename) then
         call extra%insert('basename', String(record%get_basename()))
      end if

      logMessage = FormatString(this%p, extra)

      _RETURN(_SUCCESS,rc)
   end function format

   !---------------------------------------------------------------------------  
   ! FUNCTION: 
   ! uses_keyword
   !
   ! DESCRIPTION: 
   ! Check if the format uses the keyword
   !---------------------------------------------------------------------------
   logical function uses_keyword(this, keyword)
      class (Formatter), intent(in) :: this
      character(*), intent(in) :: keyword

      integer :: idx

      idx = index(this%fmt,'%('//keyword//')')
      uses_keyword = ( idx > 0)

   end function uses_keyword


   ! TODO: make this replace all instances of substr
   function replace(str, substr, replacement) result(newstr)
      character(:), allocatable :: newstr
      character(*), intent(in) :: str
      character(*), intent(in) :: substr, replacement

      integer :: idx, n

      idx = index(str, substr)
      n = len(substr)
      
      newstr = str(1:idx-1) // replacement // str(idx+n:)
         
   end function replace



   !---------------------------------------------------------------------------  
   ! FUNCTION: 
   ! uses_message
   !
   ! DESCRIPTION: 
   ! Check if the format uses the message (probably always true)
   !---------------------------------------------------------------------------
   logical function uses_message(this)
      class (Formatter), intent(in) :: this
      uses_message = (index(this%fmt,'%(message)') > 0)
   end function uses_message


   !---------------------------------------------------------------------------  
   ! FUNCTION: 
   ! uses_name
   !
   ! DESCRIPTION: 
   ! Check if the format uses the name
   !---------------------------------------------------------------------------
   logical function uses_name(this)
      class (Formatter), intent(in) :: this
      uses_name = (index(this%fmt,'%(name)') > 0)
   end function uses_name


   !---------------------------------------------------------------------------  
   ! FUNCTION: 
   ! uses_short_name
   !
   ! DESCRIPTION: 
   ! Check if the format uses the short_name
   !---------------------------------------------------------------------------
   logical function uses_short_name(this)
      class (Formatter), intent(in) :: this
      uses_short_name = (index(this%fmt,'%(short_name)') > 0)
   end function uses_short_name


   !---------------------------------------------------------------------------  
   ! FUNCTION: 
   ! uses_level_number
   !
   ! DESCRIPTION: 
   ! Check if the format uses the level
   !---------------------------------------------------------------------------
   logical function uses_level_number(this)
      class (Formatter), intent(in) :: this
      uses_level_number = (index(this%fmt,'%(level_number)') > 0)
   end function uses_level_number

   !---------------------------------------------------------------------------  
   ! FUNCTION: 
   ! uses_level_name
   !
   ! DESCRIPTION: 
   ! Check if the format uses the level_name
   !---------------------------------------------------------------------------
   logical function uses_level_name(this)
      class (Formatter), intent(in) :: this
      uses_level_name = (index(this%fmt,'%(level_name)') > 0)
   end function uses_level_name


   !---------------------------------------------------------------------------  
   ! FUNCTION: 
   ! usesTime
   !
   ! DESCRIPTION: 
   ! Check if the format uses the creation time of the record.
   !---------------------------------------------------------------------------
   logical function uses_ascTime(this)
      class (Formatter), intent(in) :: this
      uses_ascTime = (index(this%fmt,'%(asctime)') > 0)
   end function uses_ascTime
   

   subroutine fill_extra_keywords(this, extra)
      use PFL_FormatToken
      use PFL_FormatTokenVector
      use PFL_FormatString
      class (Formatter), intent(inout) :: this
      type (StringUnlimitedMap), intent(in) :: extra

      type (FormatParser) :: p
      type (FormatToken), pointer :: token
      type (VectorIterator) :: iter
      
      iter = this%p%begin()
      do while (iter /= this%p%end())
         token => iter%get()
         
         select case (token%type)
         case (KEYWORD)
            if (extra%count(token%text) > 0) then
               call p%push_back(token)
               token%type = TEXT
               token%text = formatString(p, extra)
               token%edit_descriptor = ''
               call p%clear()
            end if
         end select
         call iter%next()
      end do

   end subroutine fill_extra_keywords

end module PFL_Formatter
