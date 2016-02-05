!!------------------------------------------------------------------------------
! NASA/GSFC, CISTO, Code 606, Advanced Software Technology Group
!------------------------------------------------------------------------------
!
!*MODULE: PFL_LogRecord_mod
!
!> @brief A LogRecord instance represents an event being logged. 
!> @details
!> LogRecord instances are created every time something is logged. They contain 
!> all the information pertinent to the event being logged. The  main information 
!> passed in is in message and optional arguments which are combined to create 
!> the message field of the record.
!
!> @author ASTG staff
!> @date 01 Jan 2015 - Initial Version  
!------------------------------------------------------------------------------
module PFL_LogRecord_mod
   use FTL
   use PFL_UnlimitedVector_mod, only: UnlimitedVector => Vector
   use PFL_StringUnlimitedMap_mod, only: StringUnlimitedMap => Map
   use PFL_Object_mod
   use PFL_SeverityLevels_mod
   implicit none
   private

   public :: LogRecord
   public :: initLogRecord

   type, extends(Object) :: LogRecord
!      private
      integer :: level
      character(len=:), allocatable :: name
      character(len=:), allocatable :: message_format
      character(len=:), allocatable :: str
      character(len=:), allocatable :: fmt
      type (UnlimitedVector), pointer :: args => null()
      type (StringUnlimitedMap) :: extra
      integer :: time_fields(8)
   contains
      procedure :: get_name
      procedure :: get_level
      procedure :: get_message
      procedure, nopass :: fill_date_and_time
   end type LogRecord


   ! TODO: Unsafe with gfortran 4.9, 5.0 due to issue with
   ! FINAL methods and the FTL Set container.
   interface LogRecord
      module procedure newLogRecord
   end interface LogRecord

   type (UnlimitedVector), target, save :: EMPTY
   
contains

   
   !---------------------------------------------------------------------------  
   ! FUNCTION: 
   ! newLogRecord
   !
   ! DESCRIPTION:
   ! Initialize a logging record with interesting information.
   !---------------------------------------------------------------------------
   function newLogRecord(name, level, message_format, args, extra) result(rec)
      character(len=*), intent(in) :: name
      integer, intent(in) :: level
      character(len=*), intent(in) :: message_format
      type (UnlimitedVector), optional, target, intent(in) :: args
      type (StringUnlimitedMap), optional, intent(in) :: extra

      type (LogRecord) :: rec
      character(len=:), allocatable :: levelName
      
      rec%name = name
      rec%level = level
      rec%message_format = message_format
      if (present(args)) then
         rec%args => args
      else
         rec%args => EMPTY
      end if

      if (present(extra)) then
         call rec%extra%deepCopy(extra)
      end if

      call rec%extra%insert('level', level)
#ifdef __GFORTRAN__
      call rec%extra%insert('name', String(name))
#else
      call rec%extra%insert('name', name)
#endif

      levelName = level_to_name(level)
      ! Compiler workarounds
#ifdef __GFORTRAN__
      call rec%extra%insert('levelName', String(levelName))
#else
      call rec%extra%insert('levelName', levelName)
#endif
      call fill_date_and_time(rec)

   end function newLogRecord

   
   !---------------------------------------------------------------------------  
   ! ROUTINE: 
   ! fill_date_and_time
   !
   ! DESCRIPTION: 
   ! Helper routine to initialize log record dictionary with date/time
   ! information.
   !---------------------------------------------------------------------------
   subroutine fill_date_and_time(rec)
      type(LogRecord), intent(inout) :: rec
      integer,dimension(8) :: values
      
      call date_and_time(VALUES=rec%time_fields)

   end subroutine fill_date_and_time
   
   
   !---------------------------------------------------------------------------  
   ! FUNCTION: 
   ! get_name
   !
   ! DESCRIPTION: 
   ! Get the name for this log record.
   !---------------------------------------------------------------------------
   function get_name(this) result(name)
      class (LogRecord), intent(in) :: this
      character(len=:), allocatable :: name
      
      name = this%name
      
   end function get_name


   !---------------------------------------------------------------------------  
   ! FUNCTION: 
   ! get_level
   !
   ! DESCRIPTION: 
   ! Get the level associated with this log record. This is needed to 'handle'
   ! a message (see AbstractHandler).
   !---------------------------------------------------------------------------
   integer function get_level(this) result(level)
      class (LogRecord), intent(in) :: this
      level = this%level
   end function get_level


   !---------------------------------------------------------------------------  
   ! FUNCTION: 
   ! get_message
   !
   ! DESCRIPTION: 
   ! Return the message for this LogRecord after parsing any user-supplied
   ! arguments associated with message.
   !---------------------------------------------------------------------------
   function get_message(this) result(message)
      use PFL_FormatString_mod, only: formatString
      class (LogRecord), intent(in) :: this
      character(len=:), allocatable :: message

      message = formatString(this%message_format,  this%args)
      
   end function get_message



   !---------------------------------------------------------------------------  
   ! FUNCTION: 
   ! initLogRecord
   !
   ! DESCRIPTION:
   ! Initialize a logging record with interesting information.
   !---------------------------------------------------------------------------
   subroutine initLogRecord(rec, name, level, message_format, args, extra)
      use PFL_UnlimitedVector_mod, only: UnlimitedVector => Vector
      use PFL_StringUnlimitedMap_mod, only: StringUnlimitedMap => Map
      type (LogRecord), intent(out) :: rec
      character(len=*), intent(in) :: name
      integer, intent(in) :: level
      character(len=*), intent(in) :: message_format
      type (UnlimitedVector), optional, target, intent(in) :: args
      type (StringUnlimitedMap), optional, intent(in) :: extra

      character(len=:), allocatable :: levelName
      
      rec%name = name
      rec%level = level
      rec%message_format = message_format

      if (present(args)) then
         rec%args => args
      else
         rec%args => EMPTY
      end if

      if (present(extra)) then
         call rec%extra%deepCopy(extra)
      end if

      call rec%extra%insert('level', level)
#ifdef __GFORTRAN__
      call rec%extra%insert('name', String(name))
#else
      call rec%extra%insert('name', name)
#endif
      levelName = level_to_name(level)
#ifdef __GFORTRAN__
      call rec%extra%insert('levelName', String(levelName))
#else
      call rec%extra%insert('levelName', levelName)
#endif
      
      call fill_date_and_time(rec)
      
   end subroutine initLogRecord

end module PFL_LogRecord_mod
