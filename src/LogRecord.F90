!------------------------------------------------------------------------------
! NASA/GSFC, CISTO, Code 606, Advanced Software Technology Group
!------------------------------------------------------------------------------
!
!*MODULE: ASTG_LogRecord_mod
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
module ASTG_LogRecord_mod
   use FTL
   use ASTG_UnlimitedVector_mod, only: UnlimitedVector => Vector
   use ASTG_StringUnlimitedMap_mod, only: StringUnlimitedMap => Map
   use ASTG_Object_mod
   use ASTG_SeverityLevels_mod
   implicit none
   private

   public :: LogRecord
   public :: initLogRecord

   type, extends(Object) :: LogRecord
!      private
      integer :: level
      character(len=:), allocatable :: name
      character(len=:), allocatable :: messageFormat
      character(len=:), allocatable :: str
      character(len=:), allocatable :: fmt
      type (UnlimitedVector) :: args
      type (StringUnlimitedMap) :: extra
      integer :: time_fields(8)
   contains
      procedure :: getName
      procedure :: getLevel
      procedure :: getMessage
      procedure, nopass :: fillDateAndTime
   end type LogRecord


   ! TODO: Unsafe with gfortran 4.9, 5.0 due to issue with
   ! FINAL methods and the FTL Set container.
   interface LogRecord
      module procedure newLogRecord
   end interface LogRecord

   
contains

   
   !---------------------------------------------------------------------------  
   ! FUNCTION: 
   ! newLogRecord
   !
   ! DESCRIPTION:
   ! Initialize a logging record with interesting information.
   !---------------------------------------------------------------------------
   function newLogRecord(name, level, messageFormat, args, extra) result(rec)
      character(len=*), intent(in) :: name
      integer, intent(in) :: level
      character(len=*), intent(in) :: messageFormat
      type (UnlimitedVector), optional, intent(in) :: args
      type (StringUnlimitedMap), optional, intent(in) :: extra

      type (LogRecord) :: rec
      character(len=:), allocatable :: levelName
      
      rec%name = name
      rec%level = level
      rec%messageFormat = messageFormat
      if (present(args)) then
         rec%args = args
      else
         rec%args = UnlimitedVector()
      end if

      if (present(extra)) then
         call rec%extra%deepCopy(extra)
      end if

      call rec%extra%insert('level', level)
#ifdef __GFORTRAN__
      call rec%extra%insert('name', newString(name))
#else
      call rec%extra%insert('name', name)
#endif

      levelName = levelToName(level)
      ! Compiler workarounds
#ifdef __GFORTRAN__
      call rec%extra%insert('levelName', newString(levelName))
#else
      call rec%extra%insert('levelName', levelName)
#endif
      call fillDateAndTime(rec)

   end function newLogRecord

   
   !---------------------------------------------------------------------------  
   ! ROUTINE: 
   ! fillDateAndTime
   !
   ! DESCRIPTION: 
   ! Helper routine to initialize log record dictionary with date/time
   ! information.
   !---------------------------------------------------------------------------
   subroutine fillDateAndTime(rec)
      type(LogRecord), intent(inout) :: rec
      integer,dimension(8) :: values
      
      call date_and_time(VALUES=rec%time_fields)

   end subroutine fillDateAndTime
   
   
   !---------------------------------------------------------------------------  
   ! FUNCTION: 
   ! getName
   !
   ! DESCRIPTION: 
   ! Get the name for this log record.
   !---------------------------------------------------------------------------
   function getName(this) result(name)
      class (LogRecord), intent(in) :: this
      character(len=:), allocatable :: name
      
      name = this%name
      
   end function getName


   !---------------------------------------------------------------------------  
   ! FUNCTION: 
   ! getLevel
   !
   ! DESCRIPTION: 
   ! Get the level associated with this log record. This is needed to 'handle'
   ! a message (see AbstractHandler).
   !---------------------------------------------------------------------------
   integer function getLevel(this) result(level)
      class (LogRecord), intent(in) :: this
      level = this%level
   end function getLevel


   !---------------------------------------------------------------------------  
   ! FUNCTION: 
   ! getMessage
   !
   ! DESCRIPTION: 
   ! Return the message for this LogRecord after parsing any user-supplied
   ! arguments associated with message.
   !---------------------------------------------------------------------------
   function getMessage(this) result(message)
      use ASTG_FormatString_mod, only: formatString
      class (LogRecord), intent(in) :: this
      character(len=:), allocatable :: message
      
      message = formatString(this%messageFormat,  this%args)
      
   end function getMessage



   !---------------------------------------------------------------------------  
   ! FUNCTION: 
   ! initLogRecord
   !
   ! DESCRIPTION:
   ! Initialize a logging record with interesting information.
   !---------------------------------------------------------------------------
   subroutine initLogRecord(rec, name, level, messageFormat, args, extra)
      use ASTG_UnlimitedVector_mod, only: UnlimitedVector => Vector
      use ASTG_StringUnlimitedMap_mod, only: StringUnlimitedMap => Map
      type (LogRecord), intent(out) :: rec
      character(len=*), intent(in) :: name
      integer, intent(in) :: level
      character(len=*), intent(in) :: messageFormat
      type (UnlimitedVector), optional, intent(in) :: args
      type (StringUnlimitedMap), optional, intent(in) :: extra

      character(len=:), allocatable :: levelName
      
      rec%name = name
      rec%level = level
      rec%messageFormat = messageFormat

      if (present(args)) then
         rec%args = args
      else
         rec%args = UnlimitedVector()
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
      levelName = levelToName(level)
#ifdef __GFORTRAN__
      call rec%extra%insert('levelName', String(levelName))
#else
      call rec%extra%insert('levelName', levelName)
#endif
      
      call fillDateAndTime(rec)
      
   end subroutine initLogRecord

end module ASTG_LogRecord_mod
