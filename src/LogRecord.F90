!------------------------------------------------------------------------------
! NASA/GSFC, CISTO, Code 606, Advanced Software Technology Group
!------------------------------------------------------------------------------
!
! MODULE: ASTG_LogRecord_mod
!
!> @author 
!> ASTG staff
!
! DESCRIPTION: 
!> @brief
!> A LogRecord instance represents an event being logged. Its instances are created
!> every time something is logged. They contain all the information pertinent to
!> the event being logged. The  main information passed in is in message and optional
!> arguments which are combined to create the message field of the record.
!
! REVISION HISTORY:
! 01 Jan 2015 - Initial Version
!------------------------------------------------------------------------------
module ASTG_LogRecord_mod
   use FTL_XWrapVec_mod
   use FTL_CIStringXUMap_mod
   use ASTG_Object_mod
   use ASTG_SeverityLevels_mod
   use iso_fortran_env, only: int32, real32, int64, real64, real128
   implicit none
   private

   public :: LogRecord
   public :: initLogRecord

   type, extends(Object) :: LogRecord
!      private
      integer :: level
      character(len=:), allocatable :: name
      character(len=:), allocatable :: message
      character(len=:), allocatable :: str
      character(len=:), allocatable :: fmt
      type (XWrapVec) :: args
      type (CIStringXUMap) :: extra
   contains
      procedure :: getName
      procedure :: getLevel
      procedure :: getMessage
!      procedure :: getStr
!      procedure :: getFmt
      procedure, nopass :: fillDateAndTime
   end type LogRecord

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
   function newLogRecord(name, level, message, args, extra) result(rec)
      use FTL_String_mod
      character(len=*), intent(in) :: name
      integer, intent(in) :: level
      character(len=*), intent(in) :: message
      type (XWrapVec), optional, intent(in) :: args
      type (CIStringXUMap), optional, intent(in) :: extra

      type (LogRecord) :: rec
      type (CIStringXUMapIter) :: iter
      type (String) :: wrapName
      character(len=:), allocatable :: levelName
      
      rec%name = name
      rec%level = level
      rec%message = message
      if (present(args)) then
         rec%args = args
      else
         rec%args = XWrapVec()
      end if

      if (present(extra)) then
         rec%extra = extra
      else
         rec%extra = CIStringXUMap()
      end if

      iter = rec%extra%emplace('level', level)
      ! workaround for ifort
      wrapName = name
      iter = rec%extra%emplace('name', wrapName)

      levelName = levelToString(level)
      ! Compiler workarounds
#ifdef __INTEL_COMPILER
      ! ifort
      iter= rec%extra%emplace('levelName', levelName)
#else
      ! gfortran
      iter= rec%extra%emplace('levelName', String(levelName))
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
      type (CIStringXUMapIter) :: iter
      
      call date_and_time(VALUES=values)
      iter = rec%extra%emplace('Y', values(1))
      iter = rec%extra%emplace('M', values(2))
      iter = rec%extra%emplace('D', values(3))
      iter = rec%extra%emplace('HH', values(5))
      iter = rec%extra%emplace('MM', values(6))
      iter = rec%extra%emplace('SS', values(7))
      iter = rec%extra%emplace('MS', values(8))
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
      use ASTG_FormatParser_mod
      class (LogRecord), intent(in) :: this
      character(len=:), allocatable :: message
      type (FormatParser) :: parser
      
      message = parser%format(this%message, this%args)
      
   end function getMessage


! TODO: Is there any use for this function?   
   function getStr(this) result(str)
      class (LogRecord), intent(in) :: this
      character(len=:), allocatable :: str
      integer :: eos

      eos = index(this%message, ' ')
      if (eos>0) then
        allocate(character(len=eos-1)::str)
        str = this%message(1:eos-1)
      else
        str = this%message
      end if
      
   end function getStr

   
! TODO: Is there any use for this function?   
   function getFmt(this) result(fmt)
      class (LogRecord), intent(in) :: this
      character(len=:), allocatable :: fmt
      integer :: eos, newLen, msgLen

      eos = index(this%message, '%')
      if (eos>0) then
        msgLen = len(this%message)
        newLen = len(this%message)-eos
        allocate(character(len=newLen)::fmt)
        fmt = this%message(eos+1:msgLen)
      else
        fmt = ''
      end if
      
   end function getFmt


   !---------------------------------------------------------------------------  
   ! FUNCTION: 
   ! initLogRecord
   !
   ! DESCRIPTION:
   ! Initialize a logging record with interesting information.
   !---------------------------------------------------------------------------
   subroutine initLogRecord(rec, name, level, message, args, extra)
      use FTL_XWrapVec_mod
      use FTL_StringXUMap_mod
      use FTL_String_mod
      character(len=*), intent(in) :: name
      integer, intent(in) :: level
      character(len=*), intent(in) :: message
      type (XWrapVec), optional, intent(in) :: args
      type (StringXUMap), optional, intent(in) :: extra

      type (LogRecord) :: rec
      type (StringXUMapIter) :: iter
      type (String) :: wrapName
      character(len=:), allocatable :: levelName
      
      rec%name = name
      rec%level = level
      rec%message = message
      if (present(args)) then
         rec%args = args
      else
         rec%args = XWrapVec()
      end if

      if (present(extra)) then
         rec%extra = extra
      else
         rec%extra = StringXUMap()
      end if

      iter = rec%extra%emplace('level', level)
      ! workaround for ifort
      wrapName = name
      iter = rec%extra%emplace('name', wrapName)

      levelName = levelToString(level)
      ! Compiler workarounds
#ifdef __INTEL_COMPILER
      ! ifort
      iter= rec%extra%emplace('levelName', levelName)
#else
      ! gfortran
      iter= rec%extra%emplace('levelName', String(levelName))
#endif
      call fillDateAndTime(rec)
      
   end subroutine initLogRecord

end module ASTG_LogRecord_mod
