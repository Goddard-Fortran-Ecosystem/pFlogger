!!------------------------------------------------------------------------------
! NASA/GSFC, CISTO, Code 606, Advanced Software Technology Group
!------------------------------------------------------------------------------
!
!*MODULE: PFL_LogRecord
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
#include "error_handling_macros.fh"
module PFL_LogRecord
   use gFTL_UnlimitedVector
   use gFTL_StringUnlimitedMap
   use PFL_SeverityLevels, only: level_to_name
   use PFL_KeywordEnforcer
   use PFL_Exception
   implicit none
   private

   public :: LogRecord
   public :: initLogRecord

   integer, public, parameter :: UNKNOWN_LINE = -1

   type :: LogRecord
!      private
      integer :: level
      integer :: line = UNKNOWN_LINE
      character(len=:), allocatable :: name
      character(len=:), allocatable :: message_format
      character(len=:), allocatable :: str
      character(len=:), allocatable :: fmt
      character(len=:), allocatable :: file
      type (UnlimitedVector), pointer :: args => null()
      type (StringUnlimitedMap), pointer :: extra => null()
      integer :: time_fields(8)
   contains
      procedure :: get_name
      procedure :: get_level
      procedure :: get_level_name
      procedure :: get_message
      procedure :: get_line
      procedure :: get_file
      procedure :: get_basename
      procedure, nopass :: fill_date_and_time
   end type LogRecord


   ! TODO: Unsafe with gfortran 4.9, 5.0 due to issue with
   ! FINAL methods and the FTL Set container.
   interface LogRecord
      module procedure newLogRecord
   end interface LogRecord

   type (UnlimitedVector), target, save :: EMPTY_VECTOR
   type (StringUnlimitedMap), target, save :: EMPTY_MAP
   
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
      type (StringUnlimitedMap), target, optional, intent(in) :: extra

      type (LogRecord) :: rec
      character(len=:), allocatable :: level_name
      
      rec%name = name
      rec%level = level
      rec%message_format = message_format

      if (present(args)) then
         rec%args => args
      else
         rec%args => EMPTY_VECTOR
      end if

      if (present(extra)) then
         rec%extra => extra
      else
         rec%extra => EMPTY_MAP
      end if

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
   ! get_level_name
   !
   ! DESCRIPTION: 
   ! Get the level associated with this log record. This is needed to 'handle'
   ! a message (see AbstractHandler).
   !---------------------------------------------------------------------------
   function get_level_name(this, rc) result(level_name)
      character(len=:), allocatable :: level_name
      class (LogRecord), intent(in) :: this
      integer, optional, intent(out) :: rc
      integer :: status
      level_name = level_to_name(this%level, rc=status)
      _VERIFY(status,'',rc)
      _RETURN(_SUCCESS,rc)
   end function get_level_name


   !---------------------------------------------------------------------------  
   ! FUNCTION: 
   ! get_message
   !
   ! DESCRIPTION: 
   ! Return the message for this LogRecord after parsing any user-supplied
   ! arguments associated with message.
   !---------------------------------------------------------------------------
   function get_message(this, unusable, rc) result(message)
      use PFL_FormatString, only: formatString
      character(len=:), allocatable :: message
      class (LogRecord), intent(in) :: this
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status
      
      message = formatString(this%message_format,  this%args, rc=status)
      _VERIFY(status,'',rc)
      _RETURN(_SUCCESS,rc)
      
   end function get_message



   !---------------------------------------------------------------------------  
   ! FUNCTION: 
   ! get_line
   !
   ! DESCRIPTION: 
   ! Get the level associated with this log record. This is needed to 'handle'
   ! a message (see AbstractHandler).
   !---------------------------------------------------------------------------
   integer function get_line(this) result(line)
      class (LogRecord), intent(in) :: this
      line = this%line
   end function get_line


   !---------------------------------------------------------------------------  
   ! FUNCTION: 
   ! get_file
   !
   ! DESCRIPTION: 
   ! Get the file name associated with this log record. This is needed to 'handle'
   ! a message (see AbstractHandler).
   !---------------------------------------------------------------------------
   function get_file(this) result(file)
      character(:), allocatable :: file
      class (LogRecord), intent(in) :: this
      if (allocated(this%file)) then
         file = this%file
      else
         file = 'unknown'
      end if
   end function get_file
   

   !---------------------------------------------------------------------------  
   ! FUNCTION: 
   ! get_basename
   !
   ! DESCRIPTION: 
   ! Get the basename of the file associated with this log record. This
   ! is needed to 'handle' a message (see AbstractHandler).
   !---------------------------------------------------------------------------
   function get_basename(this) result(basename)
      character(:), allocatable :: basename
      class (LogRecord), intent(in) :: this
      
      character(1), parameter :: DIR_SEPARATOR = '/' ! at least on Unix
      
      basename = this%file(scan(this%file,DIR_SEPARATOR,back=.true.)+1:)
   end function get_basename
    

   !---------------------------------------------------------------------------  
   ! FUNCTION: 
   ! initLogRecord
   !
   ! DESCRIPTION:
   ! Initialize a logging record with interesting information.
   !---------------------------------------------------------------------------
   subroutine initLogRecord(rec, name, level, message_format, args, extra, line, file)
      type (LogRecord), intent(out) :: rec
      character(len=*), intent(in) :: name
      integer, intent(in) :: level
      character(len=*), intent(in) :: message_format
      type (UnlimitedVector), optional, target, intent(in) :: args
      type (StringUnlimitedMap), optional, target, intent(in) :: extra
      integer, optional, intent(in) :: line
      character(*), optional, intent(in) :: file
      
      character(len=:), allocatable :: level_name
      
      rec%name = name
      rec%level = level
      rec%message_format = message_format

      if (present(args)) then
         rec%args => args
      else
         rec%args => EMPTY_VECTOR
      end if

      if (present(extra)) then
         rec%extra => extra
      else
         rec%extra => EMPTY_MAP
      end if

      if (present(line)) then
         rec%line = line
      else
         rec%line = UNKNOWN_LINE
      end if

      if (present(file)) then
         rec%file = file
      else
         ! filename component remains unallocated
      end if

      call fill_date_and_time(rec)
      
   end subroutine initLogRecord

end module PFL_LogRecord
