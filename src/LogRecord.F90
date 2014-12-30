! A LogRecord instance represents an event being logged. Its instances are created
! every time something is logged. They contain all the information pertinent to
! the event being logged. The  main information passed in is in message and optional
! arguments which are combined to create the message field of the record.
module ASTG_LogRecord_mod
   use ASTG_Object_mod
   use iso_fortran_env, only: int32, real32, int64, real64, real128
   implicit none
   private

   public :: LogRecord

   type, extends(Object) :: LogRecord
!      private
      integer :: level
      integer, allocatable :: value
      character(len=:), allocatable :: name
      character(len=:), allocatable :: message
      character(len=:), allocatable :: str
      character(len=:), allocatable :: fmt
   contains
      procedure :: getName
      procedure :: getLevel
      procedure :: getMessage
      procedure :: getStr
      procedure :: getFmt
   end type LogRecord

   interface LogRecord
      module procedure :: newLogRecord
   end interface LogRecord

   
contains

   
   ! Create a log record. 
   function newLogRecord(name, level, message, value) result(rec)
      character(len=*), intent(in) :: name
      integer, intent(in) :: level
      character(len=*), intent(in) :: message
      integer, optional, intent(in) :: value
      type (LogRecord) :: rec
      character(len=64) :: strVal
      
      rec%name = name
      rec%level = level
      rec%message = message
      if (present(value)) then
        rec%value = value
      end if
      
   end function newLogRecord

   
   
   ! return the name for this LogRecord.
   function getName(this) result(name)
      class (LogRecord), intent(in) :: this
      character(len=:), allocatable :: name
      
      name = this%name
      
   end function getName


   integer function getLevel(this) result(level)
      class (LogRecord), intent(in) :: this
      level = this%level
   end function getLevel


   ! return the message for this LogRecord.
   function getMessage(this) result(message)
      class (LogRecord), intent(in) :: this
      character(len=:), allocatable :: message
      
      message = this%message
      
   end function getMessage


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

end module ASTG_LogRecord_mod
