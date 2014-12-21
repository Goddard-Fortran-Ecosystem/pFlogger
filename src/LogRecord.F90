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
      private
      integer :: level
      character(len=:), allocatable :: name
      character(len=:), allocatable :: message
   contains
      procedure :: getName
      procedure :: getLevel
      procedure :: getMessage
   end type LogRecord

   interface LogRecord
      module procedure :: newLogRecord
   end interface LogRecord

   
contains

   
   ! Create a log record. 
   function newLogRecord(name, level, message) result(rec)
      character(len=*), intent(in) :: name
      integer, intent(in) :: level
      character(len=*), intent(in) :: message
      type (LogRecord) :: rec
      
      rec%name = name
      rec%level = level
      rec%message = message
      
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

end module ASTG_LogRecord_mod
