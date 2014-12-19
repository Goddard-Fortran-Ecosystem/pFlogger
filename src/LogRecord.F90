! A LogRecord instance represents an event being logged. Its instances are created
! every time something is logged.
module ASTG_LogRecord_mod
   use iso_fortran_env, only: int32, real32, int64, real64, real128
   implicit none
   private

   public :: LogRecord

   type :: LogRecord
      private
      character(len=:), allocatable :: name
      character(len=:), allocatable :: message
   contains
      procedure :: getMessage
      procedure :: getName
   end type LogRecord

   interface LogRecord
      module procedure :: newLogRecord
   end interface LogRecord

   
contains

   
   ! Create a log record. 
   function newLogRecord(name, message) result(rec)
      character(len=*), intent(in) :: name
      character(len=*), intent(in) :: message
      type (LogRecord) :: rec
      
      rec%name = name
      rec%message = message
      
   end function newLogRecord

   
   ! return the message for this LogRecord.
   function getMessage(this) result(message)
      class (LogRecord), intent(in) :: this
      character(len=:), allocatable :: message
      
      message = this%message
      
   end function getMessage
   
   ! return the name for this LogRecord.
   function getName(this) result(name)
      class (LogRecord), intent(in) :: this
      character(len=:), allocatable :: name
      
      name = this%name
      
   end function getName
   

end module ASTG_LogRecord_mod
