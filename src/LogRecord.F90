module ASTG_LogRecord_mod
   ! A LogRecord instance represents an event being logged. Its instances are created
   ! every time something is logged.
   use iso_fortran_env, only: int32, real32, int64, real64, real128
   implicit none
   private

   public :: LogRecord

   type :: LogRecord
      private
      character(len=:), allocatable :: message
   contains
      procedure :: getMessage
   end type LogRecord

   interface LogRecord
      module procedure :: newLogRecord
   end interface LogRecord

   
contains

   
   function newLogRecord(message) result(rec)
      ! Create a log record. 
      character(len=*), intent(in) :: message
      type (LogRecord) :: rec
      
      rec%message = message
      
   end function newLogRecord

   
   function getMessage(this) result(message)
      ! return the message for this LogRecord.
      class (LogRecord), intent(in) :: this
      character(len=:), allocatable :: message
      
      message = this%message
      
   end function getMessage
   
end module ASTG_LogRecord_mod
