module ASTG_LogRecord_mod
   ! A LogRecord instance represents an event being and  instances are created
   ! every time something is logged.
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
