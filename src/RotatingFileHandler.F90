module ASTG_RotatingFileHandler_mod
   ! Handler for logging to a set of files, which switches from one file
   ! to the next when the current file reaches a certain size.
   ! By default, the file grows indefinitely. You can specify particular
   ! values of maxBytes and backupCount to allow the file to rollover at
   ! a predetermined size.
   use ASTG_SeverityLevels_mod, only: INFO
   use ASTG_FileHandler_mod, only: FileHandler
   implicit none
   private

   public :: RotatingFileHandler

   type, extends(FileHandler) :: RotatingFileHandler
      private
      integer :: maxBytes
      integer :: backupCount
   contains
      procedure :: doRollover
      procedure :: shouldRollover
      procedure :: emitMessage
   end type RotatingFileHandler

   interface RotatingFileHandler
      module procedure newRotatingFileHandler
   end interface

   integer, parameter :: MAX_NUM_RECORDS = 1000000 ! what is a good number?
   integer, save :: backupCounter =0
   
contains

    
   function newRotatingFileHandler(fileName, maxBytes, backupCount, level) &
        result(handler)
      use iso_fortran_env
      ! Initializes the instance
      type (RotatingFileHandler) :: handler
      character(len=*), intent(in) :: fileName
      integer(int64), intent(in), optional :: maxBytes
      integer, intent(in), optional :: backupCount
      integer, intent(in), optional :: level
      
      integer :: level_
      integer :: maxBytes_
      integer :: backupCount_

      if (present(level)) then
        level_ = level
      else
        level_ = INFO
      end if
      if (present(maxBytes)) then
        maxBytes_ = maxBytes
      else
        maxBytes_ = 0
      end if
      if (present(backupCount)) then
        backupCount_ = backupCount
      else
        backupCount_ = 0
      end if
      
      call handler%setFileName(fileName)
      call handler%open()
      call handler%setLevel(level_)
      handler%maxBytes = maxBytes_
      handler%backupCount = backupCount_
      backupCounter = backupCount_
      
   end function newRotatingFileHandler

   
   subroutine emitMessage(this, levelString, message)
      ! Write a string to a file. Level is specified in levelString
      class (RotatingFileHandler), intent(inout) :: this
      character(len=*), intent(in) :: levelString
      character(len=*), intent(in) :: message

      if (this%shouldRollover()) then
         call this%doRollover()
      else
         write(this%getUnit(), '(a)') levelString // ': ' // message
      end if
    
   end subroutine emitMessage


   function shouldRollover(this) result(rollOver)
      ! Determine if rollover should occur.
      class (RotatingFileHandler), intent(in) :: this
      logical :: rollOver 
      integer :: fileSize
      
      rollOver = .false.
      inquire(this%getUnit(), SIZE=fileSize)
      if (fileSize > this%maxBytes) then
         rollOver = .true.
      end if
        
   end function shouldRollover

   
   subroutine doRollover(this)
      ! Rollover occurs whenever the current log file is nearly maxBytes
      class (RotatingFileHandler), intent(inout) :: this
      character(len=:), allocatable :: suffix
      
      if ((this%backupCount) > 0) then
         write(suffix,'(a)') backupCounter+1
         call execute_command_line('mv '//this%getFileName()// &
              ' '//this%getFileName()//'.'//suffix)
         this%backupCount = this%backupCount - 1
         call execute_command_line('touch '//this%getFileName())
       end if
      
   end subroutine doRollover


end module ASTG_RotatingFileHandler_mod
