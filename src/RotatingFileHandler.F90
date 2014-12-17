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
      integer :: numRecords
   contains
      procedure :: doRollover
      procedure :: shouldRollover
   end type RotatingFileHandler

   interface RotatingFileHandler
      module procedure newRotatingFileHandler
   end interface


contains

    
   function newRotatingFileHandler(fileName, level, maxBytes, backupCount) result(handler)
      ! Initializes the instance
      type (RotatingFileHandler) :: handler
      character(len=*), intent(in) :: fileName
      integer, intent(in), optional :: level
      integer(int64), intent(in), optional :: maxBytes
      integer, intent(in), optional :: backupCount
      integer :: level_

      if (present(level)) then
        level_ = level
      else
        level_ = INFO
      end if
      
      call handler%setFileName(fileName)
      call handler%open()
      call handler%setLevel(level_)
      handler%maxBytes = maxBytes
      handler%backupCount = backupCount
      
   end function newRotatingFileHandler

    
   logical function shouldRollover(this)
      ! Determine if rollover should occur.
      class (RotatingFileHandler), intent(in) :: this

      ! if maxBytes exceeded then...
      
   end function shouldRollover


   subroutine doRollover(this)
      ! Rollover occurs whenever the current log file is nearly maxBytes
      class (RotatingFileHandler), intent(inout) :: this

      ! if backupCount > 0
      !   rename old filename
      !   create new filename (use basename?)
      !   backupCount--
      
   end subroutine doRollover

   
end module ASTG_RotatingFileHandler_mod
