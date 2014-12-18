module ASTG_RotatingFileHandler_mod
   ! Handler for logging to a set of files, which switches from one file
   ! to the next when the current file reaches a certain size.
   ! By default, the file grows indefinitely. You can specify particular
   ! values of maxBytes and backupCount to allow the file to rollover at
   ! a predetermined size.
   use iso_fortran_env
   use ASTG_SeverityLevels_mod, only: INFO
   use ASTG_FileHandler_mod, only: FileHandler
   implicit none
   private

   public :: RotatingFileHandler

   type, extends(FileHandler) :: RotatingFileHandler
      private
      integer(int64) :: maxBytes
      integer :: backupCount
   contains
      procedure :: doRollover
      procedure :: shouldRollover
      procedure :: emitMessage
   end type RotatingFileHandler

   interface RotatingFileHandler
      module procedure newRotatingFileHandler
   end interface

   integer, save :: fileCount

   
contains

    
   function newRotatingFileHandler(fileName, numBytes, backupCount, level) &
        result(handler)
      ! Initializes the instance
      type (RotatingFileHandler) :: handler
      character(len=*), intent(in) :: fileName
      character(len=*), intent(in), optional :: numBytes
      integer, intent(in), optional :: backupCount
      integer, intent(in), optional :: level
      
      integer :: level_
      integer :: backupCount_
      integer(int64) :: maxBytes_

      if (present(level)) then
         level_ = level
      else
         level_ = INFO
      end if
      if (present(numBytes)) then
         maxBytes_ = convertNumBytes_(numBytes)
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
      fileCount = 0
      
   end function newRotatingFileHandler

   
   function convertNumBytes_(numBytes) result(nBytes)
      character(len=*), intent(in) :: numBytes
      integer(int64) :: nBytes
      
      character(len=16) :: fmt
      integer :: pos, n, nl

      pos = scan(numBytes,'kmg')      
      if (pos > 0) then
        nl = len(numBytes(1:pos-1))        
        fmt = setFmt_(nl)
        select case(numBytes(pos:pos+1))
        case ('kb')
          read(numBytes(1:pos-1), fmt) n
          nBytes = n * 1E3
        case ('mb')
          read(numBytes(1:pos-1), fmt) n
          nBytes = n * 1E6
        case ('gb')
          read(numBytes(1:pos-1), fmt) n
          nBytes = n * 1E9
        end select
      else
        nl = len(numBytes)
        fmt = setFmt_(nl)
        read(numBytes, fmt) n
        nBytes = n
      end if

   contains
      
      function setFmt_(n) result (fmt)
         integer, intent(inout) :: n
         character(len=1) :: fmt0
         character(len=4) :: fmt
         write(fmt0,'(i1)')n
         fmt =  '(i'//trim(fmt0)//')'
      end function setFmt_
      
   end function convertNumBytes_
      
   
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
      ! Rollover occurs whenever the current log file exceeds maxBytes
      class (RotatingFileHandler), intent(inout) :: this
      
      character(len=64) :: suffix
      character(len=128) :: cmd
      character(len=6) :: fmt
      logical :: is

      if (this%isOpen()) then
         call this%close()
      end if
      if (fileCount < this%backupCount) then
         fileCount = fileCount + 1
         select case (fileCount)
         case (:9)
           fmt = '(i1.1)'
         case (10:99)
           fmt = '(i2.2)'
         case (100:999)
           fmt = '(i3.3)'
         case (1000:9999)
           fmt = '(i4.4)'
         end select
         write(suffix, fmt) fileCount
         ! sanity check
         inquire(FILE=this%getFileName(), EXIST=is)
         if (is) then ! rename file
            cmd = 'mv '//this%getFileName()//' '//this%getFileName()//'.'//trim(suffix)
            call execute_command_line(cmd)
         end if
         if (.not. this%isOpen()) call this%open()
       end if
      
   end subroutine doRollover


end module ASTG_RotatingFileHandler_mod
