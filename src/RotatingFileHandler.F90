! Handler for logging to a set of files, which switches from one file
! to the next when the current file reaches a certain size.
! By default, the file grows indefinitely. You can specify particular
! values of maxBytes and backupCount to allow the file to rollover at
! a predetermined size.
module ASTG_RotatingFileHandler_mod
   use iso_fortran_env
   use ASTG_SeverityLevels_mod, only: INFO
   use ASTG_FileHandler_mod, only: FileHandler
   use ASTG_LogRecord_mod
   
   implicit none
   private

   public :: RotatingFileHandler

   type, extends(FileHandler) :: RotatingFileHandler
      private
      integer(int64) :: numBytes
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

    
   function newRotatingFileHandler(fileName, maxBytes, backupCount, level) &
        result(handler)
      type (RotatingFileHandler) :: handler
      character(len=*), intent(in) :: fileName
      character(len=*), intent(in), optional :: maxBytes
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
      if (present(maxBytes)) then
         maxBytes_ = convertNumBytes_(maxBytes)
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
      handler%numBytes = maxBytes_
      handler%backupCount = backupCount_
      fileCount = 0
      
   end function newRotatingFileHandler

   
   function convertNumBytes_(maxBytes) result(nBytes)
      character(len=*), intent(in) :: maxBytes
      integer(int64) :: nBytes
      
      character(len=16) :: fmt
      integer :: pos, n, nl

      pos = scan(maxBytes,'kmg')      
      if (pos > 0) then
        nl = len(maxBytes(1:pos-1))        
        fmt = setFmt_(nl)
        select case(maxBytes(pos:pos+1))
        case ('kb')
          read(maxBytes(1:pos-1), fmt) n
          nBytes = n * 2E10
        case ('mb')
          read(maxBytes(1:pos-1), fmt) n
          nBytes = n * 2E20
        case ('gb')
          read(maxBytes(1:pos-1), fmt) n
          nBytes = n * 2E30
        end select
      else
        nl = len(maxBytes)
        fmt = setFmt_(nl)
        read(maxBytes, fmt) n
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
      
   
   ! Write a string to a file. Level is specified in levelString
   subroutine emitMessage(this, levelString, record)
      class (RotatingFileHandler), intent(inout) :: this
      character(len=*), intent(in) :: levelString
      type (LogRecord) :: record

      if (this%shouldRollover()) then
         call this%doRollover()
      else
         write(this%getUnit(), '(a)') levelString // ': ' // record%getMessage()
      end if
    
   end subroutine emitMessage


   ! Determine if rollover should occur.
   function shouldRollover(this) result(rollOver)
      class (RotatingFileHandler), intent(in) :: this
      logical :: rollOver 
      integer :: fileSize
      
      rollOver = .false.
      inquire(this%getUnit(), SIZE=fileSize)
      if (fileSize > this%numBytes) then
         rollOver = .true.
      end if
      
   end function shouldRollover

   
   ! Rollover occurs whenever the current log file exceeds maxBytes
   subroutine doRollover(this)
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
#ifdef INTEL_14
            call system(cmd)
#else
            call execute_command_line(cmd)
#endif
         end if
         if (.not. this%isOpen()) call this%open()
       end if
      
   end subroutine doRollover


end module ASTG_RotatingFileHandler_mod
