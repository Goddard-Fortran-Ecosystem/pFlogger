!------------------------------------------------------------------------------
! NASA/GSFC, CISTO, Code 606, Advanced Software Technology Group
!------------------------------------------------------------------------------
!
!*MODULE: ASTG_RotatingFileHandler_mod
!
!> @brief Handler for logging to a set of files,.
!> @details
!> Handler for logging to a set of files, which switches from one file
!> to the next when the current file reaches a certain size.
!> By default, the file grows indefinitely. You can specify particular
!> values of maxBytes and backupCount to allow the file to rollover at
!> a predetermined size.
!
!> @author ASTG staff
!> @date 01 Jan 2015 - Initial Version  
!------------------------------------------------------------------------------
module ASTG_RotatingFileHandler_mod
   use iso_fortran_env
   use ASTG_SeverityLevels_mod, only: INFO
   use ASTG_FileHandler_mod, only: FileHandler
   use ASTG_AbstractHandler_mod, only: BASIC_FORMAT
   use ASTG_LogRecord_mod
   use ASTG_Formatter_mod
   
   implicit none
   private

   public :: RotatingFileHandler

   type, extends(FileHandler) :: RotatingFileHandler
      private
      integer(int64) :: maxBytes
      integer :: backupCount
      logical :: delay
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

    
   !---------------------------------------------------------------------------  
   ! FUNCTION: 
   ! newRotatingFileHandler
   !
   ! DESCRIPTION: 
   ! Instantiate a rotating file handler with a given file name. Optionally
   ! set maxBytes, backupcount and level. Note maxBytes can be specified
   ! in kb, mb or gb. E.g. maxBytes=100mb.
   !---------------------------------------------------------------------------
   function newRotatingFileHandler(fileName, maxBytes, backupCount, level, delay) &
        result(handler)
      type (RotatingFileHandler) :: handler
      character(len=*), intent(in) :: fileName
      character(len=*), intent(in), optional :: maxBytes
      integer, intent(in), optional :: backupCount
      integer, intent(in), optional :: level
      logical, intent(in), optional :: delay
      
      integer :: level_
      integer :: backupCount_
      integer(int64) :: maxBytes_
      logical :: delay_

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

      if (present(delay)) then
         delay_ = delay
      else
         delay_ = .false.
      end if
      
      call handler%setFileName(fileName)
      call handler%open()
      call handler%setLevel(level_)
      handler%maxBytes = maxBytes_
      handler%backupCount = backupCount_
      handler%delay = delay_
      fileCount = 0
      call handler%setFormatter(Formatter(BASIC_FORMAT))
      
   end function newRotatingFileHandler

   
   !---------------------------------------------------------------------------  
   ! FUNCTION: 
   ! convertNumBytes
   !
   ! DESCRIPTION: 
   ! Convert maxBytes descriptor to bytes.
   !---------------------------------------------------------------------------  
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
      
   
   !---------------------------------------------------------------------------  
   ! ROUTINE: 
   ! emitMessage
   !
   ! DESCRIPTION: 
   ! Write a formatted string to a file.
   !---------------------------------------------------------------------------  
   subroutine emitMessage(this, record)
      class (RotatingFileHandler), intent(inout) :: this
      type (LogRecord), intent(in) :: record
      integer :: fileSize

      if (this%shouldRollover()) then
         call this%doRollover()
      end if

      write(this%getUnit(), '(a)') this%format(record)
    
   end subroutine emitMessage


   !---------------------------------------------------------------------------  
   ! FUNCTION: 
   ! shouldRollover
   !
   ! DESCRIPTION: 
   ! Determine if rollover should occur.
   !---------------------------------------------------------------------------  
   function shouldRollover(this) result(rollOver)
      class (RotatingFileHandler), intent(in) :: this
      logical :: rollOver 
      integer :: fileSize
      
      rollOver = .false.
      flush(this%getUnit())
      inquire(this%getUnit(), size=fileSize)

      if (fileSize > this%maxBytes) then
         rollOver = .true.
      end if
      
   end function shouldRollover

   
   !---------------------------------------------------------------------------  
   ! ROUTINE: 
   ! doRollover
   !
   ! DESCRIPTION: 
   ! Rollover occurs whenever the current log file is nearly maxBytes in
   ! length. If backupCount is >= 1, the system will successively create
   ! new files with the same pathname as the base file, but with extensions
   ! ".1", ".2" etc. appended to it. For example, with a backupCount of 5
   ! and a base file name of "app.log", you would get "app.log",
   ! "app.log.1", "app.log.2", ... through to "app.log.5". The file being
   ! written to is always "app.log" - when it gets filled up, it is closed
   ! and renamed to "app.log.1", and if files "app.log.1", "app.log.2" etc.
   ! exist, then they are renamed to "app.log.2", "app.log.3" etc.
   ! respectively.
   !---------------------------------------------------------------------------  
   subroutine doRollover(this)
      use ASTG_FormatString_mod
      class (RotatingFileHandler), intent(inout) :: this
      
      character(len=:), allocatable :: srcFile
      character(len=:), allocatable :: dstFile

      character(len=24) :: suffix
      logical :: exists
      integer :: i

      if (this%isOpen()) then
         call this%close()
      end if

      if (this%backupCount > 0) then

         do i = this%backupCount, 1, -1

            write(suffix,'(i0)') i
            srcFile = this%getFileName()//'.'//trim(suffix)
            write(suffix,'(i0)') i+1
            dstFile = this%getFileName()//'.'//trim(suffix)

            inquire(FILE=srcFile, EXIST=exists)
            if (exists) then
               call delete_if_exists(dstFile)
               call execute_command_line('mv ' // srcFile // ' ' // dstFile)
            end if

         end do

         dstFile = srcFile
         srcFile = this%getFileName()
         call delete_if_exists(dstFile)

         call execute_command_line('mv ' // srcFile // ' ' // dstFile)

      end if

      if (.not. this%delay) call this%open()

   contains

      subroutine delete_if_exists(fileName)
         character(len=*), intent(in) :: fileName

         logical :: exists
         integer :: unit

         inquire(FILE=fileName, EXIST=exists)

         if (exists) then
            open (file=fileName, newunit=unit)
            close(unit, status='delete')
         end if

      end subroutine delete_if_exists
      
   end subroutine doRollover


end module ASTG_RotatingFileHandler_mod
