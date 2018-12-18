#include "error_handling_macros.fh"
!------------------------------------------------------------------------------
! NASA/GSFC, CISTO, Code 606, Advanced Software Technology Group
!------------------------------------------------------------------------------
!
!*MODULE: PFL_RotatingFileHandler_mod
!
!> @brief Handler for logging to a set of files,.
!> @details
!> Handler for logging to a set of files, which switches from one file
!> to the next when the current file reaches a certain size.
!> By default, the file grows indefinitely. You can specify particular
!> values of max_bytes and backup_count to allow the file to rollover at
!> a predetermined size.
!
!> @author ASTG staff
!> @date 01 Jan 2015 - Initial Version  
!------------------------------------------------------------------------------
module PFL_RotatingFileHandler_mod
   use, intrinsic :: iso_fortran_env, only: INT64
   use PFL_FileHandler_mod, only: FileHandler
   use PFL_LogRecord_mod, only: LogRecord
   use PFL_KeywordEnforcer_mod
   use PFL_Exception_mod
   implicit none
   private

   public :: RotatingFileHandler
   public :: newRotatingFileHandler

   type, extends(FileHandler) :: RotatingFileHandler
      private
      integer(INT64) :: max_bytes = HUGE(1)
      integer :: backup_count = 0
      logical :: delay = .false.
   contains
      procedure :: do_rollover
      procedure :: should_rollover
      procedure :: atomic_emit_message
   end type RotatingFileHandler

   interface RotatingFileHandler
      module procedure newRotatingFileHandler
   end interface

   type Unusable
   end type Unusable
   
contains

    
   !---------------------------------------------------------------------------  
   ! FUNCTION: 
   ! newRotatingFileHandler
   !
   ! DESCRIPTION: 
   ! Instantiate a rotating file handler with a given file name. Optionally
   ! set max_bytes, backupcount and level. Note max_bytes can be specified
   ! in kb, mb or gb. E.g. max_bytes=100mb.
   !---------------------------------------------------------------------------
   function newRotatingFileHandler(fileName, max_bytes, unused, backup_count, delay) &
        result(handler)
      use PFL_AbstractHandler_mod, only: BASIC_FORMAT
      use PFL_Formatter_mod
      type (RotatingFileHandler) :: handler
      character(len=*), intent(in) :: fileName
      type (Unusable), optional, intent(in) :: unused
      character(len=*), intent(in), optional :: max_bytes
      integer, intent(in), optional :: backup_count
      logical, intent(in), optional :: delay
      
      integer :: backup_count_
      integer(INT64) :: max_bytes_
      logical :: delay_

      if (present(max_bytes)) then
         max_bytes_ = convertNumBytes_(max_bytes)
      else
         max_bytes_ = 0
      end if
      if (present(backup_count)) then
         backup_count_ = backup_count
      else
         backup_count_ = 0
      end if

      if (present(delay)) then
         delay_ = delay
      else
         delay_ = .false.
      end if

      handler%FileHandler = FileHandler(fileName, delay=delay)

      handler%max_bytes = max_bytes_
      handler%backup_count = backup_count_
      handler%delay = delay_

      call handler%set_formatter(Formatter(BASIC_FORMAT))
      
   end function newRotatingFileHandler

   
   !---------------------------------------------------------------------------  
   ! FUNCTION: 
   ! convertNumBytes
   !
   ! DESCRIPTION: 
   ! Convert max_bytes descriptor to bytes.
   !---------------------------------------------------------------------------  
   function convertNumBytes_(max_bytes) result(nBytes)
      character(len=*), intent(in) :: max_bytes
      integer(INT64) :: nBytes
      
      character(len=16) :: fmt
      integer :: pos, n, nl

      pos = scan(max_bytes,'kmg')
      if (pos > 0) then
        nl = len(max_bytes(1:pos-1))        
        fmt = setFmt_(nl)
        select case(max_bytes(pos:pos+1))
        case ('kb')
          read(max_bytes(1:pos-1), fmt) n
          nBytes = n * (2_INT64 ** 10)
        case ('mb')
          read(max_bytes(1:pos-1), fmt) n
          nBytes = n * (2_INT64 ** 20)
        case ('gb')
          read(max_bytes(1:pos-1), fmt) n
          nBytes = n * (2_INT64 ** 30)
        end select
      else
        nl = len(max_bytes)
        fmt = setFmt_(nl)
        read(max_bytes, fmt) n
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
   ! atomic_emit_message
   !
   ! DESCRIPTION: 
   ! Write a formatted string to a file.
   !---------------------------------------------------------------------------  
   subroutine atomic_emit_message(this, record, unusable, rc)
      class (RotatingFileHandler), intent(inout) :: this
      type (LogRecord), intent(in) :: record
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: fileSize
      integer :: status
      
      if (this%should_rollover()) then
         call this%do_rollover()
      end if

      call this%FileHandler%atomic_emit_message(record, rc=status)
      _VERIFY(status,'',rc)

      _RETURN(_SUCCESS,rc)
    
   end subroutine atomic_emit_message


   !---------------------------------------------------------------------------  
   ! FUNCTION: 
   ! should_rollover
   !
   ! DESCRIPTION: 
   ! Determine if rollover should occur.
   !---------------------------------------------------------------------------  
   function should_rollover(this) result(rollOver)
      class (RotatingFileHandler), intent(in) :: this
      logical :: rollOver 
      integer :: fileSize
      
      rollOver = .false.
      call this%FileHandler%flush()
      ! workaround for NAG
!!$      inquire(file=this%get_file_name(), size=fileSize)
      inquire(unit=this%get_unit(), size=fileSize)

      if (fileSize > this%max_bytes) then
         rollOver = .true.
      end if
      
   end function should_rollover

   
   !---------------------------------------------------------------------------  
   ! ROUTINE: 
   ! do_rollover
   !
   ! DESCRIPTION: 
   ! Rollover occurs whenever the current log file is nearly max_bytes in
   ! length. If backup_count is >= 1, the system will successively create
   ! new files with the same pathname as the base file, but with extensions
   ! ".1", ".2" etc. appended to it. For example, with a backup_count of 5
   ! and a base file name of "app.log", you would get "app.log",
   ! "app.log.1", "app.log.2", ... through to "app.log.5". The file being
   ! written to is always "app.log" - when it gets filled up, it is closed
   ! and renamed to "app.log.1", and if files "app.log.1", "app.log.2" etc.
   ! exist, then they are renamed to "app.log.2", "app.log.3" etc.
   ! respectively.
   !---------------------------------------------------------------------------  
   subroutine do_rollover(this)
      use PFL_FormatString_mod
      class (RotatingFileHandler), intent(inout) :: this
      
      character(len=:), allocatable :: srcFile
      character(len=:), allocatable :: dstFile

      character(len=24) :: suffix
      logical :: exists
      integer :: i

      if (this%is_open()) then
         call this%close()
      end if

      if (this%backup_count > 0) then

         do i = this%backup_count, 1, -1

            write(suffix,'(i0)') i
            srcFile = this%get_file_name()//'.'//trim(suffix)
            write(suffix,'(i0)') i+1
            dstFile = this%get_file_name()//'.'//trim(suffix)

            inquire(FILE=srcFile, EXIST=exists)

            if (exists) then
               call delete_if_exists(dstFile)
               call execute_command_line('mv ' // srcFile // ' ' // dstFile)
            end if

         end do

         dstFile = srcFile
         srcFile = this%get_file_name()
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
      
   end subroutine do_rollover


end module PFL_RotatingFileHandler_mod
