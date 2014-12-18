module ASTG_FileHandler_mod
   ! A handler class which writes logging events to disk files
   use ASTG_SeverityLevels_mod, only: INFO
   use ASTG_AbstractHandler_mod, only: AbstractHandler
   use ASTG_LogRecord_mod
   
   implicit none
   private

   public :: FileHandler

   type, extends(AbstractHandler) :: FileHandler
      private
      logical :: isOpen_ = .false.
      integer :: unit
      character(len=:), allocatable :: fileName
   contains
      procedure :: setUnit
      procedure :: getUnit
      procedure :: isOpen
      procedure :: open
      procedure :: close
      procedure :: flush => flushUnit
      procedure :: setFileName
      procedure :: getFileName
      procedure :: emitMessage
   end type FileHandler

   interface FileHandler
      module procedure newFileHandler
   end interface

   
contains

    
   function newFileHandler(fileName, level) result(handler)
      ! Initializes the instance with a filename and an optional level
      type (FileHandler) :: handler
      character(len=*), intent(in) :: fileName
      integer, intent(in), optional :: level
      integer :: level_
      
      if (present (level)) then
        level_ = level
      else
        level_ = INFO
      end if
      call handler%setFileName(fileName)
      call handler%open()
      call handler%setLevel(level_)
      
   end function newFileHandler

    
   subroutine emitMessage(this, levelString, record)
      ! Write a string to a file. Level is specified in levelString
      class (FileHandler), intent(inout) :: this
      character(len=*), intent(in) :: levelString
      type(LogRecord) :: record

      write(this%unit,'(a)') levelString // ': ' // record%getMessage()
       
   end subroutine emitMessage

    
   logical function isOpen(this)
      class (FileHandler), intent(in) :: this
       
      isOpen = .false.
      if (this%isOpen_) isOpen=.true.

   end function isOpen


   subroutine flushUnit(this)
      class (FileHandler), intent(inout) :: this
      
      call flush(this%getUnit())
      
   end subroutine flushUnit

   
   subroutine open(this)
      ! Open the disk file used by this handler
      class (FileHandler), intent(inout) :: this
      integer :: unit
      
      if (this%isOpen()) return
      open(newunit=unit, file=this%getFileName(), &
           & status='unknown', form='formatted', access='append')
      call this%setUnit(unit)
      this%isOpen_ = .true.
       
   end subroutine open


   subroutine close(this)
      class (FileHandler), intent(inout) :: this

      close(this%getUnit())
      this%isOpen_ = .false.

   end subroutine close


   subroutine setUnit(this, unit)
      ! Set the fortran unit number for this handler
      class (FileHandler), intent(inout) :: this
      integer, intent(in) :: unit

      this%unit = unit

   end subroutine setUnit


   integer function getUnit(this) result(unit)
      ! Get the fortran unit number for this handler
      class (FileHandler), intent(in) :: this

      unit = this%unit

   end function getUnit


   function getFileName(this) result(fileName)
      class (FileHandler), intent(in) :: this
      character(len=:), allocatable :: fileName

      fileName = this%fileName

   end function getFileName


   subroutine setFileName(this, fileName)
      class (FileHandler), intent(inout) :: this
      character(len=*), intent(in) :: fileName

      this%fileName = fileName

   end subroutine setFileName

end module ASTG_FileHandler_mod
