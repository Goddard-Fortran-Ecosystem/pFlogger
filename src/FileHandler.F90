! A handler class which writes logging events to disk files
module ASTG_FileHandler_mod
   use ASTG_SeverityLevels_mod, only: INFO
   use ASTG_AbstractHandler_mod, only: AbstractHandler, BASIC_FORMAT
   use ASTG_LogRecord_mod
   use ASTG_Formatter_mod
   
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
      procedure :: equal
   end type FileHandler

   interface FileHandler
      module procedure newFileHandler
   end interface

   
contains

    
   ! Initializes the instance with a filename and an optional level
   function newFileHandler(fileName, level, delay) result(handler)
      type (FileHandler) :: handler
      character(len=*), intent(in) :: fileName
      integer, optional, intent(in) :: level
      logical, optional, intent(in) :: delay

      integer :: level_
      logical :: delay_
      
      if (present(level)) then
        level_ = level
      else
        level_ = INFO
      end if

      if (present(delay)) then
         delay_ = delay
      else
         delay_ = .false. ! backward compatibility
      end if

      call handler%setFileName(fileName)
      if (.not. delay_) call handler%open()
      call handler%setLevel(level_)
      call handler%setFormatter(Formatter(BASIC_FORMAT))
     
   end function newFileHandler

    
   ! Write a string to a file. Level is specified in levelString
   subroutine emitMessage(this, record)
      class (FileHandler), intent(inout) :: this
      type(LogRecord) :: record
      
      if (.not. this%isOpen()) call this%open()
      
      write(this%unit,'(a)') this%format(record)
       
   end subroutine emitMessage

    
   logical function isOpen(this)
      class (FileHandler), intent(in) :: this
       
      isOpen = .false.
      if (this%isOpen_) isOpen=.true.

   end function isOpen


   subroutine flushUnit(this)
      class (FileHandler), intent(inout) :: this
      
      flush(this%getUnit())
      
   end subroutine flushUnit

   
   ! Open the disk file used by this handler
   subroutine open(this)
      class (FileHandler), intent(inout) :: this
      integer :: unit
      
      if (this%isOpen()) return
      open(newunit=unit, file=this%getFileName(), &
           & status='unknown', form='formatted', position='append')
      call this%setUnit(unit)
      this%isOpen_ = .true.
       
   end subroutine open


   subroutine close(this)
      class (FileHandler), intent(inout) :: this

      close(this%getUnit())
      this%isOpen_ = .false.

   end subroutine close


   ! Set the fortran unit number for this handler
   subroutine setUnit(this, unit)
      class (FileHandler), intent(inout) :: this
      integer, intent(in) :: unit

      this%unit = unit

   end subroutine setUnit


   ! Get the fortran unit number for this handler
   integer function getUnit(this) result(unit)
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


   logical function equal(a, b)
      class (FileHandler), intent(in) :: a
      class (AbstractHandler), intent(in) :: b

      select type (b)
      class is (FileHandler)
         equal = (a%unit == b%unit) .and. (a%fileName == b%fileName) .and. &
              & (a%getLevel() == b%getLevel())
      class default
         equal = .false.
      end select

   end function equal


end module ASTG_FileHandler_mod
