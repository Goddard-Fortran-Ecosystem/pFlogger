!------------------------------------------------------------------------------
! NASA/GSFC, CISTO, Code 606, Advanced Software Technology Group
!------------------------------------------------------------------------------
!
!*MODULE: ASTG_FileHandler_mod
!
!> @brief A handler class which writes logging events to disk files
!
!> @author ASTG staff
!> @date 01 Jan 2015 - Initial Version
!------------------------------------------------------------------------------
module ASTG_FileHandler_mod
   use ASTG_SeverityLevels_mod, only: INFO
   use ASTG_AbstractHandler_mod, only: AbstractHandler, BASIC_FORMAT
   use ASTG_LogRecord_mod
   use ASTG_Formatter_mod
   use ASTG_StreamHandler_mod
   implicit none
   private

   public :: FileHandler

   type, extends(StreamHandler) :: FileHandler
      private
      logical :: isOpen_ = .false.
      character(len=:), allocatable :: fileName
   contains
      procedure :: isOpen
      procedure :: open
      procedure :: close
      procedure :: setFileName
      procedure :: getFileName
      procedure :: emitMessage
      procedure :: equal
   end type FileHandler

   interface FileHandler
      module procedure newFileHandler
   end interface

   
contains

    
   !---------------------------------------------------------------------------  
   ! FUNCTION: 
   ! newFileHandler
   !
   ! DESCRIPTION: 
   ! Instantiate a file handler with a given file name. Optionally
   ! set a level and a delay. If a delay is set to true then we
   ! don't open the stream.
   !---------------------------------------------------------------------------
   function newFileHandler(fileName, delay) result(handler)
      type (FileHandler) :: handler
      character(len=*), intent(in) :: fileName
      logical, optional, intent(in) :: delay

      logical :: delay_
      
      if (present(delay)) then
         delay_ = delay
      else
         delay_ = .false. ! backward compatibility
      end if

      call handler%setFormatter(Formatter(BASIC_FORMAT))

      call handler%setFileName(fileName)
      if (.not. delay_) call handler%open()
     
   end function newFileHandler

    
   !---------------------------------------------------------------------------  
   ! ROUTINE: 
   ! emitMessage
   !
   ! DESCRIPTION: 
   ! Write a formatted string to a file.
   !---------------------------------------------------------------------------  
   subroutine emitMessage(this, record)
      class (FileHandler), intent(inout) :: this
      type(LogRecord), intent(in) :: record
      
      if (.not. this%isOpen()) call this%open()

      call this%StreamHandler%emitMessage(record)
       
   end subroutine emitMessage

    
   !---------------------------------------------------------------------------  
   ! FUNCTION: 
   ! isOpen
   !
   ! DESCRIPTION: 
   ! Check if file unit is open. 
   !---------------------------------------------------------------------------  
   logical function isOpen(this)
      class (FileHandler), intent(in) :: this
       
      isOpen = .false.
      if (this%isOpen_) isOpen=.true.

   end function isOpen


   !---------------------------------------------------------------------------  
   ! ROUTINE: 
   ! open
   !
   ! DESCRIPTION: 
   ! Open the disk file used by this handler.
   !---------------------------------------------------------------------------  
   subroutine open(this)
      class (FileHandler), intent(inout) :: this
      integer :: unit
      
      if (this%isOpen()) return

      open(newunit=unit, file=this%getFileName(), &
           & status='unknown', form='formatted', position='append')

      this%isOpen_ = .true.

      this%StreamHandler = StreamHandler(unit)
       
   end subroutine open


   !---------------------------------------------------------------------------  
   ! ROUTINE: 
   ! close
   !
   ! DESCRIPTION: 
   ! Closes the disk file used by this handler.
   !---------------------------------------------------------------------------  
   subroutine close(this)
      class (FileHandler), intent(inout) :: this
      integer :: unit

      call this%flush()
      call this%StreamHandler%close()
      inquire(file=this%getFileName(), number=unit)
      close(unit)
      this%isOpen_ = .false.

   end subroutine close


   !---------------------------------------------------------------------------  
   ! FUNCTION: 
   ! getFileName
   !
   ! DESCRIPTION: 
   ! Get the file name associated with this handler.
   !---------------------------------------------------------------------------  
   function getFileName(this) result(fileName)
      class (FileHandler), intent(in) :: this
      character(len=:), allocatable :: fileName

      fileName = this%fileName

   end function getFileName


   !---------------------------------------------------------------------------  
   ! ROUTINE: 
   ! setFileName
   !
   ! DESCRIPTION: 
   ! Set the file name associated with this handler.
   !---------------------------------------------------------------------------  
   subroutine setFileName(this, fileName)
      class (FileHandler), intent(inout) :: this
      character(len=*), intent(in) :: fileName

      this%fileName = fileName

   end subroutine setFileName


   !---------------------------------------------------------------------------  
   ! FUNCTION: 
   ! equal
   !
   ! DESCRIPTION: 
   ! Overloads 'equal' operation for file handlers.
   !---------------------------------------------------------------------------  
   logical function equal(a, b)
      class (FileHandler), intent(in) :: a
      class (AbstractHandler), intent(in) :: b

      select type (b)
      class is (FileHandler)
         equal = (a%StreamHandler == b%StreamHandler) .and. (a%fileName == b%fileName)
      class default
         equal = .false.
      end select

   end function equal


end module ASTG_FileHandler_mod
