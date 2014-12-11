module ASTG_FileHandler_mod
   use ASTG_AbstractHandler_mod
   use ASTG_StringUtilities_mod
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
     procedure :: setFileName
     procedure :: getFileName
     procedure :: emit

   end type FileHandler

   interface FileHandler
      module procedure newFileHandler
   end interface


 contains


    function newFileHandler(fileName) result(handler)
       type (FileHandler) :: handler
       character(len=*), intent(in) :: fileName
       
       call handler%setFileName(fileName)
       call handler%open()
       !      call handler%setHandle(INFO_LOGGING_LEVEL)
    end function NewFileHandler
    
    
    subroutine emit(this, message)
       class (FileHandler), intent(inout) :: this
       character(len=*), intent(in) :: message

       write(this%unit,'(a)') message
       
    end subroutine emit
    
    logical function isOpen(this)
       class (FileHandler), intent(in) :: this
       
       isOpen = .false.

    end function isOpen

    
    subroutine open(this)
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
       class (FileHandler), intent(inout) :: this
       integer, intent(in) :: unit
       
       this%unit = unit
    end subroutine setUnit

    
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

end module ASTG_FileHandler_mod
