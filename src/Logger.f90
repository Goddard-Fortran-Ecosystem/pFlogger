module ASTG_Logger_mod
  use ASTG_FileHandler_mod


   implicit none
   private

   public :: Logger
   public :: newLogger

   type :: Logger
     character(len=128) :: name
     type(FileHandler) :: handler
   contains
     procedure :: log_integer
     generic :: log => log_integer
   end type Logger

   interface newLogger
     module procedure newFileLogger
   end interface
   
contains

  function newFileLogger(name) result(log)
    character(len=*), intent(in) :: name
    type (Logger) :: log
    log%handler = newFileHandler(name)
    log%name = name
  end function newFileLogger

  subroutine log_integer(this, i)
    class (Logger), intent(inout) :: this
    integer, intent(in) :: i
    
    if (.not. this%handler%isOpen()) call this%handler%open()
    write(this%handler%getUnit(),'(a)') this%handler%toString(i)
    call flush(this%handler%getUnit())

   end subroutine log_integer

  
 end module ASTG_Logger_mod


