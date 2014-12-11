module ASTG_Logger_mod
   use ASTG_AbstractHandler_mod

   implicit none
   private

   public :: Logger
   public :: newLogger

   type :: Logger
      class (AbstractHandler), allocatable :: handler
   contains
      procedure :: log
   end type Logger


   interface newLogger
     module procedure newFileLogger
   end interface

   
contains


  function newFileLogger(name) result(log)
     use ASTG_FileHandler_mod
    character(len=*), intent(in) :: name
    type (Logger) :: log

    allocate(log%handler, source=FileHandler(fileName=name))

  end function newFileLogger


  subroutine log(this, message)
    class (Logger), intent(inout) :: this
    character(len=*), intent(in) :: message

    call this%handler%emit(message)
    
 end subroutine log

  
 end module ASTG_Logger_mod


