! A handler class which writes logging events to disk files under MPI
module ASTG_MpiFileHandler_mod
   use ASTG_SeverityLevels_mod, only: INFO
   use ASTG_FileHandler_mod
   use ASTG_LogRecord_mod
   
   implicit none

#ifdef USE_MPI
#include "mpif.h"
#endif

   private

   public :: MpiFileHandler

   type, extends(FileHandler) :: MpiFileHandler
   contains
      procedure :: emitMessage
   end type MpiFileHandler

   interface MpiFileHandler
      module procedure newMpiFileHandler
   end interface

   
contains

    
   function newMpiFileHandler(fileName, level) result(handler)
      type (MpiFileHandler) :: handler
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
      
   end function newMpiFileHandler

    
   ! Write a string to a file. Level is specified in levelString
   subroutine emitMessage(this, levelString, record)
      class (MpiFileHandler), intent(inout) :: this
      character(len=*), intent(in) :: levelString
      type(LogRecord) :: record

      write(this%getUnit(),'(a)') levelString // ': ' // record%getMessage()
       
   end subroutine emitMessage

end module ASTG_MpiFileHandler_mod
