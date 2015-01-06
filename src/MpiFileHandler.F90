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
      private
   contains
      procedure :: getSuffix
   end type MpiFileHandler

   interface MpiFileHandler
      module procedure newMpiFileHandler
   end interface

   
contains

    
   function newMpiFileHandler(fileNamePrefix, mpiCommunicator, level, suffixformat) result(handler)
      type (MpiFileHandler) :: handler
      character(len=*), intent(in) :: fileNamePrefix
      integer, intent(in) :: mpiCommunicator
      integer, optional, intent(in) :: level
      character(len=*), optional, intent(in) :: suffixFormat

      character(len=:), allocatable :: suffix
      integer :: rank, ier

      call MPI_Comm_rank(mpiCommunicator, rank, ier)

      suffix = handler%getSuffix(rank, suffixFormat)
      handler%FileHandler = FileHandler(fileNamePrefix // suffix, level)
      
   end function newMpiFileHandler

    
   function getSuffix(this, rank, suffixFormat) result(suffix)
      use FTL_StringUtilities_mod
      use ASTG_FormatParser_mod
      character(len=:), allocatable :: suffix
      class (MpiFileHandler), intent(in) :: this
      integer, intent(in) :: rank
      character(len=*), optional, intent(in) :: suffixFormat

      type (FormatParser) :: parser

      if (present(suffixFormat)) then
         suffix = parser%makeString(suffixFormat, rank)
      else
         suffix = '.pe=' // toString(rank)
      end if

   end function getSuffix


end module ASTG_MpiFileHandler_mod
