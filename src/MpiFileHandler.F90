! A handler class which writes logging events to disk files under MPI
module ASTG_MpiFileHandler_mod
   use ASTG_SeverityLevels_mod, only: INFO
   use ASTG_FileHandler_mod
   use ASTG_LogRecord_mod
   
   implicit none

   private

   public :: MpiFileHandler
   public :: setDefaultMpiSuffixFormat
   public :: default_mpi_suffix_format, defaultMpiSuffixFormat

   type, extends(FileHandler) :: MpiFileHandler
      private
   contains
! TOD: Dreadful workaround for gfortran 4.9.1 and 4.9.2.  Made
! getSuffix() an external procedure.
!!$      procedure :: getSuffix
   end type MpiFileHandler

   interface MpiFileHandler
      module procedure newMpiFileHandler
   end interface

   type UnusableArg
   end type UnusableArg

   character(len=*), parameter :: DEFAULT_MPI_SUFFIX_FORMAT = '.pe=%i0'
   character(len=:), allocatable :: defaultMpiSuffixFormat
   
contains

    
   function newMpiFileHandler(fileNamePrefix, mpiCommunicator, unused, &
        & level, suffixformat, delay) result(handler)
      type (MpiFileHandler) :: handler
      character(len=*), intent(in) :: fileNamePrefix
      integer, intent(in) :: mpiCommunicator
      type (UnusableArg), optional, intent(in) :: unused
      integer, optional, intent(in) :: level
      character(len=*), optional, intent(in) :: suffixFormat
      logical, optional, intent(in) :: delay

      character(len=:), allocatable :: suffix
      integer :: rank, ier

      interface
         function getSuffix(rank, suffixFormat) result(rawString)
            character(len=:), allocatable :: rawString
            integer, intent(in) :: rank
            character(len=*), optional, intent(in) :: suffixFormat
         end function getSuffix
      end interface

      call MPI_Comm_rank(mpiCommunicator, rank, ier)

      suffix = getSuffix(rank, suffixFormat)
      handler%FileHandler = FileHandler(fileNamePrefix // suffix, level, delay=delay)
      
   end function newMpiFileHandler

    
!!$   function getSuffix(this, rank, suffixFormat) result(suffix)
!!$      use ASTG_FormatParser_mod
!!$      use FTL_StringUtilities_mod
!!$      character(len=:), allocatable :: suffix
!!$      class (MpiFileHandler), intent(in) :: this
!!$      integer, intent(in) :: rank
!!$      character(len=*), optional, intent(in) :: suffixFormat
!!$
!!$      type (FormatParser) :: parser
!!$
!!$      character(len=:), allocatable :: fmt
!!$
!!$      if (present(suffixFormat)) then
!!$         fmt = suffixFormat
!!$      elseif (allocated(defaultMpiSuffixFormat)) then
!!$         fmt = defaultMpiSuffixFormat
!!$      else
!!$         fmt = DEFAULT_MPI_SUFFIX_FORMAT
!!$      end if
!!$
!!$      suffix = parser%makeString(fmt, rank)
!!$
!!$   end function getSuffix


   subroutine setDefaultMpiSuffixFormat(suffixFormat)
      character(len=*), intent(in) :: suffixFormat
      defaultMpiSuffixFormat = suffixFormat
   end subroutine setDefaultMpiSuffixFormat


end module ASTG_MpiFileHandler_mod

function getSuffix(rank, suffixFormat) result(suffix)
   use ASTG_MpiFileHandler_mod
   use ASTG_FormatParser_mod
   use FTL_StringUtilities_mod
   character(len=:), allocatable :: suffix
   integer, intent(in) :: rank
   character(len=*), optional, intent(in) :: suffixFormat

   type (FormatParser) :: parser

   character(len=:), allocatable :: fmt

   if (present(suffixFormat)) then
      fmt = suffixFormat
   elseif (allocated(defaultMpiSuffixFormat)) then
      fmt = defaultMpiSuffixFormat
   else
      fmt = DEFAULT_MPI_SUFFIX_FORMAT
   end if

   suffix = parser%makeString(fmt, rank)

end function getSuffix




