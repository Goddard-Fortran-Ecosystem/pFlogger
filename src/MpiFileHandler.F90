!------------------------------------------------------------------------------
! NASA/GSFC, CISTO, Code 606, Advanced Software Technology Group
!------------------------------------------------------------------------------
!
!*MODULE: ASTG_MpiFileHandler_mod
!
!> @brief A handler class which writes logging events to disk files under MPI.
!
!> @author ASTG staff
!> @date 01 Jan 2015 - Initial Version  
!---------------------------------------------------------------------------
module ASTG_MpiFileHandler_mod
   use ASTG_SeverityLevels_mod, only: INFO
   use ASTG_LogRecord_mod
   use ASTG_FileHandler_mod
   use mpi
   
   implicit none
   private

   public :: MpiFileHandler
   public :: setDefaultMpiSuffixFormat
   public :: default_mpi_suffix_format, defaultMpiSuffixFormat

   type, extends(FileHandler) :: MpiFileHandler
      private
   contains
! TODO: Dreadful workaround for gfortran 4.9.1 and 4.9.2.  Made
! getSuffix() an external procedure.
      procedure :: getSuffix
   end type MpiFileHandler

   interface MpiFileHandler
      module procedure newMpiFileHandler
   end interface

   type UnusableArg
   end type UnusableArg

   character(len=*), parameter :: DEFAULT_MPI_SUFFIX_FORMAT = '.pe=%i0'
   character(len=:), allocatable :: defaultMpiSuffixFormat
   
contains

    
   !---------------------------------------------------------------------------  
   ! FUNCTION: 
   ! newMpiFileHandler
   !
   ! DESCRIPTION: 
   ! Instantiate an mpi file handler with a given communicator. Optionally
   ! set level, suffix format and delay. If a delay is set to true then we
   ! don't open the stream.
   !---------------------------------------------------------------------------
   function newMpiFileHandler(fileNamePrefix, mpiCommunicator, unused, &
        & suffixformat, delay) result(handler)
      type (MpiFileHandler) :: handler
      character(len=*), intent(in) :: fileNamePrefix
      integer, intent(in) :: mpiCommunicator
      type (UnusableArg), optional, intent(in) :: unused
      character(len=*), optional, intent(in) :: suffixFormat
      logical, optional, intent(in) :: delay

      character(len=:), allocatable :: suffix
      integer :: rank, ier

!!$      ! Interface needed for external function
!!$      interface
!!$         function getSuffix(rank, suffixFormat) result(rawString)
!!$            character(len=:), allocatable :: rawString
!!$            integer, intent(in) :: rank
!!$            character(len=*), optional, intent(in) :: suffixFormat
!!$         end function getSuffix
!!$      end interface

      call MPI_Comm_rank(mpiCommunicator, rank, ier)

      suffix = handler%getSuffix(rank, suffixFormat)
      handler%FileHandler = FileHandler(fileNamePrefix // suffix, delay=delay)

      
   end function newMpiFileHandler

    
   function getSuffix(this, rank, suffixFormat) result(suffix)
      use ASTG_FormatParser_mod
      use ASTG_FormatString_mod
      use ASTG_UnlimitedVector_mod, only: UnlimitedVector => vector
      character(len=:), allocatable :: suffix
      class (MpiFileHandler), intent(in) :: this
      integer, intent(in) :: rank
      character(len=*), optional, intent(in) :: suffixFormat

      type (FormatParser) :: parser

      character(len=:), allocatable :: fmt
      type (UnlimitedVector) :: v

      call v%push_back(rank)

      if (present(suffixFormat)) then
         fmt = suffixFormat
      elseif (allocated(defaultMpiSuffixFormat)) then
         fmt = defaultMpiSuffixFormat
      else
         fmt = DEFAULT_MPI_SUFFIX_FORMAT
      end if

      suffix = formatString(fmt, v)

   end function getSuffix


   subroutine setDefaultMpiSuffixFormat(suffixFormat)
      character(len=*), intent(in) :: suffixFormat
      defaultMpiSuffixFormat = suffixFormat
   end subroutine setDefaultMpiSuffixFormat


end module ASTG_MpiFileHandler_mod

