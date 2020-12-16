#include "error_handling_macros.fh"
module PFL_MpiFormatter
   use PFL_Formatter
   use PFL_MpiCommConfig
   use PFL_Exception
   use mpi
   implicit none
   private

   public :: MpiFormatter

   type, extends(Formatter) :: MpiFormatter
   end type MpiFormatter

   interface MpiFormatter
      module procedure newMpiFormatter_comm
      module procedure newMpiFormatter_comms
   end interface MpiFormatter

   ! Private type - to force keyword usage for optional arguments
   type :: Unusable
   end type Unusable

contains

   function newMpiFormatter_comm(comm, unused, rank_keyword, size_keyword, fmt, datefmt, rc) result(f)
      use PFL_FormatParser
      use PFL_FormatTokenVector
      use gftl_StringUnlimitedMap
      use PFL_FormatToken
      use PFL_FormatString
      type (MpiFormatter) :: f
      integer, intent(in) :: comm
      type (Unusable), optional :: unused
      character(len=*), optional, intent(in) :: rank_keyword
      character(len=*), optional, intent(in) :: size_keyword
      character(len=*), optional, intent(in) :: fmt
      character(len=*), optional, intent(in) :: datefmt
      integer, optional, intent(out) :: rc

      type (StringUnlimitedMap) :: dictionary

      character(len=:), allocatable :: fmt_
      integer :: status
      ! workaround for gfortran 10.0
      call init_MpiCommConfig(dictionary, comm, rank_keyword=rank_keyword, size_keyword=size_keyword, rc=status)
      _VERIFY(status,'',rc)

      fmt_ = default(fmt, 'pe=%(mpi_rank)i4.4~: %(name)a~: %(message)a')
      f%Formatter = Formatter(fmt_, datefmt=datefmt, extra=dictionary)
      _RETURN(_SUCCESS,rc)
   end function newMpiFormatter_comm


   function newMpiFormatter_comms(comms, unused, rank_prefix, size_prefix, fmt, datefmt, rc) result(f)
      use PFL_FormatParser
      use PFL_FormatTokenVector
      use gftl_StringUnlimitedMap
      use PFL_FormatToken
      use PFL_FormatString
      use PFL_ArgListUtilities
      type (MpiFormatter) :: f
      integer, intent(in) :: comms(:)
      type (Unusable), optional :: unused
      character(len=*), optional, intent(in) :: rank_prefix
      character(len=*), optional, intent(in) :: size_prefix
      character(len=*), optional, intent(in) :: fmt
      character(len=*), optional, intent(in) :: datefmt
      integer, optional, intent(out) :: rc

      type (FormatParser) :: p
      type (StringUnlimitedMap) :: dictionary
      character(len=:), allocatable :: fmt_
      integer :: status
 
      ! workaround for gfortran 10.0
      call init_MpiCommConfig(dictionary, comms, rank_prefix=rank_prefix, size_prefix=size_prefix, rc=status)
      _VERIFY(status,'',rc)

      if (present(fmt)) then
         fmt_ = fmt
      else
         block
           integer :: i
           character(len=1) :: c
           fmt_ = 'pe=('
           do i = 1, size(comms)
              write(c,'(i1.1)')i
              fmt_ = fmt_ // '%(' // default(rank_prefix,'mpi_rank') // '_' // c // ')i0'
              if (i < size(comms)) then ! insert a comma
                 fmt_ = fmt_ // '~,'
              end if
           end do
           fmt_ = fmt_ // '~): %(message)a'
         end block
      end if

      f%Formatter = Formatter(fmt_, datefmt=datefmt, extra=dictionary)
      _RETURN(_SUCCESS,rc)
      
   end function newMpiFormatter_comms

   ! helper funnction for convenience with optional arguments that
   ! have default values.
   function default(x_optional, x_default) result(x)
      character(len=:), allocatable :: x
      character(len=*), optional, intent(in) :: x_optional
      character(len=*), intent(in) :: x_default
      
      if (present(x_optional)) then
         x = x_optional
      else
         x = x_default
      end if
      
   end function default
   
end module PFL_MpiFormatter
