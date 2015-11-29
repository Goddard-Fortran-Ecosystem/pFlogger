module astg_MpiFormatter_mod
   use astg_Formatter_mod
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

   function newMpiFormatter_comm(comm, unused, rank_name, size_name, fmt, datefmt) result(f)
      use astg_FormatParser_mod
      use astg_FormatTokenVector_mod
      use astg_StringUnlimitedMap_mod, only: StringUnlimitedMap => Map
      use astg_FormatToken_mod
      use astg_FormatString_mod
      type (MpiFormatter) :: f
      integer, intent(in) :: comm
      type (Unusable), optional :: unused
      character(len=*), optional, intent(in) :: rank_name
      character(len=*), optional, intent(in) :: size_name
      character(len=*), intent(in) :: fmt
      character(len=*), optional, intent(in) :: datefmt

      type (StringUnlimitedMap) :: dictionary

      character(len=:), allocatable :: fmt_

      block
        character(len=:), allocatable :: rank_name_, size_name_
        integer :: rank, npes, ierror
        
        call MPI_Comm_rank(comm, rank, ierror)
        call MPI_Comm_size(comm, npes, ierror)
      
        rank_name_ = default(rank_name, 'rank')
        size_name_ = default(size_name, 'npes')
      
        call dictionary%insert(rank_name_, rank)
        call dictionary%insert(size_name_, npes)
      end block

      fmt_ = default(fmt, '%(rank)a~: %(name)a~: %(message)a')
      f%Formatter = Formatter(fmt_, datefmt=datefmt, extra=dictionary)

   end function newMpiFormatter_comm


   function newMpiFormatter_comms(comms, unused, rank_prefix, size_prefix, fmt, datefmt) result(f)
      use astg_FormatParser_mod
      use astg_FormatTokenVector_mod
      use astg_StringUnlimitedMap_mod, only: StringUnlimitedMap => Map
      use astg_FormatToken_mod
      use astg_FormatString_mod
      use astg_ArgListUtilities_mod
      type (MpiFormatter) :: f
      integer, intent(in) :: comms(:)
      type (Unusable), optional :: unused
      character(len=*), optional, intent(in) :: rank_prefix
      character(len=*), optional, intent(in) :: size_prefix
      character(len=*), optional, intent(in) :: fmt
      character(len=*), optional, intent(in) :: datefmt

      type (FormatParser) :: p
      type (StringUnlimitedMap) :: dictionary
      character(len=:), allocatable :: fmt_
      

      block
        integer :: i
        character(len=:), allocatable :: rank_prefix_, size_prefix_
        character(len=:), allocatable :: suffix
        integer :: rank, npes, ierror
        do i = 1, size(comms)
           call MPI_Comm_rank(comms(i), rank, ierror)
           call MPI_Comm_size(comms(i), npes, ierror)
           
           suffix = formatString('_%i1.1', makeArgVector(i))
           rank_prefix_ = default(rank_prefix, 'rank')
           size_prefix_ = default(size_prefix, 'npes')
           call dictionary%insert(rank_prefix_ // suffix, rank)
           call dictionary%insert(size_prefix_ // suffix, npes)
        end do
      end block

      fmt_ = default(fmt, '%(rank)a~: %(name)a~: %(message)a')
      f%Formatter = Formatter(fmt_, datefmt=datefmt, extra=dictionary)

      
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
   
end module astg_MpiFormatter_mod
