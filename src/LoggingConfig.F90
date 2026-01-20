#include "error_handling_macros.fh"
module PFL_LoggingConfig
   use PFL_StringFilterMap
   use PFL_StringFormatterMap
   use PFL_StringHandlerMap
   use PFL_StringLockMap
#ifdef _LOGGER_USE_MPI
   use MPI
#endif
   implicit none
   private

   public :: LoggingConfig

   type :: LoggingConfig
      private
      integer :: global_communicator = -1 ! not used in serial
      type (FilterMap) :: filters
      type (FormatterMap) :: formatters
      type (HandlerMap) :: handlers
      type (LockMap) :: locks
   contains
      ! Accessor methods
      procedure :: set_global_communicator
      procedure :: get_filters
      procedure :: get_formatters
      procedure :: get_handlers
      procedure :: get_locks
      procedure :: get_global_communicator
   end type LoggingConfig

contains

   function get_filters(this) result(ptr)
      type (FilterMap), pointer :: ptr
      class (LoggingConfig), target, intent(in) :: this
      ptr => this%filters
   end function get_filters

   function get_formatters(this) result(ptr)
      type (FormatterMap), pointer :: ptr
      class (LoggingConfig), target, intent(in) :: this
      ptr => this%formatters
   end function get_formatters

   function get_handlers(this) result(ptr)
      type (HandlerMap), pointer :: ptr
      class (LoggingConfig), target, intent(in) :: this
      ptr => this%handlers
   end function get_handlers

   function get_locks(this) result(ptr)
      type (LockMap), pointer :: ptr
      class (LoggingConfig), target, intent(in) :: this
      ptr => this%locks
   end function get_locks

   function get_global_communicator(this) result(comm)
      integer :: comm
      class (LoggingConfig), intent(in) :: this
      comm = this%global_communicator
   end function get_global_communicator

   subroutine set_global_communicator(this, comm)
      class (LoggingConfig), intent(inout) :: this
      integer, optional, intent(in) :: comm

#ifdef _LOGGER_USE_MPI
      if (present(comm)) then
         this%global_communicator = comm
      else
         this%global_communicator = MPI_COMM_WORLD
      end if
#endif

   end subroutine set_global_communicator

end module PFL_LoggingConfig
