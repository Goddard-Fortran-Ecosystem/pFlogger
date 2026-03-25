#include "error_handling_macros.fh"
module PFL_AbstractConfigBuilder
   use PFL_LoggingConfig
   use gFTL2_StringUnlimitedMap
   use Pfl_KeywordEnforcer
   use PFL_Exception
   implicit none
   private

   public :: AbstractConfigBuilder
   public :: SCHEMA_VERSION ! Enforce version across all builders
   
   ! Standard schema section names - all builders MUST support these
   public :: SECTION_FORMATTERS
   public :: SECTION_FILTERS
   public :: SECTION_HANDLERS
   public :: SECTION_LOCKS
   public :: SECTION_LOGGERS
   public :: SECTION_ROOT

   integer, parameter :: SCHEMA_VERSION = 1
   
   ! Schema structure contract: All builders must support config with these sections
   character(len=*), parameter :: SECTION_FORMATTERS = 'formatters'
   character(len=*), parameter :: SECTION_FILTERS = 'filters'
   character(len=*), parameter :: SECTION_HANDLERS = 'handlers'
   character(len=*), parameter :: SECTION_LOCKS = 'locks'
   character(len=*), parameter :: SECTION_LOGGERS = 'loggers'
   character(len=*), parameter :: SECTION_ROOT = 'root'

   type, abstract :: AbstractConfigBuilder
   contains
      procedure :: build
      procedure(I_load_file), deferred :: load_file
      procedure(I_get_schema_version), deferred :: get_schema_version
      procedure(I_build_locks), deferred :: build_locks
      procedure(I_build_filters), deferred :: build_filters
      procedure(I_build_formatters), deferred :: build_formatters
      procedure(I_build_handlers), deferred :: build_handlers
      procedure(I_build_loggers_from_cfg), deferred :: build_loggers_from_cfg
      procedure(I_build_root_logger_from_cfg), deferred :: build_root_logger_from_cfg
   end type AbstractConfigBuilder

   abstract interface
      subroutine I_load_file(this, filename, rc)
         import AbstractConfigBuilder
         class(AbstractConfigBuilder), intent(inout) :: this
         character(len=*), intent(in) :: filename
         integer, optional, intent(out) :: rc
      end subroutine I_load_file

      function I_get_schema_version(this, rc) result(version)
         import AbstractConfigBuilder
         class(AbstractConfigBuilder), intent(in) :: this
         integer, optional, intent(out) :: rc
         integer :: version
      end function I_get_schema_version

      subroutine I_build_locks(this, config, unusable, extra, rc)
         use PFL_LoggingConfig
         use gFTL2_StringUnlimitedMap
         use Pfl_KeywordEnforcer
         import AbstractConfigBuilder
         class(AbstractConfigBuilder), intent(in) :: this
         type(LoggingConfig), intent(inout) :: config
         class(KeywordEnforcer), optional, intent(in) :: unusable
         type(StringUnlimitedMap), optional, intent(in) :: extra
         integer, optional, intent(out) :: rc
      end subroutine I_build_locks

      subroutine I_build_filters(this, config, unusable, extra, rc)
         use PFL_LoggingConfig
         use gFTL2_StringUnlimitedMap
         use Pfl_KeywordEnforcer
         import AbstractConfigBuilder
         class(AbstractConfigBuilder), intent(in) :: this
         type(LoggingConfig), intent(inout) :: config
         class(KeywordEnforcer), optional, intent(in) :: unusable
         type(StringUnlimitedMap), optional, intent(in) :: extra
         integer, optional, intent(out) :: rc
      end subroutine I_build_filters

      subroutine I_build_formatters(this, config, unusable, extra, rc)
         use PFL_LoggingConfig
         use gFTL2_StringUnlimitedMap
         use Pfl_KeywordEnforcer
         import AbstractConfigBuilder
         class(AbstractConfigBuilder), intent(in) :: this
         type(LoggingConfig), intent(inout) :: config
         class(KeywordEnforcer), optional, intent(in) :: unusable
         type(StringUnlimitedMap), optional, intent(in) :: extra
         integer, optional, intent(out) :: rc
      end subroutine I_build_formatters

      subroutine I_build_handlers(this, config, unusable, extra, rc)
         use PFL_LoggingConfig
         use gFTL2_StringUnlimitedMap
         use Pfl_KeywordEnforcer
         import AbstractConfigBuilder
         class(AbstractConfigBuilder), intent(in) :: this
         type(LoggingConfig), intent(inout) :: config
         class(KeywordEnforcer), optional, intent(in) :: unusable
         type(StringUnlimitedMap), optional, intent(in) :: extra
         integer, optional, intent(out) :: rc
      end subroutine I_build_handlers

      subroutine I_build_loggers_from_cfg(this, loggers, config, unusable, extra, rc)
         use PFL_StringAbstractLoggerPolyMap
         use PFL_LoggingConfig
         use gFTL2_StringUnlimitedMap
         use Pfl_KeywordEnforcer
         import AbstractConfigBuilder
         class(AbstractConfigBuilder), intent(in) :: this
         type(LoggerMap), target, intent(inout) :: loggers
         type(LoggingConfig), target, intent(in) :: config
         class(KeywordEnforcer), optional, intent(in) :: unusable
         type(StringUnlimitedMap), optional, intent(in) :: extra
         integer, optional, intent(out) :: rc
      end subroutine I_build_loggers_from_cfg

      subroutine I_build_root_logger_from_cfg(this, root_logger, config, unusable, extra, rc)
         use PFL_Logger
         use PFL_LoggingConfig
         use gFTL2_StringUnlimitedMap
         use Pfl_KeywordEnforcer
         import AbstractConfigBuilder
         class(AbstractConfigBuilder), intent(in) :: this
         class(Logger), target, intent(inout) :: root_logger
         type(LoggingConfig), target, intent(in) :: config
         class(KeywordEnforcer), optional, intent(in) :: unusable
         type(StringUnlimitedMap), optional, intent(in) :: extra
         integer, optional, intent(out) :: rc
      end subroutine I_build_root_logger_from_cfg
   end interface

contains

   ! Template method - orchestrates the config build process
   subroutine build(this, config, unusable, extra, comm, rc)
      class(AbstractConfigBuilder), intent(in) :: this
      type(LoggingConfig), intent(inout) :: config
      class(KeywordEnforcer), optional, intent(in) :: unusable
      type(StringUnlimitedMap), optional, intent(in) :: extra
      integer, optional, intent(in) :: comm
      integer, optional, intent(out) :: rc

      integer :: status
      integer :: version

      ! Validate schema version (enforced across all builders)
      version = this%get_schema_version(rc=status)
      _VERIFY(status, '', rc)
      _ASSERT(version == SCHEMA_VERSION, 'Unsupported schema_version. Required version: 1', rc)

      call config%set_global_communicator(comm)
      call this%build_locks(config, extra=extra, rc=status); _VERIFY(status, '', rc)
      call this%build_filters(config, extra=extra, rc=status); _VERIFY(status, '', rc)
      call this%build_formatters(config, extra=extra, rc=status); _VERIFY(status, '', rc)
      call this%build_handlers(config, extra=extra, rc=status); _VERIFY(status, '', rc)

      _RETURN(_SUCCESS,rc)
      _UNUSED_DUMMY(unusable)
   end subroutine build

end module PFL_AbstractConfigBuilder
