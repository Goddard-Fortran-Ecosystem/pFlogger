#include "error_handling_macros.fh"
module PFL_yaFyaml_ConfigBuilder
   use yafyaml
   use gftl2_StringVector
   use gftl2_UnlimitedVector
   use PFL_AbstractConfigBuilder
   use PFL_AbstractConfigBuilder, only: SECTION_FORMATTERS, SECTION_FILTERS, SECTION_HANDLERS, &
                                       SECTION_LOCKS, SECTION_LOGGERS, SECTION_ROOT
   use PFL_LoggingConfig
   use PFL_Logger
   use PFL_Exception, only: throw
   use PFL_SeverityLevels, only: name_to_level
   use PFL_StringAbstractLoggerPolyMap, only: LoggerMap
   use PFL_StringFilterMap
   use PFL_StringLockMap
   use PFL_StreamHandler
   use PFL_StringHandlerMap
   use PFL_StringFormatterMap
   use PFL_AbstractLock
   use PFL_AbstractFilter
   use PFL_AbstractHandler
   use PFL_Formatter
   use PFL_Filterer
   use PFL_FileHandler

   use gFTL2_StringUnlimitedMap
   use PFL_Filter
   use PFL_StringUtilities, only: to_lower_case
   use Pfl_KeywordEnforcer
#ifdef _LOGGER_USE_MPI
      use MPI
#endif
   implicit none
   private

   ! Primary user interface
   public :: yaFyaml_ConfigBuilder

   ! Remaining procedures are public just for testing purposes.
   public :: check_schema_version

   public :: build_formatter
   public :: build_filter

   public :: build_streamhandler
   public :: build_handler

   public :: build_logger

   type, extends(AbstractConfigBuilder) :: yaFyaml_ConfigBuilder
      private
      class(YAML_Node), allocatable :: cfg
   contains
      procedure :: load_file
      procedure :: load_from_node
      procedure :: get_schema_version
      procedure :: build_locks
      procedure :: build_filters
      procedure :: build_formatters
      procedure :: build_handlers
      procedure :: build_loggers_from_cfg
      procedure :: build_root_logger_from_cfg
      procedure :: build_logger
   end type yaFyaml_ConfigBuilder

   interface yaFyaml_ConfigBuilder
      module procedure new_yaFyaml_ConfigBuilder
   end interface yaFyaml_ConfigBuilder


contains

   subroutine load_file(this, filename, rc)
      class(yaFyaml_ConfigBuilder), intent(inout) :: this
      character(len=*), intent(in) :: filename
      integer, optional, intent(out) :: rc

      integer :: status

      call load(this%cfg, filename, rc=status)
      _VERIFY(status, '', rc)

      _RETURN(_SUCCESS, rc)
   end subroutine load_file

   subroutine load_from_node(this, cfg, rc)
      class(yaFyaml_ConfigBuilder), intent(inout) :: this
      class(YAML_Node), intent(in) :: cfg
      integer, optional, intent(out) :: rc

      this%cfg = cfg

      _RETURN(_SUCCESS, rc)
   end subroutine load_from_node

   function new_yaFyaml_ConfigBuilder(cfg) result(builder)
      type(yaFyaml_ConfigBuilder) :: builder
      class(YAML_Node), optional, intent(in) :: cfg

      if (present(cfg)) then
         builder%cfg = cfg
      end if
   end function new_yaFyaml_ConfigBuilder

   function get_schema_version(this, rc) result(version)
      class(yaFyaml_ConfigBuilder), intent(in) :: this
      integer, optional, intent(out) :: rc
      integer :: version

      integer :: status

      if (this%cfg%has('schema_version')) then
         call this%cfg%get(version, 'schema_version', rc=status)
         _VERIFY(status, '', rc)
      else
         version = -1 ! Invalid
         _ASSERT(.false., 'Must specify a schema_version in configuration.', rc)
      end if

      _RETURN(_SUCCESS, rc)
   end function get_schema_version

   ! Expects a mapping of formatter name to formatter mapping
   subroutine build_formatters(this, config, unusable, extra, rc)
      class(yaFyaml_ConfigBuilder), intent(in) :: this
      type(LoggingConfig), intent(inout) :: config
      class (KeywordEnforcer), optional, intent(in) :: unusable
      type (StringUnlimitedMap), optional, intent(in) :: extra
      integer, optional, intent(out) :: rc

      class(NodeIterator), allocatable :: iter
      class (Formatter), allocatable :: f
      class(YAML_Node), pointer :: formatters_cfg
      character(:), allocatable :: formatter_name
      type(FormatterMap), pointer :: formatters

      integer :: status

      if (.not. this%cfg%has(SECTION_FORMATTERS)) then
         _RETURN(_SUCCESS,rc)
      end if

      formatters_cfg => this%cfg%at(SECTION_FORMATTERS, _RC)
      _ASSERT(formatters_cfg%is_mapping(), "PFL::Config::build_formatters() - input cfg not a mapping", rc)

      formatters => config%get_formatters()
      associate (b => formatters_cfg%begin(), e => formatters_cfg%end())
        iter = b
        do while (iter /= e)
           formatter_name = to_string(iter%first(), _RC)

           call build_formatter(f, iter%second(), extra=extra, global_communicator=config%get_global_communicator(), _RC)

           call formatters%insert(formatter_name, f)
           deallocate(f)
           call iter%next()
        end do
      end associate

      _RETURN(_SUCCESS,rc)
      _UNUSED_DUMMY(unusable)
   end subroutine build_formatters


   ! Expects a mapping
   subroutine build_formatter(fmtr, cfg, unusable, extra, global_communicator, rc)
      class (Formatter), allocatable, intent(out) :: fmtr
      class(YAML_Node), intent(in) :: cfg
      class (KeywordEnforcer), optional, intent(in) :: unusable
      type (StringUnlimitedMap), optional, intent(in) :: extra
      integer, optional, intent(in) :: global_communicator
      integer, optional, intent(out) :: rc

      character(len=:), allocatable :: class_name
      type (StringUnlimitedMap) :: extra_
      integer :: status

      _ASSERT(cfg%is_mapping(), "PFL::Config::build_formatter() - input cfg not a mapping", rc)

      if (cfg%has('class')) then
         call cfg%get(class_name, 'class', _RC)
      else
         class_name = 'Formatter'
      end if
      select case (class_name)
      case ('Formatter')
         call build_basic_formatter(fmtr, cfg, _RC)
#ifdef _LOGGER_USE_MPI
      case ('MpiFormatter')
         if (present(extra)) then
            extra_ = extra
         end if
         if (present(global_communicator)) then
            call extra_%insert('_GLOBAL_COMMUNICATOR', global_communicator)
         end if
         call build_mpi_formatter(fmtr, cfg, extra=extra_, _RC)
#endif
      end select
      _RETURN(_SUCCESS,rc)
      _UNUSED_DUMMY(unusable)
   end subroutine build_formatter


   subroutine build_basic_formatter(fmtr, cfg, rc)
      class (Formatter), allocatable, intent(out) :: fmtr
      class(YAML_Node), intent(in) :: cfg
      integer, optional, intent(out) :: rc

      character(len=:), allocatable :: fmt
      character(len=:), allocatable :: datefmt
      integer :: status

      if (cfg%has('format')) then
         call cfg%get(fmt,'format', _RC)

         if (cfg%has('datefmt')) then
            call cfg%get(datefmt, 'datefmt', _RC)
            allocate(fmtr,source=Formatter(fmt, datefmt=datefmt))
         else
            allocate(fmtr,source=Formatter(fmt))
         end if
      else
         allocate(fmtr, source=Formatter())
      end if

      _RETURN(_SUCCESS,rc)
   end subroutine build_basic_formatter

#ifdef _LOGGER_USE_MPI
   subroutine build_mpi_formatter(fmtr, cfg, unusable, extra, rc)
      use PFL_MpiCommConfig
      class (Formatter), allocatable, intent(out) :: fmtr
      class(YAML_Node), intent(in) :: cfg
      class (KeywordEnforcer), optional, intent(in) :: unusable
      type (StringUnlimitedMap), optional, intent(in) :: extra
      integer, optional, intent(out) :: rc

      character(len=:), allocatable :: fmt
      character(len=:), allocatable :: datefmt
      character(len=:), allocatable :: fileName
      integer :: iostat
      integer :: comm
      integer, allocatable :: comms(:)
      integer :: i, j, n

      type (StringUnlimitedMap) :: commMap
      class(YAML_Node), pointer :: subcfg
      type (StringVector) :: communicator_name_list
      character(len=:), allocatable :: communicator_name, name
      integer :: status
      class(*), pointer :: p
      type (StringUnlimitedMap) :: extra_

      _UNUSED_DUMMY(unusable)

      if (cfg%has('comms')) then
         subcfg => cfg%at('comms', _RC)
         _ASSERT(subcfg%is_sequence(), "PFL::Config::build_mpi_formatter() - 'comms' subcfg not a sequence.", rc)

         allocate(comms(0))
         n = subcfg%size()
         if (present(extra)) then
            extra_ = extra
         end if

         do i = 1, n
            call subcfg%get(name, i, _RC)

            select case (name)
            case ('MPI_COMM_WORLD','COMM_LOGGER')
               if (extra_%count('_GLOBAL_COMMUNICATOR') > 0) then
                  p => extra_%at('_GLOBAL_COMMUNICATOR')
                  select type (p)
                  type is (integer)
                     comms = [comms, p]
                  end select
               end if
            case default
               if (extra_%count(name) == 1) then
                  p => extra_%at(name)
                  select type (p)
                  type is (integer)
                     comms = [comms, p]
                  end select
               else
                  _ASSERT(.false., "PFL::Config::build_mpi_formatter() - unknown communicator '"//name//"'.", rc)
               end if
            end select
         end do

         block
           character(len=:), allocatable :: rank_prefix
           character(len=:), allocatable :: size_prefix

           rank_prefix = 'mpi_rank'
           if (cfg%has('rank_prefix')) then
              call cfg%get(rank_prefix, 'rank_prefix', _RC)
           end if

           size_prefix = 'mpi_size'
           if (cfg%has('size_prefix')) then
              call cfg%get(size_prefix, 'size_prefix', _RC)
           end if

           call init_MpiCommConfig(commMap, comm, rank_keyword=rank_prefix, size_keyword=size_prefix)
         end block

      else

         comm = get_communicator('COMM_LOGGER', extra=extra, _RC)
         block
           character(len=:), allocatable :: rank_prefix
           character(len=:), allocatable :: size_prefix
           rank_prefix = 'mpi_rank'
           if (cfg%has('rank_prefix')) then
              call cfg%get(rank_prefix, 'rank_prefix', _RC)
           end if

           size_prefix = 'mpi_size'
           if (cfg%has('size_prefix')) then
              call cfg%get(size_prefix, 'size_prefix', _RC)
           end if
           call init_MpiCommConfig(commMap, comm, rank_keyword=rank_prefix, size_keyword=size_prefix)
         end block
      end if

      if (cfg%has('format')) then
         call cfg%get(fmt, 'format', _RC)
         ! strip beginning and trailing quotes
         block
           character(len=:), allocatable :: tmp
           tmp = trim(adjustl(fmt))
         end block

         if (cfg%has('datefmt')) then
            call cfg%get(datefmt, 'datefmt', _RC)

            datefmt = trim(adjustl(datefmt))
            ! drop enclosig quotes
            datefmt = datefmt(2:len(datefmt)-1)
            allocate(fmtr,source=Formatter(fmt, datefmt=datefmt, extra=commMap))
         else
            allocate(fmtr,source=Formatter(fmt, extra=commMap))
         end if
      else
         allocate(fmtr, source=Formatter(extra=commMap))
      end if

      _RETURN(_SUCCESS,rc)
   end subroutine build_mpi_formatter
#endif

   subroutine build_locks(this, config, unusable, extra, rc)
      use PFL_AbstractLock
#ifdef _LOGGER_USE_MPI
      use PFL_MpiLock
#endif
      class (yaFyaml_ConfigBuilder), intent(in) :: this
      type(LoggingConfig), intent(inout) :: config
      class (KeywordEnforcer), optional, intent(in) :: unusable
      type (StringUnlimitedMap), optional, intent(in) :: extra
      integer, optional, intent(out) :: rc

      class(NodeIterator), allocatable :: iter
      class(YAML_Node), pointer :: locks_cfg, subcfg
      character(:), allocatable :: lock_name
      class (AbstractLock), allocatable :: lock
      type(LockMap), pointer :: locks
      integer :: status

      if (.not. this%cfg%has(SECTION_LOCKS)) then
         _RETURN(_SUCCESS,rc)
      end if

      locks_cfg => this%cfg%at(SECTION_LOCKS, _RC)
      _ASSERT(locks_cfg%is_mapping(), "PFL::Config::build_locks() - input cfg not a mapping", rc)

      locks => config%get_locks()
      associate (b => locks_cfg%begin(), e => locks_cfg%end())
        iter = b
        do while (iter /= e)

           lock_name = to_string(iter%first(), _RC)
           subcfg => iter%second()
           !lock = build_lock(subcfg, extra=extra, _RC)
           allocate(lock, source=build_lock(subcfg, extra=extra, rc=status))
           _VERIFY(status,'',rc)
           call locks%insert(lock_name, lock)
           call iter%next()
        end do
      end associate

      _RETURN(_SUCCESS,rc)
      _UNUSED_DUMMY(unusable)

   contains

      function build_lock(cfg, unusable, extra, rc) result(lock)
         class (AbstractLock), allocatable :: lock
         class(YAML_Node), intent(in) :: cfg
         class (KeywordEnforcer), optional, intent(in) :: unusable
         type (StringUnlimitedMap), optional, intent(in) :: extra
         integer, optional, intent(out) :: rc

         character (len=:), allocatable :: class_name, comm_name
         integer :: comm
         integer :: status

         if (cfg%has('class')) then
            call cfg%get(class_name, 'class', _RC)

            select case (class_name)
#ifdef _LOGGER_USE_MPI
            case ('MpiLock')
               comm_name = 'COMM_LOGGER'
               if (cfg%has(comm_name)) then
                  call cfg%get(comm_name, 'comm', _RC)
               end if
               comm = get_communicator(comm_name, extra=extra)
               allocate(lock, source=MpiLock(comm))
#endif
            case default
               _ASSERT(.false., 'PFL::Config::build_lock() - unsupported lock class.', rc)
            end select
         else
            _ASSERT(.false., 'PFL::Config::build_lock() - unsupported class of lock.', rc)
         end if

         _RETURN(_SUCCESS,rc)
         _UNUSED_DUMMY(unusable)
      end function build_lock

   end subroutine build_locks


   subroutine build_filters(this, config, unusable, extra, rc)
      class (yaFyaml_ConfigBuilder), intent(in) :: this
      type(LoggingConfig), intent(inout) :: config
      class (KeywordEnforcer), optional, intent(in) :: unusable
      type (StringUnlimitedMap), optional, intent(in) :: extra
      integer, optional, intent(out) :: rc

      class(NodeIterator), allocatable :: iter
      class(YAML_Node), pointer :: filters_cfg, subcfg
      character(:), allocatable :: filter_name
      class(AbstractFilter), allocatable :: filter
      type(FilterMap), pointer :: filters
      integer :: status

      if (.not. this%cfg%has(SECTION_FILTERS)) then
         _RETURN(_SUCCESS,rc)
      end if

      filters_cfg => this%cfg%at(SECTION_FILTERS, _RC)

      filters => config%get_filters()
      associate (b => filters_cfg%begin(), e => filters_cfg%end())
        iter = b

        do while (iter /= e)

           filter_name = to_string(iter%first(), _RC)

           subcfg => iter%second()
           ! Allocate filter from build function
           allocate(filter, source=build_filter(subcfg, extra=extra, rc=status))
           _VERIFY(status,'',rc)
           call filters%insert(filter_name, filter)
           deallocate(filter)
           call iter%next()
        end do
      end associate

      _RETURN(_SUCCESS,rc)
      _UNUSED_DUMMY(unusable)


   end subroutine build_filters


   function build_filter(cfg, unusable, extra, rc) result(f)
      class (AbstractFilter), allocatable :: f
      class(YAML_Node), intent(in) :: cfg
      class (KeywordEnforcer), optional, intent(in) :: unusable
      type (StringUnlimitedMap), optional, intent(in) :: extra
      integer, optional, intent(out) :: rc

      character(len=:), allocatable :: class_name
      integer :: status

      _ASSERT(cfg%is_mapping(), "PFL::Config::build_formatter() - input cfg not a mapping", rc)

      if (cfg%has('class')) then
         call cfg%get(class_name, 'class', _RC)
      else
         class_name = 'filter'
      end if

      select case (to_lower_case(class_name))
      case ('filter')
         allocate(f, source=build_basic_filter(cfg, rc=status))
         _VERIFY(status, '', rc)

      case ('levelfilter')
         allocate(f, source=build_LevelFilter(cfg, rc=status))
         _VERIFY(status, '', rc)

#ifdef _LOGGER_USE_MPI
      case ('mpifilter')
         allocate(f, source=build_MpiFilter(cfg, extra=extra, rc=status))
         _VERIFY(status, '', rc)

#endif
      case default
         _ASSERT(.false., 'PFL::Config::build_filter() - unknown filter type: '//class_name//'.', rc)
      end select

      _RETURN(_SUCCESS,rc)
      _UNUSED_DUMMY(unusable)
    end function build_filter

    function build_basic_filter(cfg, rc) result(f)
      type (Filter) :: f
      class(YAML_Node), intent(in) :: cfg
      integer, optional, intent(out) :: rc

      character(len=:), allocatable :: name
      integer :: status

      _ASSERT(cfg%is_mapping(), "PFL::Config::build_formatter() - input cfg not a mapping", rc)
      _ASSERT(cfg%has('name'), 'PFL::Config::build_basic_filter() - missing "name".', rc)

      call cfg%get(name, 'name', _RC)
      f = Filter(name)

      _RETURN(_SUCCESS,rc)
    end function build_basic_filter

    function build_LevelFilter(cfg, rc) result(f)
       use PFL_LevelFilter
       type (LevelFilter) :: f
       class(YAML_Node), intent(in) :: cfg
       integer, optional, intent(out) :: rc

       integer :: min_level, max_level
       integer :: status

       min_level = get_level('min_level', _RC)
       max_level = get_level('max_level', _RC)

       f = LevelFilter(min_level, max_level)

       _RETURN(_SUCCESS,rc)
    contains

       integer function get_level(key, rc) result(level)
          character(len=*), intent(in) :: key
          integer, optional, intent(out) :: rc

          character(len=:), allocatable :: level_name
          integer :: iostat
          integer :: status

          level = -1
          _ASSERT(cfg%has('max_level'), 'PFL::Config::build_LevelFilter() - missing "max_level".', rc)

          call cfg%get(level_name, 'max_level', _RC)

          read(level_name,*,iostat=iostat) level
          if (iostat /= 0) then
             level = name_to_level(level_name, _RC)
          end if

          _RETURN(_SUCCESS,rc)
        end function get_level

    end function build_LevelFilter

#ifdef _LOGGER_USE_MPI
    function build_MpiFilter(cfg, unusable, extra, rc) result(f)
       use PFL_MpiFilter
       type (MpiFilter) :: f
       class(YAML_Node), intent(in) :: cfg
       class (KeywordEnforcer), optional, intent(in) :: unusable
       type (StringUnlimitedMap), optional, intent(in) :: extra
       integer, optional, intent(out) :: rc

       character(len=:), allocatable :: comm_name
       integer :: comm
       integer :: rank, root, ierror
       integer :: status

       if (cfg%has('comm')) then
          call cfg%get(comm_name, 'comm', _RC)
       else
          comm_name = 'COMM_LOGGER'
       end if

       comm = get_communicator(comm_name, extra=extra)
       root = 0
       if (cfg%has(SECTION_ROOT)) then
          call cfg%get(root, SECTION_ROOT, _RC)
       end if

       f = MpiFilter(comm, root)

       _RETURN(_SUCCESS,rc)
       _UNUSED_DUMMY(unusable)
    end function build_MpiFilter
#endif


   subroutine build_handlers(this, config, unusable, extra, rc)
      class (yaFyaml_ConfigBuilder), intent(in) :: this
      type(LoggingConfig), intent(inout) :: config
      class (KeywordEnforcer), optional, intent(in) :: unusable
      type (StringUnlimitedMap), optional, intent(in) :: extra
      integer, optional, intent(out) :: rc

      class(NodeIterator), allocatable :: iter
      class(YAML_Node), pointer :: handlers_cfg, subcfg
      character(:), allocatable :: handler_name
      class (AbstractHandler), allocatable :: h
      type(HandlerMap), pointer :: handlers
      integer :: status

      if (.not. this%cfg%has(SECTION_HANDLERS)) then
         _RETURN(_SUCCESS,rc)
      end if

      handlers_cfg => this%cfg%at(SECTION_HANDLERS, _RC)

      handlers => config%get_handlers()
      iter = handlers_cfg%begin()
      do while (iter /= handlers_cfg%end())

         handler_name = to_string(iter%first(), _RC)

         subcfg => iter%second()
         call build_handler(h,subcfg, config, extra=extra, _RC)

         call handlers%insert(handler_name, h)
         deallocate(h)
         call iter%next()
      end do

      _RETURN(_SUCCESS,rc)
   end subroutine build_handlers

   subroutine build_loggers_from_cfg(this, loggers, config, unusable, extra, rc)
      use PFL_AbstractLogger, only: AbstractLogger
      class(yaFyaml_ConfigBuilder), intent(in) :: this
      type(LoggerMap), target, intent(inout) :: loggers
      type(LoggingConfig), target, intent(in) :: config
      class(KeywordEnforcer), optional, intent(in) :: unusable
      type(StringUnlimitedMap), optional, intent(in) :: extra
      integer, optional, intent(out) :: rc

      class(YAML_Node), pointer :: lgrs_cfg, lgr_cfg
      class(NodeIterator), allocatable :: iter
      character(len=:), allocatable :: name
      class(AbstractLogger), pointer :: lgr
      integer :: status

      if (this%cfg%has(SECTION_LOGGERS)) then
         lgrs_cfg => this%cfg%at(SECTION_LOGGERS, rc=status)
         _VERIFY(status,'',rc)
         _ASSERT(lgrs_cfg%is_mapping(), "yaFyaml_ConfigBuilder::build_loggers_from_cfg() - expected mapping for '" // SECTION_LOGGERS // "'.", rc)

         ! Loop over contained loggers
         associate (b => lgrs_cfg%begin(), e => lgrs_cfg%end())
           iter = b
           do while (iter /= e)
              name = to_string(iter%first(), rc=status)
              _VERIFY(status,'',rc)

              lgr => loggers%at(name)
              if (.not. associated(lgr)) then
                 ! Create logger if it doesn't exist
                 allocate(lgr, source=newLogger(name))
                 call loggers%set(name, lgr)
                 lgr => loggers%at(name)
              end if
              lgr_cfg => lgrs_cfg%at(name)
              
              select type (lgr)
              class is (Logger)
                 call this%build_logger(lgr, lgr_cfg, config, extra=extra, rc=status)
                 _VERIFY(status,'',rc)
              end select
              
              call iter%next()
           end do
         end associate
      end if

      _RETURN(_SUCCESS,rc)
      _UNUSED_DUMMY(unusable)
   end subroutine build_loggers_from_cfg

   subroutine build_root_logger_from_cfg(this, root_logger, config, unusable, extra, rc)
      class(yaFyaml_ConfigBuilder), intent(in) :: this
      class(Logger), target, intent(inout) :: root_logger
      type(LoggingConfig), target, intent(in) :: config
      class(KeywordEnforcer), optional, intent(in) :: unusable
      type(StringUnlimitedMap), optional, intent(in) :: extra
      integer, optional, intent(out) :: rc

      class(YAML_Node), pointer :: root_cfg
      integer :: status

      if (this%cfg%has(SECTION_ROOT)) then
         root_cfg => this%cfg%at(SECTION_ROOT, rc=status)
         _VERIFY(status,'',rc)
         call this%build_logger(root_logger, root_cfg, config, extra=extra, rc=status)
         _VERIFY(status,'',rc)
      end if

      _RETURN(_SUCCESS,rc)
      _UNUSED_DUMMY(unusable)
   end subroutine build_root_logger_from_cfg

   subroutine build_handler(h, cfg, config, unusable, extra, rc)
      class (AbstractHandler), allocatable, intent(out) :: h
      class(YAML_Node), intent(inout) :: cfg
      type (LoggingConfig), intent(inout) :: config
      class (KeywordEnforcer), optional, intent(in) :: unusable
      type (StringUnlimitedMap), optional, intent(in) :: extra
      integer, optional, intent(out) :: rc

      integer :: status
      type (FormatterMap), pointer :: formatters
      type (FilterMap), pointer :: filters
      type (LockMap), pointer :: locks

      formatters => config%get_formatters()
      filters => config%get_filters()
      locks => config%get_locks()

      call allocate_concrete_handler(h, cfg, _RC)
      call set_handler_level(h, cfg, _RC)
      call set_handler_formatter(h, cfg, formatters, _RC)
      call set_handler_filters(h, cfg, filters, _RC)
      call set_handler_lock(h, cfg, locks, _RC)

      _RETURN(_SUCCESS,rc)
      _UNUSED_DUMMY(unusable)

   contains

      subroutine allocate_concrete_handler(h, cfg, rc)
         class (AbstractHandler), allocatable, intent(out) :: h
         class(YAML_Node), intent(inout) :: cfg
         integer, optional, intent(out) :: rc

         character(len=:), allocatable :: class_name
         type (Filehandler) :: fh
         integer :: status

         class_name = 'unknown'
         if (cfg%has('class')) then
            call cfg%get(class_name, 'class', _RC)
         end if

         select case (to_lower_case(class_name))
         case ('streamhandler')
            allocate(h, source=build_streamhandler(cfg))
         case ('filehandler')
              call build_filehandler(fh, cfg)
              allocate(h, source=fh)
#ifdef _LOGGER_USE_MPI
         case ('mpifilehandler')
              call build_mpifilehandler(fh, cfg)
              allocate(h, source=fh)
#endif
         case default
            _ASSERT(.false., "PFL::Config::build_handler() - unsupported class: '" // class_name //"'.", rc)
         end select
         _RETURN(_SUCCESS,rc)
      end subroutine allocate_concrete_handler

      subroutine set_handler_level(h, cfg, rc)
         class (AbstractHandler), intent(inout) :: h
         class(YAML_Node), intent(in) :: cfg
         integer, optional, intent(out) :: rc

         character(len=:), allocatable :: level_name
         integer :: level
         integer :: iostat
         integer :: status

         level_name = 'NOTSET'
         if (cfg%has('level')) then
            call cfg%get(level_name, 'level', _RC)
         end if

         ! Try as integer
         read(level_name,*, iostat=iostat) level

         ! If that failed - interpret as a name from SeverityLevels.
         if (iostat /= 0) then
            level = name_to_level(level_name, _RC)
         end if

         call h%set_level(level)

         _RETURN(_SUCCESS,rc)
      end subroutine set_handler_level


      subroutine set_handler_formatter(h, cfg, formatters, rc)
         use PFL_Formatter
         class (AbstractHandler), intent(inout) :: h
         class(YAML_Node), intent(in) :: cfg
         type (FormatterMap), target, intent(inout) :: formatters
         integer, optional, intent(out) :: rc

         character(len=:), allocatable :: formatter_name

         class (Formatter), pointer :: p_fmt
         integer :: status

         if (cfg%has('formatter')) then
            call cfg%get(formatter_name, 'formatter', _RC)

            if (formatters%count(formatter_name) == 1) then
               p_fmt => formatters%of(formatter_name)
               call h%set_formatter(formatters%of(formatter_name))
            else
               _ASSERT(.false., "PFL::Config::build_handler() - formatter '"//formatter_name//"' not found.", rc)
            end if
         end if

         _RETURN(_SUCCESS,rc)
         _UNUSED_DUMMY(unusable)
      end subroutine set_handler_formatter


      subroutine set_handler_filters(h, cfg, filters, rc)
         class (AbstractHandler), intent(inout) :: h
         class(YAML_Node), intent(inout) :: cfg
         type (FilterMap), intent(inout) :: filters
         integer, optional, intent(out) :: rc

         class (AbstractFilter), pointer :: f
         character(len=:), allocatable :: name
         integer :: status
         class(YAML_Node), pointer :: subcfg
         type(UnlimitedVector) :: empty
         integer :: i

         if (cfg%has(SECTION_FILTERS)) then
            subcfg => cfg%of(SECTION_FILTERS)
            _ASSERT(subcfg%is_sequence(), 'PFL::Config::handler_filters() - "filters" not a squence', rc)

            do i = 1, subcfg%size()
               call subcfg%get(name, i)
               if (filters%count(name) > 0) then
                  f => filters%of(name)
                  call h%add_filter(f)
               else
                  _ASSERT(.false., "PFL::Config::build_handler() - unknown filter'"//name//"'.", rc)
               end if
            end do
         end if

         _RETURN(_SUCCESS,rc)
      end subroutine set_handler_filters

      subroutine set_handler_lock(h, cfg, locks, rc)
         class (AbstractHandler), intent(inout) :: h
         class(YAML_Node), intent(in) :: cfg
         type (LockMap), intent(inout) :: locks
         integer, optional, intent(out) :: rc

         character(len=:), allocatable :: lock_name
         class (AbstractLock), pointer :: lock
         integer :: status

         if (cfg%has('lock')) then
            call cfg%get(lock_name, 'lock', _RC)

            if (locks%count(lock_name) == 1) then
               lock => locks%of(lock_name)
               select type (h)
               class is (FileHandler)
                  call h%set_lock(lock)
               class default
                  _ASSERT(.false., 'PFL::Config::set_handler_lock() - unsupported Lock subclass.', rc)
               end select
            else
               _ASSERT(.false., "PFL::Config::set_handler_lock() - unknown lock: '"//lock_name//"'.'", rc)
            end if
         end if
         _RETURN(_SUCCESS,rc)
      end subroutine set_handler_lock


   end subroutine build_handler

   function build_streamhandler(cfg, rc) result(h)
      use iso_fortran_env, only: OUTPUT_UNIT, ERROR_UNIT
      type (StreamHandler) :: h
      class(YAML_Node), intent(inout) :: cfg
      integer, optional, intent(out) :: rc

      character(len=:), allocatable :: unit_name
      integer :: unit
      integer :: status
      integer :: iostat
      class(YAML_Node), pointer :: subcfg


      if (cfg%has('unit')) then
         subcfg => cfg%of('unit')
         if (subcfg%is_int()) then
            call subcfg%get(unit, _RC)
         elseif (subcfg%is_string()) then
            call subcfg%get(unit_name, _RC)
            read(unit_name,*,iostat=iostat) unit
            if (iostat /= 0) then
               ! if failed, maybe it is a defined name?
               select case (to_lower_case(unit_name))
               case ('output_unit','*')
                  unit = OUTPUT_UNIT
               case ('error_unit')
                  unit = ERROR_UNIT
               case default
                  _ASSERT(.false., "PFL::Config::build_streamhandler() - unknown value for unit '"//unit_name//"'.", rc)
               end select
            end if
         else !
            _ASSERT(.false., "PFL::Config::build_streamhandler() - illegal type for unit", rc)
         end if
      else
         unit = ERROR_UNIT
      end if
      h = StreamHandler(unit)
      _RETURN(_SUCCESS,rc)
   end function build_streamhandler


   subroutine build_filehandler(h, cfg, rc)
      type (FileHandler), intent(out) :: h
      class(YAML_Node), intent(in) :: cfg
      integer, optional, intent(out) :: rc

      character(len=:), allocatable :: filename
      integer :: unit
      integer :: iostat
      integer :: status
      logical :: delay

      _ASSERT(cfg%has('filename'), "PFL::Config::build_FileHandler() - must provide file name.", rc)

      call cfg%get(filename, 'filename', _RC)

      delay = .false.
      if (cfg%has(delay)) then
         call cfg%get(delay, 'delay', _RC)
      end if

      h = FileHandler(filename, delay=delay)

      _RETURN(_SUCCESS,rc)
   end subroutine build_filehandler

#ifdef _LOGGER_USE_MPI
   subroutine build_mpifilehandler(h, cfg, unusable, extra, rc)
      use PFL_MpiCommConfig

      type (FileHandler), intent(out) :: h
      class(YAML_Node), intent(in) :: cfg
      class (KeywordEnforcer), optional, intent(in) :: unusable
      type (StringUnlimitedMap), optional, intent(in) :: extra
      integer, optional, intent(out) :: rc

      character(len=:), allocatable :: fileName
      class(YAML_Node), pointer :: subcfg
      integer :: unit
      integer :: iostat
      integer :: comm
      integer, allocatable :: comms(:)
      integer :: i
      logical :: delay
      integer :: status

      type (StringUnlimitedMap) :: commMap
      character(len=:), allocatable :: communicator_name
      integer :: n

      _ASSERT(cfg%has('filename'), "PFL::Config::build_MpiFileHandler() - must provide file name.", rc)

      call cfg%get(filename, 'filename', _RC)

      if (cfg%has('comms')) then
         subcfg => cfg%at('comms', _RC)
         _ASSERT(subcfg%is_sequence(), "PFL::Config::build_mpi_formatter() - 'comms' subcfg not a sequence.", rc)


         allocate(comms(n))

         do i = 1, n
            call subcfg%get(communicator_name, i, _RC)
            comms(i) = get_communicator(communicator_name)
         end do

         block
           character(len=:), allocatable :: rank_prefix
           character(len=:), allocatable :: size_prefix

           rank_prefix = 'mpi_rank'
           if (cfg%has('rank_prefix')) then
              call cfg%get(rank_prefix, 'rank_prefix', _RC)
           end if

           size_prefix = 'mpi_size'
           if (cfg%has('size_prefix')) then
              call cfg%get(size_prefix, 'size_prefix', _RC)
           end if

           call init_MpiCommConfig(commMap, comm, rank_keyword=rank_prefix, size_keyword=size_prefix)
         end block

      else
         communicator_name = 'COMM_LOGGER'
         if (cfg%has('comm')) then
            call cfg%get(communicator_name, 'comm', _RC)
         end if
         comm = get_communicator(communicator_name, extra=extra, _RC)

         block
           character(len=:), allocatable :: rank_prefix
           character(len=:), allocatable :: size_prefix

           rank_prefix = 'mpi_rank'
           if (cfg%has('rank_prefix')) then
              call cfg%get(rank_prefix, 'rank_prefix', _RC)
           end if

           size_prefix = 'mpi_size'
           if (cfg%has('size_prefix')) then
              call cfg%get(size_prefix, 'size_prefix', _RC)
           end if

           call init_MpiCommConfig(commMap, comm, rank_keyword=rank_prefix, size_keyword=size_prefix)
         end block
      end if

      block
        use PFL_FormatString
        filename = formatString(filename, commMap)
      end block

      ! Note we have a different default for 'delay' for
      ! the MPI case.  This way we avoid creating lots of
      ! empty files in the usual case.
      delay = .true.
      if (cfg%has('delay')) then
         call cfg%get(delay, 'delay', _RC)
      end if

      h = FileHandler(fileName, delay=delay)

      _RETURN(_SUCCESS,rc)
   end subroutine build_mpifilehandler
#endif

   subroutine build_logger(this, lgr, cfg, config, unusable, extra, rc)
      use PFL_StringUtilities, only: to_lower_case
      class (yaFyaml_ConfigBuilder), intent(in) :: this
      class (Logger), intent(inout) :: lgr
      class(YAML_Node), intent(inout) :: cfg
      type (LoggingConfig), intent(in) :: config
      class (KeywordEnforcer), optional, intent(in) :: unusable
      type (StringUnlimitedMap), optional, intent(in) :: extra
      integer, optional, intent(out) :: rc

      integer :: status
      type (FilterMap), pointer :: filters
      type (HandlerMap), pointer :: handlers

      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(unusable)

      filters => config%get_filters()
      handlers => config%get_handlers()

      call set_logger_level(lgr, cfg, _RC)
      call set_logger_propagate(lgr, cfg, _RC)
      call set_logger_filters(lgr, cfg, filters, _RC)
      call set_logger_handlers(lgr, cfg, handlers, _RC)

      _RETURN(_SUCCESS,rc)

   contains

      subroutine set_logger_level(lgr, cfg, rc)
         class (Logger), intent(inout) :: lgr
         class(YAML_Node), intent(in) :: cfg
         integer, optional, intent(out) :: rc

         character(len=:), allocatable :: level_name
         character(len=:), allocatable :: root_level_name
         integer :: level
         integer :: iostat
#ifdef _LOGGER_USE_MPI
         integer :: root, rank, ierror, comm
         character(len=:), allocatable :: communicator_name
#endif
         integer :: status

         level_name = 'NOTSET'
         if (cfg%has('level')) then
            call cfg%get(level_name, 'level', _RC)
         end if

#ifdef _LOGGER_USE_MPI
         communicator_name = 'COMM_LOGGER'
         if (cfg%has('comm')) then
            call cfg%get(communicator_name, 'comm', _RC)
         end if
         comm = get_communicator(communicator_name, extra=extra, _RC)

         root = 0
         if (cfg%has(SECTION_ROOT)) then
            call cfg%get(root, SECTION_ROOT, _RC)
         end if

         call MPI_Comm_rank(comm, rank, ierror)
         if (rank == root) then
            if (cfg%has('root_level')) then
               call cfg%get(root_level_name, 'root_level', _RC)
               level_name = root_level_name
            end if
         else
            ! same as on other PEs
         end if
#endif
         ! Try as integer
         read(level_name,*, iostat=iostat) level

         if (iostat /= 0) then
            level = name_to_level(level_name, _RC)
         end if
         call lgr%set_level(level)

         _RETURN(_SUCCESS,rc)
      end subroutine set_logger_level


      subroutine set_logger_propagate(lgr, cfg, rc)
         class (Logger), intent(inout) :: lgr
         class(YAML_Node), intent(in) :: cfg
         integer, optional, intent(out) :: rc

         logical :: propagate
         integer :: level
         integer :: status

         if (cfg%has('propagate')) then
            call cfg%get(propagate, 'propagate', _RC)
            call lgr%set_propagate(propagate)
         end if

         _RETURN(_SUCCESS,rc)
      end subroutine set_logger_propagate


      subroutine set_logger_filters(lgr, cfg, filters, unusable, extra, rc)
         class (Logger), intent(inout) :: lgr
         class(YAML_Node), intent(inout) :: cfg
         type (FilterMap), intent(inout) :: filters
         class (KeywordEnforcer), optional, intent(in) :: unusable
         type (StringUnlimitedMap), optional, intent(in) :: extra
         integer, optional, intent(out) :: rc

         character(:), allocatable :: filter_name
         class(YAML_Node), pointer :: subcfg
         integer :: i
         integer :: status

         if (cfg%has(SECTION_FILTERS)) then
            subcfg => cfg%of(SECTION_FILTERS)
            _ASSERT(subcfg%is_sequence(), "PFL::Config::set_logger_filters() - expected sequence for SECTION_FILTERS key.", rc)

            do i = 1, subcfg%size()
               call subcfg%get(filter_name, i, _RC)

               if (filters%count(filter_name) > 0) then
                  call lgr%add_filter(filters%of(filter_name))
               else
                  _ASSERT(.false., "PFL::Config::set_logger_filters() - unknown filter'"//filter_name//"'.", rc)
               end if
            end do

         end if

         _RETURN(_SUCCESS,rc)
         _UNUSED_DUMMY(unusable)
      end subroutine set_logger_filters


      subroutine set_logger_handlers(lgr, cfg, handlers, unusable, extra, rc)
         class (Logger), intent(inout) :: lgr
         class(YAML_Node), intent(inout) :: cfg
         type (HandlerMap), intent(inout) :: handlers
         class (KeywordEnforcer), optional, intent(in) :: unusable
         type (StringUnlimitedMap), optional, intent(in) :: extra
         integer, optional, intent(out) :: rc

         character(len=:), allocatable :: handler_name
         class(YAML_Node), pointer :: subcfg
         integer :: i
         integer :: status

         if (cfg%has(SECTION_HANDLERS)) then
            subcfg => cfg%of(SECTION_HANDLERS)
            _ASSERT(cfg%has(SECTION_HANDLERS), "PFL::Config::set_logger_handlers() - expected sequence for SECTION_HANDLERS key.", rc)

            do i = 1, subcfg%size()
               call subcfg%get(handler_name, i, _RC)

               if (handlers%count(handler_name) > 0) then
                  call lgr%add_handler(handlers%of(handler_name))
               else
                  _ASSERT(.false., "PFL::Config::set_logger_handlers() - unknown handler'"//handler_name//"'.", rc)
               end if
            end do

         end if

         _RETURN(_SUCCESS,rc)
         _UNUSED_DUMMY(unusable)
      end subroutine set_logger_handlers


   end subroutine build_logger

#ifdef _LOGGER_USE_MPI
   integer function get_communicator(name, extra, rc) result(comm)
      character(len=*), intent(in) :: name
      type (StringUnlimitedMap), optional, intent(in) :: extra
      integer, optional, intent(out) :: rc

      character(len=:), allocatable :: name_
      class(*), pointer :: p
      integer :: status
      type(StringUnlimitedMap) :: extra_

      name_ = name
      if (name == 'MPI_COMM_WORLD') then
         name_ = 'COMM_LOGGER'
      end if

      if (present(extra)) then
         extra_ = extra
         if (extra_%count(name_) > 0) then
            p => extra_%of(name_)
            select type (p)
            type is (integer)
               comm = p
            end select
         elseif (name_ == 'MPI_COMM_WORLD' .or. name_ == 'COMM_LOGGER') then
            comm = MPI_COMM_WORLD
         else
            _ASSERT(.false., "PFL::Config::build_logger() - MPI communicator '" // name_ // "' not found.", rc)
         end if
      else
         comm = MPI_COMM_WORLD
      end if
      _RETURN(_SUCCESS,rc)
   end function get_communicator
#endif


   ! Including a version number is crucial for providing non-backward
   ! compatible updates in the future.
   subroutine check_schema_version(cfg, rc)
      class(YAML_Node), intent(in) :: cfg
      integer, optional, intent(out) :: rc

      integer :: version
      integer :: status

      if (cfg%has('schema_version')) then
         call cfg%get(version, 'schema_version', _RC)

         if (version /= 1) then
            _ASSERT(.false., 'PFL::Config::check_schema_version() - unsupported schema_version. Allowed values are [1].', rc)
         end if
      else
         _ASSERT(.false., 'PFL::Config::check_schema_version() - must specify a schema_version for Config.', rc)
      end if
      _RETURN(_SUCCESS,rc)
   end subroutine check_schema_version

end module PFL_yaFyaml_ConfigBuilder
