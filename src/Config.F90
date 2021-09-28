#include "error_handling_macros.fh"
module PFL_Config
   use yafyaml
   use gftl_StringVector
   use gftl_UnlimitedVector
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
   
   use gFTL_StringUnlimitedMap
   use PFL_Filter
   use PFL_StringUtilities, only: to_lower_case
   use Pfl_KeywordEnforcer
#ifdef _LOGGER_USE_MPI
      use MPI
#endif
   implicit none
   private

   ! Primary user interface
   public :: ConfigElements

   ! Remaining procedures are public just for testing purposes.
   public :: check_schema_version

   public :: build_formatter
   public :: build_filter

   public :: build_streamhandler
   public :: build_handler

   public :: build_logger

   type ConfigElements
      private
      integer :: global_communicator = -1 ! not used in serial
      type (FilterMap) :: filters
      type (FormatterMap) :: formatters
      type (HandlerMap) :: handlers
      type (LockMap) :: locks
   contains
      procedure :: build_locks
      procedure :: build_filters
      procedure :: build_formatters
      procedure :: build_handlers

      ! accessors
      procedure :: set_global_communicator
      procedure :: get_filters
      procedure :: get_formatters
      procedure :: get_handlers
   end type ConfigElements


contains

   ! Expects a mapping of formatter name to formatter mapping
   subroutine build_formatters(this, cfg, unusable, extra, rc)
      class(ConfigElements), intent(inout) :: this
      type(Configuration), intent(in) :: cfg
      class (KeywordEnforcer), optional, intent(in) :: unusable
      type (StringUnlimitedMap), optional, intent(in) :: extra
      integer, optional, intent(out) :: rc

      type(ConfigurationIterator) :: iter
      type (Configuration) :: subcfg
      class (Formatter), allocatable :: f
      character(:), allocatable :: formatter_name
      
      integer :: status

      _ASSERT(cfg%is_mapping(), "PFL::Config::build_formatters() - input cfg not a mapping", rc)

      associate (b => cfg%begin(), e => cfg%end())
        iter = b
        do while (iter /= e)
           call iter%get_value(subcfg, rc=status)
           _VERIFY(status, '', rc)

           call build_formatter(f, subcfg, extra=extra, global_communicator=this%global_communicator, rc=status)
           _VERIFY(status, '', rc)

           call iter%get_key(formatter_name, rc=status)
           _VERIFY(status, '', rc)
           call this%formatters%insert(formatter_name, f)
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
      type(Configuration), intent(in) :: cfg
      class (KeywordEnforcer), optional, intent(in) :: unusable
      type (StringUnlimitedMap), optional, intent(in) :: extra
      integer, optional, intent(in) :: global_communicator
      integer, optional, intent(out) :: rc

      character(len=:), allocatable :: class_name
      type (StringUnlimitedMap) :: extra_
      integer :: status

      _ASSERT(cfg%is_mapping(), "PFL::Config::build_formatter() - input cfg not a mapping", rc)

      if (cfg%has('class')) then
         call cfg%get(class_name, 'class', rc=status)
         _VERIFY(status,'',rc)
      else
         class_name = 'Formatter'
      end if
      select case (class_name)
      case ('Formatter')
         call build_basic_formatter(fmtr, cfg, rc=status)
         _VERIFY(status,'',rc)
#ifdef _LOGGER_USE_MPI         
      case ('MpiFormatter')
         call extra_%deepcopy(extra)
         if (present(global_communicator)) then
            call extra_%insert('_GLOBAL_COMMUNICATOR', global_communicator)
         end if
         call build_mpi_formatter(fmtr, cfg, extra=extra_, rc=status)
         _VERIFY(status,'',rc)
#endif
      end select
      _RETURN(_SUCCESS,rc)
      _UNUSED_DUMMY(unusable)
   end subroutine build_formatter


   subroutine build_basic_formatter(fmtr, cfg, rc)
      class (Formatter), allocatable, intent(out) :: fmtr
      type (Configuration), intent(in) :: cfg
      integer, optional, intent(out) :: rc

      character(len=:), allocatable :: fmt
      character(len=:), allocatable :: datefmt
      integer :: status

      if (cfg%has('format')) then
         call cfg%get(fmt,'format', rc=status)
         _VERIFY(status,'',rc)

         if (cfg%has('datefmt')) then
            call cfg%get(datefmt, 'datefmt', rc=status)
            _VERIFY(status,'',rc)
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
      type (Configuration), intent(in) :: cfg
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
      type (Configuration) :: subcfg
      type (StringVector) :: communicator_name_list
      character(len=:), allocatable :: communicator_name, name
      integer :: status
      class(*), pointer :: p

      _UNUSED_DUMMY(unusable)

      if (cfg%has('comms')) then
         subcfg = cfg%at('comms', rc=status)
         _VERIFY(status,'',rc)
         _ASSERT(subcfg%is_sequence(), "PFL::Config::build_mpi_formatter() - 'comms' subcfg not a sequence.", rc)

         allocate(comms(0))
         n = subcfg%size()

         do i = 1, n
            call subcfg%get(name, i, rc=status)
            _VERIFY(status,'',rc)

            select case (name)
            case ('MPI_COMM_WORLD','COMM_LOGGER')
               if (extra%count('_GLOBAL_COMMUNICATOR') > 0) then
                  p => extra%at('_GLOBAL_COMMUNICATOR')
                  select type (p)
                  type is (integer)
                     comms = [comms, p]
                  end select
               end if
            case default
               if (extra%count(name) == 1) then
                  p => extra%at(name)
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
              call cfg%get(rank_prefix, 'rank_prefix', rc=status)
              _VERIFY(status,'',rc)
           end if

           size_prefix = 'mpi_size'
           if (cfg%has('size_prefix')) then
              call cfg%get(size_prefix, 'size_prefix', rc=status)
              _VERIFY(status,'',rc)
           end if

           call init_MpiCommConfig(commMap, comm, rank_keyword=rank_prefix, size_keyword=size_prefix)
         end block

      else

         comm = get_communicator('COMM_LOGGER', extra=extra, rc=status)
         _VERIFY(status,'',rc)
         block
           character(len=:), allocatable :: rank_prefix
           character(len=:), allocatable :: size_prefix
           rank_prefix = 'mpi_rank'
           if (cfg%has('rank_prefix')) then
              call cfg%get(rank_prefix, 'rank_prefix', rc=status)
              _VERIFY(status,'',rc)
           end if

           size_prefix = 'mpi_size'
           if (cfg%has('size_prefix')) then
              call cfg%get(size_prefix, 'size_prefix', rc=status)
              _VERIFY(status,'',rc)
           end if
           call init_MpiCommConfig(commMap, comm, rank_keyword=rank_prefix, size_keyword=size_prefix)
         end block
      end if

      if (cfg%has('format')) then
         call cfg%get(fmt, 'format', rc=status)
         _VERIFY(status,'',rc)
         ! strip beginning and trailing quotes
         block
           character(len=:), allocatable :: tmp
           tmp = trim(adjustl(fmt))
         end block

         if (cfg%has('datefmt')) then
            call cfg%get(datefmt, 'datefmt', rc=status)
            _VERIFY(status,'',rc)

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
   
   subroutine build_locks(this, cfg, unusable, extra, rc)
      use PFL_AbstractLock
#ifdef _LOGGER_USE_MPI
      use PFL_MpiLock
#endif
      class (ConfigElements), intent(inout) :: this
      type (Configuration), intent(in) :: cfg
      class (KeywordEnforcer), optional, intent(in) :: unusable
      type (StringUnlimitedMap), optional, intent(in) :: extra
      integer, optional, intent(out) :: rc

      type(ConfigurationIterator) :: iter
      type(Configuration) :: subcfg
      character(:), allocatable :: lock_name
      integer :: status
      
      _ASSERT(cfg%is_mapping(), "PFL::Config::build_locks() - input cfg not a mapping", rc)

      associate (b => cfg%begin(), e => cfg%end())
        iter = b
        do while (iter /= e)

           call iter%get_key(lock_name, rc=status)
           _VERIFY(status,'',rc)
           call iter%get_value(subcfg, rc=status)
           _VERIFY(status,'',rc)
           call this%locks%insert(lock_name, build_lock(subcfg, extra=extra, rc=status))
           _VERIFY(status,'',rc)
           call iter%next()
        end do
      end associate

      _RETURN(_SUCCESS,rc)
      _UNUSED_DUMMY(unusable)

   contains

      function build_lock(cfg, unusable, extra, rc) result(lock)
         class (AbstractLock), allocatable :: lock
         type (Configuration), intent(in) :: cfg
         class (KeywordEnforcer), optional, intent(in) :: unusable
         type (StringUnlimitedMap), optional, intent(in) :: extra
         integer, optional, intent(out) :: rc

         character (len=:), allocatable :: class_name, comm_name
         integer :: comm
         integer :: status

         if (cfg%has('class')) then
            call cfg%get(class_name, 'class', rc=status)
            _VERIFY(status,'',rc)

            select case (class_name)
#ifdef _LOGGER_USE_MPI
            case ('MpiLock')
               comm_name = 'COMM_LOGGER'
               if (cfg%has(comm_name)) then
                  call cfg%get(comm_name, 'comm', rc=status)
                  _VERIFY(status,'',rc)
               end if
               comm = get_communicator(comm_name, extra=extra)
               allocate(lock, source=MpiLock(comm))
#endif
            case default
            end select
         else
            _ASSERT(.false., 'PFL::Config::build_lock() - unsupported class of lock.', rc)
         end if

         _RETURN(_SUCCESS,rc)
         _UNUSED_DUMMY(unusable)
      end function build_lock

   end subroutine build_locks


   subroutine build_filters(this, cfg, unusable, extra, rc)
      class (ConfigElements), intent(inout) :: this
      type (Configuration), intent(in) :: cfg
      class (KeywordEnforcer), optional, intent(in) :: unusable
      type (StringUnlimitedMap), optional, intent(in) :: extra
      integer, optional, intent(out) :: rc

      type (ConfigurationIterator) :: iter
      type (Configuration) :: subcfg
      character(:), allocatable :: filter_name
      integer :: status

      associate (b => cfg%begin(), e => cfg%end())
        iter = b
        
        do while (iter /= e)

           call iter%get_key(filter_name, rc=status)
           _VERIFY(status,'',rc)

           call iter%get_value(subcfg, rc=status)
           _VERIFY(status,'',rc)

           call this%filters%insert(filter_name, build_filter(subcfg, extra=extra, rc=status))
           _VERIFY(status,'',rc)
           call iter%next()
        end do
      end associate

      _RETURN(_SUCCESS,rc)
      _UNUSED_DUMMY(unusable)


   end subroutine build_filters


   function build_filter(cfg, unusable, extra, rc) result(f)
      class (AbstractFilter), allocatable :: f
      type (Configuration), intent(in) :: cfg
      class (KeywordEnforcer), optional, intent(in) :: unusable
      type (StringUnlimitedMap), optional, intent(in) :: extra
      integer, optional, intent(out) :: rc

      character(len=:), allocatable :: class_name
      integer :: status
      
      _ASSERT(cfg%is_mapping(), "PFL::Config::build_formatter() - input cfg not a mapping", rc)

      if (cfg%has('class')) then
         call cfg%get(class_name, 'class', rc=status)
         _VERIFY(status,'',rc)
      else
         class_name = 'filter'
      end if

      select case (to_lower_case(class_name))
      case ('filter')
         allocate(f, source=build_basic_filter(cfg, rc=status))
         _VERIFY(status,'',rc)
         
      case ('levelfilter')
         allocate(f, source=build_LevelFilter(cfg, rc=status))
         _VERIFY(status,'',rc)
         
#ifdef _LOGGER_USE_MPI              
      case ('mpifilter')
         allocate(f, source=build_MpiFilter(cfg, extra=extra, rc=status))
         _VERIFY(status,'',rc)

#endif
      case default
         _ASSERT(.false., 'PFL::Config::build_filter() - unknown filter type: '//class_name//'.', rc)
      end select

      _RETURN(_SUCCESS,rc)
      _UNUSED_DUMMY(unusable)
    end function build_filter

    function build_basic_filter(cfg, rc) result(f)
      type (Filter) :: f
      type (Configuration), intent(in) :: cfg
      integer, optional, intent(out) :: rc
      
      character(len=:), allocatable :: name
      integer :: status

      _ASSERT(cfg%is_mapping(), "PFL::Config::build_formatter() - input cfg not a mapping", rc)
      _ASSERT(cfg%has('name'), 'PFL::Config::build_basic_filter() - missing "name".', rc)

      call cfg%get(name, 'name', rc=status)
      _VERIFY(status,'',rc)
      f = Filter(name)

      _RETURN(_SUCCESS,rc)
    end function build_basic_filter

    function build_LevelFilter(cfg, rc) result(f)
       use PFL_LevelFilter
       type (LevelFilter) :: f
       type (Configuration), intent(in) :: cfg
       integer, optional, intent(out) :: rc
       
       integer :: min_level, max_level
       integer :: status

       min_level = get_level('min_level', rc=status)
       _VERIFY(status,'',rc)
       max_level = get_level('max_level', rc=status)
       _VERIFY(status,'',rc)
 
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

          call cfg%get(level_name, 'max_level', rc=status)
          _VERIFY(status,'',rc)

          read(level_name,*,iostat=iostat) level
          if (iostat /= 0) then
             level = name_to_level(level_name, rc=status)
             _VERIFY(status,'',rc)
          end if

          _RETURN(_SUCCESS,rc)
        end function get_level
        
    end function build_LevelFilter

#ifdef _LOGGER_USE_MPI
    function build_MpiFilter(cfg, unusable, extra, rc) result(f)
       use PFL_MpiFilter
       type (MpiFilter) :: f
       type (Configuration), intent(in) :: cfg
       class (KeywordEnforcer), optional, intent(in) :: unusable
       type (StringUnlimitedMap), optional, intent(in) :: extra
       integer, optional, intent(out) :: rc
      
       character(len=:), allocatable :: comm_name
       integer :: comm
       integer :: rank, root, ierror
       integer :: status

       if (cfg%has('comm')) then
          call cfg%get(comm_name, 'comm', rc=status)
          _VERIFY(status,'',rc)
       else
          comm_name = 'COMM_LOGGER'
       end if

       comm = get_communicator(comm_name, extra=extra)
       root = 0
       if (cfg%has('root')) then
          call cfg%get(root, 'root', rc=status)
          _VERIFY(status,'',rc)
       end if

       f = MpiFilter(comm, root)

       _RETURN(_SUCCESS,rc)
       _UNUSED_DUMMY(unusable)
    end function build_MpiFilter
#endif
    

   subroutine build_handlers(this, cfg, unusable, extra, rc)
      class (ConfigElements), intent(inout) :: this
      type (Configuration), intent(in) :: cfg
      class (KeywordEnforcer), optional, intent(in) :: unusable
      type (StringUnlimitedMap), optional, intent(in) :: extra
      integer, optional, intent(out) :: rc

      type (ConfigurationIterator) :: iter
      type (Configuration) :: subcfg
      character(:), allocatable :: handler_name
      class (AbstractHandler), allocatable :: h
      integer :: status

      iter = cfg%begin()
      do while (iter /= cfg%end())
         call iter%get_key(handler_name, rc=status)
         _VERIFY(status, '', rc)

         call iter%get_value(subcfg, rc=status)
         call build_handler(h,subcfg, this, extra=extra, rc=status)
         _VERIFY(status,'',rc)

         call this%handlers%insert(handler_name, h)
         deallocate(h)
         call iter%next()
      end do

      _RETURN(_SUCCESS,rc)
   end subroutine build_handlers
   
   subroutine build_handler(h, cfg, elements, unusable, extra, rc)
      class (AbstractHandler), allocatable, intent(out) :: h
      type (Configuration), intent(in) :: cfg
      type (ConfigElements), intent(in) :: elements
      class (KeywordEnforcer), optional, intent(in) :: unusable
      type (StringUnlimitedMap), optional, intent(in) :: extra
      integer, optional, intent(out) :: rc

      integer :: status
      
      call allocate_concrete_handler(h, cfg, rc=status)
      _VERIFY(status,'',rc)
      call set_handler_level(h, cfg, rc=status)
      _VERIFY(status,'',rc)
      call set_handler_formatter(h, cfg, elements%formatters, rc=status)
      _VERIFY(status,'',rc)
      call set_handler_filters(h, cfg, elements%filters, rc=status)
      _VERIFY(status,'',rc)
      call set_handler_lock(h, cfg, elements%locks, rc=status)
      _VERIFY(status,'',rc)

      _RETURN(_SUCCESS,rc)
      _UNUSED_DUMMY(unusable)

   contains

      subroutine allocate_concrete_handler(h, cfg, rc)
         class (AbstractHandler), allocatable, intent(out) :: h
         type (Configuration), intent(in) :: cfg
         integer, optional, intent(out) :: rc

         character(len=:), allocatable :: class_name
         type (Filehandler) :: fh
         integer :: status

         class_name = 'unknown'
         if (cfg%has('class')) then
            call cfg%get(class_name, 'class', rc=status)
            _VERIFY(status,'',rc)
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
         type (Configuration), intent(in) :: cfg
         integer, optional, intent(out) :: rc 

         character(len=:), allocatable :: level_name
         integer :: level
         integer :: iostat
         integer :: status

         level_name = 'NOTSET'
         if (cfg%has('level')) then
            call cfg%get(level_name, 'level', rc=status)
            _VERIFY(status,'',rc)
         end if

         ! Try as integer
         read(level_name,*, iostat=iostat) level

         ! If that failed - interpret as a name from SeverityLevels.
         if (iostat /= 0) then
            level = name_to_level(level_name, rc=status)
            _VERIFY(status,'',rc)
         end if

         call h%set_level(level)

         _RETURN(_SUCCESS,rc)
      end subroutine set_handler_level
      

      subroutine set_handler_formatter(h, cfg, formatters, rc)
         use PFL_Formatter
         class (AbstractHandler), intent(inout) :: h
         type (Configuration), intent(in) :: cfg
         type (FormatterMap), target, intent(in) :: formatters
         integer, optional, intent(out) :: rc

         character(len=:), allocatable :: formatter_name

         class (Formatter), pointer :: p_fmt
         integer :: status

         if (cfg%has('formatter')) then
            call cfg%get(formatter_name, 'formatter', rc=status)
            _VERIFY(status,'',rc)

            if (formatters%count(formatter_name) == 1) then
               p_fmt => formatters%at(formatter_name)
               call h%set_formatter(formatters%at(formatter_name))
            else
               _ASSERT(.false., "PFL::Config::build_handler() - formatter '"//formatter_name//"' not found.", rc)
            end if
         end if

         _RETURN(_SUCCESS,rc)
         _UNUSED_DUMMY(unusable)
      end subroutine set_handler_formatter


      subroutine set_handler_filters(h, cfg, filters, rc)
         class (AbstractHandler), intent(inout) :: h
         type (Configuration), intent(in) :: cfg
         type (FilterMap), intent(in) :: filters
         integer, optional, intent(out) :: rc

         class (AbstractFilter), pointer :: f
         character(len=:), allocatable :: name
         integer :: status
         type(Configuration) :: subcfg
         type(UnlimitedVector) :: empty
         integer :: i

         if (cfg%has('filters')) then
            subcfg = cfg%of('filters')
            _ASSERT(subcfg%is_sequence(), 'PFL::Config::handler_filters() - "filters" not a squence', rc)

            do i = 1, subcfg%size()
               call subcfg%get(name, i)
               if (filters%count(name) > 0) then
                  f => filters%at(name)
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
         type (Configuration), intent(in) :: cfg
         type (LockMap), intent(in) :: locks
         integer, optional, intent(out) :: rc

         character(len=:), allocatable :: lock_name
         class (AbstractLock), pointer :: lock
         integer :: status

         if (cfg%has('lock')) then
            call cfg%get(lock_name, 'lock', rc=status)
            _VERIFY(status,'',rc)

            if (locks%count(lock_name) == 1) then
               lock => locks%at(lock_name)
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
      type (Configuration), intent(in) :: cfg
      integer, optional, intent(out) :: rc

      character(len=:), allocatable :: unit_name
      integer :: unit
      integer :: status
      integer :: iostat
      type(Configuration) :: subcfg


      if (cfg%has('unit')) then
         subcfg = cfg%of('unit')
         if (subcfg%is_int()) then
            call subcfg%get(unit, rc=status)
            _VERIFY(status,'',rc)
         elseif (subcfg%is_string()) then
            call subcfg%get(unit_name, rc=status)
            _VERIFY(status,'',rc)
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
      type (Configuration), intent(in) :: cfg
      integer, optional, intent(out) :: rc

      character(len=:), allocatable :: filename
      integer :: unit
      integer :: iostat
      integer :: status
      logical :: delay

      _ASSERT(cfg%has('filename'), "PFL::Config::build_FileHandler() - must provide file name.", rc)

      call cfg%get(filename, 'filename', rc=status)
      _VERIFY(status,'',rc)

      delay = .false.
      if (cfg%has(delay)) then
         call cfg%get(delay, 'delay', rc=status)
         _VERIFY(status,'',rc)
      end if

      h = FileHandler(filename, delay=delay)

      _RETURN(_SUCCESS,rc)
   end subroutine build_filehandler

#ifdef _LOGGER_USE_MPI
   subroutine build_mpifilehandler(h, cfg, unusable, extra, rc)
      use PFL_MpiCommConfig

      type (FileHandler), intent(out) :: h
      type (Configuration), intent(in) :: cfg
      class (KeywordEnforcer), optional, intent(in) :: unusable
      type (StringUnlimitedMap), optional, intent(in) :: extra
      integer, optional, intent(out) :: rc

      character(len=:), allocatable :: fileName
      type(Configuration) :: subcfg
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

      call cfg%get(filename, 'filename', rc=status)
      _VERIFY(status,'',rc)

      if (cfg%has('comms')) then
         subcfg = cfg%at('comms', rc=status)
         _VERIFY(status,'',rc)
         _ASSERT(subcfg%is_sequence(), "PFL::Config::build_mpi_formatter() - 'comms' subcfg not a sequence.", rc)


         allocate(comms(n))

         do i = 1, n
            call subcfg%get(communicator_name, i, rc=status)
            _VERIFY(status,'',rc)
            comms(i) = get_communicator(communicator_name)
         end do

         block
           character(len=:), allocatable :: rank_prefix
           character(len=:), allocatable :: size_prefix

           rank_prefix = 'mpi_rank'
           if (cfg%has('rank_prefix')) then
              call cfg%get(rank_prefix, 'rank_prefix', rc=status)
              _VERIFY(status,'',rc)
           end if

           size_prefix = 'mpi_size'
           if (cfg%has('size_prefix')) then
              call cfg%get(size_prefix, 'size_prefix', rc=status)
              _VERIFY(status,'',rc)
           end if

           call init_MpiCommConfig(commMap, comm, rank_keyword=rank_prefix, size_keyword=size_prefix)
         end block

      else
         communicator_name = 'COMM_LOGGER'
         if (cfg%has('comm')) then
            call cfg%get(communicator_name, 'comm', rc=status)
            _VERIFY(status,'',rc)
         end if
         comm = get_communicator(communicator_name, extra=extra, rc=status)
         _VERIFY(status,'',rc)

         block
           character(len=:), allocatable :: rank_prefix
           character(len=:), allocatable :: size_prefix

           rank_prefix = 'mpi_rank'
           if (cfg%has('rank_prefix')) then
              call cfg%get(rank_prefix, 'rank_prefix', rc=status)
              _VERIFY(status,'',rc)
           end if

           size_prefix = 'mpi_size'
           if (cfg%has('size_prefix')) then
              call cfg%get(size_prefix, 'size_prefix', rc=status)
              _VERIFY(status,'',rc)
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
         call cfg%get(delay, 'delay', rc=status)
         _VERIFY(status,'',rc)
      end if

      h = FileHandler(fileName, delay=delay)
     
      _RETURN(_SUCCESS,rc) 
   end subroutine build_mpifilehandler
#endif

   subroutine build_logger(lgr, cfg, elements, unusable, extra, rc)
      use PFL_StringUtilities, only: to_lower_case
      class (Logger), intent(inout) :: lgr
      type (Configuration), intent(in) :: cfg
      type (ConfigElements), intent(in) :: elements
      class (KeywordEnforcer), optional, intent(in) :: unusable
      type (StringUnlimitedMap), optional, intent(in) :: extra
      integer, optional, intent(out) :: rc

      integer :: status

      _UNUSED_DUMMY(unusable)

      call set_logger_level(lgr, cfg, rc=status)
      _VERIFY(status,'',rc)
      call set_logger_propagate(lgr, cfg,rc=status)
      _VERIFY(status,'',rc)
      call set_logger_filters(lgr, cfg, elements%filters, rc=status)
      _VERIFY(status,'',rc)
      call set_logger_handlers(lgr, cfg, elements%handlers, rc=status)
      _VERIFY(status,'',rc)

      _RETURN(_SUCCESS,rc)

   contains

      subroutine set_logger_level(lgr, cfg, rc)
         class (Logger), intent(inout) :: lgr
         type (Configuration), intent(in) :: cfg
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
            call cfg%get(level_name, 'level', rc=status)
            _VERIFY(status,'',rc)
         end if

#ifdef _LOGGER_USE_MPI
         communicator_name = 'COMM_LOGGER'
         if (cfg%has('comm')) then
            call cfg%get(communicator_name, 'comm', rc=status)
            _VERIFY(status,'',rc)
         end if
         comm = get_communicator(communicator_name, extra=extra, rc=status)
         _VERIFY(status,'',rc)

         root = 0
         if (cfg%has('root')) then
            call cfg%get(root, 'root', rc=status)
            _VERIFY(status,'',rc)
         end if

         call MPI_Comm_rank(comm, rank, ierror)
         if (rank == root) then
            if (cfg%has('root_level')) then
               call cfg%get(root_level_name, 'root_level', rc=status)
               _VERIFY(status,'',rc)
               level_name = root_level_name
            end if
         else
            ! same as on other PEs
         end if
#endif
         ! Try as integer
         read(level_name,*, iostat=iostat) level

         if (iostat /= 0) then
            level = name_to_level(level_name, rc=status)
            _VERIFY(status,'',rc)
         end if
         call lgr%set_level(level)

         _RETURN(_SUCCESS,rc)
      end subroutine set_logger_level


      subroutine set_logger_propagate(lgr, cfg, rc)
         class (Logger), intent(inout) :: lgr
         type (Configuration), intent(in) :: cfg
         integer, optional, intent(out) :: rc

         logical :: propagate
         integer :: level
         integer :: status

         if (cfg%has('propagate')) then
            call cfg%get(propagate, 'propagate', rc=status)
            _VERIFY(status,'',rc)
            call lgr%set_propagate(propagate)
         end if

         _RETURN(_SUCCESS,rc)
      end subroutine set_logger_propagate
      

      subroutine set_logger_filters(lgr, cfg, filters, unusable, extra, rc)
         class (Logger), intent(inout) :: lgr
         type (Configuration), intent(in) :: cfg
         type (FilterMap), intent(in) :: filters
         class (KeywordEnforcer), optional, intent(in) :: unusable
         type (StringUnlimitedMap), optional, intent(in) :: extra
         integer, optional, intent(out) :: rc

         character(:), allocatable :: filter_name
         type(Configuration) :: subcfg
         integer :: i
         integer :: status

         if (cfg%has('filters')) then
            subcfg = cfg%of('filters')
            _ASSERT(subcfg%is_sequence(), "PFL::Config::set_logger_filters() - expected sequence for 'filters' key.", rc)

            do i = 1, subcfg%size()
               call subcfg%get(filter_name, i, rc=status)
               _VERIFY(status,'',rc)

               if (filters%count(filter_name) > 0) then
                  call lgr%add_filter(filters%at(filter_name))
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
         type (Configuration), intent(in) :: cfg
         type (HandlerMap), intent(in) :: handlers
         class (KeywordEnforcer), optional, intent(in) :: unusable
         type (StringUnlimitedMap), optional, intent(in) :: extra
         integer, optional, intent(out) :: rc

         character(len=:), allocatable :: handler_name         
         type(Configuration) :: subcfg
         integer :: i
         integer :: status

         if (cfg%has('handlers')) then
            subcfg = cfg%of('handlers')
            _ASSERT(cfg%has('handlers'), "PFL::Config::set_logger_handlers() - expected sequence for 'handlers' key.", rc)
         
            do i = 1, subcfg%size()
               call subcfg%get(handler_name, i, rc=status)
               _VERIFY(status,'',rc)

               if (handlers%count(handler_name) > 0) then
                  call lgr%add_handler(handlers%at(handler_name))
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

      name_ = name
      if (name == 'MPI_COMM_WORLD') then
         name_ = 'COMM_LOGGER'
      end if

      if (present(extra)) then
         if (extra%count(name_) > 0) then
            p => extra%at(name_)
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
      type (Configuration), intent(in) :: cfg
      integer, optional, intent(out) :: rc

      integer :: version
      integer :: status

      if (cfg%has('schema_version')) then
         call cfg%get(version, 'schema_version', rc=status)
         _VERIFY(status,'',rc)

         if (version /= 1) then
            _ASSERT(.false., 'PFL::Config::check_schema_version() - unsupported schema_version. Allowed values are [1].', rc)
         end if
      else
         _ASSERT(.false., 'PFL::Config::check_schema_version() - must specify a schema_version for Config.', rc)
      end if
      _RETURN(_SUCCESS,rc)
   end subroutine check_schema_version


   function get_filters(this) result(ptr)
      type (FilterMap), pointer :: ptr
      class (ConfigElements), target, intent(in) :: this
      ptr => this%filters
   end function get_filters


   function get_formatters(this) result(ptr)
      type (FormatterMap), pointer :: ptr
      class (ConfigElements), target, intent(in) :: this
      ptr => this%formatters
   end function get_formatters


   function get_handlers(this) result(ptr)
      type (HandlerMap), pointer :: ptr
      class (ConfigElements), target, intent(in) :: this
      ptr => this%handlers
   end function get_handlers

   subroutine set_global_communicator(this, comm)
      class (ConfigElements), intent(inout) :: this
      integer, optional, intent(in) :: comm

#ifdef _LOGGER_USE_MPI      
      if (present(comm)) then
         this%global_communicator = comm
      else
         this%global_communicator = MPI_COMM_WORLD
      end if
#endif

   end subroutine set_global_communicator


end module PFL_Config
