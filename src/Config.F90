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

   subroutine build_formatters(this, cfg, unusable, extra, rc)
      class(ConfigElements), intent(inout) :: this
      type(Configuration), intent(in) :: cfg
      class (KeywordEnforcer), optional, intent(in) :: unusable
      type (StringUnlimitedMap), optional, intent(in) :: extra
      integer, optional, intent(out) :: rc

      type(ConfigurationIterator) :: iter
      class(*), pointer :: node
      type (Configuration) :: subcfg
      class (Formatter), allocatable :: f
      
      integer :: status

      _UNUSED_DUMMY(unusable)

      iter = cfg%begin()
      do while (iter /= cfg%end())
         subcfg = iter%value()
         call build_formatter(f, subcfg, extra=extra, global_communicator=this%global_communicator, rc=status)
         _VERIFY(status, '', rc)
         call this%formatters%insert(iter%key(), f)
         deallocate(f)
         call iter%next()
      end do

      _RETURN(_SUCCESS,rc)
   end subroutine build_formatters


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
      
      _UNUSED_DUMMY(unusable)

      call cfg%get(class_name, 'class', default='Formatter', rc=status)
      _VERIFY(status,'',rc)

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

   end subroutine build_formatter


   subroutine build_basic_formatter(fmtr, cfg, rc)
      class (Formatter), allocatable, intent(out) :: fmtr
      type (Configuration), intent(in) :: cfg
      integer, optional, intent(out) :: rc

      character(len=:), allocatable :: fmt
      character(len=:), allocatable :: datefmt
      logical :: is_present
      integer :: status

      call cfg%get(fmt,'format', is_present=is_present, default='', rc=status)
      _VERIFY(status,'',rc)
!!$      fmt = trim(adjustl(fmt))
      if (is_present) then
         call cfg%get(datefmt, 'datefmt', is_present=is_present, rc=status)
         _VERIFY(status,'',rc)
         if (is_present) then
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
      logical :: is_present
      integer :: iostat
      integer :: comm
      integer, allocatable :: comms(:)
      integer :: i, j, n

      type (StringUnlimitedMap) :: commMap
      type (StringVector) :: communicator_name_list
      character(len=:), allocatable :: communicator_name, name
      integer :: status
      class(*), pointer :: p

      _UNUSED_DUMMY(unusable)

      call cfg%get(communicator_name_list, 'comms', is_present=is_present, rc=status)
      _VERIFY(status,'',rc)

      if (is_present) then
         allocate(comms(0))
         n = communicator_name_list%size()

         do i = 1, n
            name = communicator_name_list%at(i)

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
           call cfg%get(rank_prefix, 'rank_prefix', default='mpi_rank', rc=status)
           _VERIFY(status,'',rc)
           call cfg%get(size_prefix, 'size_prefix', default='mpi_size', rc=status)
           _VERIFY(status,'',rc)
           call init_MpiCommConfig(commMap, comm, rank_keyword=rank_prefix, size_keyword=size_prefix)
         end block
      else
         comm = get_communicator('COMM_LOGGER', extra=extra, rc=status)
         _VERIFY(status,'',rc)
         block
           character(len=:), allocatable :: rank_prefix
           character(len=:), allocatable :: size_prefix
           call cfg%get(rank_prefix, 'rank_prefix', default='mpi_rank', rc=status)
           _VERIFY(status,'',rc)
           call cfg%get(size_prefix, 'size_prefix', default='mpi_size', rc=status)
           _VERIFY(status,'',rc)
           call init_MpiCommConfig(commMap, comm, rank_keyword=rank_prefix, size_keyword=size_prefix)
         end block
      end if

      call cfg%get(fmt, 'format', is_present=is_present, rc=status)
      _VERIFY(status,'',rc)
      ! strip beginning and trailing quotes
      block
        character(len=:), allocatable :: tmp
        tmp = trim(adjustl(fmt))
      end block
      
      if (is_present) then
         call cfg%get(datefmt, 'datefmt', is_present=is_present, rc=status)
         _VERIFY(status,'',rc)
         if (is_present) then
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
      integer :: status
      
      _UNUSED_DUMMY(unusable)

      iter = cfg%begin()
      do while (iter /= cfg%end())
         subcfg = iter%value()
         call this%locks%insert(iter%key(), build_lock(subcfg, extra=extra, rc=status))
         _VERIFY(status,'',rc)
         call iter%next()
      end do
      _RETURN(_SUCCESS,rc)
   contains

      function build_lock(cfg, unusable, extra, rc) result(lock)
         class (AbstractLock), allocatable :: lock
         type (Configuration), intent(in) :: cfg
         class (KeywordEnforcer), optional, intent(in) :: unusable
         type (StringUnlimitedMap), optional, intent(in) :: extra
         integer, optional, intent(out) :: rc

         logical :: is_present
         character (len=:), allocatable :: class_name, comm_name
         integer :: comm
         integer :: status

         _UNUSED_DUMMY(unusable)

         call cfg%get(class_name, 'class', is_present=is_present, rc=status)
         _VERIFY(status,'',rc)
         if (is_present) then
            select case (class_name)
#ifdef _LOGGER_USE_MPI
            case ('MpiLock')
               call cfg%get(comm_name, 'comm', default='COMM_LOGGER', rc=status)
               _VERIFY(status,'',rc)
               comm = get_communicator(comm_name, extra=extra)
               allocate(lock, source=MpiLock(comm))
#endif
            case default
            end select
         else
            _ASSERT(.false., 'PFL::Config::build_lock() - unsupported class of lock.', rc)
         end if
         _RETURN(_SUCCESS,rc)
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
      integer :: status

      _UNUSED_DUMMY(unusable)

      iter = cfg%begin()
      do while (iter /= cfg%end())
         subcfg = iter%value()
         call this%filters%insert(iter%key(), build_filter(subcfg, extra=extra, rc=status))
         _VERIFY(status,'',rc)
         call iter%next()
      end do
      _RETURN(_SUCCESS,rc)

   end subroutine build_filters


   function build_filter(cfg, unusable, extra, rc) result(f)
      class (AbstractFilter), allocatable :: f
      type (Configuration), intent(in) :: cfg
      class (KeywordEnforcer), optional, intent(in) :: unusable
      type (StringUnlimitedMap), optional, intent(in) :: extra
      integer, optional, intent(out) :: rc

      character(len=:), allocatable :: class_name
      integer :: status
      
      _UNUSED_DUMMY(unusable)


      call cfg%get(class_name, 'class', default='filter', rc=status)
      _VERIFY(status,'',rc)

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

    end function build_filter

    function build_basic_filter(cfg, rc) result(f)
      type (Filter) :: f
      type (Configuration), intent(in) :: cfg
      integer, optional, intent(out) :: rc
      
      character(len=:), allocatable :: name
      logical :: is_present
      integer :: status

      call cfg%get(name, 'name', is_present=is_present, rc=status)
      _VERIFY(status,'',rc)
      if (is_present) then
         f = Filter(name)
      else
         _ASSERT(.false., 'PFL::Config::build_filter() - missing "name".', rc)
      end if

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
          logical :: is_present
          integer :: iostat
          integer :: status

          call cfg%get(level_name, 'max_level', is_present=is_present, rc=status)
          _VERIFY(status,'',rc)

          if (.not. is_present) then
             level = -1
             _ASSERT(.false., 'PFL::Config::build_LevelFilter() - missing "max_level".', rc)
          end if
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

       _UNUSED_DUMMY(unusable)

       call cfg%get(comm_name, 'comm', default='COMM_LOGGER', rc=status)
       _VERIFY(status,'',rc)
       comm = get_communicator(comm_name, extra=extra)
       call cfg%get(root, 'root', default=0, rc=status)
       _VERIFY(status,'',rc)
       f = MpiFilter(comm, root)
       _RETURN(_SUCCESS,rc)
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
      class (AbstractHandler), allocatable :: h
      integer :: status

      iter = cfg%begin()
      do while (iter /= cfg%end())
         subcfg = iter%value()
         call build_handler(h,subcfg, this, extra=extra, rc=status)
         _VERIFY(status,'',rc)
         call this%handlers%insert(iter%key(), h)
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
      
      _UNUSED_DUMMY(unusable)

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
   contains

      subroutine allocate_concrete_handler(h, cfg, rc)
         class (AbstractHandler), allocatable, intent(out) :: h
         type (Configuration), intent(in) :: cfg
         integer, optional, intent(out) :: rc

         character(len=:), allocatable :: class_name
         type (Filehandler) :: fh
         integer :: status

         call cfg%get(class_name, 'class', default='unknown', rc=status)
         _VERIFY(status,'',rc)

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

         call cfg%get(level_name, 'level', default='NOTSET')
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
         logical :: is_present

         class (Formatter), pointer :: p_fmt
         integer :: status
         
         _UNUSED_DUMMY(unusable)


         call cfg%get(formatter_name, 'formatter', is_present=is_present, rc=status)
         _VERIFY(status,'',rc)

         if (is_present) then  ! OK if no formatter
            if (formatters%count(formatter_name) == 1) then
               p_fmt => formatters%at(formatter_name)
               call h%set_formatter(formatters%at(formatter_name))
            else
               _ASSERT(.false., "PFL::Config::build_handler() - formatter '"//formatter_name//"' not found.", rc)
            end if
         end if
         _RETURN(_SUCCESS,rc)
      end subroutine set_handler_formatter


      subroutine set_handler_filters(h, cfg, filters, rc)
         class (AbstractHandler), intent(inout) :: h
         type (Configuration), intent(in) :: cfg
         type (FilterMap), intent(in) :: filters
         integer, optional, intent(out) :: rc

         class (AbstractFilter), pointer :: f
         character(len=:), allocatable :: name
         logical :: is_present
         integer :: status
         type (ConfigurationIterator) :: iter
         type(Configuration) :: subcfg
         type(Configuration) :: subsubcfg
         type(UnlimitedVector) :: empty

         call cfg%get(subcfg,'filters',default=empty,rc=status)
         _VERIFY(status,'',rc)

         iter = subcfg%begin()
         do while (iter /= subcfg%end())
            subsubcfg = iter%get()
            call subsubcfg%get(name)
            if (filters%count(name) > 0) then
               f => filters%at(name)
               call h%add_filter(f)
            else
               _ASSERT(.false., "PFL::Config::build_handler() - unknown filter'"//name//"'.", rc)
            end if
            call iter%next()
         end do
         _RETURN(_SUCCESS,rc)
      end subroutine set_handler_filters

      subroutine set_handler_lock(h, cfg, locks, rc)
         class (AbstractHandler), intent(inout) :: h
         type (Configuration), intent(in) :: cfg
         type (LockMap), intent(in) :: locks
         integer, optional, intent(out) :: rc

         character(len=:), allocatable :: lock_name
         logical :: is_present
         class (AbstractLock), pointer :: lock
         integer :: status

         call cfg%get(lock_name, 'lock', is_present=is_present, rc=status)
         _VERIFY(status,'',rc)

         if (is_present) then
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
      logical :: is_present
      class(*), pointer :: node

      call cfg%get_node_at_selector(node, 'unit', is_present=is_present, rc=status)
      _VERIFY(status,'',rc)

      if (is_present) then
         select type (node)
         type is (integer)
            unit = node
         type is (String)
            unit_name = node
            ! if failed, maybe it is a defined name?
            select case (to_lower_case(unit_name))
            case ('output_unit','*')
               unit = OUTPUT_UNIT
            case ('error_unit')
               unit = ERROR_UNIT
            case default
               _ASSERT(.false., "PFL::Config::build_streamhandler() - unknown value for unit '"//unit_name//"'.", rc)
            end select
         class default
            _ASSERT(.false., "PFL::Config::build_streamhandler() - illegal type for unit", rc)
         end select
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
      logical :: is_present
      integer :: iostat
      integer :: status
      logical :: delay

      call cfg%get(filename, 'filename', is_present=is_present, rc=status)
      _VERIFY(status,'',rc)
      if (.not. is_present) then
         _ASSERT(.false., "PFL::Config::build_FileHandler() - must provide file name.", rc)
      end if

      call cfg%get(delay, 'delay', default=.false., rc=status)
      _VERIFY(status,'',rc)
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
      integer :: unit
      logical :: is_present
      integer :: iostat
      integer :: comm
      integer, allocatable :: comms(:)
      integer :: i
      logical :: delay
      integer :: status

      type (StringUnlimitedMap) :: commMap
      type(StringVector) :: communicator_names
      character(len=:), pointer :: name
      character(len=:), allocatable :: communicator_name
      integer :: n

      call cfg%get(filename, 'filename', is_present=is_present, rc=status)
      _VERIFY(status,'',rc)
      if (.not. is_present) then
         _ASSERT(.false., "PFL::Config::build_MpiFileHandler() - must provide file name.", rc)
      end if

      call cfg%get(communicator_names, 'comms', is_present=is_present, rc=status)
      _VERIFY(status,'',rc)
      if (is_present) then
         n = communicator_names%size()
         allocate(comms(n))

         do i = 1, n
            comms(i) = get_communicator(communicator_names%at(i))
         end do

         block
           character(len=:), allocatable :: rank_prefix
           character(len=:), allocatable :: size_prefix
           call cfg%get(rank_prefix, 'rank_prefix', default='mpi_rank', rc=status)
           _VERIFY(status,'',rc)
           call cfg%get(size_prefix, 'size_prefix', default='mpi_size', rc=status)
           _VERIFY(status,'',rc)
           call init_MpiCommConfig(commMap, comm, rank_keyword=rank_prefix, size_keyword=size_prefix)
         end block
      else
         call cfg%get(communicator_name, 'comm', default='COMM_LOGGER')
         comm = get_communicator(communicator_name, extra=extra, rc=status)
         _VERIFY(status,'',rc)

         block
           character(len=:), allocatable :: rank_prefix
           character(len=:), allocatable :: size_prefix
           call cfg%get(rank_prefix, 'rank_prefix', default='mpi_rank', rc=status)
           _VERIFY(status,'',rc)
           call cfg%get(size_prefix, 'size_prefix', default='mpi_size', rc=status)
           _VERIFY(status,'',rc)
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
      call cfg%get(delay, 'delay', default=.true., rc=status)
      _VERIFY(status,'',rc)

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
         logical :: is_present, is_present_root
#ifdef _LOGGER_USE_MPI
         integer :: root, rank, ierror, comm
         character(len=:), allocatable :: communicator_name
#endif
         integer :: status

         call cfg%get(level_name, 'level', is_present=is_present, default='NOTSET', rc=status)
         _VERIFY(status,'',rc)
#ifdef _LOGGER_USE_MPI
         call cfg%get(communicator_name, 'comm', default='COMM_LOGGER', rc=status)
         _VERIFY(status,'',rc)
         comm = get_communicator(communicator_name, extra=extra, rc=status)
         _VERIFY(status,'',rc)
         call cfg%get(root, 'root', default=0, rc=status)
         _VERIFY(status,'',rc)

         call MPI_Comm_rank(comm, rank, ierror)
         if (rank == root) then
            call cfg%get(root_level_name, 'root_level', is_present=is_present_root, rc=status)
            _VERIFY(status,'',rc)
            if (is_present_root) then
               level_name = root_level_name
               is_present = .true.
            end if
         else
            ! same as on other PEs
         end if
#endif
         if (is_present) then
            ! Try as integer
            read(level_name,*, iostat=iostat) level

            if (iostat /= 0) then
               level = name_to_level(level_name, rc=status)
               _VERIFY(status,'',rc)
            end if
            call lgr%set_level(level)
         end if
         _RETURN(_SUCCESS,rc)
      end subroutine set_logger_level


      subroutine set_logger_propagate(lgr, cfg, rc)
         class (Logger), intent(inout) :: lgr
         type (Configuration), intent(in) :: cfg
         integer, optional, intent(out) :: rc

         logical :: propagate
         integer :: level
         logical :: is_present
         integer :: status

         call cfg%get(propagate, 'propagate', is_present=is_present, rc=status)
         _VERIFY(status,'',rc)
         if (is_present) then
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

         type(StringVector) :: filter_names
         character(len=:), pointer :: name
         integer :: i
         logical :: is_present
         integer :: status

         _UNUSED_DUMMY(unusable)

         call cfg%get(filter_names,'filters', is_present=is_present, rc=status)
         _VERIFY(status,'',rc)
         if (is_present) then

            do i = 1, filter_names%size()
               name => filter_names%at(i)
               if (filters%count(name) > 0) then
                  call lgr%add_filter(filters%at(name))
               else
                  _ASSERT(.false., "PFL::Config::build_logger() - unknown filter'"//name//"'.", rc)
               end if
            end do
         end if
         _RETURN(_SUCCESS,rc)
      end subroutine set_logger_filters


      subroutine set_logger_handlers(lgr, cfg, handlers, unusable, extra, rc)
         class (Logger), intent(inout) :: lgr
         type (Configuration), intent(in) :: cfg
         type (HandlerMap), intent(in) :: handlers
         class (KeywordEnforcer), optional, intent(in) :: unusable
         type (StringUnlimitedMap), optional, intent(in) :: extra
         integer, optional, intent(out) :: rc

         character(len=:), allocatable :: name         

         integer :: i

         logical :: is_present
         class (AbstractHandler), pointer :: h
         integer :: status
         type(Configuration) :: handler_names
         type(ConfigurationIterator) :: iter

         _UNUSED_DUMMY(unusable)

         call cfg%get(handler_names, 'handlers', rc=status)
         _VERIFY(status,'',rc)
         iter = handler_names%begin()
         do while (iter /= handler_names%end())
            name = iter%get()
            if (handlers%count(name) > 0) then
               call lgr%add_handler(handlers%at(name))
            else
               _ASSERT(.false., "PFL::Config::build_logger() - handlers is not of the form '[a,b,...,c]'", rc)
            end if
            call iter%next()
         end do
         _RETURN(_SUCCESS,rc)
      end subroutine set_logger_handlers


   end subroutine build_logger

#ifdef _LOGGER_USE_MPI
   integer function get_communicator(name, extra, rc) result(comm)
      character(len=*), intent(in) :: name
      type (StringUnlimitedMap), optional, intent(in) :: extra
      integer, optional, intent(out) :: rc

      character(len=:), allocatable :: name_
      logical :: is_present
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
      logical :: is_present
      integer :: status

      call cfg%get(version, 'schema_version', is_present=is_present, rc=status)
      _VERIFY(status,'',rc)

      if (is_present) then
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
