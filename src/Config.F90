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

   subroutine build_formatters(this, cfg, unusable, extra)
      class(ConfigElements), intent(inout) :: this
      type(Configuration), intent(in) :: cfg
      class (KeywordEnforcer), optional, intent(in) :: unusable
      type (StringUnlimitedMap), optional, intent(in) :: extra

      type(ConfigurationIterator) :: iter
      class(*), pointer :: node
      type (Configuration) :: subcfg
      class (Formatter), allocatable :: f
      
      integer :: status

      _UNUSED_DUMMY(unusable)

      iter = cfg%begin()
      do while (iter /= cfg%end())
         subcfg = iter%value()
         call build_formatter(f, subcfg, extra=extra, global_communicator=this%global_communicator)
         call this%formatters%insert(iter%key(), f)
         deallocate(f)
         call iter%next()
      end do

   end subroutine build_formatters


   subroutine build_formatter(fmtr, cfg, unusable, extra, global_communicator)
      class (Formatter), allocatable, intent(out) :: fmtr
      type(Configuration), intent(in) :: cfg
      class (KeywordEnforcer), optional, intent(in) :: unusable
      type (StringUnlimitedMap), optional, intent(in) :: extra
      integer, optional, intent(in) :: global_communicator

      character(len=:), allocatable :: class_name
      type (StringUnlimitedMap) :: extra_
      integer :: status
      
      _UNUSED_DUMMY(unusable)

      call cfg%get(class_name, 'class', default='Formatter', rc=status)
      select case (class_name)
      case ('Formatter')
         call build_basic_formatter(fmtr, cfg)
#ifdef _LOGGER_USE_MPI         
      case ('MpiFormatter')
         call extra_%deepcopy(extra)
         if (present(global_communicator)) then
            call extra_%insert('_GLOBAL_COMMUNICATOR', global_communicator)
         end if
         call build_mpi_formatter(fmtr, cfg, extra=extra_)
#endif
      end select

   end subroutine build_formatter


   subroutine build_basic_formatter(fmtr, cfg)
      class (Formatter), allocatable, intent(out) :: fmtr
      type (Configuration), intent(in) :: cfg

      character(len=:), allocatable :: fmt
      character(len=:), allocatable :: datefmt
      logical :: is_present
      integer :: status

      call cfg%get(fmt,'format', is_present=is_present, default='', rc=status)
!!$      fmt = trim(adjustl(fmt))
      if (is_present) then
         call cfg%get(datefmt, 'datefmt', is_present=is_present, rc=status)
         if (is_present) then
            allocate(fmtr,source=Formatter(fmt, datefmt=datefmt))
         else
            allocate(fmtr,source=Formatter(fmt))
         end if

      else
         allocate(fmtr, source=Formatter())
      end if

   end subroutine build_basic_formatter

#ifdef _LOGGER_USE_MPI
   subroutine build_mpi_formatter(fmtr, cfg, unusable, extra)
      use PFL_MpiCommConfig
      class (Formatter), allocatable, intent(out) :: fmtr
      type (Configuration), intent(in) :: cfg
      class (KeywordEnforcer), optional, intent(in) :: unusable
      type (StringUnlimitedMap), optional, intent(in) :: extra

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
                  call throw(__FILE__,__LINE__,"PFL::Config::build_mpi_formatter() - unknown communicator '"//name//"'.")
                  return
               end if
            end select
         end do

         block
           character(len=:), allocatable :: rank_prefix
           character(len=:), allocatable :: size_prefix
           call cfg%get(rank_prefix, 'rank_prefix', default='mpi_rank', rc=status)
           call cfg%get(size_prefix, 'size_prefix', default='mpi_size', rc=status)
           call init_MpiCommConfig(commMap, comm, rank_keyword=rank_prefix, size_keyword=size_prefix)
         end block
      else
         comm = get_communicator('COMM_LOGGER', extra=extra)
         block
           character(len=:), allocatable :: rank_prefix
           character(len=:), allocatable :: size_prefix
           call cfg%get(rank_prefix, 'rank_prefix', default='mpi_rank', rc=status)
           call cfg%get(size_prefix, 'size_prefix', default='mpi_size', rc=status)
           call init_MpiCommConfig(commMap, comm, rank_keyword=rank_prefix, size_keyword=size_prefix)
         end block
      end if

      call cfg%get(fmt, 'format', is_present=is_present, rc=status)
      ! strip beginning and trailing quotes
      block
        character(len=:), allocatable :: tmp
        tmp = trim(adjustl(fmt))
      end block
      
      if (is_present) then
         call cfg%get(datefmt, 'datefmt', is_present=is_present, rc=status)
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

   end subroutine build_mpi_formatter
#endif
   
   subroutine build_locks(this, cfg, unusable, extra)
      use PFL_AbstractLock
#ifdef _LOGGER_USE_MPI
      use PFL_MpiLock
#endif
      class (ConfigElements), intent(inout) :: this
      type (Configuration), intent(in) :: cfg
      class (KeywordEnforcer), optional, intent(in) :: unusable
      type (StringUnlimitedMap), optional, intent(in) :: extra

      type(ConfigurationIterator) :: iter
      type(Configuration) :: subcfg
      integer :: status
      
      _UNUSED_DUMMY(unusable)

      iter = cfg%begin()
      do while (iter /= cfg%end())
         subcfg = iter%value()
         call this%locks%insert(iter%key(), build_lock(subcfg, extra=extra))
         call iter%next()
      end do

   contains

      function build_lock(cfg, unusable, extra) result(lock)
         class (AbstractLock), allocatable :: lock
         type (Configuration), intent(in) :: cfg
         class (KeywordEnforcer), optional, intent(in) :: unusable
         type (StringUnlimitedMap), optional, intent(in) :: extra

         logical :: is_present
         character (len=:), allocatable :: class_name, comm_name
         integer :: comm
         integer :: status

         _UNUSED_DUMMY(unusable)

         call cfg%get(class_name, 'class', is_present=is_present, rc=status)
         if (is_present) then
            select case (class_name)
#ifdef _LOGGER_USE_MPI
            case ('MpiLock')
               call cfg%get(comm_name, 'comm', default='COMM_LOGGER', rc=status)
               comm = get_communicator(comm_name, extra=extra)
               allocate(lock, source=MpiLock(comm))
#endif
            case default
            end select
         else
            call throw(__FILE__,__LINE__,'PFL::Config::build_lock() - unsupported class of lock.')
         end if
         
      end function build_lock

   end subroutine build_locks


   subroutine build_filters(this, cfg, unusable, extra)
      class (ConfigElements), intent(inout) :: this
      type (Configuration), intent(in) :: cfg
      class (KeywordEnforcer), optional, intent(in) :: unusable
      type (StringUnlimitedMap), optional, intent(in) :: extra

      type (ConfigurationIterator) :: iter
      type (Configuration) :: subcfg

      _UNUSED_DUMMY(unusable)

      iter = cfg%begin()
      do while (iter /= cfg%end())
         subcfg = iter%value()
         call this%filters%insert(iter%key(), build_filter(subcfg, extra=extra))
         call iter%next()
      end do

   end subroutine build_filters


   function build_filter(cfg, unusable, extra) result(f)
      class (AbstractFilter), allocatable :: f
      type (Configuration), intent(in) :: cfg
      class (KeywordEnforcer), optional, intent(in) :: unusable
      type (StringUnlimitedMap), optional, intent(in) :: extra

      character(len=:), allocatable :: class_name
      integer :: status
      
      _UNUSED_DUMMY(unusable)


      call cfg%get(class_name, 'class', default='filter', rc=status)

      select case (to_lower_case(class_name))
      case ('filter')
         allocate(f, source=build_basic_filter(cfg))
      case ('levelfilter')
         allocate(f, source=build_LevelFilter(cfg))
#ifdef _LOGGER_USE_MPI              
      case ('mpifilter')
         allocate(f, source=build_MpiFilter(cfg, extra=extra))
#endif
      case default
         call throw(__FILE__,__LINE__,'PFL::Config::build_filter() - unknow filter type: '//class_name//'.')
      end select
    end function build_filter

    function build_basic_filter(cfg) result(f)
      type (Filter) :: f
      type (Configuration), intent(in) :: cfg
      
      character(len=:), allocatable :: name
      logical :: is_present
      integer :: status

      call cfg%get(name, 'name', is_present=is_present, rc=status)

      if (is_present) then
         f = Filter(name)
      else
         call throw(__FILE__,__LINE__,'PFL::Config::build_filter() - missing "name".')
      end if

    end function build_basic_filter

    function build_LevelFilter(cfg) result(f)
       use PFL_LevelFilter
       type (LevelFilter) :: f
       type (Configuration), intent(in) :: cfg
       
       integer :: min_level, max_level

       min_level = get_level('min_level')
       max_level = get_level('max_level')
 
       f = LevelFilter(min_level, max_level)

    contains
       
       integer function get_level(key) result(level)
          character(len=*), intent(in) :: key

          character(len=:), allocatable :: level_name
          logical :: is_present
          integer :: iostat
          integer :: status

          call cfg%get(level_name, 'max_level', is_present=is_present, rc=status)
          
          if (.not. is_present) then
             call throw(__FILE__,__LINE__,'PFL::Config::build_LevelFilter() - missing "max_level".')
             level = -1
             return
          end if
          read(level_name,*,iostat=iostat) level
          if (iostat /= 0) then
             level = name_to_level(level_name)
          end if

        end function get_level
        
    end function build_LevelFilter

#ifdef _LOGGER_USE_MPI
    function build_MpiFilter(cfg, unusable, extra) result(f)
       use PFL_MpiFilter
       type (MpiFilter) :: f
       type (Configuration), intent(in) :: cfg
       class (KeywordEnforcer), optional, intent(in) :: unusable
       type (StringUnlimitedMap), optional, intent(in) :: extra
      
       character(len=:), allocatable :: comm_name
       integer :: comm
       integer :: rank, root, ierror
       integer :: status

       _UNUSED_DUMMY(unusable)

       call cfg%get(comm_name, 'comm', default='COMM_LOGGER', rc=status)
       comm = get_communicator(comm_name, extra=extra)
       call cfg%get(root, 'root', default=0, rc=status)
       f = MpiFilter(comm, root)
    end function build_MpiFilter
#endif
    

   subroutine build_handlers(this, cfg, unusable, extra)
      class (ConfigElements), intent(inout) :: this
      type (Configuration), intent(in) :: cfg
      class (KeywordEnforcer), optional, intent(in) :: unusable
      type (StringUnlimitedMap), optional, intent(in) :: extra

      type (ConfigurationIterator) :: iter
      type (Configuration) :: subcfg
      class (AbstractHandler), allocatable :: h

      iter = cfg%begin()
      do while (iter /= cfg%end())
         subcfg = iter%value()
         call build_handler(h,subcfg, this, extra=extra)
         call this%handlers%insert(iter%key(), h)
         deallocate(h)
         call iter%next()
      end do

   end subroutine build_handlers
   
   subroutine build_handler(h, cfg, elements, unusable, extra)
      class (AbstractHandler), allocatable, intent(out) :: h
      type (Configuration), intent(in) :: cfg
      type (ConfigElements), intent(in) :: elements
      class (KeywordEnforcer), optional, intent(in) :: unusable
      type (StringUnlimitedMap), optional, intent(in) :: extra

      integer :: status
      
      _UNUSED_DUMMY(unusable)

      call allocate_concrete_handler(h, cfg)
      if (.not. allocated(h)) return

      call set_handler_level(h, cfg)
      call set_handler_formatter(h, cfg, elements%formatters)
      call set_handler_filters(h, cfg, elements%filters)
      call set_handler_lock(h, cfg, elements%locks)

   contains

      subroutine allocate_concrete_handler(h, cfg)
         class (AbstractHandler), allocatable, intent(out) :: h
         type (Configuration), intent(in) :: cfg

         character(len=:), allocatable :: class_name
         type (Filehandler) :: fh
         integer :: status

         call cfg%get(class_name, 'class', default='unknown', rc=status)

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
            call throw(__FILE__,__LINE__,"PFL::Config::build_handler() - unsupported class: '" // class_name //"'.")
         end select

      end subroutine allocate_concrete_handler

      subroutine set_handler_level(h, cfg)
         class (AbstractHandler), intent(inout) :: h
         type (Configuration), intent(in) :: cfg

         character(len=:), allocatable :: level_name
         integer :: level
         integer :: iostat

         call cfg%get(level_name, 'level', default='NOTSET')
         ! Try as integer
         read(level_name,*, iostat=iostat) level

         ! If that failed - interpret as a name from SeverityLevels.
         if (iostat /= 0) then
            level = name_to_level(level_name)
         end if

         call h%set_level(level)

      end subroutine set_handler_level
      

      subroutine set_handler_formatter(h, cfg, formatters)
         use PFL_Formatter
         class (AbstractHandler), intent(inout) :: h
         type (Configuration), intent(in) :: cfg
         type (FormatterMap), target, intent(in) :: formatters

         character(len=:), allocatable :: formatter_name
         logical :: is_present

         class (Formatter), pointer :: p_fmt
         integer :: status
         
         _UNUSED_DUMMY(unusable)


         call cfg%get(formatter_name, 'formatter', is_present=is_present, rc=status)

         if (is_present) then  ! OK if no formatter
            if (formatters%count(formatter_name) == 1) then
               p_fmt => formatters%at(formatter_name)
               call h%set_formatter(formatters%at(formatter_name))
            else
               call throw(__FILE__,__LINE__,"PFL::Config::build_handler() - formatter '"//formatter_name//"' not found.")
            end if
         end if

      end subroutine set_handler_formatter


      subroutine set_handler_filters(h, cfg, filters)
         class (AbstractHandler), intent(inout) :: h
         type (Configuration), intent(in) :: cfg
         type (FilterMap), intent(in) :: filters

         class (AbstractFilter), pointer :: f
         character(len=:), allocatable :: name
         logical :: is_present
         integer :: status
         type (ConfigurationIterator) :: iter
         type(Configuration) :: subcfg
         type(Configuration) :: subsubcfg
         type(UnlimitedVector) :: empty

         call cfg%get(subcfg,'filters',default=empty,rc=status)
         iter = subcfg%begin()
         do while (iter /= subcfg%end())
            subsubcfg = iter%get()
            call subsubcfg%get(name)
            if (filters%count(name) > 0) then
               f => filters%at(name)
               call h%add_filter(f)
            else
               call throw(__FILE__,__LINE__,"PFL::Config::build_handler() - unknown filter'"//name//"'.")
            end if
            call iter%next()
         end do

      end subroutine set_handler_filters

      subroutine set_handler_lock(h, cfg, locks)
         class (AbstractHandler), intent(inout) :: h
         type (Configuration), intent(in) :: cfg
         type (LockMap), intent(in) :: locks

         character(len=:), allocatable :: lock_name
         logical :: is_present
         class (AbstractLock), pointer :: lock
         integer :: status

         call cfg%get(lock_name, 'lock', is_present=is_present, rc=status)
         if (is_present) then
            if (locks%count(lock_name) == 1) then
               lock => locks%at(lock_name)
               select type (h)
               class is (FileHandler)
                  call h%set_lock(lock)
               class default
                  call throw(__FILE__,__LINE__,'PFL::Config::set_handler_lock() - unsupported Lock subclass.')
               end select
            else
               call throw(__FILE__,__LINE__,"PFL::Config::set_handler_lock() - unknown lock: '"//lock_name//"'.'")
            end if
         end if
      end subroutine set_handler_lock
      

   end subroutine build_handler

   function build_streamhandler(cfg) result(h)
      use iso_fortran_env, only: OUTPUT_UNIT, ERROR_UNIT
      type (StreamHandler) :: h
      type (Configuration), intent(in) :: cfg

      character(len=:), allocatable :: unit_name
      integer :: unit
      integer :: status
      integer :: iostat
      logical :: is_present
      class(*), pointer :: node

      call cfg%get_node_at_selector(node, 'unit', is_present=is_present, rc=status)
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
               call throw(__FILE__,__LINE__,"PFL::Config::build_streamhandler() - unknown value for unit '"//unit_name//"'.")
               return
            end select
         class default
            call throw(__FILE__,__LINE__,"PFL::Config::build_streamhandler() - illegal type for unit")
            return
         end select
      else
         unit = ERROR_UNIT
      end if
      h = StreamHandler(unit)

   end function build_streamhandler


   subroutine build_filehandler(h, cfg)
      type (FileHandler), intent(out) :: h
      type (Configuration), intent(in) :: cfg

      character(len=:), allocatable :: filename
      integer :: unit
      logical :: is_present
      integer :: iostat
      integer :: status
      logical :: delay

      call cfg%get(filename, 'filename', is_present=is_present, rc=status)
      if (.not. is_present) then
         call throw(__FILE__,__LINE__,"PFL::Config::build_FileHandler() - must provide file name.")
         return
      end if

      call cfg%get(delay, 'delay', default=.false., rc=status)
      h = FileHandler(filename, delay=delay)
      
   end subroutine build_filehandler

#ifdef _LOGGER_USE_MPI
   subroutine build_mpifilehandler(h, cfg, unusable, extra)
      use PFL_MpiCommConfig

      type (FileHandler), intent(out) :: h
      type (Configuration), intent(in) :: cfg
      class (KeywordEnforcer), optional, intent(in) :: unusable
      type (StringUnlimitedMap), optional, intent(in) :: extra

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
      if (.not. is_present) then
         call throw(__FILE__,__LINE__,"PFL::Config::build_MpiFileHandler() - must provide file name.")
         return
      end if

      call cfg%get(communicator_names, 'comms', is_present=is_present, rc=status)
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
           call cfg%get(size_prefix, 'size_prefix', default='mpi_size', rc=status)
           call init_MpiCommConfig(commMap, comm, rank_keyword=rank_prefix, size_keyword=size_prefix)
         end block
      else
         call cfg%get(communicator_name, 'comm', default='COMM_LOGGER')
         comm = get_communicator(communicator_name, extra=extra)
         block
           character(len=:), allocatable :: rank_prefix
           character(len=:), allocatable :: size_prefix
           call cfg%get(rank_prefix, 'rank_prefix', default='mpi_rank', rc=status)
           call cfg%get(size_prefix, 'size_prefix', default='mpi_size', rc=status)
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

      h = FileHandler(fileName, delay=delay)
      
   end subroutine build_mpifilehandler
#endif

   subroutine build_logger(lgr, cfg, elements, unusable, extra)
      use PFL_StringUtilities, only: to_lower_case
      class (Logger), intent(inout) :: lgr
      type (Configuration), intent(in) :: cfg
      type (ConfigElements), intent(in) :: elements
      class (KeywordEnforcer), optional, intent(in) :: unusable
      type (StringUnlimitedMap), optional, intent(in) :: extra

      _UNUSED_DUMMY(unusable)

      call set_logger_level(lgr, cfg)
      call set_logger_propagate(lgr, cfg)
      call set_logger_filters(lgr, cfg, elements%filters)
      call set_logger_handlers(lgr, cfg, elements%handlers)

   contains

      subroutine set_logger_level(lgr, cfg)
         class (Logger), intent(inout) :: lgr
         type (Configuration), intent(in) :: cfg

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
#ifdef _LOGGER_USE_MPI
         call cfg%get(communicator_name, 'comm', default='COMM_LOGGER', rc=status)
         comm = get_communicator(communicator_name, extra=extra)
         call cfg%get(root, 'root', default=0, rc=status)

         call MPI_Comm_rank(comm, rank, ierror)
         if (rank == root) then
            call cfg%get(root_level_name, 'root_level', is_present=is_present_root, rc=status)
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
               level = name_to_level(level_name)
            end if
            call lgr%set_level(level)
         end if

      end subroutine set_logger_level


      subroutine set_logger_propagate(lgr, cfg)
         class (Logger), intent(inout) :: lgr
         type (Configuration), intent(in) :: cfg

         logical :: propagate
         integer :: level
         logical :: is_present
         integer :: status

         call cfg%get(propagate, 'propagate', is_present=is_present, rc=status)
         if (is_present) then
            call lgr%set_propagate(propagate)
         end if

      end subroutine set_logger_propagate
      

      subroutine set_logger_filters(lgr, cfg, filters, unusable, extra)
         class (Logger), intent(inout) :: lgr
         type (Configuration), intent(in) :: cfg
         type (FilterMap), intent(in) :: filters
         class (KeywordEnforcer), optional, intent(in) :: unusable
         type (StringUnlimitedMap), optional, intent(in) :: extra

         type(StringVector) :: filter_names
         character(len=:), pointer :: name
         integer :: i
         logical :: is_present
         integer :: status

         _UNUSED_DUMMY(unusable)

         call cfg%get(filter_names,'filters', is_present=is_present, rc=status)
         if (is_present) then

            do i = 1, filter_names%size()
               name => filter_names%at(i)
               if (filters%count(name) > 0) then
                  call lgr%add_filter(filters%at(name))
               else
                  call throw(__FILE__,__LINE__,"PFL::Config::build_logger() - unknown filter'"//name//"'.")
                  return
               end if
            end do
         end if

      end subroutine set_logger_filters


      subroutine set_logger_handlers(lgr, cfg, handlers, unusable, extra)
         class (Logger), intent(inout) :: lgr
         type (Configuration), intent(in) :: cfg
         type (HandlerMap), intent(in) :: handlers
         class (KeywordEnforcer), optional, intent(in) :: unusable
         type (StringUnlimitedMap), optional, intent(in) :: extra

         character(len=:), allocatable :: name         

         integer :: i

         logical :: is_present
         class (AbstractHandler), pointer :: h
         integer :: status
         type(Configuration) :: handler_names
         type(ConfigurationIterator) :: iter

         _UNUSED_DUMMY(unusable)

         call cfg%get(handler_names, 'handlers', rc=status)
         iter = handler_names%begin()
         do while (iter /= handler_names%end())
            name = iter%get()
            if (handlers%count(name) > 0) then
               call lgr%add_handler(handlers%at(name))
            else
               call throw(__FILE__,__LINE__,"PFL::Config::build_logger() - handlers is not of the form '[a,b,...,c]'")
               return
            end if
            call iter%next()
         end do

      end subroutine set_logger_handlers


   end subroutine build_logger

#ifdef _LOGGER_USE_MPI
   integer function get_communicator(name, extra) result(comm)
      character(len=*), intent(in) :: name
      type (StringUnlimitedMap), optional, intent(in) :: extra

      character(len=:), allocatable :: name_
      logical :: is_present
      class(*), pointer :: p
      
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
            call throw(__FILE__,__LINE__,"PFL::Config::build_logger() - MPI communicator '" // name_ // "' not found.")
         end if
      else
         comm = MPI_COMM_WORLD
      end if

   end function get_communicator
#endif


   ! Including a version number is crucial for providing non-backward
   ! compatible updates in the future.
   subroutine check_schema_version(cfg)
      type (Configuration), intent(in) :: cfg

      integer :: version
      logical :: is_present
      integer :: status

      call cfg%get(version, 'schema_version', is_present=is_present, rc=status)
      if (is_present) then
         if (version /= 1) then
            call throw(__FILE__,__LINE__,'PFL::Config::check_schema_version() -' // &
                 & ' unsupported schema_version. Allowed values are [1].')
            return
         end if
      else
         call throw(__FILE__,__LINE__,'PFL::Config::check_schema_version() -' // &
              & ' must specify a schema_version for Config.')
      end if

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
