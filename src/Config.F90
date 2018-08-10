#include "error_handling_macros.fh"
module PFL_Config_mod
   use PFL_String_mod, only: String
   use FTL_Config_mod, only: config
   use PFL_Logger_mod
   use PFL_Exception_mod, only: throw
   use PFL_SeverityLevels_mod, only: name_to_level
   use PFL_StringAbstractLoggerPolyMap_mod, only: LoggerMap
   use PFL_StringFilterMap_mod
   use PFL_StringLockMap_mod
   use PFL_StreamHandler_mod
   use PFL_StringHandlerMap_mod
   use PFL_StringFormatterMap_mod
   use PFL_AbstractLock_mod
   use PFL_AbstractFilter_mod
   use PFL_AbstractHandler_mod
   use PFL_Formatter_mod
   use PFL_Filterer_mod
   use PFL_FileHandler_mod
   
   use PFL_StringUnlimitedPolyMap_mod, only: ConfigIterator
   use PFL_StringUnlimitedMap_mod, only: Map
   use PFL_Filter_mod
   use PFL_StringUtilities_mod, only: to_lower_case
   use Pfl_KeywordEnforcer_Mod
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

   subroutine build_formatters(this, formattersDict, unusable, extra)
      class (ConfigElements), intent(inout) :: this
      type (Config), intent(in) :: formattersDict
      class (KeywordEnforcer), optional, intent(in) :: unusable
      type (Config), optional, intent(in) :: extra

      type (ConfigIterator) :: iter
      type (Config), pointer :: cfg
      class (Formatter), allocatable :: f
      
      _UNUSED_DUMMY(unusable)

      iter = formattersDict%begin()
      do while (iter /= formattersDict%end())
         cfg => formattersDict%toConfigPtr(iter%key())
         call build_formatter(f, cfg, extra=extra, global_communicator=this%global_communicator)
         call this%formatters%insert(iter%key(), f)
         deallocate(f)
         call iter%next()
      end do

   end subroutine build_formatters


   subroutine build_formatter(fmtr, dict, unusable, extra, global_communicator)
      class (Formatter), allocatable, intent(out) :: fmtr
      type (Config), intent(in) :: dict
      class (KeywordEnforcer), optional, intent(in) :: unusable
      type (Config), optional, intent(in) :: extra
      integer, optional, intent(in) :: global_communicator

      character(len=:), allocatable :: class_name
      type (Config) :: extra_
      
      _UNUSED_DUMMY(unusable)

      class_name = dict%toString('class', default='Formatter')

      select case (class_name)
      case ('Formatter')
         call build_basic_formatter(fmtr, dict)
#ifdef _LOGGER_USE_MPI         
      case ('MpiFormatter')
         call extra_%deepcopy(extra)
         if (present(global_communicator)) then
            call extra_%insert('_GLOBAL_COMMUNICATOR', global_communicator)
         end if
         call build_mpi_formatter(fmtr, dict, extra=extra_)
#endif
      end select

   end subroutine build_formatter


   subroutine build_basic_formatter(fmtr, dict)
      class (Formatter), allocatable, intent(out) :: fmtr
      type (Config), intent(in) :: dict

      character(len=:), allocatable :: fmt
      character(len=:), allocatable :: datefmt
      character(len=:), allocatable :: tmp
      logical :: found

      fmt = dict%toString('format', found=found)
      ! strip beginning and trailing quotes
      fmt = trim(adjustl(fmt))
      fmt = fmt(2:len(fmt)-1)

      if (found) then
         datefmt = dict%toString('datefmt', found=found)
         if (found) then
            datefmt = trim(adjustl(datefmt))
            tmp = datefmt(2:len(datefmt)-1)
            datefmt = tmp
            allocate(fmtr,source=Formatter(fmt, datefmt=datefmt))
         else
            allocate(fmtr,source=Formatter(fmt))
         end if

      else
         allocate(fmtr, source=Formatter())
      end if

   end subroutine build_basic_formatter

#ifdef _LOGGER_USE_MPI
   subroutine build_mpi_formatter(fmtr, dict, unusable, extra)
      use PFL_MpiCommConfig_mod
      class (Formatter), allocatable, intent(out) :: fmtr
      type (Config), intent(in) :: dict
      class (KeywordEnforcer), optional, intent(in) :: unusable
      type (Config), optional, intent(in) :: extra

      character(len=:), allocatable :: fmt
      character(len=:), allocatable :: datefmt
      character(len=:), allocatable :: fileName
      logical :: found
      integer :: iostat
      integer :: comm
      integer, allocatable :: comms(:)
      integer :: i, j, n

      type (Map) :: commMap
      character(len=:), allocatable :: communicator_name_list, communicator_name, name

      _UNUSED_DUMMY(unusable)

      communicator_name_list = dict%toString('comms', found=found)
      if (found) then
         allocate(comms(0))
         n = len_trim(communicator_name_list)
         if (communicator_name_list(1:1) /= '[' .or. communicator_name_list(n:n) /= ']') then
            call throw(__FILE__,__LINE__,"PFL::Config::build_mpi_formatter() - misformed list of communicators.")
            return
         end if

         i = 2
         do while (i < n)
            j = index(communicator_name_list(i:n-1),',')
            if (j == 0) then
               if (i < n-1) then
                  name = communicator_name_list(i:n-1)
                  i = n
               end if
            else
               name = communicator_name_list(i:i+j-2)
               i = i + j
            end if

            select case (name)
            case ('MPI_COMM_WORLD','COMM_LOGGER')
               comms = [comms, extra%toInteger('_GLOBAL_COMMUNICATOR')]
            case default
               if (extra%count(name) == 1) then
                  comms = [comms, extra%toInteger(name)]
               else
                  call throw(__FILE__,__LINE__,"PFL::Config::build_mpi_formatter() - unknown communicator '"//name//"'.")
                  return
               end if
            end select
         end do

         block
           character(len=:), allocatable :: rank_prefix
           character(len=:), allocatable :: size_prefix
           rank_prefix = dict%toString('rank_prefix', default='mpi_rank')
           size_prefix = dict%toString('size_prefix', default='mpi_size')
           call commMap%deepcopy(MpiCommConfig(comms, rank_prefix=rank_prefix, size_prefix=size_prefix))
         end block
      else
         communicator_name = dict%toString('comm:', default='COMM_LOGGER')
         comm = get_communicator(communicator_name, extra=extra)
         block
           character(len=:), allocatable :: rank_prefix
           character(len=:), allocatable :: size_prefix
           rank_prefix = dict%toString('rank_prefix', default='mpi_rank')
           size_prefix = dict%toString('size_prefix', default='mpi_size')
           call commMap%deepCopy(MpiCommConfig(comm, rank_keyword=rank_prefix, size_keyword=size_prefix))
         end block
      end if

      fmt = dict%toString('format', found=found)
      ! strip beginning and trailing quotes
      block
        character(len=:), allocatable :: tmp
        tmp = trim(adjustl(fmt))
        n = len(tmp)
        fmt = tmp(2:n-1)
      end block
      
      if (found) then
         datefmt = dict%toString('datefmt', found=found)
         if (found) then
            datefmt = trim(adjustl(datefmt))
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
   
   subroutine build_locks(this, locksDict, unusable, extra)
      use PFL_AbstractLock_mod
#ifdef _LOGGER_USE_MPI
      use PFL_MpiLock_mod
#endif
      class (ConfigElements), intent(inout) :: this
      type (Config), intent(in) :: locksDict
      class (KeywordEnforcer), optional, intent(in) :: unusable
      type (Config), optional, intent(in) :: extra

      type (ConfigIterator) :: iter
      type (Config), pointer :: cfg

      _UNUSED_DUMMY(unusable)

      iter = locksDict%begin()
      do while (iter /= locksDict%end())
         cfg => locksDict%toConfigPtr(iter%key())
         call this%locks%insert(iter%key(), build_lock(cfg, extra=extra))
         call iter%next()
      end do

   contains

      function build_lock(cfg, unusable, extra) result(lock)
         class (AbstractLock), allocatable :: lock
         type (Config), intent(in) :: cfg
         class (KeywordEnforcer), optional, intent(in) :: unusable
         type (Config), optional, intent(in) :: extra

         logical :: found
         character (len=:), allocatable :: class_name, comm_name
         integer :: comm

      _UNUSED_DUMMY(unusable)

         class_name = cfg%toString('class', found=found)
         if (found) then
            select case (class_name)
#ifdef _LOGGER_USE_MPI
            case ('MpiLock')
               comm_name = cfg%toString('comm', default='COMM_LOGGER')
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


   subroutine build_filters(this, filtersDict, unusable, extra)
      class (ConfigElements), intent(inout) :: this
      type (Config), intent(in) :: filtersDict
      class (KeywordEnforcer), optional, intent(in) :: unusable
      type (Config), optional, intent(in) :: extra

      type (ConfigIterator) :: iter
      type (Config), pointer :: cfg

      _UNUSED_DUMMY(unusable)

      iter = filtersDict%begin()
      do while (iter /= filtersDict%end())
         cfg => filtersDict%toConfigPtr(iter%key())
         call this%filters%insert(iter%key(), build_filter(cfg, extra=extra))
         call iter%next()
      end do

   end subroutine build_filters


   function build_filter(dict, unusable, extra) result(f)
      class (AbstractFilter), allocatable :: f
      type (Config), intent(in) :: dict
      class (KeywordEnforcer), optional, intent(in) :: unusable
      type (Config), optional, intent(in) :: extra

      character(len=:), allocatable :: class_name

      _UNUSED_DUMMY(unusable)

      class_name = dict%toString('class', default='filter')

      select case (to_lower_case(class_name))
      case ('filter')
         allocate(f, source=build_basic_filter(dict))
      case ('levelfilter')
         allocate(f, source=build_LevelFilter(dict))
#ifdef _LOGGER_USE_MPI              
      case ('mpifilter')
         allocate(f, source=build_MpiFilter(dict, extra=extra))
#endif
      case default
         call throw(__FILE__,__LINE__,'PFL::Config::build_filter() - unknow filter type: '//class_name//'.')
      end select
    end function build_filter

    function build_basic_filter(dict) result(f)
      type (Filter) :: f
      type (Config), intent(in) :: dict
      
      character(len=:), allocatable :: name
      logical :: found

      name = dict%toString('name', found=found)
      if (found) then
         f = Filter(name)
      else
         call throw(__FILE__,__LINE__,'PFL::Config::build_filter() - missing "name".')
      end if

    end function build_basic_filter

    function build_LevelFilter(dict) result(f)
       use PFL_LevelFilter_mod
       type (LevelFilter) :: f
       type (Config), intent(in) :: dict
       
       integer :: min_level, max_level

       min_level = get_level('min_level')
       max_level = get_level('max_level')
 
       f = LevelFilter(min_level, max_level)

    contains
       
       integer function get_level(key) result(level)
          character(len=*), intent(in) :: key

          character(len=:), allocatable :: level_name
          logical :: found
          integer :: iostat

          level_name = dict%toString('max_level', found=found)
          if (.not. found) then
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
    function build_MpiFilter(dict, unusable, extra) result(f)
       use PFL_MpiFilter_mod
       type (MpiFilter) :: f
       type (Config), intent(in) :: dict
       class (KeywordEnforcer), optional, intent(in) :: unusable
       type (Config), optional, intent(in) :: extra
      
       character(len=:), allocatable :: comm_name
       integer :: comm
       integer :: rank, root, ierror

      _UNUSED_DUMMY(unusable)

       comm_name = dict%toString('comm', default='COMM_LOGGER')
       comm = get_communicator(comm_name, extra=extra)
       root = dict%toInteger('root:', default=0)
       f = MpiFilter(comm, root)
    end function build_MpiFilter
#endif
    

   subroutine build_handlers(this, handlersDict, unusable, extra)
      class (ConfigElements), intent(inout) :: this
      type (Config), intent(in) :: handlersDict
      class (KeywordEnforcer), optional, intent(in) :: unusable
      type (Config), optional, intent(in) :: extra

      type (ConfigIterator) :: iter
      type (Config), pointer :: cfg
      class (AbstractHandler), allocatable :: h

      iter = handlersDict%begin()
      do while (iter /= handlersDict%end())
         cfg => handlersDict%toConfigPtr(iter%key())
         call build_handler(h,cfg, this, extra=extra)
         call this%handlers%insert(iter%key(), h)
         deallocate(h)
         call iter%next()
      end do

   end subroutine build_handlers
   
   subroutine build_handler(h, handlerDict, elements, unusable, extra)
      class (AbstractHandler), allocatable, intent(out) :: h
      type (Config), intent(in) :: handlerDict
      type (ConfigElements), intent(in) :: elements
      class (KeywordEnforcer), optional, intent(in) :: unusable
      type (Config), optional, intent(in) :: extra

      _UNUSED_DUMMY(unusable)

      call allocate_concrete_handler(h, handlerDict)
      if (.not. allocated(h)) return

      call set_handler_level(h, handlerDict)
      call set_handler_formatter(h, handlerDict, elements%formatters)
      call set_handler_filters(h, handlerDict, elements%filters)
      call set_handler_lock(h, handlerDict, elements%locks)

   contains

      subroutine allocate_concrete_handler(h, handlerDict)
         class (AbstractHandler), allocatable, intent(out) :: h
         type (Config), intent(in) :: handlerDict

         character(len=:), allocatable :: className
              type (Filehandler) :: fh

         className = handlerDict%toString('class', default='unknown')

         select case (to_lower_case(className))
         case ('streamhandler')
            allocate(h, source=build_streamhandler(handlerDict))
         case ('filehandler')
              call build_filehandler(fh, handlerDict)
              allocate(h, source=fh)
#ifdef _LOGGER_USE_MPI              
         case ('mpifilehandler')
              call build_mpifilehandler(fh, handlerDict)
              allocate(h, source=fh)
#endif              
         case default
            call throw(__FILE__,__LINE__,"PFL::Config::build_handler() - unsupported class: '" // className //"'.")
         end select

      end subroutine allocate_concrete_handler

      subroutine set_handler_level(h, handlerDict)
         class (AbstractHandler), intent(inout) :: h
         type (Config), intent(in) :: handlerDict

         character(len=:), allocatable :: level_name
         integer :: level
         integer :: iostat

         level_name = handlerDict%toString('level', default='NOTSET')
         ! Try as integer
         read(level_name,*, iostat=iostat) level

         ! If that failed - interpret as a name from SeverityLevels.
         if (iostat /= 0) then
            level = name_to_level(level_name)
         end if

         call h%set_level(level)

      end subroutine set_handler_level
      

      subroutine set_handler_formatter(h, handlerDict, formatters)
         use PFL_Formatter_mod
         class (AbstractHandler), intent(inout) :: h
         type (Config), intent(in) :: handlerDict
         type (FormatterMap), target, intent(in) :: formatters

         character(len=:), allocatable :: formatterName
         logical :: found

         class (Formatter), pointer :: p_fmt

         _UNUSED_DUMMY(unusable)

         formatterName = handlerDict%toString('formatter', found=found)

         if (found) then  ! OK if no formatter
            if (formatters%count(formatterName) == 1) then
               p_fmt => formatters%at(formatterName)
               call h%set_formatter(formatters%at(formatterName))
            else
               call throw(__FILE__,__LINE__,"PFL::Config::build_handler() - formatter '"//formatterName//"' not found.")
            end if
         end if

      end subroutine set_handler_formatter


      subroutine set_handler_filters(h, handlerDict, filters)
         class (AbstractHandler), intent(inout) :: h
         type (Config), intent(in) :: handlerDict
         type (FilterMap), intent(in) :: filters

         character(len=:), allocatable :: filterNamesList ! '[ str1, str2, ..., strn ]'
         character(len=:), allocatable :: name
         logical :: found
         integer :: i, j, n

         filterNamesList = handlerDict%toString('filters', found=found)
         if (found) then

            n = len_trim(filterNamesList)
            if (filterNamesList(1:1) /= '[' .or. filterNamesList(n:n) /= ']') then
               call throw(__FILE__,__LINE__,"PFL::Config::build_handler() - filters is not of the form '[a,b,...,c]'")
               return
            end if

            i = 2
            do while (i < n)
               j = index(filterNamesList(i:n-1),',')
               if (j == 0) then
                  if (i < n-1) then
                     name = adjustl(trim(filterNamesList(i:n-1)))
                     i = n
                  end if
               else
                  name = adjustl(trim(filterNamesList(i:i + j-2)))
                  i = i + j
               end if
               block
                 class (AbstractFilter), pointer :: f
                 f => filters%at(name)
                 if (associated(f)) then
                    call h%add_filter(f)
                 else
                    call throw(__FILE__,__LINE__,"PFL::Config::build_handler() - unknown filter'"//name//"'.")
                 end if
               end block
            end do
            
         end if

      end subroutine set_handler_filters

      subroutine set_handler_lock(h, handlerDict, locks)
         class (AbstractHandler), intent(inout) :: h
         type (Config), intent(in) :: handlerDict
         type (LockMap), intent(in) :: locks

         character(len=:), allocatable :: lock_name
         logical :: found
         class (AbstractLock), pointer :: lock

         lock_name = handlerDict%toString('lock', found=found)
         if (found) then
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

   function build_streamhandler(handlerDict) result(h)
      use iso_fortran_env, only: OUTPUT_UNIT, ERROR_UNIT
      type (StreamHandler) :: h
      type (Config), intent(in) :: handlerDict

      character(len=:), allocatable :: unitName
      integer :: unit
      logical :: found
      integer :: iostat
      
      unitName = handlerDict%toString('unit', default='error_unit')

      ! is it an integer?
      read(unitName,*,iostat=iostat) unit

      ! if failed, maybe it is a defined name?
      if (iostat /= 0) then ! try as a string
         select case (to_lower_case(unitName))
         case ('output_unit','*')
            unit = OUTPUT_UNIT
         case ('error_unit')
            unit = ERROR_UNIT
         case default
            call throw(__FILE__,__LINE__,"PFL::Config::build_streamhandler() - unknown value for unit '"//unitName//"'.")
            return
         end select
      end if
      h = StreamHandler(unit)

   end function build_streamhandler


   subroutine build_filehandler(h, handlerDict)
      type (FileHandler), intent(out) :: h
      type (Config), intent(in) :: handlerDict

      character(len=:), allocatable :: fileName
      integer :: unit
      logical :: found
      integer :: iostat
      logical :: delay

      fileName = handlerDict%toString('filename', found=found)
      if (found) then
      else
         call throw(__FILE__,__LINE__,"PFL::Config::build_FileHandler() - must provide file name.")
         return
      end if

      delay = handlerDict%toLogical('delay', default=.false.)
      h = FileHandler(fileName, delay=delay)

      
   end subroutine build_filehandler

#ifdef _LOGGER_USE_MPI
   subroutine build_mpifilehandler(h, handlerDict, unusable, extra)
      use PFL_MpiCommConfig_mod

      type (FileHandler), intent(out) :: h
      type (Config), intent(in) :: handlerDict
      class (KeywordEnforcer), optional, intent(in) :: unusable
      type (Config), optional, intent(in) :: extra

      character(len=:), allocatable :: fileName
      integer :: unit
      logical :: found
      integer :: iostat
      integer :: comm
      integer, allocatable :: comms(:)
      integer :: i, j, n
      logical :: delay

      type (Map) :: commMap
      character(len=:), allocatable :: communicator_name_list, communicator_name, name
      
      fileName = handlerDict%toString('filename', found=found)
      if (found) then
      else
         call throw(__FILE__,__LINE__,"PFL::Config::build_MpiFileHandler() - must provide file name.")
         return
      end if

      communicator_name_list = handlerDict%toString('comms:', found=found)
      if (found) then
         allocate(comms(0))
         n = len_trim(communicator_name_list)
         if (communicator_name_list(1:1) /= '[' .or. communicator_name_list /= ']') then
            call throw(__FILE__,__LINE__,"PFL::Config::build_mpifilehandler() - misformed list of communicators.")
            return
         end if

         i = 2
         do while (i < n)
            j = index(communicator_name_list(i:n-1),',')
            if (j == 0) then
               if (i < n-1) then
                  name = communicator_name_list(i:n-1)
                  i = n
               end if
            else
               name = communicator_name_list(i:i+j-2)
               i = i + j
            end if
            
            comms = [comms, get_communicator(name)]
         end do

         block
           character(len=:), allocatable :: rank_prefix
           character(len=:), allocatable :: size_prefix
           rank_prefix = handlerDict%toString('rank_prefix', default='mpi_rank')
           size_prefix = handlerDict%toString('size_prefix', default='mpi_size')
           call commMap%deepcopy(MpiCommConfig(comms, rank_prefix=rank_prefix, size_prefix=size_prefix))
!!$           commMap = MpiCommConfig(comms, rank_prefix=rank_prefix, size_prefix=size_prefix)
         end block
      else
         communicator_name = handlerDict%toString('comm:', default='COMM_LOGGER')
         comm = get_communicator(communicator_name, extra=extra)
         block
           character(len=:), allocatable :: rank_prefix
           character(len=:), allocatable :: size_prefix
           rank_prefix = handlerDict%toString('rank_prefix', default='mpi_rank')
           size_prefix = handlerDict%toString('size_prefix', default='mpi_size')
           call commMap%deepCopy(MpiCommConfig(comm, rank_keyword=rank_prefix, size_keyword=size_prefix))
!!$           commMap = MpiCommConfig(comm, rank_keyword=rank_prefix, size_keyword=size_prefix)
         end block
      end if

      block
        use PFL_FormatString_mod
        fileName = formatString(filename, commMap)
      end block

      ! Note we have a different default for 'delay' for
      ! the MPI case.  This way we avoid creating lots of
      ! empty files in the usual case.
      delay = handlerDict%toLogical('delay', default=.true.)

      h = FileHandler(fileName, delay=delay)
      
   end subroutine build_mpifilehandler
#endif

   subroutine build_logger(lgr, loggerDict, elements, unusable, extra)
      use PFL_StringUtilities_mod, only: to_lower_case
      class (Logger), intent(inout) :: lgr
      type (Config), intent(in) :: loggerDict
      type (ConfigElements), intent(in) :: elements
      class (KeywordEnforcer), optional, intent(in) :: unusable
      type (Config), optional, intent(in) :: extra

      _UNUSED_DUMMY(unusable)
         
      call set_logger_level(lgr, loggerDict)
      call set_logger_propagate(lgr, loggerDict)
      call set_logger_filters(lgr, loggerDict, elements%filters)
      call set_logger_handlers(lgr, loggerDict, elements%handlers)

   contains

      subroutine set_logger_level(lgr, loggerDict)
         class (Logger), intent(inout) :: lgr
         type (Config), intent(in) :: loggerDict

         character(len=:), allocatable :: level_name
         integer :: level
         integer :: iostat
         logical :: found
#ifdef _LOGGER_USE_MPI
         integer :: root, rank, ierror, comm
         character(len=:), allocatable :: communicator_name
#endif
         
         level_name = loggerDict%toString('level', found=found,default='NOTSET')
#ifdef _LOGGER_USE_MPI
         communicator_name = loggerDict%toString('comm:', default='COMM_LOGGER')
         comm = get_communicator(communicator_name, extra=extra)

         root = loggerDict%toInteger('root:', default=0)
         call MPI_Comm_rank(comm, rank, ierror)
         if (rank == root) then
            level_name = loggerDict%toString('root_level', default=level_name)
            found=.true.
         else
            ! same as on other PEs
         end if
#endif
         if (found) then
            ! Try as integer
            read(level_name,*, iostat=iostat) level
            if (iostat /= 0) then
               level = name_to_level(level_name)
            end if
            call lgr%set_level(level)
         end if


      end subroutine set_logger_level
      

      subroutine set_logger_propagate(lgr, loggerDict)
         class (Logger), intent(inout) :: lgr
         type (Config), intent(in) :: loggerDict

         logical :: propagate
         integer :: level
         logical :: found

         propagate = loggerDict%toLogical('propagate', found=found)
         if (found) then
            call lgr%set_propagate(propagate)
         end if

      end subroutine set_logger_propagate
      

      subroutine set_logger_filters(lgr, loggerDict, filters, unusable, extra)
         class (Logger), intent(inout) :: lgr
         type (Config), intent(in) :: loggerDict
         type (FilterMap), intent(in) :: filters
         class (KeywordEnforcer), optional, intent(in) :: unusable
         type (Config), optional, intent(in) :: extra
      
         character(len=:), allocatable :: filterNamesList ! '[ str1, str2, ..., strn ]'
         character(len=:), allocatable :: name
         type (FilterIterator) :: iter
         integer :: i, j, n
         logical :: found

         _UNUSED_DUMMY(unusable)

         filterNamesList = loggerDict%toString('filters', found=found)
         if (found) then

            n = len_trim(filterNamesList)
            if (filterNamesList(1:1) /= '[' .or. filterNamesList(n:n) /= ']') then
               call throw(__FILE__,__LINE__,"PFL::Config::build_logger() - filters is not of the form '[a,b,...,c]'")
               return
            end if

            i = 2
            do while (i < n)
               j = index(filterNamesList(i:n-1),',')
               if (j == 0) then
                  if (i < n-1) then
                     name = adjustl(trim(filterNamesList(i:n-1)))
                     i = n
                  end if
               else
                  name = adjustl(trim(filterNamesList(i:i+j-2)))
                  i = i + j
               end if
               iter = filters%find(name)
               if (iter /= filters%end()) then
                  call lgr%add_filter(iter%value())
               else
                  call throw(__FILE__,__LINE__,"PFL::Config::build_logger() - unknown filter'"//name//"'.")
               end if
            end do
            
         end if

      end subroutine set_logger_filters


      subroutine set_logger_handlers(lgr, loggerDict, handlers, unusable, extra)
         class (Logger), intent(inout) :: lgr
         type (Config), intent(in) :: loggerDict
         type (HandlerMap), intent(in) :: handlers
         class (KeywordEnforcer), optional, intent(in) :: unusable
         type (Config), optional, intent(in) :: extra

         character(len=:), allocatable :: handlerNamesList ! '[ str1, str2, ..., strn ]'
         character(len=:), allocatable :: name
         type (HandlerIterator) :: iter
         integer :: i, j, n

         logical :: found
         class (AbstractHandler), pointer :: h


         _UNUSED_DUMMY(unusable)

         handlerNamesList = loggerDict%toString('handlers', found=found)
         if (found) then
            n = len_trim(handlerNamesList)
            if (handlerNamesList(1:1) /= '[' .or. handlerNamesList(n:n) /= ']') then
               call throw(__FILE__,__LINE__,"PFL::Config::build_logger() - handlers is not of the form '[a,b,...,c]'")
               return
            end if

            i = 2
            do while (i < n)
               j = index(handlerNamesList(i:n-1),',')
               if (j == 0) then
                  if (i < n-1) then
                     name = handlerNamesList(i:n-1)
                     i = n
                  end if
               else
                  name = handlerNamesList(i:i+j-2)
                  i = i + j
               end if

               iter = handlers%find(name)
               if (iter /= handlers%end()) then
                  h => iter%value()
                  call lgr%add_handler(h)
               else
                  call throw(__FILE__,__LINE__,"PFL::Config::build_logger() - unknown handler'"//name//"'.")
               end if
            end do
            
         end if

      end subroutine set_logger_handlers


   end subroutine build_logger

#ifdef _LOGGER_USE_MPI
   integer function get_communicator(name, extra) result(comm)
      character(len=*), intent(in) :: name
      type (Config), optional, intent(in) :: extra

      character(len=:), allocatable :: name_
      logical :: found
      
      name_ = name
      if (name == 'MPI_COMM_WORLD') then
         name_ = 'COMM_LOGGER'
      end if

      if (present(extra)) then
         comm = extra%toInteger(name_, found=found, default=MPI_COMM_NULL)
         if (.not. found) then
            if (name == 'MPI_COMM_WORLD' .or. name == 'COMM_LOGGER') then
               comm = MPI_COMM_WORLD
            else
               call throw(__FILE__,__LINE__,'PFL::Config::build_logger() - MPI communicator ' // name // ' not found.')
            end if
         end if
      else
         comm = MPI_COMM_WORLD
      end if

   end function get_communicator
#endif


   ! Including a version number is crucial for providing non-backward
   ! compatible updates in the future.
   subroutine check_schema_version(dict)
      type (Config), intent(in) :: dict

      integer :: version
      logical :: found

      version = dict%toInteger('schema_version', found=found)
      if (found) then
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


end module PFL_Config_mod
