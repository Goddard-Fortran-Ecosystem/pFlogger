! Singleton pattern for now
module ASTG_Config_mod
   use FTL
   use ASTG_Logger_mod
   use ASTG_Exception_mod
   use ASTG_SeverityLevels_mod
   use ASTG_StringAbstractLoggerPolyMap_mod
   use ASTG_StringFilterMap_mod
   use ASTG_StringHandlerMap_mod
   use ASTG_StringFormatterMap_mod

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
      type (FilterMap) :: filters
      type (FormatterMap) :: formatters
      type (HandlerMap) :: handlers
   contains
      procedure :: build_filters
      procedure :: build_formatters
      procedure :: build_handlers

      ! accessors
      procedure :: get_filters
      procedure :: get_formatters
      procedure :: get_handlers
   end type ConfigElements

   type Unusable
   end type Unusable

contains

   subroutine build_formatters(this, formattersDict, unused, extra)
      use ftl_StringUnlimitedPolyMap_mod, only: ConfigIterator
      class (ConfigElements), intent(inout) :: this
      type (Config), intent(in) :: formattersDict
      type (Unusable), optional, intent(in) :: unused
      type (Config), optional, intent(in) :: extra

      type (ConfigIterator) :: iter
      type (Config), pointer :: cfg

      iter = formattersDict%begin()
      do while (iter /= formattersDict%end())
         cfg => formattersDict%toConfigPtr(iter%key())
         call this%formatters%insert(iter%key(), build_formatter(cfg))
         call iter%next()
      end do

   end subroutine build_formatters


   function build_formatter(dict) result(fmtr)
      use ASTG_Formatter_mod
      use ASTG_AbstractHandler_mod
      class (Formatter), allocatable :: fmtr
      type (Config), intent(in) :: dict

      character(len=:), allocatable :: fmt
      character(len=:), allocatable :: datefmt
      logical :: found

      fmt = dict%toString('fmt', found=found)
      
      if (found) then
         datefmt = dict%toString('datefmt', found=found)
         if (found) then
            allocate(fmtr,source=Formatter(fmt, datefmt=datefmt))
         else
            allocate(fmtr,source=Formatter(fmt))
         end if
      else
         allocate(fmtr, source=Formatter())
      end if

   end function build_formatter


   subroutine build_filters(this, filtersDict, unused, extra)
      use ASTG_Filter_mod
      use ftl_StringUnlimitedPolyMap_mod, only: ConfigIterator
      class (ConfigElements), intent(inout) :: this
      type (Config), intent(in) :: filtersDict
      type (Unusable), optional, intent(in) :: unused
      type (Config), optional, intent(in) :: extra

      type (ConfigIterator) :: iter
      type (Config), pointer :: cfg
      type (Filter) :: f

      iter = filtersDict%begin()
      do while (iter /= filtersDict%end())
         cfg => filtersDict%toConfigPtr(iter%key())
         call this%filters%insert(iter%key(), build_filter(cfg))
         call iter%next()
      end do

   end subroutine build_filters


   function build_filter(dict) result(f)
      use ASTG_Filter_mod
      type (Filter) :: f
      type (Config), intent(in) :: dict

      character(len=:), allocatable :: name
      logical :: found

      name = dict%toString('name', found=found)
      if (found) then
         f = Filter(name)
      else
         call throw('FTL::Config::build_filter() - empty list of filters or nameless filter.')
      end if

   end function build_filter

   subroutine build_handlers(this, handlersDict, unused, extra)
      use ftl_StringUnlimitedPolyMap_mod, only: ConfigIterator
      use ASTG_AbstractHandler_mod
      class (ConfigElements), intent(inout) :: this
      type (Config), intent(in) :: handlersDict
      type (Unusable), optional, intent(in) :: unused
      type (Config), optional, intent(in) :: extra

      type (ConfigIterator) :: iter
      type (Config), pointer :: cfg

      iter = handlersDict%begin()
      do while (iter /= handlersDict%end())
         cfg => handlersDict%toConfigPtr(iter%key())
         call this%handlers%insert(iter%key(), &
              & build_handler(cfg, this, extra=extra))
         call iter%next()
      end do

!!$   end function build_handlers
   end subroutine build_handlers
   
   function build_handler(handlerDict, elements, unused, extra) result(h)
      use ASTG_AbstractHandler_mod
      use ASTG_StringUtilities_mod, only: toLowerCase
      use ASTG_Filter_mod
      class (AbstractHandler), allocatable :: h
      type (Config), intent(in) :: handlerDict
      type (ConfigElements), intent(in) :: elements
      type (Unusable), optional, intent(in) :: unused
      type (Config), optional, intent(in) :: extra


      call allocate_concrete_handler(h, handlerDict)
      if (.not. allocated(h)) return

      call set_handler_level(h, handlerDict)
      call set_handler_formatter(h, handlerDict, elements%formatters)
      call set_handler_filters(h, handlerDict, elements%filters)

   contains

      subroutine allocate_concrete_handler(h, handlerDict)
         use ASTG_Filehandler_mod
         class (AbstractHandler), allocatable, intent(out) :: h
         type (Config), intent(in) :: handlerDict

         character(len=:), allocatable :: className
              type (Filehandler) :: fh

         className = handlerDict%toString('class', default='unknown')

         select case (toLowerCase(className))
         case ('streamhandler')
            allocate(h, source=build_streamhandler(handlerDict))
         case ('filehandler')
              call build_filehandler(fh, handlerDict)
              allocate(h, source=fh)
#ifdef LOGGER_USE_MPI              
         case ('mpifilehandler')
              call build_mpifilehandler(fh, handlerDict)
              allocate(h, source=fh)
#endif              
         case default
            call throw("ASTG::Config::build_handler() - unsupported class: '" // className //"'.")
         end select

      end subroutine allocate_concrete_handler

      subroutine set_handler_level(h, handlerDict)
         class (AbstractHandler), intent(inout) :: h
         type (Config), intent(in) :: handlerDict

         character(len=:), allocatable :: levelName
         integer :: level
         integer :: iostat

         levelName = handlerDict%toString('level', default='NOTSET')
         ! Try as integer
         read(levelName,*, iostat=iostat) level

         ! If that failed - interpret as a name from SeverityLevels.
         if (iostat /= 0) then
            level = nameToLevel(levelName)
         end if

         call h%setLevel(level)

      end subroutine set_handler_level
      

      subroutine set_handler_formatter(h, handlerDict, formatters)
         class (AbstractHandler), intent(inout) :: h
         type (Config), intent(in) :: handlerDict
         type (FormatterMap), intent(in) :: formatters

         character(len=:), allocatable :: formatterName
         logical :: found

         formatterName = handlerDict%toString('formatter', found=found)

         if (found) then  ! OK if no formatter
            if (formatters%count(formatterName) == 1) then
               call h%setFormatter(formatters%at(formatterName))
            else
               call throw("ASTG::Config::build_handler() - formatter '"//formatterName//"' not found.")
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
               call throw("FTL::Config::build_handler() - filters is not of the form '[a,b,...,c]'")
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
                 class (Filter), pointer :: f
                 f => filters%at(name)
                 if (associated(f)) then
                    call h%addFilter(f)
                 else
                    call throw("FTL::Config::build_handler() - unknown filter'"//name//"'.")
                 end if
               end block
            end do
            
         end if

      end subroutine set_handler_filters


   end function build_handler

   function build_streamhandler(handlerDict) result(h)
      use ASTG_StreamHandler_mod
      use ASTG_StringUtilities_mod, only: toLowerCase
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
         select case (toLowerCase(unitName))
         case ('output_unit','*')
            unit = OUTPUT_UNIT
         case ('error_unit')
            unit = ERROR_UNIT
         case default
            call throw("ASTG::Config::build_streamhandler() - unknown value for unit '"//unitName//"'.")
            return
         end select
      end if

      h = StreamHandler(unit)

   end function build_streamhandler

   subroutine build_filehandler(h, handlerDict)
      use ASTG_FileHandler_mod
      use ASTG_StringUtilities_mod, only: toLowerCase
      type (FileHandler), intent(out) :: h
      type (Config), intent(in) :: handlerDict

      character(len=:), allocatable :: fileName
      integer :: unit
      logical :: found
      integer :: iostat

      fileName = handlerDict%toString('filename', found=found)
      if (found) then
      else
         call throw("ASTG::Config::build_FileHandler() - must provide file name.")
         return
      end if

      h = FileHandler(fileName)

   end subroutine build_filehandler

#ifdef LOGGER_USE_MPI
   subroutine build_mpifilehandler(h, handlerDict, unused, extra)
      use ASTG_FileHandler_mod
      use ASTG_StringUtilities_mod, only: toLowerCase
      use ASTG_MpiCommConfig_mod
      use ASTG_StringUnlimitedMap_mod, only: Map
      use mpi
      type (FileHandler), intent(out) :: h
      type (Config), intent(in) :: handlerDict
      type (Unusable), optional, intent(in) :: unused
      type (Config), optional, intent(in) :: extra

      character(len=:), allocatable :: fileName
      integer :: unit
      logical :: found
      integer :: iostat
      integer :: comm
      integer, allocatable :: comms(:)
      integer :: i, j, n

      type (Map) :: commMap
      character(len=:), allocatable :: communicator_name_list, communicator_name, name
      
      fileName = handlerDict%toString('filename', found=found)
      if (found) then
      else
         call throw("ASTG::Config::build_MpiFileHandler() - must provide file name.")
         return
      end if

      communicator_name_list = handlerDict%toString('comms:', found=found)
      if (found) then
         allocate(comms(0))
         n = len_trim(communicator_name_list)
         if (communicator_name_list(1:1) /= '[' .or. communicator_name_list /= ']') then
            call throw("ASTG::Config::build_mpifilehandler() - misformed list of communicators.")
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
            case ('MPI_COMM_WORLD')
               comms = [comms, MPI_COMM_WORLD]
            case default
               if (extra%count(name) == 1) then
                  comms = [comms, extra%toInteger(name)]
               else
                  call throw("ASTG::Config::build_mpifilehandler() - unknown communicator '"//name//"'.")
                  return
               end if
            end select
         end do

         block
           character(len=:), allocatable :: rank_prefix
           character(len=:), allocatable :: size_prefix
           rank_prefix = handlerDict%toString('rank_prefix', default='mpi_rank')
           size_prefix = handlerDict%toString('size_prefix', default='mpi_size')
           commMap = MpiCommConfig(comms, rank_prefix=rank_prefix, size_prefix=size_prefix)
         end block
      else
         communicator_name = handlerDict%toString('comm:', default='MPI_COMM_WORLD')
         select case (communicator_name)
         case ('MPI_COMM_WORLD')
            comm = MPI_COMM_WORLD
         case default
            comm = extra%toInteger(communicator_name)
         end select
         block
           character(len=:), allocatable :: rank_prefix
           character(len=:), allocatable :: size_prefix
           rank_prefix = handlerDict%toString('rank_prefix', default='mpi_rank')
           size_prefix = handlerDict%toString('size_prefix', default='mpi_size')
           commMap = MpiCommConfig(comm, rank_keyword=rank_prefix, size_keyword=size_prefix)
         end block
      end if

      block
        use ASTG_FormatString_mod
        fileName = formatString(filename, commMap)
      end block

      h = FileHandler(fileName)
      
   end subroutine build_mpifilehandler
#endif

   subroutine build_logger(lgr, loggerDict, elements, unused, extra)
      use ASTG_AbstractHandler_mod
      use ASTG_StringUtilities_mod, only: toLowerCase
      type (Logger), intent(inout) :: lgr
      type (Config), intent(in) :: loggerDict
      type (ConfigElements), intent(in) :: elements
      type (Unusable), optional, intent(in) :: unused
      type (Config), optional, intent(in) :: extra

      call set_logger_level(lgr, loggerDict)
      call set_logger_propagate(lgr, loggerDict)
      call set_logger_filters(lgr, loggerDict, elements%filters)
      call set_logger_handlers(lgr, loggerDict, elements%handlers)

   contains

      subroutine set_logger_level(lgr, loggerDict)
         class (Logger), intent(inout) :: lgr
         type (Config), intent(in) :: loggerDict

         character(len=:), allocatable :: levelName
         integer :: level
         integer :: iostat
         logical :: found

         levelName = loggerDict%toString('level', found=found)
         if (found) then
            ! Try as integer
            read(levelName,*, iostat=iostat) level
            if (iostat /= 0) then
               level = nameToLevel(levelName)
            end if
            call lgr%setLevel(level)
         else
            ! leave as default level
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
            call lgr%setPropagate(propagate)
         end if

      end subroutine set_logger_propagate
      

      subroutine set_logger_filters(lgr, loggerDict, filters, unused, extra)
         class (Logger), intent(inout) :: lgr
         type (Config), intent(in) :: loggerDict
         type (FilterMap), intent(in) :: filters
         type (Unusable), optional, intent(in) :: unused
         type (Config), optional, intent(in) :: extra
      
         character(len=:), allocatable :: filterNamesList ! '[ str1, str2, ..., strn ]'
         character(len=:), allocatable :: name
         type (FilterIterator) :: iter
         integer :: i, j, n
         logical :: found

         filterNamesList = loggerDict%toString('filters', found=found)
         if (found) then

            n = len_trim(filterNamesList)
            if (filterNamesList(1:1) /= '[' .or. filterNamesList(n:n) /= ']') then
               call throw("Config::build_logger() - filters is not of the form '[a,b,...,c]'")
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
                  call lgr%addFilter(iter%value())
               else
                  call throw("Config::build_logger() - unknown filter'"//name//"'.")
               end if
            end do
            
         end if

      end subroutine set_logger_filters


      subroutine set_logger_handlers(lgr, loggerDict, handlers, unused, extra)
#ifdef LOGGER_USE_MPI
         use mpi
#endif
         class (Logger), intent(inout) :: lgr
         type (Config), intent(in) :: loggerDict
         type (HandlerMap), intent(in) :: handlers
         type (Unusable), optional, intent(in) :: unused
         type (Config), optional, intent(in) :: extra

         character(len=:), allocatable :: handlerNamesList ! '[ str1, str2, ..., strn ]'
         character(len=:), allocatable :: name
         type (HandlerIterator) :: iter
         integer :: i, j, n

         logical :: found

#ifdef LOGGER_USE_MPI
         block
           logical :: parallel
           character(len=:), allocatable :: communicator_name
           integer :: comm, rank, ier

           parallel = loggerDict%toLogical('parallel', default=.false.)
           if (.not. parallel) then
              communicator_name = loggerDict%toString('comm:', default='MPI_COMM_WORLD')
              select case (communicator_name)
              case ('MPI_COMM_WORLD')
                 comm = MPI_COMM_WORLD
              case default
                 if (present(extra)) then
                    comm = extra%toInteger(communicator_name, found=found)
                 else
                    call throw('ASTG::Config::build_logger() - MPI communicator not found.')
                    return
                 end if
              end select
              call MPI_Comm_rank(comm, rank, ier)
              if (rank /= 0) return
           end if
         end block
#endif

         handlerNamesList = loggerDict%toString('handlers', found=found)
         if (found) then
            n = len_trim(handlerNamesList)
            if (handlerNamesList(1:1) /= '[' .or. handlerNamesList(n:n) /= ']') then
               call throw("ASTG::Config::build_logger() - handlers is not of the form '[a,b,...,c]'")
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
                  call lgr%addHandler(iter%value())
               else
                  call throw("Config::build_logger() - unknown handler'"//name//"'.")
               end if
            end do
            
         end if

      end subroutine set_logger_handlers


   end subroutine build_logger


   ! Including a version number is crucial for providing non-backward
   ! compatible updates in the future.
   subroutine check_schema_version(dict)
      type (Config), intent(in) :: dict

      integer :: version
      logical :: found

      version = dict%toInteger('schema_version', found=found)
      if (found) then
         if (version /= 1) then
            call throw('ASTG::Config::check_schema_version() -' // &
                 & ' unsupported schema_version. Allowed values are [1].')
            return
         end if
      else
         call throw('ASTG::Config::check_schema_version() -' // &
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
   
end module ASTG_Config_mod
