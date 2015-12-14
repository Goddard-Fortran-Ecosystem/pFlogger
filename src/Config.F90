! Singleton pattern for now
module ASTG_Config_mod
   use FTL
   use ASTG_LoggerManager_mod
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
   public :: Config
   public :: load_file
   public :: dictConfig

   ! Remaining procedures are public just for testing purposes.
   public :: build_formatters
   public :: build_formatter
   public :: build_filters
   public :: build_filter

   public :: build_streamhandler
   public :: build_handler
   public :: build_handlers

   public :: build_logger
   public :: P

   type Unusable
   end type Unusable

contains

   subroutine dictConfig(dict, unused, extra)
      type (Config), intent(in) :: dict
      type (Unusable), optional, intent(in) :: unused
      type (Config), optional, intent(in) :: extra

      type (FilterMap) :: filters
      type (FormatterMap) :: formatters
      type (HandlerMap) :: handlers

      logical :: found

      call check_schema_version(dict)

      block
        type (Config), pointer :: filtersCfg
        filtersCfg => dict%toConfigPtr('filters', found=found)
        if (found) filters = build_filters(filtersCfg)
      end block

      block
        type (Config), pointer :: formattersCfg
        formattersCfg => dict%toConfigPtr('formatters', found=found)
        if (found) formatters = build_formatters(formattersCfg)
      end block
      
      block
        type (Config), pointer :: handlersCfg
        handlersCfg => dict%toConfigPtr('handlers', found=found)
        if (found) then
           call build_handlers(handlers, handlersCfg, formatters, filters)
        end if
      end block

      call create_loggers(dict, filters, handlers, extra=extra)

   end subroutine dictConfig


   function build_formatters(formattersDict) result(formatters)
      use ftl_StringUnlimitedPolyMap_mod, only: ConfigIterator
      type (FormatterMap) :: formatters
      type (Config), intent(in) :: formattersDict

      type (ConfigIterator) :: iter
      type (Config), pointer :: cfg

      iter = formattersDict%begin()
      do while (iter /= formattersDict%end())
         cfg => formattersDict%toConfigPtr(iter%key())
         call formatters%insert(iter%key(), build_formatter(cfg))
         call iter%next()
      end do

   end function build_formatters


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


   function build_filters(filtersDict) result(filters)
      use ASTG_Filter_mod
      use ftl_StringUnlimitedPolyMap_mod, only: ConfigIterator
      type (FilterMap) :: filters
      type (Config), intent(in) :: filtersDict

      type (ConfigIterator) :: iter
      type (Config), pointer :: cfg
      type (Filter) :: f

      iter = filtersDict%begin()
      do while (iter /= filtersDict%end())
         cfg => filtersDict%toConfigPtr(iter%key())
         call filters%insert(iter%key(), build_filter(cfg))
         call iter%next()
      end do

   end function build_filters


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

!!$   function build_handlers(handlersDict, formatters, filters) result(handlers)
   subroutine build_handlers(handlers, handlersDict, formatters, filters)
      use ftl_StringUnlimitedPolyMap_mod, only: ConfigIterator
      use ASTG_AbstractHandler_mod
      type (HandlerMap) :: handlers
      type (Config), intent(in) :: handlersDict
      type (FormatterMap), intent(in) :: formatters
      type (FilterMap), intent(in) :: filters

      type (ConfigIterator) :: iter
      type (Config), pointer :: cfg
      class (AbstractHandler), allocatable :: h

      iter = handlersDict%begin()
      do while (iter /= handlersDict%end())
         cfg => handlersDict%toConfigPtr(iter%key())
         call handlers%insert(iter%key(), build_handler(cfg, formatters, filters))
         call iter%next()
      end do

!!$   end function build_handlers
   end subroutine build_handlers
   
   function build_handler(handlerDict, formatters, filters) result(h)
      use ASTG_AbstractHandler_mod
      use ASTG_StringUtilities_mod, only: toLowerCase
      use ASTG_Filter_mod
      class (AbstractHandler), allocatable :: h
      type (Config), intent(in) :: handlerDict
      type (FormatterMap), intent(in) :: formatters
      type (FilterMap), intent(in) :: filters


      call allocate_concrete_handler(h, handlerDict)
      if (.not. allocated(h)) return

      call set_handler_level(h, handlerDict)

      call set_handler_formatter(h, handlerDict, formatters)

      call set_handler_filters(h, handlerDict, filters)

   contains

      subroutine allocate_concrete_handler(h, handlerDict)
         use ASTG_Filehandler_mod
         class (AbstractHandler), allocatable, intent(out) :: h
         type (Config), intent(in) :: handlerDict

         character(len=:), allocatable :: className

         className = handlerDict%toString('class', default='unknown')

         select case (toLowerCase(className))
         case ('streamhandler')
            allocate(h, source=build_streamhandler(handlerDict))
         case ('filehandler')
            block
              type (Filehandler) :: fh
              call build_filehandler(fh, handlerDict)
              allocate(h, source=fh)
            end block
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

!!$   function build_filehandler(handlerDict)
   subroutine build_filehandler(h, handlerDict)
      use ASTG_FileHandler_mod
      use ASTG_StringUtilities_mod, only: toLowerCase
      use iso_fortran_env, only: OUTPUT_UNIT, ERROR_UNIT
      type (FileHandler) :: h
      type (Config), intent(in) :: handlerDict

      character(len=:), allocatable :: fileName
      integer :: unit
      logical :: found
      integer :: iostat

      fileName = handlerDict%toString('name', found=found)

      if (found) then
      else
         call throw("ASTG::Config::build_FileHandler() - must provide file name.")
         return
      end if

      h = FileHandler(fileName)

!!$   end function build_filehandler
   end subroutine build_filehandler

   subroutine build_logger(name, loggerDict, filters, handlers, unused, extra)
      use ASTG_AbstractHandler_mod
      use ASTG_StringUtilities_mod, only: toLowerCase
      character(len=*), intent(in) :: name
      type (Config), intent(in) :: loggerDict
      type (FilterMap), intent(in) :: filters
      type (HandlerMap), intent(in) :: handlers
      type (Unusable), optional, intent(in) :: unused
      type (Config), optional, intent(in) :: extra

      type (Logger), pointer :: lgr

      lgr => logging%getLogger(name)

      call set_logger_level(lgr, loggerDict)
      call set_logger_filters(lgr, loggerDict, filters)
      call set_logger_handlers(lgr, loggerDict, handlers)



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
                  name = handlerNamesList(i:j-1)
                  i = j + 1
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


   subroutine create_loggers(dict, filters, handlers, unused, extra)
      use ftl_StringUnlimitedPolyMap_mod, only: ConfigIterator
      type (Config), intent(in) :: dict
      type (FilterMap) :: filters
      type (HandlerMap) :: handlers
      type (Unusable), optional, intent(in) :: unused
      type (Config), optional, intent(in) :: extra

      type (Config), pointer :: mPtr1, mPtr2
      
      logical :: found
      type (ConfigIterator) :: iter

      mPtr1 => dict%toConfigPtr('loggers', found=found)

      if (found) then
         ! Loop over contained loggers
         iter = mPtr1%begin()
         do while (iter /= mPtr1%end())
            mPtr2 => mPtr1%toConfigPtr(iter%key())
            call addLogger(iter%key(), mPtr2, dict)
            call iter%next()
         end do
      end if

   end subroutine create_loggers


   subroutine addLogger(name, args, dict)
      character(len=*), intent(in) :: name
      class (Config), intent(in) :: args
      class (Config), intent(in) :: dict

      class (Logger), pointer :: lgr
      integer :: level
      integer :: iostat
      character(len=:), allocatable :: levelName
      logical :: found
      
      lgr => logging%getLogger(name)

      levelName = args%toString('level', found=found)
      if (found) then
         ! Try as integer
         read(levelName,*, iostat=iostat) level
         if (iostat /= 0) then
            level = nameToLevel(levelName)
         end if
         call lgr%setLevel(level)
      end if

   end subroutine addLogger


   ! Including a version number is crucial for providing non-backward
   ! compatible updates in the future.
   subroutine check_schema_version(dict)
      type (Config), intent(in) :: dict

      integer :: version
      logical :: found

      version = dict%toInteger('schema_version', found=found)
      if (found) then
         if (version /= 1) then
            call throw('ASTG::Config::dictConfig() - unsupported schema_version. Allowed values are [1].')
            return
         end if
      else
         call throw('ASTG::Config::dictConfig() - must specify a schema_version for Config.')
      end if

   end subroutine check_schema_version



end module ASTG_Config_mod
