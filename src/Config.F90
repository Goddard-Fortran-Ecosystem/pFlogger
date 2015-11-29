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

   public :: dictConfig
   public :: build_formatters
   public :: build_formatter
   public :: build_filters
   public :: build_filter

   public :: build_streamhandler
   public :: build_handler
   public :: build_handlers

   public :: build_logger

contains

   subroutine dictConfig(dict)
      type (Config), intent(in) :: dict

      type (FormatterMap) :: formatters

      call check_schema_version(dict)

!!$      formatters = build_formatters(dict)
!!$      loggers = build_loggers(dict)
!!$      call add_loggers(dict)

      call create_loggers(dict)

   end subroutine dictConfig


   function build_formatters(formattersDict) result(formatters)
      use ftl_StringUnlimitedPolyMap_mod, only: ConfigIterator
      type (FormatterMap) :: formatters
      type (Config), intent(in) :: formattersDict

      type (ConfigIterator) :: iter
      type (Config), pointer :: cfg

      iter = formattersDict%begin()
      do while (iter /= formattersDict%end())
         cfg => toMap(formattersDict, iter%key())
         call formatters%insert(iter%key(), build_formatter(cfg))
         call iter%next()
      end do

   end function build_formatters


   function build_formatter(dict) result(fmtr)
      use ASTG_Formatter_mod
      use ASTG_AbstractHandler_mod
      class (Formatter), allocatable :: fmtr
      type (Config), intent(in) :: dict
      character(len=:), pointer :: fmt
      character(len=:), pointer :: datefmt

      fmt => toString(dict, 'fmt')
      datefmt => toString(dict, 'datefmt')
      
      if (associated(fmt)) then
         if (associated(datefmt)) then
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
         cfg => toMap(filtersDict, iter%key())
         call filters%insert(iter%key(), build_filter(cfg))
         call iter%next()
      end do

   end function build_filters

   function build_filter(dict) result(f)
      use ASTG_Filter_mod
      type (Filter) :: f
      type (Config), intent(in) :: dict
      character(len=:), pointer :: name

      name => toString(dict, 'name', require=.true.)
      if (associated(name)) then
         f = Filter(name)
      end if

   end function build_filter

   function build_handlers(handlersDict, formatters, filters) result(handlers)
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
         cfg => toMap(handlersDict, iter%key())
         call handlers%insert(iter%key(), build_handler(cfg, formatters, filters))
         call iter%next()
      end do

   end function build_handlers
   
   function build_handler(handlerDict, formatters, filters) result(h)
      use ASTG_AbstractHandler_mod
      use ASTG_StringUtilities_mod, only: toLowerCase
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
         class (AbstractHandler), allocatable, intent(out) :: h
         type (Config), intent(in) :: handlerDict

         character(len=:), pointer :: classNamePtr

         classNamePtr => toString(handlerDict, 'class', require=.true.)
         if (associated(classNamePtr)) then
            select case (toLowerCase(classNamePtr))
            case ('streamhandler')
               allocate(h, source=build_streamhandler(handlerDict, formatters, filters))
            case default
               call throw("Config::build_handler() - unsupported class: '" // classNamePtr //"'.")
            end select
         end if

      end subroutine allocate_concrete_handler

      subroutine set_handler_level(h, handlerDict)
         class (AbstractHandler), intent(inout) :: h
         type (Config), intent(in) :: handlerDict

         character(len=:), pointer :: levelNamePtr
         integer :: level
         integer :: iostat

         levelNamePtr => toString(handlerDict, 'level')
         if (associated(levelNamePtr)) then
            ! Try as integer
            read(levelNamePtr,*, iostat=iostat) level
            if (iostat /= 0) then
               level = nameToLevel(levelNamePtr)
            end if

            call h%setLevel(level)
         end if

      end subroutine set_handler_level
      

      subroutine set_handler_formatter(h, handlerDict, formatters)
         class (AbstractHandler), intent(inout) :: h
         type (Config), intent(in) :: handlerDict
         type (FormatterMap), intent(in) :: formatters

         character(len=:), pointer :: formatterNamePtr
         type (FormatterIterator) :: iter

         formatternameptr => toString(handlerDict, 'formatter')
         if (associated(formatterNamePtr)) then
            iter = formatters%find(formatterNamePtr)
            if (iter /= formatters%end()) then
               call h%setFormatter(iter%value())
            end if
         end if

      end subroutine set_handler_formatter


      subroutine set_handler_filters(h, handlerDict, filters)
         class (AbstractHandler), intent(inout) :: h
         type (Config), intent(in) :: handlerDict
         type (FilterMap), intent(in) :: filters

         character(len=:), pointer :: filterNamesList ! '[ str1, str2, ..., strn ]'
         character(len=:), allocatable :: name
         type (FilterIterator) :: iter
         integer :: i, j, n

         filterNamesList => toString(handlerDict, 'filters')
         if (associated(filterNamesList)) then

            n = len_trim(filterNamesList)
            if (filterNamesList(1:1) /= '[' .or. filterNamesList(n:n) /= ']') then
               call throw("Config::build_handler() - filters is not of the form '[a,b,...,c]'")
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
               iter = filters%find(name)
               if (iter /= filters%end()) then
                  call h%addFilter(iter%value())
               else
                  call throw("Config::build_handler() - unknown filter'"//name//"'.")
               end if
            end do
            
         end if

      end subroutine set_handler_filters


   end function build_handler


   function build_streamhandler(handlerDict, formatters, filters) result(h)
      use ASTG_StreamHandler_mod
      use ASTG_StringUtilities_mod, only: toLowerCase
      use iso_fortran_env, only: OUTPUT_UNIT, ERROR_UNIT
      type (StreamHandler) :: h
      type (Config), intent(in) :: handlerDict
      type (FormatterMap), intent(in) :: formatters
      type (FilterMap), intent(in) :: filters

      integer, pointer :: unitPtr
      integer :: unit
      character(len=:), pointer :: unitNamePtr
      
      unitNamePtr => toString(handlerDict, 'unit')
      if (associated(unitNamePTr)) then
         select case (toLowerCase(unitNamePtr))
         case ('output_unit')
            unit = OUTPUT_UNIT
         case ('error_unit')
            unit = ERROR_UNIT
         case default
            call throw("Config::build_streamhandler() - unknown value for unit '"//unitNamePtr//"'.")
            return
         end select
         h = StreamHandler(unit)
      else
         unitPtr => toInteger(handlerDict, 'unit')
         if (associated(unitPtr)) then
            h = StreamHandler(unit)
         else
            h = StreamHandler()
         end if
      end if

   end function build_streamhandler


   subroutine build_logger(name, loggerDict, filters, handlers)
      use ASTG_AbstractHandler_mod
      use ASTG_StringUtilities_mod, only: toLowerCase
      character(len=*), intent(in) :: name
      type (Config), intent(in) :: loggerDict
      type (FilterMap), intent(in) :: filters
      type (HandlerMap), intent(in) :: handlers

      type (Logger), pointer :: lgr

      lgr => logging%getLogger(name)

      call set_logger_level(lgr, loggerDict)
      call set_logger_filters(lgr, loggerDict, filters)
      call set_logger_handlers(lgr, loggerDict, handlers)



   contains

      subroutine set_logger_level(lgr, loggerDict)
         class (Logger), intent(inout) :: lgr
         type (Config), intent(in) :: loggerDict

         character(len=:), pointer :: levelNamePtr
         integer :: level
         integer :: iostat

         levelNamePtr => toString(loggerDict, 'level')
         if (associated(levelNamePtr)) then
            ! Try as integer
            read(levelNamePtr,*, iostat=iostat) level
            if (iostat /= 0) then
               level = nameToLevel(levelNamePtr)
            end if

            call lgr%setLevel(level)
         end if

      end subroutine set_logger_level
      

      subroutine set_logger_filters(lgr, loggerDict, filters)
         class (Logger), intent(inout) :: lgr
         type (Config), intent(in) :: loggerDict
         type (FilterMap), intent(in) :: filters

         character(len=:), pointer :: filterNamesList ! '[ str1, str2, ..., strn ]'
         character(len=:), allocatable :: name
         type (FilterIterator) :: iter
         integer :: i, j, n

         filterNamesList => toString(loggerDict, 'filters')
         if (associated(filterNamesList)) then

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


      subroutine set_logger_handlers(lgr, loggerDict, handlers)
         class (Logger), intent(inout) :: lgr
         type (Config), intent(in) :: loggerDict
         type (HandlerMap), intent(in) :: handlers

         character(len=:), pointer :: handlerNamesList ! '[ str1, str2, ..., strn ]'
         character(len=:), allocatable :: name
         type (HandlerIterator) :: iter
         integer :: i, j, n

         handlerNamesList => toString(loggerDict, 'handlers')
         if (associated(handlerNamesList)) then

            n = len_trim(handlerNamesList)
            if (handlerNamesList(1:1) /= '[' .or. handlerNamesList(n:n) /= ']') then
               call throw("Config::build_logger() - handlers is not of the form '[a,b,...,c]'")
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


   subroutine create_loggers(dict)
      use ftl_StringUnlimitedPolyMap_mod, only: ConfigIterator
      type (Config), intent(in) :: dict

      type (ConfigIterator) :: iter
      type (Config), pointer :: mPtr1, mPtr2

      iter = dict%find('loggers')
      if (iter /= dict%end()) then
         mPtr1 => toMap(dict, 'loggers')

         if (associated(mPtr1)) then
            iter = mPtr1%begin()
            do while (iter /= mPtr1%end())
               mPtr2 => toMap(mPtr1, iter%key())
               call addLogger(iter%key(), mPtr2, dict)
               call iter%next()
            end do
         end if
      end if
   end subroutine create_loggers


   subroutine addLogger(name, args, dict)
      character(len=*), intent(in) :: name
      class (Config), intent(in) :: args
      class (Config), intent(in) :: dict

      class (Logger), pointer :: lgr
      integer :: level
      integer :: iostat
      character(len=:), pointer :: levelNamePtr

      lgr => logging%getLogger(name)

      levelNamePtr => toString(args, 'level')
      if (associated(levelNamePtr)) then
         ! Try as integer
         read(levelNamePtr,*, iostat=iostat) level
         if (iostat /= 0) then
            level = nameToLevel(levelNamePtr)
         end if
         call lgr%setLevel(level)
      end if

   end subroutine addLogger


   subroutine check_schema_version(dict)
      type (Config), intent(in) :: dict

      integer, pointer :: schema_version

      schema_version => toInteger(dict, 'schema_version', require=.true.)
      if (associated(schema_version)) then
         if (schema_version /= 1) then
            call throw('Config::dictConfig() - unsupported schema version. Must be 1.')
            return
         end if
      end if

   end subroutine check_schema_version


   function toInteger(m, key, require) result(i)
      use ftl_StringUnlimitedPolyMap_mod, only: ConfigIterator
      integer, pointer :: i
      type (Config), target, intent(in) :: m
      character(len=*), intent(in) :: key
      logical, optional, intent(in)  :: require

      type (ConfigIterator) :: iter
      class (*), pointer :: ptr
      logical :: require_

      require_ = .false.
      if (present(require)) require_ = require

      iter = m%find(key)
      if (iter == m%end()) then
         i => null()
         if (require_) then
            call throw("Config::dictConfig() - '"//key//"' not found.")
         end if
         return
      end if

      ptr => iter%value()
      i => cast(ptr)

   contains

      function cast(anything) result(i)
         integer, pointer :: i
         class (*), target, intent(in) :: anything

         select type (anything)
         type is (integer)
            i => anything
         class default
            i => null()
            call throw("Config::dictConfig() - cannot cast '"//key//"' as integer.")
         end select

      end function cast

   end function toInteger


   function toString(m, key, require) result(str)
      use ftl_StringUnlimitedPolyMap_mod, only: ConfigIterator
      character(len=:), pointer :: str
      type (Config), target, intent(in) :: m
      character(len=*), intent(in) :: key
      logical, optional, intent(in)  :: require

      type (ConfigIterator) :: iter
      class (*), pointer :: ptr
      logical :: require_

      require_ = .false.
      if (present(require)) require_ = require

      iter = m%find(key)
      if (iter == m%end()) then
         str => null()
         if (require_) then
            call throw("Config::dictConfig() - '"//key//"' not found.")
         end if
         return
      end if

      ptr => iter%value()
      str => cast(ptr)

   contains

     function cast(anything) result(str)
       use FTL, only: String
         character(len=:), pointer :: str
         class (*), target, intent(in) :: anything

         select type (anything)
         type is (character(len=*))
            str => anything
         type is (String)
            str => anything%get()
         class default
            str => null()
            call throw("Config::dictConfig() - cannot cast '"//key//"' as character.")
            return
         end select

      end function cast

   end function toString


   function toMap(m, key, require) result(mPtr)
      use ftl_StringUnlimitedPolyMap_mod, only: ConfigIterator
      type (Config), pointer :: mPtr
      type (Config), target, intent(in) :: m
      character(len=*), intent(in) :: key
      logical, optional, intent(in) :: require

      type (ConfigIterator) :: iter
      class (*), pointer :: ptr
      logical :: require_

      require_ = .false.
      if (present(require)) require_ = require


      iter = m%find(key)
      if (iter == m%end()) then
         mPtr => null()
         call throw("Config::dictConfig() - '"//key//"' not found.")
         return
      end if

      ptr => iter%value()
      mPtr => cast(ptr)

   contains

      function cast(anything) result(m)
         type (Config), pointer :: m
         class (*), target, intent(in) :: anything

         select type (anything)
         type is (Config)
            m => anything
         class default
            m => null()
            call throw("Config::dictConfig() - cannot cast '"//key//"' as Map.")
         end select

      end function cast

   end function toMap

end module ASTG_Config_mod
