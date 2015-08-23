! Singleton pattern for now
module ASTG_Config_mod
   use ASTG_CIStringUnlimitedMap_mod
   use ASTG_LoggerManager_mod
   use ASTG_Logger_mod
   use ASTG_Exception_mod
   use ASTG_SeverityLevels_mod

   use ASTG_CIStringFilterMap_mod, only: FilterMap => map
   use ASTG_CIStringFormatterMap_mod, only: FormatterMap => map

   implicit none
   private

   public :: dictConfig
   public :: build_formatters
   public :: build_formatter
   public :: build_filters
   public :: build_filter

   public :: build_streamhandler
   public :: build_handler

contains

   subroutine dictConfig(dict)
      type (Map), intent(in) :: dict

      type (FormatterMap) :: formatters

      call check_schema_version(dict)

!!$      formatters = build_formatters(dict)
!!$      loggers = build_loggers(dict)
!!$      call add_loggers(dict)

      call create_loggers(dict)

   end subroutine dictConfig


   function build_formatters(formattersDict) result(formatters)
      type (FormatterMap) :: formatters
      type (Map), intent(in) :: formattersDict

      type (MapIterator) :: iter
      type (Map), pointer :: cfg

      iter = formattersDict%begin()
      do while (iter /= formattersDict%end())
         cfg => toMap(formattersDict, iter%key())
         call formatters%insert(iter%key(), build_formatter(cfg))
         call iter%next()
      end do

   end function build_formatters


   function build_formatter(dict) result(fmtr)
      use ASTG_Formatter_mod
      type (Formatter) :: fmtr
      type (Map), intent(in) :: dict
      character(len=:), pointer :: fmt
      character(len=:), pointer :: datefmt

      fmt => toString(dict, 'fmt')
      datefmt => toString(dict, 'datefmt')
      
      if (associated(fmt)) then
         if (associated(datefmt)) then
            fmtr = Formatter(fmt, datefmt)
         else
            fmtr = Formatter(fmt)
         end if
      else
         fmtr = Formatter()
      end if

   end function build_formatter


   function build_filters(filtersDict) result(filters)
      use ASTG_Filter_mod
      type (FilterMap) :: filters
      type (Map), intent(in) :: filtersDict

      type (MapIterator) :: iter
      type (Map), pointer :: cfg
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
      type (Map), intent(in) :: dict
      character(len=:), pointer :: name

      name => toString(dict, 'name', require=.true.)
      if (associated(name)) then
         f = Filter(name)
      end if

   end function build_filter

   
   function build_handler(handlerDict, formatters, filters) result(h)
      use ASTG_CIStringFormatterMap_mod, only: FormatterMap => map, FormatterMapIterator => mapIterator
      use ASTG_CIStringFilterMap_mod, only: FilterMap => map, FilterMapIterator => mapIterator
      use ASTG_AbstractHandler_mod
      use ASTG_StringUtilities_mod, only: toLowerCase
      class (AbstractHandler), allocatable :: h
      type (Map), intent(in) :: handlerDict
      type (FormatterMap), intent(in) :: formatters
      type (FilterMap), intent(in) :: filters

      character(len=:), pointer :: classNamePtr
      character(len=:), pointer :: formatterNamePtr
      type (FormatterMapIterator) :: fIter

      character(len=:), pointer :: filterNamesList ! '[ str1, str2, ..., strn ]'
      character(len=:), allocatable :: name
      type (FilterMapIterator) :: fltrIter
      integer :: i, j, n

      character(len=:), pointer :: levelNamePtr
      integer, pointer :: levelPtr
      integer :: level

      classNamePtr => toString(handlerDict, 'class', require=.true.)
      if (associated(classNamePtr)) then
         select case (toLowerCase(classNamePtr))
         case ('streamhandler')
            allocate(h, source=build_streamhandler(handlerDict, formatters, filters))
         case default
            call throw("Config::build_handler() - unsupported class: '" // classNamePtr //"'.")
         end select
      end if

      levelNamePtr => toString(handlerDict, 'level')
      if (associated(levelNamePtr)) then
         select case (toLowerCase(levelNamePtr))
         case ('debug')
            level = DEBUG
         case ('info')
            level = INFO
         case ('warning')
            level = WARNING
         case ('error')
            level = ERROR
         case ('critical')
            level = CRITICAL
         case default
            call throw("Config::build_streamhandler() - unknown value for level '"//levelNamePtr//"'.")
            return
         end select
         call h%setLevel(level)
      else
         levelPtr => toInteger(handlerDict, 'level')
         if (associated(levelPtr)) then
            call h%setLevel(levelPtr)
         end if
      end if
      
      formatterNamePtr => toString(handlerDict, 'formatter')
      if (associated(formatterNamePtr)) then
         fIter = formatters%find(formatterNamePtr)
         if (fIter /= formatters%end()) then
            call h%setFormatter(fIter%value())
         end if
      end if

      filterNamesList => toString(handlerDict, 'formats')
      if (associated(filterNamesList)) then
         n = len_trim(filterNamesList)
         print*,'names: <',filterNamesList,'>'
         if (filterNamesList(1:1) /= '[' .or. filterNamesList(n:n) /= ']') then
            call throw("Config::build_handler() - filters is not of the form '[a,b,...,c]'")
            return
         end if

         i = 2
         do while (i < n)
            j = index(filterNamesList(i:n-1),',')
            if (j == 0) then
               if (i < n-1) then
                  name = filterNamesList(i:n-1)
                  i = n
               end if
            else
               name = filterNamesList(i:j-1)
               i = j + 1
            end if
            fltrIter = filters%find(name)
            if (fltrIter /= filters%end()) then
               call h%addFilter(fltrIter%value())
            else
               call throw("Config::build_handler() - unknown filter'"//name//"'.")
            end if
         end do

      end if

   end function build_handler


   function build_streamhandler(handlerDict, formatters, filters) result(h)
      use ASTG_CIStringFormatterMap_mod, only: FormatterMap => map
      use ASTG_CIStringFilterMap_mod, only: FilterMap => map
      use ASTG_StreamHandler_mod
      use ASTG_StringUtilities_mod, only: toLowerCase
      use iso_fortran_env, only: OUTPUT_UNIT, ERROR_UNIT
      type (StreamHandler) :: h
      type (Map), intent(in) :: handlerDict
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


   subroutine create_loggers(dict)
      type (Map), intent(in) :: dict

      type (MapIterator) :: iter
      type (Map), pointer :: mPtr1, mPtr2

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
      class (Map), intent(in) :: args
      class (Map), intent(in) :: dict

      class (Logger), pointer :: lgr
      integer :: level

      lgr => logging%getLogger(name)
      level = nameToLevel(toString(args, 'level'))
      call lgr%setLevel(level)

   end subroutine addLogger


   subroutine check_schema_version(dict)
      type (Map), intent(in) :: dict

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
      integer, pointer :: i
      type (Map), target, intent(in) :: m
      character(len=*), intent(in) :: key
      logical, optional, intent(in)  :: require

      type (MapIterator) :: iter
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
            return
         end select

      end function cast

   end function toInteger


   function toString(m, key, require) result(str)
      character(len=:), pointer :: str
      type (Map), target, intent(in) :: m
      character(len=*), intent(in) :: key
      logical, optional, intent(in)  :: require

      type (MapIterator) :: iter
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
         use ASTG_String_mod
         character(len=:), pointer :: str
         class (*), target, intent(in) :: anything

         select type (anything)
         type is (character(len=*))
            str => anything
         type is (String)
            str => anything%str
         class default
            call throw("Config::dictConfig() - cannot cast '"//key//"' as character.")
            return
         end select

      end function cast

   end function toString

   function toStringArray(m, key, require) result(str)
      character(len=:), pointer :: str(:)
      type (Map), target, intent(in) :: m
      character(len=*), intent(in) :: key
      logical, optional, intent(in)  :: require

      type (MapIterator) :: iter
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
         use ASTG_WrapArray_mod
         character(len=:), pointer :: str(:)
         class (*), target, intent(in) :: anything

         select type (anything)
         type is (WrapArray1D)
            select type (p => anything%array)
            type is (character(len=*))
               str => p
            end select
         class default
            call throw("Config::dictConfig() - cannot cast '"//key//"' as an array.")
            return
         end select

      end function cast

   end function toStringArray


   function toMap(m, key, require) result(mPtr)
      type (Map), pointer :: mPtr
      type (Map), target, intent(in) :: m
      character(len=*), intent(in) :: key
      logical, optional, intent(in) :: require

      type (MapIterator) :: iter
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
         type (Map), pointer :: m
         class (*), target, intent(in) :: anything

         select type (anything)
         type is (map)
            m => anything
         class default
            m => null()
            call throw("Config::dictConfig() - cannot cast '"//key//"' as Map.")
            return
         end select

      end function cast

   end function toMap

end module ASTG_Config_mod
