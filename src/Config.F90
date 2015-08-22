! Singleton pattern for now
module ASTG_Config_mod
   use ASTG_CIStringUnlimitedMap_mod
   use ASTG_LoggerManager_mod
   use ASTG_Logger_mod
   use ASTG_Exception_mod
   use ASTG_SeverityLevels_mod

   use ASTG_CIStringFormatterMap_mod, only: FormatterMap => map

   implicit none
   private

   public :: dictConfig
   public :: build_formatters
   public :: build_formatter

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
