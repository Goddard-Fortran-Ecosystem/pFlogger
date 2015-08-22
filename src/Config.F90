! Singleton pattern for now
module ASTG_Config_mod
   use ASTG_CIStringUnlimitedMap_mod
   use ASTG_LoggerManager_mod
   use ASTG_Logger_mod
   use ASTG_Exception_mod
   use ASTG_SeverityLevels_mod
   implicit none
   private

   public :: dictConfig

contains

   subroutine dictConfig(dict)
      type (Map), intent(in) :: dict

      type (Map), pointer :: mPtr1, mPtr2
      type (MapIterator) :: iter
      class(*), pointer :: p

      call check_schema_version(dict)

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

   end subroutine dictConfig


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
      type (MapIterator) :: iter
      
      iter = dict%find('schema_version')
      if (iter == dict%end()) then
         call throw('Config::dictConfig() - No version specified for schema.')
         return
      end if

      schema_version => toInteger(dict, 'schema_version')
      if (schema_version /= 1) then
         call throw('Config::dictConfig() - unsupported schema version. Must be 1.')
         return
      end if

   end subroutine check_schema_version

   function toInteger(m, key) result(i)
      integer, pointer :: i
      type (Map), target, intent(in) :: m
      character(len=*), intent(in) :: key

      type (MapIterator) :: iter
      class (*), pointer :: ptr

      iter = m%find(key)
      if (iter == m%end()) then
         i => null()
         call throw("Config::dictConfig() - '"//key//"' not found.")
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


   function toString(m, key) result(str)
      character(len=:), pointer :: str
      type (Map), target, intent(in) :: m
      character(len=*), intent(in) :: key

      type (MapIterator) :: iter
      class (*), pointer :: ptr

      iter = m%find(key)
      if (iter == m%end()) then
         str => null()
         call throw("Config::dictConfig() - '"//key//"' not found.")
         return
      end if

      ptr => iter%value()
      str => cast(ptr)

   contains

      function cast(anything) result(str)
         character(len=:), pointer :: str
         class (*), target, intent(in) :: anything

         select type (anything)
         type is (character(len=*))
            str => anything
         class default
            call throw("Config::dictConfig() - cannot cast '"//key//"' as character.")
            return
         end select

      end function cast

   end function toString


   function toMap(m, key) result(mPtr)
      type (Map), pointer :: mPtr
      type (Map), target, intent(in) :: m
      character(len=*), intent(in) :: key

      type (MapIterator) :: iter
      class (*), pointer :: ptr

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
