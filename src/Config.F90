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

      type (MapIterator) :: iter
      class(*), pointer :: p

      call check_schema_version(dict)

      iter = dict%find('loggers')
      if (iter /= dict%end()) then
         p => iter%value()
         select type (p)
         class is (Map)
            iter = p%begin()
            do while (iter /= p%end())
               call addLogger(iter%key(), iter%value(), dict)
               call iter%next()
            end do
         class default
            call throw("Config::dictConfig() - invalid value for 'loggers'. "// &
                 & "Should be class Map.")
         end select
      end if

   end subroutine dictConfig


   subroutine addLogger(name, args, dict)
      character(len=*), intent(in) :: name
      class (Map), intent(in) :: args
      class (Map), intent(in) :: dict

      type (MapIterator) :: iter
      class (*), pointer :: p
      class (Logger), pointer :: lgr
      integer :: level

      lgr => logging%getLogger(name)

      iter = args%find('level')
      if (iter /= args%end()) then
         p => iter%value()
         select type (p)
         class is (character(len=*))
            level = nameToLevel(p)
         class default
            call throw('Config::dictConfig() - logger level must be of type integer.')
         end select
         call lgr%setLevel(level)
      end if

   end subroutine addLogger

   subroutine check_schema_version(dict)
      type (Map), intent(in) :: dict

      integer, pointer :: schema_version
      type (MapIterator) :: iter
      
      iter = dict%find('schema_version')
      if (iter == dict%end()) then
         call throw('Config::dictConfig() - No version specified for schema.')
         return
      else
         schema_version => iter%value()
         if (schema_version /= 1) then
            call throw('Config::dictConfig() - unsupported schema version. Must be 1.')
            return
         end if
      end if

   end subroutine check_schema_version

end module ASTG_Config_mod
