module ASTG_LoggerManager_mod
   use ASTG_SeverityLevels_mod
   use ASTG_Object_mod
   use FTL_CIStringAbstractLoggerPolyUMap_mod
   use ASTG_Logger_mod
   use ASTG_AbstractLogger_mod
   implicit none
   private

   public :: LoggerManager

   type, extends(Object) :: LoggerManager
      private
      type (CIStringAbstractLoggerPolyUMap) :: loggers
   contains
      procedure :: getLogger
      procedure, nopass :: getParentPrefix
   end type LoggerManager


   interface LoggerManager
      module procedure :: newLoggerManager
   end interface LoggerManager


contains


   function newLoggerManager() result(manager)
      type (LoggerManager) :: manager

      manager%loggers = CIStringAbstractLoggerPolyUMap()

   end function newLoggerManager

   function getLogger(this, name) result(lgr)
      use FTL_CIString_mod
      class (Logger), pointer :: lgr
      class (LoggerManager), target, intent(inout) :: this
      character(len=*), intent(in) :: name
      type (CIStringAbstractLoggerPolyUMapIter) :: iter
      character(len=:), allocatable :: parentName

      class (AbstractLogger), pointer :: tmp

      if (this%loggers%count(name) > 0) then
         tmp => this%loggers%at(name)
         select type (tmp)
         class is (Logger)
            lgr => tmp
         end select
         return
      end if

      iter = this%loggers%emplace(name, newLogger(name))
      parentName = this%getParentPrefix(name)

      tmp => this%loggers%at(name)
      select type (tmp)
      class is (Logger)
         lgr => tmp
      end select

      if (parentName /= '') then ! should exist !
         call lgr%setParent(this%loggers%at(parentName))
      end if

   end function getLogger


   function getParentPrefix(name) result(prefix)
      character(len=*), intent(in) :: name
      character(len=:), allocatable :: prefix

      integer :: idx

      idx = index(name, '.', back=.true.)
      prefix = name(1:idx-1)

   end function getParentPrefix

end module ASTG_LoggerManager_mod
