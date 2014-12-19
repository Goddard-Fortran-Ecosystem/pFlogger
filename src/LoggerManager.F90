module ASTG_LoggerManager_mod
   use ASTG_Object_mod
   use ASTG_Logger_mod
   use FTL_CaseInsensitiveStringLoggerUnorderedMap_mod
   implicit none
   private

   public :: LoggerManager

   type, extends(Object) :: LoggerManager
      private
      type (CaseInsensitiveStringLoggerUnorderedMap) :: loggers
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

      manager%loggers = CaseInsensitiveStringLoggerUnorderedMap()

   end function newLoggerManager

   function getLogger(this, name) result(lgr)
      use FTL_CaseInsensitiveString_mod
      class (Logger), pointer :: lgr
      class (LoggerManager), target, intent(inout) :: this
      character(len=*), intent(in) :: name
      type (CaseInsensitiveStringLoggerUnorderedMapIterator) :: iter
      character(len=:), allocatable :: parentName

      if (this%loggers%count(name) > 0) then
         lgr => this%loggers%at(name)
         return
      end if

      iter = this%loggers%emplace(name, Logger(name))
      parentName = this%getParentPrefix(name)

      lgr => this%loggers%at(name)
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
