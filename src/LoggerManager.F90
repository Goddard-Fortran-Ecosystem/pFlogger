!------------------------------------------------------------------------------
! NASA/GSFC, CISTO, Code 606, Advanced Software Technology Group
!------------------------------------------------------------------------------
!
! MODULE: ASTG_LoggerManager_mod
!
!> @brief A manager instance that holds the hierarchy of loggers. 
!
!> @author ASTG staff
!> @date 01 Jan 2015 - Initial Version  
!---------------------------------------------------------------------------
module ASTG_LoggerManager_mod
   use ASTG_CIStringAbstractLoggerPolyMap_mod, only: LoggerMap => Map
   use ASTG_CIStringAbstractLoggerPolyMap_mod, only: LoggerMapIterator => MapIterator
   use ASTG_SeverityLevels_mod
   use ASTG_Object_mod
   use ASTG_Logger_mod
   use ASTG_AbstractLogger_mod
   implicit none
   private

   public :: LoggerManager
   public :: logging ! singleton instance

   type, extends(Object) :: LoggerManager
      private
      type (LoggerMap) :: loggers
   contains
      procedure :: getLogger
      procedure, nopass :: getParentPrefix
   end type LoggerManager


   interface LoggerManager
      module procedure newLoggerManager
   end interface LoggerManager

   type (LoggerManager), protected, save :: logging


contains


   !---------------------------------------------------------------------------  
   ! FUNCTION: 
   ! getLogger
   !
   ! DESCRIPTION: 
   ! Initialize with the root node of the logger hierarchy.
   !---------------------------------------------------------------------------
   function newLoggerManager() result(manager)
      type (LoggerManager) :: manager

   end function newLoggerManager


   !---------------------------------------------------------------------------  
   ! FUNCTION: 
   ! getLogger
   !
   ! DESCRIPTION: 
   ! Get a logger with the specified 'name', creating it if necessary.
   ! Note that:
   ! 1) 'name' is a dot-separated hierarchical name such as 'A','A.B','A.B.C',
   !    etc.
   ! 2) 'name' is case insensitive.
   !---------------------------------------------------------------------------
   function getLogger(this, name) result(lgr)
      use ASTG_Exception_mod
      class (Logger), pointer :: lgr
      class (LoggerManager), target, intent(inout) :: this
      character(len=*), intent(in) :: name
      character(len=:), allocatable :: parentName

      class (AbstractLogger), pointer :: tmp

      tmp => this%loggers%at(name)
      if (associated(tmp)) then  ! cast to Logger
         select type (tmp)
         class is (Logger)
            lgr => tmp
         class default
            call throw('LoggerManager::getLogger() - Illegal type of logger <' &
                 & // name // '>')
         end select
         return

      else ! new logger

         call this%loggers%insert(name, newLogger(name))
         parentName = this%getParentPrefix(name)

         tmp => this%loggers%at(name)
         select type (tmp)
         class is (Logger)
            lgr => tmp
         end select

         if (parentName /= '') then ! should exist !
            call lgr%setParent(this%loggers%at(parentName))
         end if
      end if

   end function getLogger


   !---------------------------------------------------------------------------  
   ! FUNCTION: 
   ! getParentPrefix
   !
   ! DESCRIPTION: 
   ! In the logger hierarchy, the parent prefix is the string preceding the
   ! last logger in the hierarchy. For example: the parent prefix of c in the
   ! logger hierarchy a.b.c is a.b
   !---------------------------------------------------------------------------
   function getParentPrefix(name) result(prefix)
      character(len=*), intent(in) :: name
      character(len=:), allocatable :: prefix

      integer :: idx

      idx = index(name, '.', back=.true.)
      prefix = name(1:idx-1)

   end function getParentPrefix

end module ASTG_LoggerManager_mod
