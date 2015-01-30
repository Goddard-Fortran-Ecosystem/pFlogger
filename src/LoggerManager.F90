!------------------------------------------------------------------------------
! NASA/GSFC, CISTO, Code 606, Advanced Software Technology Group
!------------------------------------------------------------------------------
!
! MODULE: ASTG_LoggerManager_mod
!
!> @author 
!> ASTG staff
!
! DESCRIPTION: 
!> @brief
!> A manager instance that holds the hierarchy of loggers. 
!
! REVISION HISTORY:
! 01 Jan 2015 - Initial Version
!------------------------------------------------------------------------------
module ASTG_LoggerManager_mod
   use FTL_CIStringAbstractLoggerPolyUMap_mod
   use ASTG_SeverityLevels_mod
   use ASTG_Object_mod
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
      module procedure newLoggerManager
   end interface LoggerManager


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

      manager%loggers = CIStringAbstractLoggerPolyUMap()

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
