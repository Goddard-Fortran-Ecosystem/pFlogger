!------------------------------------------------------------------------------
! NASA/GSFC, CISTO, Code 606, Advanced Software Technology Group
!------------------------------------------------------------------------------
!
!*MODULE: PFL_SeverityLevels
!
!> @brief Specify severity levels used to logging messages.
!
!> @author ASTG staff
!> @date 01 Jan 2015 - Initial Version  
!------------------------------------------------------------------------------
#include "error_handling_macros.fh"
module PFL_SeverityLevels
   use PFL_Exception
   use gftl_StringIntegerMap
   use gftl_IntegerStringMap
   implicit none
   private

   public :: NOTSET
   public :: DEBUG
   public :: INFO
   public :: WARNING
   public :: ERROR
   public :: CRITICAL

   public :: level_to_name
   public :: name_to_level

   public :: initialize_severity_levels
   public :: finalize_severity_levels

   enum, bind(c)
      enumerator :: &
           & NOTSET   =  0, &
           & DEBUG    = 10, &
           & INFO     = 20, &
           & WARNING  = 30, &
           & ERROR    = 40, &
           & CRITICAL = 50
   end enum

   ! (Harmless?) Singletons?
   type (StringIntegerMap), save :: name_to_level_
   type (IntegerStringMap), save :: level_to_name_

contains

   
   !---------------------------------------------------------------------------  
   ! FUNCTION: 
   ! level_to_name
   !
   ! DESCRIPTION:
   ! Convert a numeric severity level to string identifier.
   !---------------------------------------------------------------------------
   function level_to_name(level, rc) result(name)
      character(len=:), allocatable :: name
      integer, intent(in) :: level
      integer, optional, intent(out) :: rc

      type (IntegerStringMapIterator) :: iter
      integer :: status

      iter = level_to_name_%find(level)
      if (iter == level_to_name_%end()) then
         name = ''
         _ASSERT(.false., 'PFL::SeverityLevels::level_to_name - unknown level. Please use a valid level.', rc)
      end if

      name = iter%value()
      _RETURN(_SUCCESS,rc)
   end function level_to_name
    

   !---------------------------------------------------------------------------  
   ! FUNCTION: 
   ! name_to_level
   !
   ! DESCRIPTION:
   ! Convert a level name to a numeric severity level
   !---------------------------------------------------------------------------
   function name_to_level(name, rc) result(level)
      integer :: level
      character(len=*), intent(in) :: name
      integer, optional, intent(out) :: rc

      type (StringIntegerMapIterator) :: iter
      integer :: status

      iter = name_to_level_%find(name)
      if (iter == name_to_level_%end()) then
         level = NOTSET
          _ASSERT(.false., 'PFL::SeverityLevels::name_to_level - unknown level name "'//name//'". Please use a valid name.', rc)
      end if

      level = iter%value()
      _RETURN(_SUCCESS,rc)
   end function name_to_level

   subroutine initialize_severity_levels()
      call level_to_name_%insert(NOTSET, 'NOTSET')
      call level_to_name_%insert(DEBUG, 'DEBUG')
      call level_to_name_%insert(INFO, 'INFO')
      call level_to_name_%insert(WARNING, 'WARNING')
      call level_to_name_%insert(ERROR, 'ERROR')
      call level_to_name_%insert(CRITICAL, 'CRITICAL')

      call name_to_level_%insert('NOTSET', NOTSET)
      call name_to_level_%insert('INFO', INFO)
      call name_to_level_%insert('DEBUG', DEBUG)
      call name_to_level_%insert('WARNING', WARNING)
      call name_to_level_%insert('ERROR', ERROR)
      call name_to_level_%insert('CRITICAL', CRITICAL)

   end subroutine initialize_severity_levels

   subroutine finalize_severity_levels()
      call level_to_name_%clear()
      call name_to_level_%clear()
   end subroutine finalize_severity_levels

end module PFL_SeverityLevels
