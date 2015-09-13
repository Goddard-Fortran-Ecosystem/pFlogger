!------------------------------------------------------------------------------
! NASA/GSFC, CISTO, Code 606, Advanced Software Technology Group
!------------------------------------------------------------------------------
!
!*MODULE: ASTG_SeverityLevels_mod
!
!> @brief Specify severity levels used to logging messages.
!
!> @author ASTG staff
!> @date 01 Jan 2015 - Initial Version  
!------------------------------------------------------------------------------
module ASTG_SeverityLevels_mod
   use ASTG_Exception_mod
   use ASTG_StringIntegerMap_mod, only: StringIntegerMap => map
   use ASTG_IntegerStringMap_mod, only: IntegerStringMap => map
   implicit none
   private

   public :: NOTSET
   public :: DEBUG
   public :: INFO
   public :: WARNING
   public :: ERROR
   public :: CRITICAL

   public :: levelToName
   public :: nameToLevel

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
   ! levelToName
   !
   ! DESCRIPTION:
   ! Convert a numeric severity level to string identifier.
   !---------------------------------------------------------------------------
   function levelToName(level) result(name)
      use ASTG_IntegerStringMap_mod, only: mapIterator
      character(len=:), allocatable :: name
      integer, intent(in) :: level

      type (mapIterator) :: iter

      iter = level_to_name_%find(level)
      if (iter == level_to_name_%end()) then
         name = ''
         call throw('Unknown level. Please use a valid level.')
         return
      end if

      name = iter%value()
      
   end function levelToName
    

   !---------------------------------------------------------------------------  
   ! FUNCTION: 
   ! nameToLevel
   !
   ! DESCRIPTION:
   ! Convert a level name to a numeric severity level
   !---------------------------------------------------------------------------
   function nameToLevel(name) result(level)
      use ASTG_StringIntegerMap_mod, only: mapiterator
      integer :: level
      character(len=*), intent(in) :: name

      type (mapiterator) :: iter

      iter = name_to_level_%find(name)
      if (iter == name_to_level_%end()) then
         level = NOTSET
         call throw('Unknown level name. Please use a valid name.')
         return
      end if

      level = iter%value()

   end function nameToLevel

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

end module ASTG_SeverityLevels_mod
