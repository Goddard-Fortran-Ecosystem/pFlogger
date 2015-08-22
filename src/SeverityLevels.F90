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
   
   enum, bind(c)
      enumerator :: &
           & NOTSET   =  0, &
           & DEBUG    = 10, &
           & INFO     = 20, &
           & WARNING  = 30, &
           & ERROR    = 40, &
           & CRITICAL = 50
   end enum

contains

   
   !---------------------------------------------------------------------------  
   ! FUNCTION: 
   ! levelToName
   !
   ! DESCRIPTION:
   ! Convert a numeric severity level to string identifier.
   !---------------------------------------------------------------------------
   function levelToName(level) result(name)
      character(len=:), allocatable :: name
      integer, intent(in) :: level

      select case (level)
      case (NOTSET)
         name = 'NOTSET'
      case (DEBUG)
         name = 'DEBUG'
      case (INFO)
         name = 'INFO'
      case (WARNING)
         name = 'WARNING'
      case (ERROR)
         name = 'ERROR'
      case (CRITICAL)
         name = 'CRITICAL'
      case default
         name=''
         call throw('Unknown level. Please use a valid level.')
      end select
      
   end function levelToName
    

   !---------------------------------------------------------------------------  
   ! FUNCTION: 
   ! nameToLevel
   !
   ! DESCRIPTION:
   ! Convert a level name to a numeric severity level
   !---------------------------------------------------------------------------
   function nameToLevel(name) result(level)
      integer :: level
      character(len=*), intent(in) :: name

      select case (name)
      case ('NOTSET')
         level = NOTSET
      case ('DEBUG')
         level = DEBUG
      case ('INFO')
         level = INFO
      case ('WARNING')
         level = WARNING
      case ('ERROR')
         level = ERROR
      case ('CRITICAL')
         level = CRITICAL
      case default
         level = NOTSET
         call throw('Unknown level name. Please use a valid name.')
      end select
      
   end function nameToLevel
    
end module ASTG_SeverityLevels_mod
