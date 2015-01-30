!------------------------------------------------------------------------------
! NASA/GSFC, CISTO, Code 606, Advanced Software Technology Group
!------------------------------------------------------------------------------
!
! MODULE: ASTG_SeverityLevels_mod
!
! AUTHOR: ASTG staff
!
! DESCRIPTION:
! Specify severity levels used to logging messages.
!------------------------------------------------------------------------------
module ASTG_SeverityLevels_mod
   implicit none
   private

   public :: NOTSET
   public :: DEBUG
   public :: INFO
   public :: WARNING
   public :: ERROR
   public :: CRITICAL

   public :: levelToString
   
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
   ! levelToString
   !
   ! DESCRIPTION:
   ! Convert a numeric severity level to string identifier.
   !---------------------------------------------------------------------------
   function levelToString(level) result(string)
      character(len=:), allocatable :: string
      integer, intent(in) :: level

      select case (level)
      case (NOTSET)
        string = 'NOTSET'
      case (DEBUG)
        string = 'DEBUG'
      case (INFO)
        string = 'INFO'
      case (WARNING)
        string = 'WARNING'
      case (ERROR)
        string = 'ERROR'
      case (CRITICAL)
        string = 'CRITICAL'
      end select
      
   end function levelToString
    
end module ASTG_SeverityLevels_mod
