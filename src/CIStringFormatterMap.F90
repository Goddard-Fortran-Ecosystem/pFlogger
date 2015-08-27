!------------------------------------------------------------------------------
! NASA/GSFC, CISTO, Code 606, Advanced Software Technology Group
!------------------------------------------------------------------------------
!
!*MODULE: ASTG_CIStringFormatterMap_mod
!
!> @brief Case insensitive abstract logger map.
!
!> @author ASTG staff
!> @date 01 Jan 2015 - Initial Version
!------------------------------------------------------------------------------
module  ASTG_CIStringFormatterMap_mod
   use ASTG_StringUtilities_mod, only: caseInsensitiveLessThan
   use ASTG_Formatter_mod
#include "types/key_deferredLengthString.inc"
#define _KEY_LESS_THAN(x,y) caseInsensitiveLessThan(x,y)

#define _value class (Formatter)
#define _value_polymorphic

#define _alt
#include "templates/map.inc"


end module ASTG_CIStringFormatterMap_mod

