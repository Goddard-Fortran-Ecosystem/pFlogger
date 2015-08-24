!------------------------------------------------------------------------------
! NASA/GSFC, CISTO, Code 606, Advanced Software Technology Group
!------------------------------------------------------------------------------
!
!*MODULE: ASTG_CIStringIntegerMap_mod
!
!> @brief Case insensitive abstract logger map.
!
!> @author ASTG staff
!> @date 01 Jan 2015 - Initial Version
!------------------------------------------------------------------------------
module  ASTG_CIStringIntegerMap_mod
   use ASTG_StringUtilities_mod, only: caseInsensitiveLessThan
#include "types/key_deferredLengthString.inc"
#define _KEY_LESS_THAN(x,y) caseInsensitiveLessThan(x,y)

#include "types/value_integer.inc"

#define _alt
#include "templates/map.inc"


end module ASTG_CIStringIntegerMap_mod

