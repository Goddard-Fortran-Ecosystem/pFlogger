!------------------------------------------------------------------------------
! NASA/GSFC, CISTO, Code 606, Advanced Software Technology Group
!------------------------------------------------------------------------------
!
!*MODULE: ASTG_CIStringUnlimitedMap_mod
!
!> @brief Case insensitive unlimited polymorphic map.
!
!> @author ASTG staff
!> @date 01 Jan 2015 - Initial Version
!------------------------------------------------------------------------------
module  ASTG_CIStringUnlimitedMap_mod
   use ASTG_StringUtilities_mod, only: caseInsensitiveLessThan
#include "types/key_deferredLengthString.inc"
#define _KEY_LESS_THAN(x,y) caseInsensitiveLessThan(x,y)

#define _value class(*)
#define _value_polymorphic

#define _alt
#include "templates/map.inc"


end module ASTG_CIStringUnlimitedMap_mod

