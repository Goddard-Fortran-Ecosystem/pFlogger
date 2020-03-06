!------------------------------------------------------------------------------
! NASA/GSFC, CISTO, Code 606, Advanced Software Technology Group
!------------------------------------------------------------------------------
!
!*MODULE: PFL_StringIntegerMap
!
!> @brief Case insensitive abstract logger map.
!
!> @author ASTG staff
!> @date 01 Jan 2015 - Initial Version
!------------------------------------------------------------------------------
module  PFL_StringIntegerMap
#include "types/key_deferredLengthString.inc"
#include "types/value_integer.inc"

#define _alt
#include "templates/map.inc"
#undef _alt

end module PFL_StringIntegerMap

