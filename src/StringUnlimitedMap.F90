!------------------------------------------------------------------------------
! NASA/GSFC, CISTO, Code 606, Advanced Software Technology Group
!------------------------------------------------------------------------------
!
!*MODULE: PFL_StringUnlimitedMap_mod
!
!> @brief Case insensitive unlimited polymorphic map.
!
!> @author ASTG staff
!> @date 01 Jan 2015 - Initial Version
!------------------------------------------------------------------------------
module  PFL_StringUnlimitedMap_mod
#include "types/key_deferredLengthString.inc"
#define _value class(*)
#define _value_allocatable

#define _alt
#include "templates/map.inc"


end module PFL_StringUnlimitedMap_mod

