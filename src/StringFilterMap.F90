!------------------------------------------------------------------------------
! NASA/GSFC, CISTO, Code 606, Advanced Software Technology Group
!------------------------------------------------------------------------------
!
!*MODULE: PFL_StringFilterMap_mod
!
!> @brief Case insensitive abstract logger map.
!
!> @author ASTG staff
!> @date 01 Jan 2015 - Initial Version
!------------------------------------------------------------------------------
module  PFL_StringFilterMap_mod
   use PFL_AbstractFilter_mod
#define _map FilterMap
#define _iterator FilterIterator
   
#include "types/key_deferredLengthString.inc"

#define _value class(AbstractFilter)
#define _value_allocatable

#define _alt
#include "templates/map.inc"

end module PFL_StringFilterMap_mod

