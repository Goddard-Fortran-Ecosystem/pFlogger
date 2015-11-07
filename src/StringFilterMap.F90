!------------------------------------------------------------------------------
! NASA/GSFC, CISTO, Code 606, Advanced Software Technology Group
!------------------------------------------------------------------------------
!
!*MODULE: ASTG_StringFilterMap_mod
!
!> @brief Case insensitive abstract logger map.
!
!> @author ASTG staff
!> @date 01 Jan 2015 - Initial Version
!------------------------------------------------------------------------------
module  ASTG_StringFilterMap_mod
   use ASTG_Filter_mod
#define _map FilterMap
#define _iterator FilterIterator
   
#include "types/key_deferredLengthString.inc"

#define _value class(Filter)
#define _value_allocatable

#define _alt
#include "templates/map.inc"

end module ASTG_StringFilterMap_mod

