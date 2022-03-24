!------------------------------------------------------------------------------
! NASA/GSFC, CISTO, Code 606, Advanced Software Technology Group
!------------------------------------------------------------------------------
!
!*MODULE: PFL_StringFilterMap
!
!> @brief Case insensitive abstract logger map.
!
!> @author ASTG staff
!> @date 01 Jan 2015 - Initial Version
!------------------------------------------------------------------------------
module  PFL_StringFilterMap
   use PFL_AbstractFilter

#define Key __CHARACTER_DEFERRED
#define T AbstractFilter
#define T_polymorphic
#define Map FilterMap
#define MapIterator FilterIterator
#define Pair FilterPair

#include "map/template.inc"

#undef Pair
#undef MapIterator
#undef Map
#undef T_polymorphic
#undef T
#undef Key

end module PFL_StringFilterMap

