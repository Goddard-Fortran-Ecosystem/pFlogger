!------------------------------------------------------------------------------
! NASA/GSFC, CISTO, Code 606, Advanced Software Technology Group
!------------------------------------------------------------------------------
!
!*MODULE: PFL_StringLockMap
!
!> @brief Case insensitive abstract logger map.
!
!> @author ASTG staff
!> @date 01 Jan 2015 - Initial Version
!------------------------------------------------------------------------------
module  PFL_StringLockMap
   use PFL_AbstractLock

#define Key __CHARACTER_DEFERRED
#define T AbstractLock
#define T_polymorphic
#define Map LockMap
#define MapIterator LockIterator
#define Pair LockPair

#include "map/template.inc"

#undef Pair
#undef MapIterator
#undef Map
#undef T_polymorphic
#undef T
#undef Key

end module PFL_StringLockMap

