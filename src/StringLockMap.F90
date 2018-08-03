!------------------------------------------------------------------------------
! NASA/GSFC, CISTO, Code 606, Advanced Software Technology Group
!------------------------------------------------------------------------------
!
!*MODULE: PFL_StringLockMap_mod
!
!> @brief Case insensitive abstract logger map.
!
!> @author ASTG staff
!> @date 01 Jan 2015 - Initial Version
!------------------------------------------------------------------------------
module  PFL_StringLockMap_mod
   use PFL_AbstractLock_mod
#define _map LockMap
#define _iterator LockIterator
   
#include "types/key_deferredLengthString.inc"

#define _value class(AbstractLock)
#define _value_allocatable

#define _alt
#include "templates/map.inc"

end module PFL_StringLockMap_mod

