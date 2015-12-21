!------------------------------------------------------------------------------
! NASA/GSFC, CISTO, Code 606, Advanced Software Technology Group
!------------------------------------------------------------------------------
!
!*MODULE: PFL_StringHandlerMap_mod
!
!> @brief Case insensitive abstract logger map.
!
!> @author ASTG staff
!> @date 01 Jan 2015 - Initial Version
!------------------------------------------------------------------------------
module  PFL_StringHandlerMap_mod
   use PFL_AbstractHandler_mod
#define _map HandlerMap
#define _iterator HandlerIterator
   
#include "types/key_deferredLengthString.inc"

#define _value class(AbstractHandler)
#define _value_allocatable

#define _alt
#include "templates/map.inc"

end module PFL_StringHandlerMap_mod

