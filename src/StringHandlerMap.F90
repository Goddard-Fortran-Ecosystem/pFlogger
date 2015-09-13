!------------------------------------------------------------------------------
! NASA/GSFC, CISTO, Code 606, Advanced Software Technology Group
!------------------------------------------------------------------------------
!
!*MODULE: ASTG_StringHandlerMap_mod
!
!> @brief Case insensitive abstract logger map.
!
!> @author ASTG staff
!> @date 01 Jan 2015 - Initial Version
!------------------------------------------------------------------------------
module  ASTG_StringHandlerMap_mod
   use ASTG_AbstractHandler_mod
#include "types/key_deferredLengthString.inc"

#define _value class(AbstractHandler)
#define _value_polymorphic

#define _alt
#include "templates/map.inc"

end module ASTG_StringHandlerMap_mod

