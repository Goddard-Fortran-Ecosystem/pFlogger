!------------------------------------------------------------------------------
! NASA/GSFC, CISTO, Code 606, Advanced Software Technology Group
!------------------------------------------------------------------------------
!
!*MODULE: PFL_StringHandlerMap
!
!> @brief Case insensitive abstract logger map.
!
!> @author ASTG staff
!> @date 01 Jan 2015 - Initial Version
!------------------------------------------------------------------------------
module  PFL_StringHandlerMap
   use PFL_AbstractHandler

#define Key __CHARACTER_DEFERRED
#define T AbstractHandler
#define T_polymorphic
#define Map HandlerMap
#define MapIterator HandlerIterator
#define Pair HandlerPair

#include "map/template.inc"

#undef Pair
#undef MapIterator
#undef Map
#undef T_polymorphic
#undef T
#undef Key

end module PFL_StringHandlerMap

