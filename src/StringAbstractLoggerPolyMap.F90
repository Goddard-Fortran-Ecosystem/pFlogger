!------------------------------------------------------------------------------
! NASA/GSFC, CISTO, Code 606, Advanced Software Technology Group
!------------------------------------------------------------------------------
!
!*MODULE: PFL_StringAbstractLoggerPolyMap
!
!> @brief Case insensitive abstract logger map.
!
!> @author ASTG staff
!> @date 01 Jan 2015 - Initial Version
!------------------------------------------------------------------------------
module  PFL_StringAbstractLoggerPolyMap
   use PFL_AbstractLogger

#define Key __CHARACTER_DEFERRED
#define T AbstractLogger
#define T_polymorphic
#define Map LoggerMap
#define MapIterator LoggerIterator
#define Pair StringLoggerPair

#include "map/template.inc"

#undef Pair
#undef MapIterator
#undef Map
#undef T_polymorphic
#undef T
#undef Key

end module PFL_StringAbstractLoggerPolyMap

