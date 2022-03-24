!------------------------------------------------------------------------------
! NASA/GSFC, CISTO, Code 606, Advanced Software Technology Group
!------------------------------------------------------------------------------
!
!*MODULE: PFL_StringFormatterMap
!
!> @brief Case insensitive abstract logger map.
!
!> @author ASTG staff
!> @date 01 Jan 2015 - Initial Version
!------------------------------------------------------------------------------
module  PFL_StringFormatterMap
   use PFL_Formatter

#define Key __CHARACTER_DEFERRED
#define T Formatter
#define T_polymorphic
#define Map FormatterMap
#define MapIterator FarmatterIterator
#define Pair FormatterPair

#include "map/template.inc"

#undef Pair
#undef MapIterator
#undef Map
#undef T_polymorphic
#undef T
#undef Key

end module PFL_StringFormatterMap

