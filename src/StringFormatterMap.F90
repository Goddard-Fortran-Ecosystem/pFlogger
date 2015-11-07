!------------------------------------------------------------------------------
! NASA/GSFC, CISTO, Code 606, Advanced Software Technology Group
!------------------------------------------------------------------------------
!
!*MODULE: ASTG_StringFormatterMap_mod
!
!> @brief Case insensitive abstract logger map.
!
!> @author ASTG staff
!> @date 01 Jan 2015 - Initial Version
!------------------------------------------------------------------------------
module  ASTG_StringFormatterMap_mod
   use ASTG_Formatter_mod
#define _map FormatterMap
#define _iterator FormatterIterator
   
#include "types/key_deferredLengthString.inc"

#define _value class (Formatter)
#define _value_allocatable

#define _alt
#include "templates/map.inc"


end module ASTG_StringFormatterMap_mod

