!------------------------------------------------------------------------------
! NASA/GSFC, CISTO, Code 606, Advanced Software Technology Group
!------------------------------------------------------------------------------
!
!*MODULE: ASTG_StringAbstractLoggerPolyMap_mod
!
!> @brief Case insensitive abstract logger map.
!
!> @author ASTG staff
!> @date 01 Jan 2015 - Initial Version
!------------------------------------------------------------------------------
module  ASTG_StringAbstractLoggerPolyMap_mod
   use ASTG_AbstractLogger_mod

#define _map LoggerMap
#define _iterator LoggerIterator
#include "types/key_deferredLengthString.inc"

#define _value class(AbstractLogger)
#define _value_allocatable
#define _value_equal_defined

#define _alt
#define _pair_allocatable   
#include "templates/map.inc"

end module ASTG_StringAbstractLoggerPolyMap_mod

