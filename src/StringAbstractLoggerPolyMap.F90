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

#define _map LoggerMap
#define _iterator LoggerIterator
#define _pair StringLoggerPair
#include "types/key_deferredLengthString.inc"

#define _value class(AbstractLogger)
#define _value_allocatable
#define _value_equal_defined


#define _alt
#define _pair_allocatable   
#include "templates/map.inc"

end module PFL_StringAbstractLoggerPolyMap

