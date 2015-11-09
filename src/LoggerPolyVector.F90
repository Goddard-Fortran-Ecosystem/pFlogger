!------------------------------------------------------------------------------
! NASA/GSFC, CISTO, Code 606, Advanced Software Technology Group
!------------------------------------------------------------------------------
!
!*MODULE: ASTG_AbstractLoggerPolyVector_mod
!
!> @brief Vector of loggers
!
!> @author ASTG staff
!> @date 01 Nov 2015 - Initial Version
!------------------------------------------------------------------------------
module  ASTG_LoggerPolyVector_mod
   use ASTG_Logger_mod

#define _vector LoggerVector
#define _iterator LoggerVecIterator

#define _type class(Logger)
#define _pointer   

#include "templates/vector.inc"

end module ASTG_LoggerPolyVector_mod

