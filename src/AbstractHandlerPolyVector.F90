!------------------------------------------------------------------------------
! NASA/GSFC, CISTO, Code 606, Advanced Software Technology Group
!------------------------------------------------------------------------------
!
! MODULE: ASTG_AbstractHandlerPolyVector_mod
!
!> @brief
!> This class provides a vector of handlers. Uses FTL.
!
!> @author 
!> ASTG staff
!
!> @date 01 Jun 2015 - Initial Version
!------------------------------------------------------------------------------
module ASTG_AbstractHandlerPolyVector_mod
   use ASTG_AbstractHandler_mod

#define _type class (AbstractHandler)
#define _polymorphic
#define _equal_defined

#include "templates/vector.inc"

end module ASTG_AbstractHandlerPolyVector_mod
