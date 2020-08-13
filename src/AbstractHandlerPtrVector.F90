!------------------------------------------------------------------------------
! NASA/GSFC, CISTO, Code 606, Advanced Software Technology Group
!------------------------------------------------------------------------------
!
! MODULE: PFL_AbstractHandlerPtrVector
!
!> @brief
!> This class provides a vector of handlers. Uses FTL.
!
!> @author 
!> ASTG staff
!
!> @date 01 Jun 2015 - Initial Version
!------------------------------------------------------------------------------
module PFL_AbstractHandlerPtrVector
   use PFL_AbstractHandler

#define _type class (AbstractHandler)
#define _pointer
#define _vector HandlerPtrVector
#define _iterator HandlerPtrVectorIterator

#include "templates/vector.inc"

end module PFL_AbstractHandlerPtrVector
