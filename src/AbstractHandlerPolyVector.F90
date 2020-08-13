!------------------------------------------------------------------------------
! NASA/GSFC, CISTO, Code 606, Advanced Software Technology Group
!------------------------------------------------------------------------------
!
! MODULE: PFL_AbstractHandlerPolyVector
!
!> @brief
!> This class provides a vector of handlers. Uses FTL.
!
!> @author 
!> ASTG staff
!
!> @date 01 Jun 2015 - Initial Version
!------------------------------------------------------------------------------
module PFL_AbstractHandlerPolyVector
   use PFL_AbstractHandler

#define _type class (AbstractHandler)
#define _allocatable
#define _equal_defined
#define _vector HandlerVector
#define _iterator HandlerVectorIterator

#include "templates/vector.inc"

end module PFL_AbstractHandlerPolyVector
