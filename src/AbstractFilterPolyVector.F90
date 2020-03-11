!------------------------------------------------------------------------------
! NASA/GSFC, CISTO, Code 606, Advanced Software Technology Group
!------------------------------------------------------------------------------
!
! MODULE: PFL_AbstractFilterPolyVector
!
!> @brief
!> This class provides a vector of filters. Uses FTL.
!
!> @author 
!> ASTG staff
!
!> @date 01 Jun 2015 - Initial Version
!------------------------------------------------------------------------------
module PFL_AbstractFilterPolyVector
   use PFL_AbstractFilter

#define _type class (AbstractFilter)
#define _allocatable
#define _equal_defined

#include "templates/vector.inc"

end module PFL_AbstractFilterPolyVector
