!------------------------------------------------------------------------------
! NASA/GSFC, CISTO, Code 606, Advanced Software Technology Group
!------------------------------------------------------------------------------
!
! MODULE: PFL_AbstractFilterPolyVector_mod
!
!> @brief
!> This class provides a vector of filters. Uses FTL.
!
!> @author 
!> ASTG staff
!
!> @date 01 Jun 2015 - Initial Version
!------------------------------------------------------------------------------
module PFL_AbstractFilterPolyVector_mod
   use PFL_AbstractFilter_mod

#define _type class (AbstractFilter)
#define _allocatable
#define _equal_defined

#include "templates/vector.inc"

end module PFL_AbstractFilterPolyVector_mod
