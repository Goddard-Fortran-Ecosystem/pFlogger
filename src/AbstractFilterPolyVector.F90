!------------------------------------------------------------------------------
! NASA/GSFC, CISTO, Code 606, Advanced Software Technology Group
!------------------------------------------------------------------------------
!
! MODULE: ASTG_AbstractFilterPolyVector_mod
!
!> @brief
!> This class provides a vector of filters. Uses FTL.
!
!> @author 
!> ASTG staff
!
!> @date 01 Jun 2015 - Initial Version
!------------------------------------------------------------------------------
module ASTG_AbstractFilterPolyVector_mod
   use ASTG_AbstractFilter_mod

#define _type class (AbstractFilter)
#define _polymorphic
#define _equal_defined

#include "templates/vector.inc"

end module ASTG_AbstractFilterPolyVector_mod
