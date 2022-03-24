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

#define T AbstractFilter
#define T_polymorphic
#define Vector FilterVector
#define VectorIterator FilterVectorIterator
#define T_EQ(lhs,rhs) (lhs==rhs)

#include "vector/template.inc"

end module PFL_AbstractFilterPolyVector
