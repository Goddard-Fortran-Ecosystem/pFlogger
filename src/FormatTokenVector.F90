!------------------------------------------------------------------------------
! NASA/GSFC, CISTO, Code 606, Advanced Software Technology Group
!------------------------------------------------------------------------------
!
!*MODULE: PFL_FormatTokenVector
!
!> @brief Class that provides format token vector.
!
!> @author ASTG staff
!> @date 01 Jan 2015 - Initial Version
!------------------------------------------------------------------------------
module PFL_FormatTokenVector
   use PFL_FormatToken

#define T FormatToken
#define Vector FormatTokenVector
#define VectorIterator FormatTokenVectorIterator
#include "vector/template.inc"
end module PFL_FormatTokenVector
