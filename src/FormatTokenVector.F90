!------------------------------------------------------------------------------
! NASA/GSFC, CISTO, Code 606, Advanced Software Technology Group
!------------------------------------------------------------------------------
!
!*MODULE: ASTG_FormatTokenVector_mod
!
!> @brief Class that provides format token vector.
!
!> @author ASTG staff
!> @date 01 Jan 2015 - Initial Version
!------------------------------------------------------------------------------
module ASTG_FormatTokenVector_mod
   use ASTG_FormatToken_mod
#define _type type(FormatToken)
#include "templates/vector.inc"
end module ASTG_FormatTokenVector_mod
