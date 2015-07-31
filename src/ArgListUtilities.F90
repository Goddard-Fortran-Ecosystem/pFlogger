!------------------------------------------------------------------------------
! NASA/GSFC, CISTO, Code 606, Advanced Software Technology Group
!------------------------------------------------------------------------------
!
!*MODULE: ASTG_ArgListUtilities_mod
!
!> @brief Utilities used to deal with unlimited polymorphic entities.
!
!> @author ASTG staff
!> @date 01 Jan 2015 - Initial Version
!------------------------------------------------------------------------------
module ASTG_ArgListUtilities_mod
   implicit none
   private

   public :: makeArgVector

#define ARG_LIST arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9

contains

!---------------------------------------------------------------------------  
!*FUNCTION: makeArgVector
!
!> @brief Budle optional arguments into a vector.   
!> @details
!> This function bundles a list of optional arguments into a vector container
!> of unlimited polymorphic entities.
!---------------------------------------------------------------------------
   function makeArgVector(ARG_LIST) result(args)
      use ASTG_UnlimitedVector_mod, only: UnlimitedVector => Vector
      type (UnlimitedVector) :: args
      include 'recordOptArgs.inc'  

      args = UnlimitedVector()

      if (present(arg1)) call args%push_back(arg1)
      if (present(arg2)) call args%push_back(arg2)
      if (present(arg3)) call args%push_back(arg3)
      if (present(arg4)) call args%push_back(arg4)
      if (present(arg5)) call args%push_back(arg5)
      if (present(arg6)) call args%push_back(arg6)
      if (present(arg7)) call args%push_back(arg7)
      if (present(arg8)) call args%push_back(arg8)
      if (present(arg9)) call args%push_back(arg9)
      
   end function makeArgVector

end module ASTG_ArgListUtilities_mod
