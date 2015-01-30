!------------------------------------------------------------------------------
! NASA/GSFC, CISTO, Code 606, Advanced Software Technology Group
!------------------------------------------------------------------------------
!
! MODULE: ASTG_ArgListUtilities_mod
!
! AUTHOR: ASTG staff
!
! DESCRIPTION:
! Utilities used to deal with unlimited polymorphic entities.
!------------------------------------------------------------------------------
module ASTG_ArgListUtilities_mod
   implicit none
   private

   public :: makeArgVector

#define ARG_LIST arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9

contains

   !---------------------------------------------------------------------------  
   ! FUNCTION: 
   ! makeArgVector
   !
   ! DESCRIPTION: 
   ! This function bundles alist of optional arguments into a vector container
   ! of unlimited polymorphic entities.
   !---------------------------------------------------------------------------
   function makeArgVector(ARG_LIST) result(args)
      use FTL_XWrapVec_mod
      type (XWrapVec) :: args
      include 'recordOptArgs.inc'  

      args = XWrapVec()

      if (present(arg1)) call args%push_back_alt(arg1)
      if (present(arg2)) call args%push_back_alt(arg2)
      if (present(arg3)) call args%push_back_alt(arg3)
      if (present(arg4)) call args%push_back_alt(arg4)
      if (present(arg5)) call args%push_back_alt(arg5)
      if (present(arg6)) call args%push_back_alt(arg6)
      if (present(arg7)) call args%push_back_alt(arg7)
      if (present(arg8)) call args%push_back_alt(arg8)
      if (present(arg9)) call args%push_back_alt(arg9)
      
   end function makeArgVector

end module ASTG_ArgListUtilities_mod
