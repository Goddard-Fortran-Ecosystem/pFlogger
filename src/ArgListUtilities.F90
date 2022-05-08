!------------------------------------------------------------------------------
! NASA/GSFC, CISTO, Code 606, Advanced Software Technology Group
!------------------------------------------------------------------------------
!
!*MODULE: PFL_ArgListUtilities
!
!> @brief Utilities used to deal with unlimited polymorphic entities.
!
!> @author ASTG staff
!> @date 01 Jan 2015 - Initial Version
!------------------------------------------------------------------------------
module PFL_ArgListUtilities
   implicit none
   private

   public :: make_arg_vector

#define ARG_LIST arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9

contains

!---------------------------------------------------------------------------  
!*FUNCTION: make_arg_vector
!
!> @brief Budle optional arguments into a vector.   
!> @details
!> This function bundles a list of optional arguments into a vector container
!> of unlimited polymorphic entities.
!---------------------------------------------------------------------------
   function make_arg_vector(ARG_LIST) result(args)
      use gFTL2_UnlimitedVector
      type (UnlimitedVector) :: args
      include 'recordOptArgs.inc'  

      args = UnlimitedVector()

      if (.not. present(arg1)) return
      call push_one(args, arg1)
      if (.not. present(arg2)) return
      call push_one(args, arg2)
      if (.not. present(arg3)) return
      call push_one(args, arg3)
      if (.not. present(arg4)) return
      call push_one(args, arg4)
      if (.not. present(arg5)) return
      call push_one(args, arg5)
      if (.not. present(arg6)) return
      call push_one(args, arg6)
      if (.not. present(arg7)) return
      call push_one(args, arg7)
      if (.not. present(arg8)) return
      call push_one(args, arg8)
      if (.not. present(arg9)) return
      call push_one(args, arg9)

   contains

      subroutine push_one(args, arg)
         use yafyaml, only: String
         type(UnlimitedVector), intent(inout) :: args
         class(*), intent(in) :: arg

         select type (arg)
         type is (character(*))
            call args%push_back(String(arg))
         class default
            call args%push_back(arg)
         end select
      end subroutine push_one
      
   end function make_arg_vector

end module PFL_ArgListUtilities
