!---------------------------------------------------------------------
! This module enables the encapsulation of arrays as scalars for
! processing with Formatters.
!
! NOTE: that with Gfortran 5.1, incorrect results will be obtained
! if a literal (as opposed to a variable) is passed to wrapArray()
!---------------------------------------------------------------------

module ASTG_WrapArray_mod
   implicit none
   private

   public :: wrapArray
   public :: WrapArray1d
   public :: WrapArray2d
   
   type :: WrapArray1d
      class(*), allocatable :: array(:)
   end type WrapArray1d

   type :: WrapArray2d
      class(*), allocatable :: array(:,:)
   end type WrapArray2d

   interface wrapArray
      module procedure wrap1d
      module procedure wrap2d
   end interface wrapArray

contains


   function wrap1d(array) result(wrapper)
      type (WrapArray1d) :: wrapper
      class (*), intent(in) :: array(:)
#ifndef __GFORTRAN__
      allocate(wrapper%array, source=array)
#else
      allocate(wrapper%array(size(array,1)), source=array)
#endif
   end function wrap1d

   function wrap2d(array) result(wrapper)
      type (WrapArray2d) :: wrapper
      class (*), intent(in) :: array(:,:)
#ifndef __GFORTRAN__
      allocate(wrapper%array, source=array)
#else
      allocate(wrapper%array(size(array,1),size(array,2)), source=array)
#endif
   end function wrap2d
   
end module ASTG_WrapArray_mod
