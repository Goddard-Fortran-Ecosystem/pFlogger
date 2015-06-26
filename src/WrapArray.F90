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

      block
        use iso_fortran_env, only: int32, real32, int64, real64, real128
        select type (p => wrapper%array)
        type is (integer(int32))
           select type (q => array)
           type is (integer(int32))
              p = q
           end select
        type is (integer(int64))
           select type (q => array)
           type is (integer(int64))
              p = q
           end select
        type is (real(real32))
           select type (q => array)
           type is (real(real32))
              p = q
           end select
        type is (real(real64))
           select type (q => array)
           type is (real(real64))
              p = q
           end select
        type is (logical)
           select type (q => array)
           type is (logical)
              p = q
           end select
        type is (character(len=*))
           select type (q => array)
           type is (character(len=*))
              p = q
           end select
           class default ! user defined
        end select
      end block

      
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
