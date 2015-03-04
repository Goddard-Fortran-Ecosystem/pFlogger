!------------------------------------------------------------------------------
! NASA/GSFC, CISTO, Code 606, Advanced Software Technology Group
!------------------------------------------------------------------------------
!
! MODULE: ASTG_AbstractFilter_mod
!
!> @author 
!> ASTG staff
!
! DESCRIPTION:
!> @brief
!> AbstractFilter class provides common code to use in Filter class.
!
! REVISION HISTORY:
! 01 Jan 2015 - Initial Version
!------------------------------------------------------------------------------
module ASTG_AbstractFilter_mod
   use ASTG_Object_mod
   use ASTG_LogRecord_mod
   
   implicit none
   private

   public :: AbstractFilter

   type, abstract, extends (Object) :: AbstractFilter
      private
   contains
      procedure(doFilter), deferred :: doFilter
      procedure(equal), deferred :: equal
      generic :: operator(==) => equal
   end type AbstractFilter

   abstract interface

      logical function doFilter(this, record)
         import AbstractFilter
         import LogRecord
         class (AbstractFilter), intent(in) :: this
         class (LogRecord), intent(inout) :: record
      end function doFilter

      logical function equal(a, b)
         import AbstractFilter
         class(AbstractFilter), intent(in) :: a
         class(AbstractFilter), intent(in) :: b
      end function equal

   end interface

   
end module ASTG_AbstractFilter_mod
