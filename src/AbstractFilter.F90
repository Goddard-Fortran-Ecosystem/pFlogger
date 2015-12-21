!------------------------------------------------------------------------------
! NASA/GSFC, CISTO, Code 606, Advanced Software Technology Group
!------------------------------------------------------------------------------
!
! MODULE: PFL_AbstractFilter_mod
!
!> @brief
!> AbstractFilter class provides common code to use in Filter class.
!
!> @author 
!> ASTG staff
!
!> @date 01 Jan 2015 - Initial Version
!------------------------------------------------------------------------------
module PFL_AbstractFilter_mod
   use PFL_Object_mod
   use PFL_LogRecord_mod
   
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

   
end module PFL_AbstractFilter_mod
