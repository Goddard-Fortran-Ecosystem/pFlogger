!------------------------------------------------------------------------------
! NASA/GSFC, CISTO, Code 606, Advanced Software Technology Group
!------------------------------------------------------------------------------
!
! MODULE: PFL_AbstractFilter
!
!> @brief
!> AbstractFilter class provides common code to use in Filter class.
!
!> @author 
!> ASTG staff
!
!> @date 01 Jan 2015 - Initial Version
!------------------------------------------------------------------------------
module PFL_AbstractFilter
   implicit none
   private

   public :: AbstractFilter

   type, abstract :: AbstractFilter
      private
   contains
      procedure(do_filter), deferred :: do_filter
      procedure(equal), deferred :: equal
      generic :: operator(==) => equal
   end type AbstractFilter

   abstract interface

      logical function do_filter(this, record)
         use PFL_LogRecord, only: LogRecord
         import AbstractFilter
         class (AbstractFilter), intent(in) :: this
         class (LogRecord), intent(in) :: record
      end function do_filter

      logical function equal(a, b)
         import AbstractFilter
         class(AbstractFilter), intent(in) :: a
         class(AbstractFilter), intent(in) :: b
      end function equal

   end interface

   
end module PFL_AbstractFilter
