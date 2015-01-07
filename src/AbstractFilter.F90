module ASTG_AbstractFilter_mod
   use ASTG_Object_mod
   use ASTG_LogRecord_mod
   
   implicit none
   private

   public :: AbstractFilter

   type, abstract, extends (Object) :: AbstractFilter
      private
   contains
      procedure(filter), deferred :: filter
      procedure(equal), deferred :: equal
      generic :: operator(==) => equal
   end type AbstractFilter

   abstract interface

      logical function filter(this, record)
         import AbstractFilter
         import LogRecord
         class (AbstractFilter), intent(in) :: this
         class (LogRecord), intent(inout) :: record
      end function filter 

      logical function equal(a, b)
         import AbstractFilter
         class(AbstractFilter), intent(in) :: a
         class(AbstractFilter), intent(in) :: b
      end function equal

   end interface

   
end module ASTG_AbstractFilter_mod
