! Perform arbitrary filtering of LogRecords.
! Filter class only allows records which are below a certain point in the
! logger hierarchy. 
module ASTG_Filter_mod
   use ASTG_Object_mod
   use ASTG_LogRecord_mod
   implicit none
   private

   public :: Filter

   type, extends(Object) :: Filter
      private
      character(len=:), allocatable :: name
   contains
      procedure :: filter => filter_ ! name conflict
      procedure :: equal
      generic :: operator(==) => equal
      procedure :: notEqual
      generic :: operator(/=) => notEqual
   end type Filter

   interface Filter
      module procedure newFilter
   end interface Filter


contains


   ! Initialize filter with the name of the Logger
   function newFilter(name) result(f)
      type (Filter) :: f
      character(len=*), intent(in) :: name
      f%name = name
   end function newFilter

   
   ! Determine if LogRecord can be logged
   logical function filter_(this, record)
      class (Filter), intent(in) :: this
      class (LogRecord), intent(inout) :: record

      character(len=:), allocatable :: recordName

      recordName = record%getName()
      if (this%name == recordName(1:len(this%name))) then
         filter_ = .true.  ! do emit
      else
         filter_ = .false. ! do NOT emit
      end if

   end function filter_

   logical function equal(a, b)
      class(Filter), intent(in) :: a
      class(Filter), intent(in) :: b

      equal = (a%name == b%name)

   end function equal


   logical function notEqual(a, b)
      class(Filter), intent(in) :: a
      class(Filter), intent(in) :: b

      notEqual = .not. (a == b)

   end function notEqual

end module ASTG_Filter_mod
