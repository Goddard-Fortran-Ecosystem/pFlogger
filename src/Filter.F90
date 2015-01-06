! Perform arbitrary filtering of LogRecords.
! Filter class only allows records which are below a certain point in the
! logger hierarchy. 
module ASTG_Filter_mod
   use ASTG_Object_mod
   use ASTG_LogRecord_mod
   use FTL_CaseInsensitiveString_mod
   implicit none
   private

   public :: Filter

   type, extends(Object) :: Filter
      private
      ! 'allocatable' below is a workaround for ifort 15.0.1
      ! Of course it then breaks gfortrn 4.9.1.  sigh.
#ifdef __INTEL_COMPILER
      type (CaseInsensitiveString), allocatable :: name
#else
      type (CaseInsensitiveString) :: name
#endif
   contains
      procedure :: filter => filter_ ! name conflict
      procedure :: equal
      generic :: operator(==) => equal
      procedure :: notEqual
      generic :: operator(/=) => notEqual
      procedure :: setName
      procedure :: getName
      procedure :: toString_self
   end type Filter


   interface Filter
      module procedure newFilter
   end interface Filter


contains


   ! Initialize filter with the name of the Logger
   function newFilter(name) result(f)
      type (Filter) :: f
      character(len=*), intent(in) :: name

      call f%setName(name)

   end function newFilter

   
   ! Determine if LogRecord can be logged
   logical function filter_(this, record)
      class (Filter), intent(in) :: this
      class (LogRecord), intent(inout) :: record

      character(len=:), allocatable :: recordName
      integer :: n

      recordName = record%getName()
      n = len(this%name)
      if (this%name == recordName(1:n)) then
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


   function getName(this) result(name)
      class (Filter), intent(in) :: this
      character(len=:), allocatable :: name

      name = this%name

   end function getName


   subroutine setName(this, name)
      class (Filter), intent(inout) :: this
      character(len=*), intent(in) :: name

      this%name = CaseInsensitiveString(name)

   end subroutine setName

   
   function toString_self(this) result(string)
      character(len=:), allocatable :: string
      class (Filter), intent(in) :: this
      
      string = 'Filter - name = ' // this%name%toString()

   end function toString_self


end module ASTG_Filter_mod
