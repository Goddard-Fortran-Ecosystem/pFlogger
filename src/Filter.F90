!------------------------------------------------------------------------------
! NASA/GSFC, CISTO, Code 606, Advanced Software Technology Group
!------------------------------------------------------------------------------
!
! MODULE: ASTG_Filter_mod
!
! AUTHOR: ASTG staff
!
! DESCRIPTION: 
! Use a filter to perform arbitrary filtering of LogRecords.
! Loggers and Handlers can optionally use Filter instances to filter
! records as desired. The base filter class only allows events which are
! below a certain point in the logger hierarchy. For example, a filter
! initialized with "A.B" will allow events logged by loggers "A.B",
! "A.B.C", "A.B.C.D", "A.B.D" etc. but not "A.BB", "B.A.B" etc. If
! initialized with the empty string, all events are passed.
!------------------------------------------------------------------------------
module ASTG_Filter_mod
   use FTL_CaseInsensitiveString_mod
   use ASTG_Object_mod
   use ASTG_LogRecord_mod
   use ASTG_AbstractFilter_mod, only: AbstractFilter
   implicit none
   private

   public :: Filter

   type, extends(AbstractFilter) :: Filter
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


   !---------------------------------------------------------------------------  
   ! FUNCTION: 
   ! newFilter
   !
   ! DESCRIPTION: 
   ! Initialize filter with the name of the logger which, together with its
   ! children, will have its events allowed through the filter. If no
   ! name is specified, allow every event.
   !---------------------------------------------------------------------------
   function newFilter(name) result(f)
      type (Filter) :: f
      character(len=*), intent(in) :: name

      call f%setName(name)

   end function newFilter

   
   !---------------------------------------------------------------------------  
   ! FUNCTION: 
   ! filter
   !
   ! DESCRIPTION: 
   ! Determine if LogRecord can be logged.
   ! Is the specified record to be logged? Returns FALSE for no, TRUE for
   ! yes.
   !---------------------------------------------------------------------------
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

   !---------------------------------------------------------------------------  
   ! FUNCTION: 
   ! equal
   !
   ! DESCRIPTION: 
   ! Overloads 'equal' operation for filters.
   !---------------------------------------------------------------------------  
   logical function equal(a, b)
      class(Filter), intent(in) :: a
      class(AbstractFilter), intent(in) :: b

      select type (b)
      type is (Filter)
         equal = (a%name == b%name)
      class default
         equal = .false.
      end select

   end function equal


   !---------------------------------------------------------------------------  
   ! FUNCTION: 
   ! notEqual
   !
   ! DESCRIPTION: 
   ! Overloads 'not equal' operation for filters.
   !---------------------------------------------------------------------------  
   logical function notEqual(a, b)
      class(Filter), intent(in) :: a
      class(Filter), intent(in) :: b

      notEqual = .not. (a == b)

   end function notEqual


   !---------------------------------------------------------------------------  
   ! FUNCTION: 
   ! getName
   !
   ! DESCRIPTION: 
   ! Get the name associated with this filter.
   !---------------------------------------------------------------------------  
   function getName(this) result(name)
      class (Filter), intent(in) :: this
      character(len=:), allocatable :: name

      name = this%name

   end function getName


   !---------------------------------------------------------------------------  
   ! ROUTINE: 
   ! setName
   !
   ! DESCRIPTION: 
   ! Set the name associated with this filter.
   !---------------------------------------------------------------------------  
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
