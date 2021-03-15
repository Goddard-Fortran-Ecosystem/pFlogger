!------------------------------------------------------------------------------
! NASA/GSFC, CISTO, Code 606, Advanced Software Technology Group
!------------------------------------------------------------------------------
!
! MODULE: PFL_Filter
!
!> @author 
!> ASTG staff
!
! DESCRIPTION: 
!> @brief
!> Use a filter to perform arbitrary filtering of LogRecords.
!> Loggers and Handlers can optionally use Filter instances to filter
!> records as desired. The base filter class only allows events which are
!> below a certain point in the logger hierarchy. For example, a filter
!> initialized with "A.B" will allow events logged by loggers "A.B",
!> "A.B.C", "A.B.C.D", "A.B.D" etc. but not "A.BB", "B.A.B" etc. If
!> initialized with the empty string, all events are passed.
!
! REVISION HISTORY:
! 01 Jan 2015 - Initial Version
!------------------------------------------------------------------------------
module PFL_Filter
   use PFL_Object
   use PFL_LogRecord
   use PFL_AbstractFilter, only: AbstractFilter
   implicit none
   private

   public :: Filter
   public :: initfilter

   type, extends(AbstractFilter) :: Filter
      private
      character(len=:), allocatable :: name
   contains
      procedure :: do_filter
      procedure :: equal
      procedure :: notEqual
      generic :: operator(/=) => notEqual
      procedure :: set_name
      procedure :: get_name
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

      call f%set_name(name)

   end function newFilter


   !---------------------------------------------------------------------------  
   ! FUNCTION: 
   ! initFilter
   !
   ! DESCRIPTION: 
   ! Initialize filter with the name of the logger which, together with its
   ! children, will have its events allowed through the filter. If no
   ! name is specified, allow every event.
   !---------------------------------------------------------------------------
   subroutine initFilter(f, name)
      type (Filter), intent(out) :: f
      character(len=*), intent(in) :: name

      call f%set_name(name)

   end subroutine initFilter

   
   !---------------------------------------------------------------------------  
   ! FUNCTION: 
   ! filter
   !
   ! DESCRIPTION: 
   ! Determine if LogRecord can be logged.
   ! Is the specified record to be logged? Returns FALSE for no, TRUE for
   ! yes.
   !---------------------------------------------------------------------------
   logical function do_filter(this, record)
      class (Filter), intent(in) :: this
      class (LogRecord), intent(in) :: record

      character(len=:), allocatable :: recordName
      integer :: n

      recordName = record%get_name()

      n = len(this%name)

      if (sameString(this%name,recordName)) then
         do_filter = .true.  ! do emit
      else
         do_filter = .false. ! do NOT emit
      end if

   end function do_filter

   logical function sameString(a, b) result(same)
      character(len=*), intent(in) :: a
      character(len=*), intent(in) :: b

      character(len=1) :: charA, charB
      integer :: i

      do i = 1, min(len(a),len(b))
         charA = lowerCase(a(i:i))
         charB = lowerCase(b(i:i))
         if (charA /= charB) then
            same = .false.
            return
         end if
      end do

      ! only match if _all_ of a is included in b.
      same = (len(a) <= len(b))

   contains

      character(len=1) function lowerCase(char) result(lChar)
         integer, parameter :: UPPER_LOWER_DELTA = iachar('A') - iachar('a')
         character(len=1), intent(in) :: char

         if (char >= 'A' .and. char <= 'Z') then
            lChar = achar(iachar(char) - UPPER_LOWER_DELTA)
         else
            lChar = char
         end if

      end function lowerCase
            
   end function sameString

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
   ! get_name
   !
   ! DESCRIPTION: 
   ! Get the name associated with this filter.
   !---------------------------------------------------------------------------  
   function get_name(this) result(name)
      class (Filter), intent(in) :: this
      character(len=:), allocatable :: name

      name = this%name

   end function get_name


   !---------------------------------------------------------------------------  
   ! ROUTINE: 
   ! set_name
   !
   ! DESCRIPTION: 
   ! Set the name associated with this filter.
   !---------------------------------------------------------------------------  
   subroutine set_name(this, name)
      class (Filter), intent(inout) :: this
      character(len=*), intent(in) :: name

      this%name = name

   end subroutine set_name

   
   function toString_self(this) result(string)
      character(len=:), allocatable :: string
      class (Filter), intent(in) :: this
      
      string = 'Filter - name = ' // this%name

   end function toString_self


end module PFL_Filter
