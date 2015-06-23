! This module implements a derived type that behaves like
! a case-insensitive string, except that accessors preserve
! capitalization.

module ASTG_CaseInsensitiveString_mod
   use ASTG_StringUtilities_mod, only: toLowerCase
   implicit none
   private

   public :: CaseInsensitiveString
   public :: toString

   type CaseInsensitiveString
      private
      character(:), allocatable :: string
      character(:), allocatable :: lowerCase
   contains
      procedure :: toString => toString_cistring
      procedure :: get
      generic :: assignment(=) => copyFromString, copyToString
      generic :: operator(==) => equal_ciStr_ciStr
      generic :: operator(==) => equal_str_ciStr
      generic :: operator(==) => equal_ciStr_str
      generic :: operator(/=) => unequal_ciStr_ciStr
      generic :: operator(/=) => unequal_str_ciStr
      generic :: operator(/=) => unequal_ciStr_str

      procedure :: copyFromString
      procedure, pass(this) :: copyToString
      procedure :: equal_ciStr_ciStr
      procedure, pass(this) :: equal_str_ciStr
      procedure :: equal_ciStr_str

      procedure :: unequal_ciStr_ciStr
      procedure, pass(this) :: unequal_str_ciStr
      procedure :: unequal_ciStr_str
   end type CaseInsensitiveString

   interface CaseInsensitiveString
      module procedure newCaseInsensitiveString
   end interface CaseInsensitiveString

   interface toString
      module procedure toString_cistring
   end interface toString

contains


   function newCaseInsensitiveString(string) result(ciString)
      type (CaseInsensitiveString) :: ciString
      character(*), intent(in) :: string

      ciString = string ! user assignment overload rather than duplicate

   end function newCaseInsensitiveString


   pure function toString_cistring(this) result(string)
      class (CaseInsensitiveString), intent(in) :: this
      character(:), allocatable :: string

      string = this%string

   end function toString_cistring


   ! This used by containers to enable raw Fortran strings
   function get(this) result(string)
      class (CaseInsensitiveString), target, intent(in) :: this
      character(:), pointer :: string

      string => this%string

   end function get

   subroutine copyToString(string, this)
      character(len=:), allocatable, intent(out) :: string
      class (CaseInsensitiveString), intent(in) :: this

      string = this%string
   end subroutine copyToString


   subroutine copyFromString(a, b)
      class (CaseInsensitiveString), intent(inout) :: a
      character(len=*), intent(in) :: b

      a%string = b
      a%lowerCase = toLowerCase(b)

   end subroutine copyFromString


   logical function equal_ciStr_ciStr(this, b) result(areEqual)
      class (CaseInsensitiveString), intent(in) :: this
      class (CaseInsensitiveString), intent(in) :: b

      areEqual = (this%lowerCase == b%lowerCase)
      
   end function equal_ciStr_ciStr

   logical function equal_ciStr_str(this, b) result(areEqual)
      class (CaseInsensitiveString), intent(in) :: this
      character(*), intent(in) :: b

      areEqual = (this%lowerCase == toLowerCase(b))
      
   end function equal_ciStr_str

   logical function equal_str_ciStr(a, this) result(areEqual)
      character(*), intent(in) :: a
      class (CaseInsensitiveString), intent(in) :: this

      areEqual = (this == a)
      
   end function equal_str_ciStr


   logical function unequal_ciStr_ciStr(this, b) result(areUnequal)
      class (CaseInsensitiveString), intent(in) :: this
      class (CaseInsensitiveString), intent(in) :: b

      areUnequal = (this%lowerCase /= b%lowerCase)
      
   end function unequal_ciStr_ciStr

   logical function unequal_ciStr_str(this, b) result(areUnequal)
      class (CaseInsensitiveString), intent(in) :: this
      character(*), intent(in) :: b

      areUnequal = (this%lowerCase /= toLowerCase(b))
      
   end function unequal_ciStr_str

   logical function unequal_str_ciStr(a, this) result(areUnequal)
      character(*), intent(in) :: a
      class (CaseInsensitiveString), intent(in) :: this

      areUnequal = (this /= a)
      
   end function unequal_str_ciStr


end module ASTG_CaseInsensitiveString_mod
