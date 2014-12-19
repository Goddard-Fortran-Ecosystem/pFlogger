module FTL_String_mod
   use FTL_Object_mod
   implicit none
   private

   public :: String
   public :: toString

   type, extends(Object) :: String
      private
      character(len=:), allocatable :: item
   contains
      procedure :: toString => toString_string
      procedure :: get
      procedure, pass(a) :: copy_fromStr
      procedure, pass(b) :: copy_toStr
      generic :: assignment(=) => copy_fromStr, copy_toStr
      procedure :: areEqual
      generic :: operator(==) => areEqual
   end type String

   interface String
      module procedure newString_empty
      module procedure newString_str
   end interface String

   interface toString
      module procedure toString_string
   end interface toString

contains


   function newString_empty()
      type (String) :: newString_empty
      newString_empty = String('')
   end function newString_empty


   function newString_str(str)
      type (String) :: newString_str
      character(len=*), intent(in) :: str
      newString_str%item = trim(str)
   end function newString_str


   pure function toString_string(this) result(str)
      character(len=:), allocatable :: str
      class (String), intent(in) :: this
      str = this%item
   end function toString_string

   
   function get(this) result(str)
      character(len=:), pointer :: str
      class (String), target, intent(in) :: this
      str => this%item
   end function get

   
   subroutine copy_fromStr(a, b)
      class (String), intent(out) :: a
      character(len=*), intent(in) :: b

      a%item = trim(b)

   end subroutine copy_fromStr


   subroutine copy_toStr(a, b)
      character(len=:), allocatable, intent(out) :: a
      class (String), intent(in) :: b

      a = b%item

   end subroutine copy_toStr
   

   logical function areEqual(a, b) 
      class (String), intent(in) :: a
      type (String), intent(in) :: b

      areEqual = (a%item == b%item)

   end function areEqual


end module FTL_String_mod
