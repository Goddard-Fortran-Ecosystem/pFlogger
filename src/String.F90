module PFL_String_mod
   use PFL_Object_mod
   implicit none
   private

   public :: String
   public :: toString
   public :: len

   type, extends(Object) :: String
      private
      character(len=:), allocatable :: item
   contains
      procedure :: toString_self ! override toString() from Object
      procedure :: get
      generic :: assignment(=) => copyFromString, copyToString !, copySelf
      generic :: operator(==) => equal_str_string, equal_string_str, equal_string_string
      generic :: operator(/=) => notEqual_str_string, notEqual_string_str, notEqual_string_string

      procedure, pass(a) :: copySelf
      procedure, pass(a) :: copyFromString
      procedure, pass(b) :: copyToString
      procedure :: equal_string_string
      procedure, pass(b) :: equal_str_string
      procedure :: equal_string_str
      procedure :: notEqual_string_string
      procedure, pass(b) :: notEqual_str_string
      procedure :: notEqual_string_str
   end type String


   interface String
      module procedure newString_empty
      module procedure newString_str
    end interface

   interface toString
      module procedure toString_self
   end interface toString


   interface len
      module procedure len_string
   end interface len


contains


   function newString_empty()
      type (String) :: newString_empty
      newString_empty = String('')
   end function newString_empty


   function newString_str(str)
      type (String) :: newString_str
      character(len=*), intent(in) :: str

      newString_str%item = str

   end function newString_str


   pure function toString_self(this) result(str)
      character(len=:), allocatable :: str
      class (String), intent(in) :: this
      str = this%item
   end function toString_self

   
   function get(this) result(str)
      character(len=:), pointer :: str
      class (String), target, intent(in) :: this
      str => this%item
   end function get

   
   subroutine copySelf(a, b)
      class (String), intent(out) :: a
      class (String), intent(in) :: b

      a = b%toString()

   end subroutine copySelf


   subroutine copyFromString(a, b)
      class (String), intent(out) :: a
      character(len=*), intent(in) :: b

      a%item = trim(b)

   end subroutine copyFromString


   subroutine copyToString(a, b)
      character(len=:), allocatable, intent(out) :: a
      class (String), intent(in) :: b

      a = b%item

   end subroutine copyToString
   

   logical function equal_string_string(a, b) result(areEqual)
      class (String), intent(in) :: a
      class (String), intent(in) :: b

      areEqual = (a%item == b%item)
      
   end function equal_string_string


   logical function equal_str_string(a, b)  result(areEqual)
      character(len=*), intent(in) :: a
      class (String), intent(in) :: b

      areEqual = (a == b%item)
      
   end function equal_str_string

   logical function equal_string_str(a, b)  result(areEqual)
      class (String), intent(in) :: a
      character(len=*), intent(in) :: b

      areEqual = (a%item == b)
      
   end function equal_string_str


   logical function notEqual_string_string(a, b) result(notEqual)
      class (String), intent(in) :: a
      class (String), intent(in) :: b

      notEqual = (.not. (a == b))
      
   end function notEqual_string_string


   logical function notEqual_str_string(a, b)  result(notEqual)
      character(len=*), intent(in) :: a
      class (String), intent(in) :: b

      notEqual = (.not. (a == b))
      
   end function notEqual_str_string


   logical function notEqual_string_str(a, b)  result(notEqual)
      class (String), intent(in) :: a
      character(len=*), intent(in) :: b

      notEqual = (.not. (a == b))
      
   end function notEqual_string_str


   integer function len_string(str)
      type (String), intent(in) :: str
      len_string = len(str%item)
   end function len_string


end module PFL_String_mod
