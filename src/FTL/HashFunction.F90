module FTL_HashFunction_mod
   implicit none
   private

   public :: hashFunction

   interface hashFunction
      module procedure hashFunction_integer
      module procedure hashFunction_character
      module procedure hashFunction_String
      module procedure hashFunction_caseInsensitiveString
   end interface hashFunction

contains


   ! Trivial implementation for now - should be improved.
   pure integer function hashFunction_integer(i) result(hash)
      integer, intent(in) :: i

      hash = 1 + abs(i)

   end function hashFunction_integer


   ! Trivial implementation for now - should be improved.
   pure integer function hashFunction_character(string) result(hash)
      character(len=*), intent(in) :: string

      integer :: i

      hash = 0

      do i = 1, len_trim(string)
         hash = hash + iachar(string(i:i))
      end do
      
   end function hashFunction_character


   ! Trivial implementation for now - should be improved.
   pure integer function hashFunction_string(str) result(hash)
      use FTL_String_mod
      type (String), intent(in) :: str

      hash = hashFunction(str%toString())
      
   end function hashFunction_string


   pure integer function hashFunction_caseInsensitiveString(ciString) result(hash)
      use FTL_CaseInsensitiveString_mod
      use FTL_StringUtilities_mod, only : toLowerCase
      type (CaseInsensitiveString), intent(in) :: ciString

      hash = hashFunction(toLowerCase(ciString%toString()))
      
   end function hashFunction_caseInsensitiveString


end module FTL_HashFunction_mod
