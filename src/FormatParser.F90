module ASTG_FormatParser_mod
   use FTL_StringVec_mod
   implicit none
   private

   public FormatParser
   
   type FormatParser
   contains
      procedure, nopass :: isFormat
      procedure, nopass :: getTokens
   end type FormatParser

   character(len=1), parameter :: FORMAT_DELIMITER = '%'
   
contains


   logical function isFormat(string)
      character(len=*) :: string

      isFormat = (string(1:1) == FORMAT_DELIMITER)
      
   end function isFormat

   function getTokens(str) result(tokens)
      type(StringVec) :: tokens
      character(len=*), intent(in) :: str
      character(len=:), allocatable :: tmp
      integer :: loc, n

      tokens = StringVec()

      if (str == '') return

      tmp = str
      do while (len(tmp) > 0)
         loc = scan(tmp, FORMAT_DELIMITER)
         if (loc > 1) then
           call tokens%push_back(tmp(1:loc-1))
           n = len(tmp)
           tmp = tmp(loc+1:n)
         else
           call tokens%push_back(tmp)
           tmp = ''
         end if
      end do
      
   end function getTokens
   
end module ASTG_FormatParser_mod
