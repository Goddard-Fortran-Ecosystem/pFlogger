module ASTG_FormatParser_mod
   implicit none
   private

   public FormatParser
   public Token
   
   type FormatParser
   contains
      procedure, nopass :: isFormat
      procedure, nopass :: getTokens
   end type FormatParser

   type Token
      character(len=:), allocatable :: token
   end type Token
   
   character(len=1), parameter :: FORMAT_DELIMITER = '%'
   
contains


   logical function isFormat(string)
      character(len=*) :: string

      isFormat = (string(1:1) == FORMAT_DELIMITER)
      
   end function isFormat

   function getTokens(string) result(tokens)
      type(Token), allocatable :: tokens(:)
      character(len=*) :: string

      allocate(tokens(0))
      
   end function getTokens
   
end module ASTG_FormatParser_mod
