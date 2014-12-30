module ASTG_FormatParser_mod
   implicit none
   private

   public FormatParser
   
   type FormatParser
   contains
      procedure, nopass :: isFormat
   end type FormatParser
   
contains


   logical function isFormat(string)
      character(len=*) :: string

      isFormat = (string(1:1)=='%')
      
   end function isFormat

end module ASTG_FormatParser_mod
