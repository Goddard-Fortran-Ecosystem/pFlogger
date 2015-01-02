module ASTG_FormatParser_mod
   use FTL_StringVec_mod
   implicit none
   private

   public FormatParser
   
   type FormatParser
   contains
      procedure, nopass :: isFormat
      procedure, nopass :: getToken
      procedure, nopass :: getTokens
   end type FormatParser

   character(len=*), parameter :: FORMAT_DELIMITER = '%'
   
contains


   logical function isFormat(string)
      character(len=*) :: string

      integer :: idx
      character(len=1) :: nextChar

      idx = scan(string(1:1), FORMAT_DELIMITER)
      if (idx == 0) then
         isFormat = .false.
      else
         if (idx < len(string)) then
            nextChar = string(idx+1:idx+1)
            isFormat = (nextChar /= FORMAT_DELIMITER)
         else
            isFormat = .true.
         end if
      end if
      
   end function isFormat

   !-------------------------------------------------------
   ! Format strings consist of two types of tokens. First there are
   ! regular text strings that do not contain any FORMAT_DELIMITER.  Ther
   ! there are format specifiers that begin with a FORMAT_DELIMITER.
   ! E.g.  'hello %i2.1' has two tokens: 'hello ' and '%2.1'.  An
   ! important issue is how to detect the _end_ of a format specifier
   ! token.  For the moment, we require that those end with a space
   ! (i.e. '_').
   !-------------------------------------------------------

   function getToken(string) result(token)
      character(len=:), allocatable :: token
      character(len=*), intent(in) :: string

      integer :: idx

      idx = scan(string, FORMAT_DELIMITER)
      select case (idx)
      case (0) ! no format tokens
         token = string
      case (1) ! first token is a format 
         idx = scan(string,' ')
         if (idx == 0) then ! entire string is token
            token = string
         else
            token = string(1:idx-1)
         end if
      case (2:) ! first token is text
         token = string(1:idx-1)
      end select
         
      if (idx > 1) then ! there is a text token
         token = string(1:idx-1)
      else ! there is a format token
      end if
      
   end function getToken
   
   function getTokens(string) result(tokens)
      type(StringVec) :: tokens
      character(len=*), intent(in) :: string
      character(len=:), allocatable :: buffer
      character(len=:), allocatable :: token
      integer :: n

      tokens = StringVec()

      buffer = string
      do
         n = len(buffer)
         if (n == 0) return
         
         token = getToken(buffer)
         call tokens%push_back(token)
         buffer = buffer(len(token)+1:n)

      end do
      
   end function getTokens
   
end module ASTG_FormatParser_mod
