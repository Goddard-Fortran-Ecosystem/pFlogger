module ASTG_FormatParser_mod
   use ASTG_Exception_mod
   use FTL_StringVec_mod
   implicit none
   private

   public FormatParser
   
   type FormatParser
   contains
      procedure, nopass :: isFormat
      procedure, nopass :: startOfNextToken
      procedure, nopass :: getToken
      procedure, nopass :: getTokens
   end type FormatParser

   ! allow for variants that use other characters;  E.g. % -> $, or () -> {}
   character(len=*), parameter :: FORMAT_DELIMITER = '%'
   character(len=*), parameter :: OPEN_PAREN = '('
   character(len=*), parameter :: CLOSE_PAREN = ')'
   character(len=*), parameter :: SPACE = ' '
   character(len=*), parameter :: ESCAPE = '\' ! '


contains


   logical function isFormat(string)
      character(len=*) :: string

      if (len(string) == 0) then
         isFormat = .false.
         call throw("Illegal - empty string in FormatParser.")
      else
         isFormat = (string(1:1) == FORMAT_DELIMITER)
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

   integer function startOfNextToken(string) result(idx)
      character(len=*), intent(in) :: string

      logical :: isFormatToken
      character(len=1) :: previousCharacter
      integer :: i

      isFormatToken = isFormat(string)

      if (isFormatToken) then

         do i = 2, len(string)
            select case(string(i:i))
            case (FORMAT_DELIMITER, SPACE)
               idx = i
               return
            case (CLOSE_PAREN)
               idx = i + 1
               return
            end select
         end do
         
         ! entire string
         idx = len(string) + 1 

      else

         ! scan for start of format
         do i = 2, len(string)
            previousCharacter = string(i-1:i-1)
            if (previousCharacter == ESCAPE) then
               cycle
            elseif (string(i:i) == FORMAT_DELIMITER) then
               idx = i
               return
            end if

         end do
         ! entire string
         idx = len(string) + 1

      end if
      
   end function startOfNextToken

   function getToken(string) result(token)
      character(len=:), allocatable :: token
      character(len=*), intent(in) :: string

      integer :: idx
      character(len=1) :: nextCharacter

      idx = startOfNextToken(string)
      if (idx == 0) then
         token = ''
         call throw("Illegal - empty string in FormatParser.")
         return
      end if

      token = string(1:idx-1)
         
      
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
