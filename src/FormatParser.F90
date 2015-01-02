module ASTG_FormatParser_mod
   use ASTG_Object_mod
   use ASTG_Exception_mod
   implicit none
   private

   public FormatParser
   
   type, extends(Object) :: FormatParser
   contains
      procedure, nopass :: isFormat
      procedure, nopass :: startOfNextToken
      procedure, nopass :: getPayload
      procedure, nopass :: getTokens
      procedure, nopass :: makeString
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

   function getPayload(string) result(payload)
      character(len=:), allocatable :: payload
      character(len=*), intent(in) :: string

      integer :: n

      n  = len(string)

      if (isFormat(string)) then
         select case(n)
         case (1)
            payload = ''
            call throw("Empty format descripter in FormatParser.")
         case (2:)
            if (string(2:2) == OPEN_PAREN) then
               ! keep all but FORMAT_DELIMETER (now 1st character)
               payload = string(2:n)
               if (n == 3) then
                  call throw("Empty format descripter in FormatParser.")
                  return
               end if
            else
               ! Fortran requires PAREN to be '(' when passing to write statements
               payload = '(' // string(2:n) // ')' ! 
            end if
         end select
      else
         payload = string
      end if

   end function getPayload
   
   function getTokens(rawString) result(tokens)
      use FTL_String_mod
      use FTL_StringVec_mod
      type(StringVec) :: tokens
      character(len=*), intent(in) :: rawString
      character(len=:), allocatable :: buffer

      character(len=:), allocatable :: token

      integer :: i, n

      tokens = StringVec()

      buffer = rawString
      do
         n = len(buffer)

         if (n == 0) return

         i = startOfNextToken(buffer) ! always > 0

         token = buffer(1:i-1)
         call tokens%push_back(String(token))
         buffer = buffer(i:n)

      end do
      
   end function getTokens


   function makeString(fmt, arg1, arg2, arg3) result(rawString)
      use FTL_String_mod
      use FTL_StringVec_mod
      use FTL_XWrapVec_mod
      character(len=:), allocatable :: rawString
      character(len=*), intent(in) :: fmt

      integer, optional :: arg1
      integer, optional :: arg2
      integer, optional :: arg3

      character(len=80) :: buffer
      character(len=:), allocatable :: tokenString
      character(len=:), allocatable :: payload
      type (String), pointer :: token
      type (StringVec), target :: tokens
      type (XWrapVec), target :: args

      type (StringVecIter) :: fmtIter
      type (XWrapVecIter) :: argIter


      args = XWrapVec()
      if (present(arg1)) call args%push_back_alt(arg1)
      if (present(arg2)) call args%push_back_alt(arg2)
      if (present(arg3)) call args%push_back_alt(arg3)
         
      rawString = ''
      tokens = getTokens(fmt)
      
      fmtIter = tokens%begin()
      argIter = args%begin()

      do while (fmtIter /= tokens%end())
         token => fmtIter%get()
         tokenString = token%item

         payload = getPayload(tokenString)
         if (isFormat(tokenString)) then

            ! Is there another value?
            if (argIter == args%end()) then
               call throw('Not enough values for format string in FormatParser.')
               return
            end if

            select type (arg => argIter%get_alt())
            type is (integer)
               write(buffer,payload) arg
               rawString = rawString // trim(buffer)
            end select
            call argIter%next()
         else
            rawString = rawString // payload
         end if

         call fmtIter%next()

      end do

   contains

   subroutine check(n, i, arg)
      integer, intent(in) :: n
      integer, intent(in) :: i
      integer, optional :: arg

      if (n >= i) then 
         if (.not. present(arg1)) then
            call throw('Missing data item in FormatParser.')
         end if
      end if
   end subroutine check


   end function makeString
   
end module ASTG_FormatParser_mod
