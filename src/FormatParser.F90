! FormatParser methods are used to parse format strings that represent
! fortran specification expressions. Format strings contain 
! “replacement fields” delimited by the % sign.
module ASTG_FormatParser_mod
   use ASTG_Object_mod
   use ASTG_Exception_mod
   implicit none
   private

   public FormatParser
   
   type, extends(Object) :: FormatParser
   contains
      procedure, nopass :: isFormat
      procedure, nopass :: formatContainsKey
      procedure, nopass :: getFormatKey
      procedure, nopass :: startOfNextToken
      procedure, nopass :: getPayload
      procedure, nopass :: getTokens
      procedure, nopass :: format
      procedure, nopass :: makeString
   end type FormatParser

   ! allow for variants that use other characters;  E.g. % -> $, or () -> {}
   character(len=*), parameter :: FORMAT_DELIMITER = '%'
   character(len=*), parameter :: OPEN_PAREN = '('
   character(len=*), parameter :: CLOSE_PAREN = ')'
   character(len=*), parameter :: SPACE = ' '
   character(len=*), parameter :: ESCAPE = '\' ! '
   character(len=*), parameter :: NAME_SEPARATOR = ':'


   ! This private type is used to force some arguments to be passed by keyword.
   type UnusableArgument
   end type UnusableArgument

contains


   logical function isFormat(string)
      character(len=*), intent(in) :: string

      if (len(string) == 0) then
         isFormat = .false.
         call throw("Illegal - empty string in FormatParser.")
      else
         isFormat = (string(1:1) == FORMAT_DELIMITER)
      end if

   end function isFormat

   
   logical function formatContainsKey(string)
      character(len=*), intent(in) :: string
      
      formatContainsKey = (index(string,NAME_SEPARATOR) > 0)

   end function formatContainsKey


   
   function getFormatKey(tokenString) result(key)
      character(len=:), allocatable :: key
      character(len=*), intent(in) :: tokenString

      integer :: idx

      idx = index(tokenString, NAME_SEPARATOR)
      select case (idx)
      case (0)
         key = ''
      case (1:3)
         key = ''
         call throw('Illegal key/format in FormatParser: <' // tokenString // '>')
      case (4:)
         key = tokenString(3:idx-1)
      end select

   end function getFormatKey

   !-------------------------------------------------------
   ! Format strings consist of two types of tokens. First there are
   ! regular text strings that do not contain any FORMAT_DELIMITER. Then
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
      integer :: idx

      n  = len(string)

      if (isFormat(string)) then
         select case(n)
         case (1)
            payload = ''
            call throw("Empty format descriptor in FormatParser.")
         case (2:)
            if (string(2:2) == OPEN_PAREN) then
               ! keep all but FORMAT_DELIMETER (now 1st character)
               if (formatContainsKey(string)) then
                  idx = index(string, ':')
                  payload = string(idx+1:n-1)
               else
                  payload = string(3:n-1)
               end if
            else
               payload = string(2:n)
            end if
            if (payload == '') then
               call throw("Empty format descriptor in FormatParser.")
               return
            end if
            ! Wrap with parens for use as Fortran fmt.
            payload = '(' // payload // ')' ! 

         end select

      else ! raw text

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

   
   function format(fmt, args, unusable, extra) result(rawString)
      use FTL_String_mod
      use FTL_StringVec_mod
      use FTL_XWrapVec_mod
      use FTL_CIStringXUMap_mod
      character(len=:), allocatable :: rawString
      character(len=*), intent(in) :: fmt
      type (XWrapVec), optional :: args
      type (UnusableArgument), optional :: unusable
      type (CIStringXUMap), optional :: extra

      type (XWrapVec) :: args_
      type (CIStringXUMap) :: extra_

      character(len=80) :: buffer
      character(len=:), allocatable :: tokenString
      character(len=:), allocatable :: key
      character(len=:), allocatable :: payload
      type (String), pointer :: token
      type (StringVec), target :: tokens
      class (*), pointer :: arg

      type (StringVecIter) :: fmtIter
      type (XWrapVecIter) :: argIter

      if (present(args)) then
         args_ = args
      else
         args_ = XWrapVec()
      end if

      if (present(extra)) then
         extra_ = extra
      else
         extra_ = CIStringXUMap()
      end if

      rawString = ''
      tokens = getTokens(fmt)
      
      fmtIter = tokens%begin()
      argIter = args_%begin()

      do while (fmtIter /= tokens%end())
         token => fmtIter%get()
         ! Todo:  "item" is now public in FTL as a workaround for compiler problems.
         tokenString = token%item

         payload = getPayload(tokenString)
         if (isFormat(tokenString)) then
            ! Does it contain a key?
            if (formatContainsKey(tokenString)) then
               key = getFormatKey(tokenString)
               arg => extra_%at_alt(key)
               if (.not. associated(arg)) then
                  call throw('No such key: <' // key // '> in "extra".')
                  return
               end if
            else
               ! Is there another position value?
               if (argIter == args_%end()) then
                  call throw('Not enough values for format string in FormatParser.')
                  return
               end if
               arg => argIter%get_alt()
               call argIter%next()
            end if

            select type (arg)
            type is (integer)
               write(buffer,payload) arg
               rawString = rawString // trim(buffer)
            type is (real)
               write(buffer,payload) arg
               rawString = rawString // trim(buffer)
            end select
         else
            rawString = rawString // payload
         end if

         call fmtIter%next()

      end do

   end function format

      
   function makeString(fmt, arg1, arg2, arg3, unusable, ...,  extra) result(rawString)
      use FTL_XWrapVec_mod
      use FTL_CIStringXUMap_mod
      character(len=:), allocatable :: rawString
      character(len=*), intent(in) :: fmt

      integer, optional :: arg1
      integer, optional :: arg2
      integer, optional :: arg3
      type (UnusableArgument), optional :: unusable
      type (CIStringXUMap), optional :: extra

      type (XWrapVec) :: args

      args = XWrapVec()
      if (present(arg1)) call args%push_back_alt(arg1)
      if (present(arg2)) call args%push_back_alt(arg2)
      if (present(arg3)) call args%push_back_alt(arg3)

      rawString = format(fmt, args, extra=extra)
         
   end function makeString
   
end module ASTG_FormatParser_mod
