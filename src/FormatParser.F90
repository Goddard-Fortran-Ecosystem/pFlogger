! FormatParser methods are used to parse format strings that represent
! fortran specification expressions. Format strings contain 
! “replacement fields” delimited by the % sign.
module ASTG_FormatParser_mod
   use ASTG_Object_mod
   use ASTG_Exception_mod
   implicit none
   private

   public :: FormatParser
   
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
   character(len=*), parameter :: LIST_DIRECTED_FORMAT = '(*)'
   character(len=*), parameter :: SPACE = ' '
   ! CPP safe escape
   character(len=*), parameter :: CPP_SAFE_ESCAPE = '\\'
   character(len=1), parameter :: ESCAPE = CPP_SAFE_ESCAPE(1:1)
   character(len=*), parameter :: NAME_SEPARATOR = ':'


   ! This private type is used to force some arguments to be passed by keyword.
   type UnusableArgument
   end type UnusableArgument

#define ARG_LIST arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9

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

      integer :: i, j, n
      integer :: idx
      logical :: escapeFlag

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
         if (scan(string, ESCAPE) == 0) return
         
         escapeFlag = .false.
         j = 1
         do i = 1, len(payload)
            if (payload(i:i) == ESCAPE) then
               if (escapeFlag) then
                  escapeFlag = .false.
                  payload(j:j) = ESCAPE
                  j = j + 1
               else
                  escapeFlag = .true.
                  if (i == len(string)) then ! final character
                     call throw("FormatParser:: Cannot terminate format with bare escape '\' character.")
                  end if
               end if
            else
               if (escapeFlag) then
                  if (payload(i:i) == 'n') then ! newline
                     payload(j:j) = new_line('a')
                     j = j + 1
                  else ! unsupported
                     payload(j:j+1) = payload(i-1:i)
                     j = j + 2
                     
                  end if
                  escapeFlag = .false.
               else
                  payload(j:j) = string(i:i)
                  j = j + 1
               end if
            end if
         end do
         payload = payload(1:j-1)

      end if

   end function getPayload

   
   function getTokens(rawString) result(tokens)
      use FTL_String_mod
      use FTL_StringVec_mod
      type(StringVec) :: tokens
      character(len=*), intent(in) :: rawString
      character(len=:), allocatable :: buffer

      character(len=:), allocatable :: token
      character(len=:), allocatable :: s

      integer :: i, n

      tokens = StringVec()

      buffer = rawString
      do
         n = len(buffer)
         if (n == 0) return

         i = startOfNextToken(buffer) ! always > 0

         token = buffer(1:i-1)
         call tokens%push_back(String(token))
! Workaround for gfortran
         s = buffer(i:n)
         buffer = s
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
            rawString = rawString // handle_(arg, payload)
         else
            rawString = rawString // payload
         end if

         call fmtIter%next()

      end do

   end function format


   function makeString(fmt, ARG_LIST, unusable, extra) result(rawString)
      use FTL_XWrapVec_mod
      use FTL_CIStringXUMap_mod
      use ASTG_ArgListUtilities_mod
      character(len=:), allocatable :: rawString
      character(len=*), intent(in) :: fmt

      include 'recordOptArgs.inc'    
      type (UnusableArgument), optional, intent(in) :: unusable
      type (CIStringXUMap), optional :: extra
      type (XWrapVec) :: args
      type (CIStringXUMap) :: extra_

      if (present(extra)) then
         extra_ = extra
      else
         extra_ = CIStringXUMap()
      end if

      args = makeArgVector(ARG_LIST)

      rawString = format(fmt, args, extra=extra_)
       
   end function makeString

   function handle_(arg, payload) result(rawString)
      use iso_fortran_env, only: int32, real32, int64, real64, real128
      use FTL_String_mod
      character(len=:), allocatable :: rawString
      class (*), intent(in) :: arg
      character(len=*), intent(in) :: payload
      character(len=80) :: buffer

      rawString = ''
      
      select type (arg)
      type is (integer(int32))
         if (payload == LIST_DIRECTED_FORMAT) then
            write(buffer,*) arg
         else
            write(buffer,payload) arg
         end if
      type is (integer(int64))
         if (payload == LIST_DIRECTED_FORMAT) then
            write(buffer,*) arg
         else
            write(buffer,payload) arg
         end if
      type is (real(real32))
         if (payload == LIST_DIRECTED_FORMAT) then
            write(buffer,*) arg
         else
            write(buffer,payload) arg
         end if
      type is (real(real64))
         if (payload == LIST_DIRECTED_FORMAT) then
            write(buffer,*) arg
         else
            write(buffer,payload) arg
         end if
      type is (logical)
         if (payload == LIST_DIRECTED_FORMAT) then
            write(buffer,*) arg
         else
            write(buffer,payload) arg
         end if
      type is (character(len=*))
         if (payload == LIST_DIRECTED_FORMAT) then
            write(buffer,*) arg
         else
            write(buffer,payload) arg
         end if
      type is (String)
         if (payload == LIST_DIRECTED_FORMAT) then
            write(buffer,*) arg%item
         else
            write(buffer,payload) arg%item
         end if
      class default ! user defined
         buffer = 'unsupported'
      end select

      rawString = rawString // trim(buffer)

   end function handle_

end module ASTG_FormatParser_mod
