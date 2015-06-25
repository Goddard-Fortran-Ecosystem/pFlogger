! Note that FormatParser exposes more methods than is desirable for
! encapsulation.  E.g. get/set buffer.  But this exposure is extremely
! useful for unit testing, which has a higher priority.

module ASTG_NewFormatParser_mod
   use ASTG_FormatTokenVector_mod, only: FormatTokenVector => Vector
   use ASTG_FormatToken_mod, only: FormatToken
   use ASTG_FormatToken_mod, only: KEYWORD_SEPARATOR
   use ASTG_FormatToken_mod, only: TEXT, POSITION, KEYWORD
   use iso_c_binding, only: C_NULL_CHAR
   use ASTG_Exception_mod, only: throw

   implicit none
   private

   public :: format
   public :: FormatParser
   public :: ContextInterface

   public :: textContext
   public :: positionContext
   public :: keywordContext
   public :: singleQuoteContext
   public :: doubleQuoteContext
   public :: escapeContext
   public :: illegalContext

   integer, parameter :: MAX_LEN_TOKEN=1000
   character(len=1), parameter :: FORMAT_DELIMITER = '%'
   character(len=1), parameter :: OPEN_CURLY_BRACE = '{'
   character(len=1), parameter :: CLOSE_CURLY_BRACE = '}'
   character(len=1), parameter :: SPACE = ' '
   ! Tricky to make a literal with backslash when using FPP:
   character(len=*), parameter :: FPP_SAFE_ESCAPE = '\\'
   character(len=1), parameter :: ESCAPE = FPP_SAFE_ESCAPE(1:1)
   character(len=*), parameter :: LIST_DIRECTED_FORMAT = '(*)'

   type, extends(FormatTokenVector) :: FormatParser
      private
      integer :: currentPosition = 0
      character(len=MAX_LEN_TOKEN) :: buffer
      procedure (ContextInterface), pointer :: context => null()
   contains
      procedure :: setContext
      procedure :: getContext
      procedure :: setBuffer ! for testing
      procedure :: getBuffer ! for testing
      procedure :: parseCharacter
      procedure :: parse
   end type FormatParser

   !-----------------
   ! This implements the state pattern
   ! Because there is only one method, and we have singleton states,
   ! we can get away with procedure pointers instead of objects.
   !-----------------

   interface
      subroutine ContextInterface(this, char)
         import FormatParser
         class (FormatParser), intent(inout) :: this
         character(len=1), intent(in) :: char
      end subroutine ContextInterface
   end interface

   interface FormatParser
      module procedure newFormatParser
   end interface FormatParser

   ! This private type is used to force some arguments to be passed by keyword.
   type UnusableArgument
   end type UnusableArgument


contains


   function newFormatParser() result(parser)
      type (FormatParser) :: parser

      parser%buffer = ''
      parser%currentPosition = 0 ! buffer is empty

      call parser%setContext(textContext) ! assume start with text (but confirm)

   end function newFormatParser

   subroutine setContext(this, context)
      class (FormatParser), intent(inout) :: this
      procedure (ContextInterface) :: context

      this%context => context

   end subroutine setContext


   subroutine getContext(this, context)
      class (FormatParser), intent(in) :: this
      procedure (ContextInterface), pointer :: context

      context => this%context

   end subroutine getContext


   ! Delegate to context for current context
   subroutine parseCharacter(this, char)
      class (FormatParser), intent(inout) :: this
      character(len=1), intent(in) :: char

      call this%context(char)

   end subroutine parseCharacter


   ! Delegate to context for current context
   subroutine parse(this, str)
      class (FormatParser), intent(inout) :: this
      character(len=*), intent(in) :: str

      integer :: pos

      if (.not. associated(this%context)) then
         this%context => textContext
      end if

      do pos = 1, len(str)
         call this%parseCharacter(str(pos:pos))
      end do

      call this%parseCharacter(C_NULL_CHAR)

   end subroutine parse


   subroutine setBuffer(this, buffer)
      class (FormatParser), intent(inout) :: this
      character(len=*), intent(in) :: buffer
      
      this%buffer = buffer
      this%currentPosition = len(buffer)
   end subroutine setBuffer


   ! This should be a function that returns a pointer.
   ! Unfortunately gfortran 4.9.1 munges string pointers in
   ! that context.
   subroutine getBuffer(this, buffer)
      character(:), pointer :: buffer
      class (FormatParser), target, intent(in) :: this

      buffer => this%buffer(1:this%currentPosition)
   end subroutine getBuffer



! Various contexts:

   subroutine textContext(this, char)
      class (FormatParser), intent(inout) :: this
      character(len=1), intent(in) :: char

      select case (char)
      case ("'")
         call this%setContext(singleQuoteContext)
         call pushChar(char)
      case ('"')
         call this%setContext(doubleQuoteContext)
         call pushChar(char)
      case (ESCAPE)
         call this%setContext(escapeContext)
      case (FORMAT_DELIMITER, C_NULL_CHAR)
         call this%setContext(positionContext)
         associate (pos => this%currentPosition)
           if (pos > 0) then ! send buffer to new token
              call this%push_back(FormatToken(TEXT, this%buffer(1:pos)))
              pos = 0
           end if
         end associate
         return ! char should not be put in buffer
      case default
         call pushChar(char)
      end select

   contains
      
      subroutine pushChar(char)
         character(len=1), intent(in) :: char

         associate (pos => this%currentPosition)
           pos = pos + 1
           this%buffer(pos:pos) = char
         end associate

      end subroutine pushChar

   end subroutine textContext


   ! Single Quote context treats other special characters as ordinary.
   ! Returns to text context when matching close quote is found.
   subroutine singleQuoteContext(this, char)
      class (FormatParser), intent(inout) :: this
      character(len=1), intent(in) :: char

      select case (char)
      case ("'")
         call this%setContext(textContext)
      case (C_NULL_CHAR)
         call this%setContext(illegalContext)
         call throw('FormatParser::singleQuoteContext() - unclosed single quote')
         return
      case default
         ! stay single quote
      end select

      associate (pos => this%currentPosition)
        pos = pos + 1
        this%buffer(pos:pos) = char
      end associate

   end subroutine singleQuoteContext


   ! Double Quote context treats other special characters as ordinary.
   ! Returns to text context when matching close quote is found.
   subroutine doubleQuoteContext(this, char)
      class (FormatParser), intent(inout) :: this
      character(len=1), intent(in) :: char

      select case (char)
      case ('"')
         call this%setContext(textContext)
      case (C_NULL_CHAR)
         call this%setContext(illegalContext)
         call throw('FormatParser::doubleQuoteContext() - unclosed double quote')
         return
      case default
         ! stay double quote
      end select

      associate (pos => this%currentPosition)
        pos = pos + 1
        this%buffer(pos:pos) = char
      end associate

   end subroutine doubleQuoteContext


   subroutine positionContext(this, char)
      class (FormatParser), intent(inout) :: this
      character(len=1), intent(in) :: char

      associate (pos => this%currentPosition)

        select case (char)
        case (SPACE, ESCAPE, C_NULL_CHAR)
         call this%setContext(textContext)
           if (pos > 0) then ! send buffer to new token
              call this%push_back(FormatToken(POSITION, this%buffer(1:pos)))
              pos = 0
              if (char == ESCAPE) return ! discard char
           else
              call this%setContext(illegalContext)
              call throw('FormatParser::positionContext() - empty edit descriptor')
              return
           end if
        case (OPEN_CURLY_BRACE) ! {
           if (pos > 0) then
              call this%setContext(illegalContext)
              call throw('FormatParser::positionContext() - ' // &
                   & 'illegal start of keyword format: "' // this%buffer(1:pos) // '{"')
              return
           end if
           call this%setContext(keywordContext)
           return ! do not retain char
        case default
         ! stay position format
        end select

        pos = pos + 1
        this%buffer(pos:pos) = char

      end associate

   end subroutine positionContext


   subroutine keywordContext(this, char)
      class (FormatParser), intent(inout) :: this
      character(len=1), intent(in) :: char

      integer :: idx

      associate ( pos => this%currentPosition )

        select case (char)
        case (CLOSE_CURLY_BRACE)
           call this%setContext(textContext)
           if (pos > 0) then ! send buffer to new token
              call this%push_back(FormatToken(KEYWORD, this%buffer(1:pos)))
              pos = 0
              return ! do not retain the closing brace
           end if
        case (KEYWORD_SEPARATOR)
           if (pos == 0) then
              call this%setContext(illegalContext)
              call throw('FormatParser::keywordContext() - missing keyword?')
           end if
        case (ESCAPE)
           idx = index(this%buffer, KEYWORD_SEPARATOR)
           if (idx <= 1) then
              call this%setContext(illegalContext)
              call throw('FormatParser::keywordContext() - no escape sequence permitted.')
              return
           end if
        case (C_NULL_CHAR)
           call this%setContext(illegalContext)
           idx = index(this%buffer, KEYWORD_SEPARATOR)
           if (pos == 0 .or. idx == 1) then
              call throw('FormatParser::keywordContext() - missing keyword')
           elseif (idx == 0) then
              call throw('FormatParser::keywordContext() - missing edit descriptor')
           elseif (idx == pos) then
              call throw('FormatParser::keywordContext() - empty edit descriptor')
           else
              call throw('FormatParser::keywordContext() - missing "}"')
           end if
           return
        case default
           ! stay keyword format
        end select

        pos = pos + 1
        this%buffer(pos:pos) = char

      end associate

   end subroutine keywordContext


   subroutine escapeContext(this, char)
      class (FormatParser), intent(inout) :: this
      character(len=1), intent(in) :: char

      associate ( pos => this%currentPosition )

        select case (char)
        case ('n','N') ! newline
           pos = pos + 1
           this%buffer(pos:pos) = new_line('a')
           call this%setContext(textContext)
        case default
           call this%setContext(illegalContext)
           call throw('FormatParser::escapeContext() - ' // &
                & 'no such escape sequence: ' // ESCAPE // this%buffer(1:pos))
        end select


      end associate

   end subroutine escapeContext


   subroutine illegalContext(this, char)
      class (FormatParser), intent(inout) :: this
      character(len=1), intent(in) :: char

      associate ( pos => this%currentPosition )
        call throw('FormatParser - illegal format specification <' // &
             & this%buffer(1:pos) // '>')
      end associate

   end subroutine illegalContext


   !---------------------------------------------------------------------------  
   ! FUNCTION: 
   ! format
   !
   ! DESCRIPTION:
   ! Parses LogRecord 'message' attributes and returns a 'formatted message'.
   ! For example, the default format for a Logger message is:
   !     %(levelName::a): %(name::a): %(message::a)
   ! Thus, a typical output (rawString) might be:
   !     INFO: myLog: Hello world!
   !---------------------------------------------------------------------------
#define ARG_LIST arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9

   function format(fmt, ARG_LIST, unusable, arr1D_1, extra) result(string)
      use ASTG_FormatTokenVector_mod, only: TokenVector => Vector
      use ASTG_FormatTokenVector_mod, only: TokenVectorIterator => VectorIterator
      use ASTG_UnlimitedVector_mod, only: UnlimitedVector => Vector
      use ASTG_UnlimitedVector_mod, only: UnlimitedVectorIterator => VectorIterator
      use ASTG_CIStringUnlimitedMap_mod, only: CIStringUnlimitedMap => Map
      use ASTG_CIStringUnlimitedMap_mod, only: CIStringUnlimitedMapIterator => MapIterator
      use ASTG_ArgListUtilities_mod

      character(len=:), allocatable :: string
      character(len=*), intent(in) :: fmt
      include 'recordOptArgs.inc'    

      type (UnusableArgument), optional :: unusable
      class (*), optional, intent(in) :: arr1D_1(:)
      type (CIStringUnlimitedMap), optional :: extra

      type (UnlimitedVector) :: args
      type (CIStringUnlimitedMap), target :: extra_

      character(len=:), allocatable :: tokenString
      character(len=:), allocatable :: key
      character(len=:), allocatable :: payload
      character(len=:), allocatable :: append
      type (FormatToken), pointer :: token
      class (*), pointer :: arg

      type (TokenVectorIterator) :: tokenIter
      type (UnlimitedVectorIterator) :: argIter
      type (CIStringUnlimitedMapIterator) :: extraIter

      type (FormatParser) :: p

      args = makeArgVector(ARG_LIST)

      if (present(extra)) then
         call extra_%deepcopy(extra)
      end if

      string = ''

      call p%parse(fmt)
      
      tokenIter = p%begin()
      argIter = args%begin()

      do while (tokenIter /= p%end())
         token => tokenIter%get()

         select case (token%type)
         case (TEXT)
            string = string // token%text
         case (POSITION)
            if (argIter == args%end()) then
               call throw('Not enough values for format string in FormatParser.')
               return
            end if
            arg => argIter%get()
            string = string // handleScalar(arg, token%editDescriptor)
            call argIter%next()
         case (KEYWORD)
            extraIter = extra_%find(token%text)
            if (extraIter == extra_%end()) then
               call throw('No such keyword: <' // token%text // '> in "extra".')
               return
            end if
            arg => extraIter%value()
            string = string // handleScalar(arg, token%editDescriptor)
         end select

         call tokenIter%next()

      end do

   end function format

   !---------------------------------------------------------------------------  
   ! FUNCTION: 
   ! handleScalar
   !
   ! DESCRIPTION:
   ! This function is used by format to deal with all unlimited polymorphic
   ! scalar variables passed to it.
   !---------------------------------------------------------------------------
   function handleScalar(arg, editDescriptor) result(string)
      use iso_fortran_env, only: int32, real32, int64, real64, real128
      character(len=:), allocatable :: string
      class (*), intent(in) :: arg
      character(len=*), intent(in) :: editDescriptor
      character(len=800) :: buffer
      character(len=:), allocatable :: fmt

      string = ''
      fmt = '(' // trim(editDescriptor) // ')'
      
      select type (arg)
      type is (integer(int32))
         if (editDescriptor == LIST_DIRECTED_FORMAT) then
            write(buffer,*) arg
         else
            write(buffer,fmt) arg
         end if
      type is (integer(int64))
         if (fmt == LIST_DIRECTED_FORMAT) then
            write(buffer,*) arg
         else
            write(buffer,fmt) arg
         end if
      type is (real(real32))
         if (fmt == LIST_DIRECTED_FORMAT) then
            write(buffer,*) arg
         else
            write(buffer,fmt) arg
         end if
      type is (real(real64))
         if (fmt == LIST_DIRECTED_FORMAT) then
            write(buffer,*) arg
         else
            write(buffer,fmt) arg
         end if
      type is (logical)
         if (fmt == LIST_DIRECTED_FORMAT) then
            write(buffer,*) arg
         else
            write(buffer,fmt) arg
         end if
      type is (character(len=*))
         if (fmt == LIST_DIRECTED_FORMAT) then
            write(buffer,*) arg
         else
            write(buffer,fmt) arg
         end if
      class default ! user defined
         buffer = 'unsupported'
      end select

      string = string // trim(buffer)

   end function handleScalar



end module ASTG_NewFormatParser_mod
