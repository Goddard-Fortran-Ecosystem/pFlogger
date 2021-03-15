!------------------------------------------------------------------------------
! NASA/GSFC, CISTO, Code 606, Advanced Software Technology Group
!------------------------------------------------------------------------------
!
!*MODULE: PFL_FormatParser
!
!> @brief Format parser classes and functions.
!> @note  FormatParser exposes more methods than is desirable for
!! encapsulation.  E.g. get/set buffer.  But this exposure is extremely
!! useful for unit testing, which has a higher priority.
!
!> @author ASTG staff
!> @date 01 Jan 2015 - Initial Version
!------------------------------------------------------------------------------
#include "error_handling_macros.fh"
module PFL_FormatParser
   use PFL_FormatTokenVector, only: FormatTokenVector => Vector
   use PFL_FormatToken, only: FormatToken
   use PFL_FormatToken, only: KEYWORD_SEPARATOR
   use PFL_FormatToken, only: TEXT, POSITION, KEYWORD
   use iso_c_binding, only: C_NULL_CHAR
   use PFL_Exception, only: throw
   use PFL_WrapArray

   implicit none
   private

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
   character(len=1), parameter :: OPEN_PARENTHESES = '('
   character(len=1), parameter :: CLOSE_PARENTHESES = '('
   character(len=1), parameter :: SPACE = ' '
   character(len=1), parameter :: TERMINATOR = '~'
   ! Tricky to make a literal with backslash when using FPP:
   character(len=*), parameter :: FPP_SAFE_ESCAPE = '\\'
   character(len=1), parameter :: ESCAPE = FPP_SAFE_ESCAPE(1:1)
   character(len=*), parameter :: LIST_DIRECTED_FORMAT = '*'

   type, extends(FormatTokenVector) :: FormatParser
      private
      integer :: currentPosition = 0
      character(len=MAX_LEN_TOKEN) :: buffer
      procedure (ContextInterface), pointer :: context => null()
      procedure (ContextInterface), pointer :: previousContext => null()
   contains
      procedure :: parse
      procedure :: parse_character
      procedure :: set_context
      procedure :: get_context
      procedure :: set_buffer ! for testing
      procedure :: get_buffer ! for testing
      procedure :: push_context
      procedure :: pop_context
      procedure :: push_char
      procedure :: reset
   end type FormatParser

   !-----------------
   ! This implements the state pattern
   ! Because there is only one method, and we have singleton states,
   ! we can get away with procedure pointers instead of objects.
   !-----------------

   interface
      subroutine ContextInterface(this, char, rc)
         import FormatParser
         class (FormatParser), intent(inout) :: this
         character(len=1), intent(in) :: char
         integer, optional, intent(out) :: rc
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

      call parser%set_context(textContext) ! assume start with text (but confirm)

   end function newFormatParser


   subroutine set_context(this, context)
      class (FormatParser), intent(inout) :: this
      procedure (ContextInterface) :: context

      this%context => context

   end subroutine set_context


   subroutine get_context(this, context)
      class (FormatParser), intent(in) :: this
      procedure (ContextInterface), pointer :: context

      context => this%context

   end subroutine get_context


   subroutine pop_context(this)
      class (FormatParser), intent(inout) :: this

      this%context => this%previousContext
      nullify(this%previousContext)

   end subroutine pop_context

   subroutine push_context(this)
      class (FormatParser), intent(inout) :: this
      this%previousContext => this%context
   end subroutine push_context


   ! Delegate to context for current context
   subroutine parse_character(this, char)
      class (FormatParser), intent(inout) :: this
      character(len=1), intent(in) :: char

      call this%context(char)

   end subroutine parse_character


   ! Delegate to context for current context
   subroutine parse(this, str)
      class (FormatParser), intent(inout) :: this
      character(len=*), intent(in) :: str

      integer :: pos

      if (.not. associated(this%context)) then
         this%context => textContext
      end if

      do pos = 1, len(str)
         call this%parse_character(str(pos:pos))
      end do

      call this%parse_character(C_NULL_CHAR)

   end subroutine parse


   subroutine set_buffer(this, buffer)
      class (FormatParser), intent(inout) :: this
      character(len=*), intent(in) :: buffer
      
      this%buffer = buffer
      this%currentPosition = len(buffer)
   end subroutine set_buffer


   ! This should be a function that returns a pointer.
   ! Unfortunately gfortran 4.9.1 munges string pointers in
   ! that context.
   subroutine get_buffer(this, buffer2)
      character(:), allocatable :: buffer2
      class (FormatParser), target, intent(in) :: this

      buffer2 = this%buffer(1:this%currentPosition)

   end subroutine get_buffer



! Various contexts:

   subroutine textContext(this, char, rc)
      class (FormatParser), intent(inout) :: this
      character(len=1), intent(in) :: char
      integer, optional, intent(out) :: rc

      integer :: status

      select case (char)
      case ("'")
         call this%push_context()
         call this%set_context(singleQuoteContext)
         call this%push_char(char)
      case ('"')
         call this%push_context()
         call this%set_context(doubleQuoteContext)
         call this%push_char(char)
      case (ESCAPE)
         call this%set_context(escapeContext)
      case (FORMAT_DELIMITER, C_NULL_CHAR)
         call this%set_context(positionContext)
         associate (pos => this%currentPosition)
           if (pos > 0) then ! send buffer to new token
              call this%push_back(FormatToken(TEXT, this%buffer(1:pos), rc=status))
              _VERIFY(status,'',rc) 
              pos = 0
           end if
         end associate
         ! char should not be put in buffer
         _RETURN(_SUCCESS,rc)
      case default
         call this%push_char(char)
      end select
      _RETURN(_SUCCESS,rc)
   end subroutine textContext


   subroutine push_char(this,char)
      class (FormatParser), intent(inout) :: this
      character(len=1), intent(in) :: char
      
      associate (pos => this%currentPosition)
        pos = pos + 1
        this%buffer(pos:pos) = char
      end associate
      
   end subroutine push_char


   ! Single Quote context treats other special characters as ordinary.
   ! Returns to text context when matching close quote is found.
   subroutine singleQuoteContext(this, char, rc)
      class (FormatParser), intent(inout) :: this
      character(len=1), intent(in) :: char
      integer, optional, intent(out) :: rc

      integer :: status

      select case (char)
      case ("'")
         call this%pop_context()
      case (C_NULL_CHAR)
         call this%set_context(illegalContext)
         _ASSERT(.false., 'FormatParser::singleQuoteContext() - unclosed single quote', rc)
      case default
         ! stay single quote
      end select

      associate (pos => this%currentPosition)
        pos = pos + 1
        this%buffer(pos:pos) = char
      end associate
      _RETURN(_SUCCESS,rc)
   end subroutine singleQuoteContext


   ! Double Quote context treats other special characters as ordinary.
   ! Returns to text context when matching close quote is found.
   subroutine doubleQuoteContext(this, char, rc)
      class (FormatParser), intent(inout) :: this
      character(len=1), intent(in) :: char
      integer, optional, intent(out) :: rc

      integer :: status

      select case (char)
      case ('"')
         call this%pop_context()
      case (C_NULL_CHAR)
         call this%set_context(illegalContext)
         _ASSERT(.false., 'FormatParser::doubleQuoteContext() - unclosed double quote', rc)
      case default
         ! stay double quote
      end select

      associate (pos => this%currentPosition)
        pos = pos + 1
        this%buffer(pos:pos) = char
      end associate

      _RETURN(_SUCCESS,rc)
   end subroutine doubleQuoteContext


   subroutine positionContext(this, char, rc)
      class (FormatParser), intent(inout) :: this
      character(len=1), intent(in) :: char
      integer, optional, intent(out) :: rc

      integer :: status

      associate (pos => this%currentPosition)

        select case (char)
        case (SPACE, TERMINATOR, C_NULL_CHAR)
           call this%set_context(textContext)
           if (pos > 0) then ! send buffer to new token
              call this%push_back(FormatToken(POSITION, this%buffer(1:pos), rc=status))
              _VERIFY(status,'',rc)
              pos = 0
              if (char == TERMINATOR) return ! discard char
           else
              call this%push_back(FormatToken(POSITION, '*', rc=status))
              _VERIFY(status,'',rc)
              pos = 0
              call this%set_context(textContext)
              return
           end if
        case ("'")
           call this%push_context()
           call this%set_context(singleQuoteContext)
        case ('"')
           call this%push_context()
           call this%set_context(doubleQuoteContext)
        case (OPEN_PARENTHESES) ! (
           if (pos == 0) then
              call this%set_context(keywordContext)
              return ! do not retain char
           end if
        case (FORMAT_DELIMITER) ! repeated delimeter should be interpreted as text
           if (pos == 0) then
              call this%set_context(textContext)
           end if
        case default
         ! stay position format
        end select

        call this%push_char(char)

      end associate
      _RETURN(_SUCCESS,rc)
   end subroutine positionContext

   subroutine keywordContext(this, char, rc)
      class (FormatParser), intent(inout) :: this
      character(len=1), intent(in) :: char
      integer, optional, intent(out) :: rc

      integer :: idx
      integer :: status

      associate ( pos => this%currentPosition )

        select case (char)
        case (KEYWORD_SEPARATOR)
           if (pos == 0) then
              call this%set_context(illegalContext)
              _ASSERT(.false., 'FormatParser::keywordContext() - missing keyword?', rc)
           end if
        case (C_NULL_CHAR)
           call this%set_context(illegalContext)
           idx = index(this%buffer, KEYWORD_SEPARATOR)
           if (pos == 0) then
              _ASSERT(.false., 'FormatParser::keywordContext() - missing keyword', rc)
           elseif (idx == 0) then
              _ASSERT(.false., 'FormatParser::keywordContext() - missing ")"', rc)
           elseif (idx == 1) then
              _ASSERT(.false., 'FormatParser::keywordContext() - missing keyword?', rc)
           elseif (idx == pos) then
              call this%set_context(textContext)
              call this%push_back(FormatToken(KEYWORD, this%buffer(1:pos) // '*', rc=status))
              _VERIFY(status,'',rc)
              pos = 0
           else
              call this%set_context(textContext)
              call this%push_back(FormatToken(KEYWORD, this%buffer(1:pos), rc=status))
              _VERIFY(status,'',rc)
              pos = 0
           end if
           return
        case (SPACE, TERMINATOR)
           call this%set_context(textContext)
           if (pos > 0) then ! send buffer to new token
              call this%push_back(FormatToken(KEYWORD, this%buffer(1:pos), rc=status))
              _VERIFY(status,'',rc)
              pos = 0
              if (char == TERMINATOR) return ! discard char
           else
              call this%set_context(illegalContext)
              _ASSERT(.false., 'FormatParser::keywordContext() - empty edit descriptor', rc)
           end if
        case default
           ! stay keyword format
        end select

        pos = pos + 1
        this%buffer(pos:pos) = char

      end associate
      _RETURN(_SUCCESS,rc)
   end subroutine keywordContext


   subroutine escapeContext(this, char, rc)
      class (FormatParser), intent(inout) :: this
      character(len=1), intent(in) :: char
      integer, optional, intent(out) :: rc

      integer :: status

      associate ( pos => this%currentPosition )

        select case (char)
        case ('n','N') ! newline
           pos = pos + 1
           this%buffer(pos:pos) = new_line('a')
           call this%set_context(textContext)
        case default
           call this%set_context(illegalContext)
           _ASSERT(.false., 'FormatParser::escapeContext() - no such escape sequence: ' // ESCAPE // char, rc)
        end select

      end associate

      _RETURN(_SUCCESS,rc)
   end subroutine escapeContext


   subroutine illegalContext(this, char, rc)
      class (FormatParser), intent(inout) :: this
      character(len=1), intent(in) :: char
      integer, optional, intent(out) :: rc

      integer :: status

      associate ( pos => this%currentPosition )
        _ASSERT(.false., 'FormatParser - illegal format specification <' // this%buffer(1:pos) // '>', rc)
      end associate
      _RETURN(_FAILURE, rc)
   end subroutine illegalContext

   subroutine reset(this)
      class (FormatParser), intent(inout) :: this

      call this%clear() ! vector parent
      this%currentPosition = 0
      this%buffer = ''
      nullify(this%context)
      nullify(this%previousContext)
   end subroutine reset

end module PFL_FormatParser
