! Note that FormatParser exposes more methods than is desirable for
! encapsulation.  E.g. get/set buffer.  But this exposure is extremely
! useful for unit testing, which has a higher priority.

module ASTG_NewFormatParser_mod
   use ASTG_FormatTokenVector_mod, only: FormatTokenVector => Vector
   implicit none
   private

   public :: FormatParser
   public :: ContextInterface

   public :: textContext
   public :: positionContext
   public :: keywordContext
   public :: singleQuoteContext
   public :: doubleQuoteContext

   integer, parameter :: MAX_LEN_TOKEN=1000
   character(len=1), parameter :: FORMAT_DELIMITER = '%'
   character(len=1), parameter :: OPEN_CURLY_BRACE = '{'
   character(len=1), parameter :: CLOSE_CURLY_BRACE = '}'
   character(len=1), parameter :: SPACE = ' '
   character(len=1), parameter :: COMMA = ','
   ! Tricky to make a literal with backslash when using FPP:
   character(len=*), parameter :: FPP_SAFE_ESCAPE = '\\'
   character(len=1), parameter :: ESCAPE = FPP_SAFE_ESCAPE(1:1)

   type, extends(FormatTokenVector) :: FormatParser
      private
      integer :: currentPosition = 0
      character(len=MAX_LEN_TOKEN) :: buffer
      procedure (ContextInterface), pointer :: context
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

      do pos = 1, len(str)
         call this%context(str(pos:pos))
      end do

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
      use ASTG_FormatToken_mod
      use iso_c_binding, only: C_NULL_CHAR
      class (FormatParser), intent(inout) :: this
      character(len=1), intent(in) :: char

      select case (char)
      case ("'")
         call this%setContext(singleQuoteContext)
      case ('"')
         call this%setContext(doubleQuoteContext)
      case (FORMAT_DELIMITER, C_NULL_CHAR)
         call this%setContext(positionContext)
         associate (pos => this%currentPosition)
           if (pos > 0) then ! send buffer to new token
              call this%push_back(FormatToken(TEXT, this%buffer(1:pos)))
              pos = 0
           end if
         end associate
         return ! char should not be put in buffer
      end select

      associate (pos => this%currentPosition)
        pos = pos + 1
        this%buffer(pos:pos) = char
      end associate

   end subroutine textContext


   ! Single Quote context treats other special characters as ordinary.
   ! Returns to text context when matching close quote is found.
   subroutine singleQuoteContext(this, char)
      class (FormatParser), intent(inout) :: this
      character(len=1), intent(in) :: char

      associate (pos => this%currentPosition)
        pos = pos + 1
        this%buffer(pos:pos) = char
      end associate

      select case (char)
      case ("'")
         call this%setContext(textContext)
      case default
         ! stay single quote
      end select

   end subroutine singleQuoteContext


   ! Double Quote context treats other special characters as ordinary.
   ! Returns to text context when matching close quote is found.
   subroutine doubleQuoteContext(this, char)
      class (FormatParser), intent(inout) :: this
      character(len=1), intent(in) :: char

      associate (pos => this%currentPosition)
        pos = pos + 1
        this%buffer(pos:pos) = char
      end associate

      select case (char)
      case ('"')
         call this%setContext(textContext)
      case default
         ! stay double quote
      end select

   end subroutine doubleQuoteContext


   subroutine positionContext(this, char)
      use iso_c_binding, only: C_NULL_CHAR
      use ASTG_Exception_mod, only: throw
      use ASTG_FormatToken_mod
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
           end if
        case (OPEN_CURLY_BRACE) ! {
           if (pos > 0) then
              call throw('FormatParser::positionContext() - ' // &
                   & 'illegal start of keyword format: ' // this%buffer(1:pos))
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
      use ASTG_FormatToken_mod
      use iso_c_binding, only: C_NULL_CHAR
      use ASTG_Exception_mod, only: throw
      class (FormatParser), intent(inout) :: this
      character(len=1), intent(in) :: char

      associate ( pos => this%currentPosition )

        select case (char)
        case (C_NULL_CHAR)
           call throw('FormatParser::keywordContext() - incomplete keyword format specifier.')
        case (CLOSE_CURLY_BRACE)
           call this%setContext(textContext)
           if (pos > 0) then ! send buffer to new token
              call this%push_back(FormatToken(KEYWORD, this%buffer(1:pos)))
              pos = 0
              return ! do not retain the closing brace
           end if
        case default
           ! stay keyword format
        end select

        pos = pos + 1
        this%buffer(pos:pos) = char

      end associate

   end subroutine keywordContext

end module ASTG_NewFormatParser_mod
