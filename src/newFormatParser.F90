module ASTG_NewFormatParser_mod
   use ASTG_FormatToken_mod
   implicit none
   private

   public :: FormatParser
   public :: HandlerInterface

   public :: textHandler
   public :: singleQuoteHandler
   public :: doubleQuoteHandler

   integer, parameter :: MAX_LEN_TOKEN=1000

   type FormatParser
      private
      integer :: currentPosition = 0
      character(len=MAX_LEN_TOKEN) :: buffer
      type (FormatToken) :: currentToken
      procedure (HandlerInterface), pointer :: handler
   contains
      procedure :: setHandler
      procedure :: getHandler
      procedure :: getBuffer
      procedure :: getCurrentToken
      procedure :: parseCharacter
      procedure :: parse
   end type FormatParser

   !-----------------
   ! This implements the state pattern
   ! Because there is only one method, and we have singleton states,
   ! we can get away with procedure pointers instead of objects.
   !-----------------

   interface
      subroutine HandlerInterface(this, char)
         import FormatParser
         class (FormatParser), intent(inout) :: this
         character(len=1), intent(in) :: char
      end subroutine HandlerInterface
   end interface

   interface FormatParser
      module procedure newFormatParser
   end interface FormatParser

contains


   function newFormatParser() result(parser)
      type (FormatParser) :: parser

      parser%buffer = ''
      parser%currentPosition = 0 ! buffer is empty

      parser%handler => textHandler ! assume start with text (but confirm)

   end function newFormatParser

   subroutine setHandler(this, handler)
      class (FormatParser), intent(inout) :: this
      procedure (HandlerInterface) :: handler

      this%handler => handler

   end subroutine setHandler


   subroutine getHandler(this, handler)
      class (FormatParser), intent(in) :: this
      procedure (HandlerInterface), pointer :: handler

      handler => this%handler
   end subroutine getHandler


   ! Delegate to handler for current context
   subroutine parseCharacter(this, char)
      class (FormatParser), intent(inout) :: this
      character(len=1), intent(in) :: char

      call this%handler(char)

   end subroutine parseCharacter


   ! Delegate to handler for current context
   subroutine parse(this, str)
      class (FormatParser), intent(inout) :: this
      character(len=*), intent(in) :: str

      integer :: pos

      do pos = 1, len(str)
         call this%handler(str(pos:pos))
      end do

   end subroutine parse


   ! This should be a function that returns a pointer.
   ! Unfortunately gfortran 4.9.1 munges string pointers in
   ! that context.
   subroutine getBuffer(this, buffer)
      character(:), pointer :: buffer
      class (FormatParser), target, intent(in) :: this

      buffer => this%buffer(1:len_trim(this%buffer))
   end subroutine getBuffer

   function getCurrentToken(this) result(token)
      use ASTG_FormatToken_mod
      type (FormatToken), pointer :: token
      class (FormatParser), target, intent(in) :: this

      token => this%CurrentToken
   end function getCurrentToken



! Various contexts:

   subroutine textHandler(this, char)
      class (FormatParser), intent(inout) :: this
      character(len=1), intent(in) :: char

      associate (pos => this%currentPosition)
        pos = pos + 1
        this%buffer(pos:pos) = char
      end associate

      select case (char)
      case ("'")
         this%handler => singleQuoteHandler
      case ('"')
         this%handler => doubleQuoteHandler
      case default
         ! stay text
      end select


   end subroutine textHandler


   ! Single Quote handler treats other special characters as ordinary.
   ! Returns to text context when matching close quote is found.
   subroutine singleQuoteHandler(this, char)
      class (FormatParser), intent(inout) :: this
      character(len=1), intent(in) :: char

      associate (pos => this%currentPosition)
        pos = pos + 1
        this%buffer(pos:pos) = char
      end associate

      select case (char)
      case ("'")
         this%handler => textHandler
      case default
         ! stay single quote
      end select

   end subroutine singleQuoteHandler


   ! Double Quote handler treats other special characters as ordinary.
   ! Returns to text context when matching close quote is found.
   subroutine doubleQuoteHandler(this, char)
      class (FormatParser), intent(inout) :: this
      character(len=1), intent(in) :: char

      associate (pos => this%currentPosition)
        pos = pos + 1
        this%buffer(pos:pos) = char
      end associate

      select case (char)
      case ('"')
         this%handler => textHandler
      case default
         ! stay double quote
      end select

   end subroutine doubleQuoteHandler


end module ASTG_NewFormatParser_mod
