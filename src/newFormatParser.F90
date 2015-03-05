module ASTG_NewFormatParser_mod
   use ASTG_FormatToken_mod
   implicit none
   private

   public :: FormatParser
   public :: HandlerInterface
   public :: textHandler

   type FormatParser
      private
      type (FormatToken) :: currentToken
      procedure (HandlerInterface), pointer :: handler
   contains
      procedure :: setHandler
      procedure :: getHandler
      procedure :: getCurrentToken
      procedure :: parseCharacter
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

      this%currentToken%textString = char
      
   end subroutine textHandler


end module ASTG_NewFormatParser_mod
