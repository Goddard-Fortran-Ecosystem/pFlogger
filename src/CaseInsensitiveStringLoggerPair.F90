module FTL_CaseInsensitiveStringLoggerPair_mod
   
   use FTL_CaseInsensitiveString_mod
   
   
   use ASTG_Logger_mod, only: Logger
   
   
   implicit none
   private

   public :: CaseInsensitiveStringLoggerPair
   public :: swap

   type CaseInsensitiveStringLoggerPair
      type(CaseInsensitiveString) :: first
      type(Logger) :: second
   contains
      procedure :: swap => swapPair
      procedure :: copy
      generic :: assignment(=) => copy
   end type CaseInsensitiveStringLoggerPair

   interface CaseInsensitiveStringLoggerPair
      module procedure newPair
   end interface CaseInsensitiveStringLoggerPair

   interface swap
      module procedure swapPair
   end interface swap

contains


   function newPair(first, second) result(pair)
      type (CaseInsensitiveStringLoggerPair) :: pair
      type(CaseInsensitiveString), intent(in) :: first
      type(Logger), intent(in) :: second

      pair%first = first
      pair%second = second

   end function newPair


   subroutine swapPair(this, other)
      class (CaseInsensitiveStringLoggerPair), intent(inout) :: this
      type (CaseInsensitiveStringLoggerPair), intent(inout) :: other

      type (CaseInsensitiveStringLoggerPair) :: tmp

      tmp = this
      this = other
      other = this      

   end subroutine swapPair


   subroutine copy(a, b)
      class (CaseInsensitiveStringLoggerPair), intent(out) :: a
      type (CaseInsensitiveStringLoggerPair), intent(in) :: b

      a%first = b%first
      a%second = b%second

   end subroutine copy

end module FTL_CaseInsensitiveStringLoggerPair_mod
