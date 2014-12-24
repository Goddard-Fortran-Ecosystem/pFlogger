module FTL_CaseInsensitiveStringLoggerPolyPair_mod
   
   use FTL_CaseInsensitiveString_mod
   
   
   use ASTG_Logger_mod, only: Logger
   
   
   implicit none
   private

   public :: CaseInsensitiveStringLoggerPolyPair
   public :: swap

   type CaseInsensitiveStringLoggerPolyPair
      type(CaseInsensitiveString) :: first
      class(Logger), allocatable :: second
   contains
      procedure :: swap => swapPair
      procedure :: copy
      generic :: assignment(=) => copy
   end type CaseInsensitiveStringLoggerPolyPair

   interface CaseInsensitiveStringLoggerPolyPair
      module procedure newPair
   end interface CaseInsensitiveStringLoggerPolyPair

   interface swap
      module procedure swapPair
   end interface swap

contains


   function newPair(first, second) result(pair)
      type (CaseInsensitiveStringLoggerPolyPair) :: pair
      type(CaseInsensitiveString), intent(in) :: first
      class(Logger), intent(in) :: second

      pair%first = first
      allocate(pair%second, source=second)

   end function newPair


   subroutine swapPair(this, other)
      class (CaseInsensitiveStringLoggerPolyPair), intent(inout) :: this
      type (CaseInsensitiveStringLoggerPolyPair), intent(inout) :: other

      type (CaseInsensitiveStringLoggerPolyPair) :: tmp

      tmp = this
      this = other
      other = this      

   end subroutine swapPair


   subroutine copy(a, b)
      class (CaseInsensitiveStringLoggerPolyPair), intent(out) :: a
      type (CaseInsensitiveStringLoggerPolyPair), intent(in) :: b

      a%first = b%first
      allocate(a%second, source=b%second)

   end subroutine copy

end module FTL_CaseInsensitiveStringLoggerPolyPair_mod
