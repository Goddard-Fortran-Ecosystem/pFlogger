module FTL_CIStringAbstractLoggerPolyPair_mod
   
   use FTL_CIString_mod
   
   
   use ASTG_AbstractLogger_mod, only: AbstractLogger
   
   
   implicit none
   private

   public :: CIStringAbstractLoggerPolyPair
   public :: swap

   type CIStringAbstractLoggerPolyPair
      type(CIString) :: first
      class(AbstractLogger), allocatable :: second
   contains
      procedure :: swap => swapPair
      procedure :: copy
      generic :: assignment(=) => copy
   end type CIStringAbstractLoggerPolyPair

   interface CIStringAbstractLoggerPolyPair
      module procedure newPair
   end interface CIStringAbstractLoggerPolyPair

   interface swap
      module procedure swapPair
   end interface swap

contains


   function newPair(first, second) result(pair)
      type (CIStringAbstractLoggerPolyPair) :: pair
      type(CIString), intent(in) :: first
      class(AbstractLogger), intent(in) :: second

      pair%first = first
      allocate(pair%second, source=second)

   end function newPair


   subroutine swapPair(this, other)
      class (CIStringAbstractLoggerPolyPair), intent(inout) :: this
      type (CIStringAbstractLoggerPolyPair), intent(inout) :: other

      type (CIStringAbstractLoggerPolyPair) :: tmp

      tmp = this
      this = other
      other = this      

   end subroutine swapPair


   subroutine copy(a, b)
      class (CIStringAbstractLoggerPolyPair), intent(out) :: a
      type (CIStringAbstractLoggerPolyPair), intent(in) :: b

      a%first = b%first
      allocate(a%second, source=b%second)

   end subroutine copy

end module FTL_CIStringAbstractLoggerPolyPair_mod
