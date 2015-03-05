module ASTG_FormatToken_mod
   implicit none
   private
   
   public :: FormatToken
   public :: TEXT, POSITION, KEYWORD

   enum, bind(c)
      enumerator :: TEXT, POSITION, KEYWORD
   end enum

   type FormatToken
      integer :: type ! use enum
      character(len=:), allocatable :: textString
      character(len=:), allocatable :: formatString
      character(len=:), allocatable :: keywordString
   end type FormatToken

end module ASTG_FormatToken_mod
   
