!------------------------------------------------------------------------------
! NASA/GSFC, CISTO, Code 606, Advanced Software Technology Group
!------------------------------------------------------------------------------
!
!*MODULE: ASTG_FormatToken_mod
!
!> @brief Provides constructor for a format token.
!
!> @author ASTG staff
!> @date 01 Jan 2015 - Initial Version  
!---------------------------------------------------------------------------
module ASTG_FormatToken_mod
   use ASTG_Exception_mod, only: throw
   implicit none
   private
   
   public :: FormatToken
   public :: KEYWORD_SEPARATOR
   public :: TEXT, POSITION, KEYWORD

   enum, bind(c)
      enumerator :: TEXT, POSITION, KEYWORD
   end enum

   character(len=1), parameter :: KEYWORD_SEPARATOR = ')'

   type FormatToken
      integer :: type ! use enum
      character(len=:), allocatable :: text
      character(len=:), allocatable :: editDescriptor
   end type FormatToken


   interface FormatToken
      module procedure newFormatToken
   end interface FormatToken


contains


   function newFormatToken(type, string) result(token)
      type (FormatToken) :: token
      integer, intent(in) :: type
      character(len=*), intent(in) :: string

      integer :: idx

      token%type = type

      select case (type)

      case (TEXT)
         token%text = string

      case (POSITION)
         token%editDescriptor = string

      case (KEYWORD)
         idx = index(string, KEYWORD_SEPARATOR)
         if (idx == 1) then
            token%text = ''
            call throw('FormatParser::keywordFormatHandler() - missing keyword in format specifier')
            return
         end if
         token%text = string(:idx-1)
         token%editDescriptor = string(idx+1:len_trim(string))

      end select
      
   end function newFormatToken


end module ASTG_FormatToken_mod
   
