module ASTG_StringUtilities_mod
   use iso_fortran_env
   implicit none
   private

!   public :: toString
   public :: toLowerCase
   public :: toUpperCase
   public :: toString

   integer, parameter :: UPPER_LOWER_DELTA = iachar('A') - iachar('a')

   interface toString
      module procedure toString_int32
      module procedure toString_int64
      module procedure toString_real32
      module procedure toString_real64
      module procedure toString_cmplx64
      module procedure toString_cmplx128
      module procedure toString_string
      module procedure toString_logical
   end interface toString

contains


   pure function toLowerCase(string) result(lowerCase)
      character(len=*), intent(in) :: string
      character(len=len_trim(string)) :: lowerCase

      integer :: i

      do i= 1, len_trim(string)
         lowerCase(i:i) = toLowerCaseChar(string(i:i))
      end do

   contains

      pure function toLowerCaseChar(char) result(lowerCaseChar)
         character :: lowerCaseChar
         character, intent(in) :: char

         if (isUpperCase(char)) then
            lowerCaseChar = achar(iachar(char) - UPPER_LOWER_DELTA)
         else
            lowerCaseChar = char
         end if

      end function toLowerCaseChar

   end function toLowerCase


   pure function toUpperCase(string) result(upperCase)
      character(len=*), intent(in) :: string
      character(len=len_trim(string)) :: upperCase

      integer :: i

      do i= 1, len_trim(string)
         upperCase(i:i) = toUpperCaseChar(string(i:i))
      end do

   contains

      pure function toUpperCaseChar(char) result(upperCaseChar)
         character :: upperCaseChar
         character, intent(in) :: char

         if (isLowerCase(char)) then
            upperCaseChar = achar(iachar(char) + UPPER_LOWER_DELTA)
         else
            upperCaseChar = char
         end if

      end function toUpperCaseChar

   end function toUpperCase


   pure logical function isUpperCase(char)
      character(len=1), intent(in) :: char
      
      integer :: i
      integer, parameter :: iA = iachar('A')
      integer, parameter :: iZ = iachar('Z')

      i = iachar(char)
      isUpperCase = ((i >= iA) .and. (i <= iZ))

   end function isUpperCase


   pure logical function isLowerCase(char)
      character(len=1), intent(in) :: char
      
      integer :: i
      integer, parameter :: ia = iachar('a')
      integer, parameter :: iz = iachar('z')

      i = iachar(char)
      isLowerCase = ((i >= ia) .and. (i <= iz))

   end function isLowerCase


   pure function toString_int32(i) result(string)
      character(:), allocatable :: string
      integer(kind=int32), intent(in) :: i
      
      character(24) :: buf

      write(buf,'(i0)') i
      string = trim(buf)
      
   end function toString_int32


   pure function toString_int64(i) result(string)
      character(:), allocatable :: string
      integer(kind=int64), intent(in) :: i
      
      character(24) :: buf

      write(buf,'(i0)') i
      string = trim(buf)
      
   end function toString_int64


   pure function toString_real32(x) result(string)
      use iso_fortran_env, only: REAL32
      character(:), allocatable :: string
      real(kind=REAL32), intent(in) :: x
      
      character(24) :: buf

      write(buf,'(g14.7)') x
      string = trim(adjustl(buf))
      
   end function toString_real32


   pure function toString_real64(x) result(string)
      use iso_fortran_env, only: REAL64
      character(:), allocatable :: string
      real(kind=REAL64), intent(in) :: x
      
      character(32) :: buf

      write(buf,'(g25.17)') x
      string = trim(adjustl(buf))

   end function toString_real64


   pure function toString_cmplx64(x) result(string)
      use iso_fortran_env, only: REAL32
      character(:), allocatable :: string
      complex(kind=REAL32), intent(in) :: x
      
      string = '(' // toString_real32(real(x,kind=REAL32)) // ',' &
           & // toString_real32(aimag(x)) // ')'
      
   end function toString_cmplx64


   pure function toString_cmplx128(x) result(string)
      use iso_fortran_env, only: REAL64
      character(:), allocatable :: string
      complex(kind=REAL64), intent(in) :: x
      
      string = '(' // toString_real64(real(x,kind=REAL64)) // ',' &
           & // toString_real64(aimag(x)) // ')'
      
   end function toString_cmplx128


   pure function toString_string(inString) result(string)
      character(:), allocatable :: string
      character(*), intent(in) :: inSTring
      
      string = trim(inString)
      
   end function toString_string


   pure function toString_logical(flag) result(string)
      character(:), allocatable :: string
      logical, intent(in) :: flag

      if (flag) then
         string = 'T'
      else
         string = 'F'
      end if
      
   end function toString_logical


end module ASTG_StringUtilities_mod
