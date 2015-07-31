!------------------------------------------------------------------------------
! NASA/GSFC, CISTO, Code 606, Advanced Software Technology Group
!------------------------------------------------------------------------------
!
!*MODULE: ASTG_FormatString_mod
!
!> @brief Format strings specified by formatter.
!> @details
!! This module imitates the Python "%" operator for formatting strings.
!! There are two cases:
!!
!!    str = fmtSpec  .fmt.    <map>
!!
!!  and
!!
!!    str = fmtSpec  .fmt.    <vector>
!!
!!  The former uses keywords, and the latter uses position arguments.
!
!> @author ASTG staff
!> @date 01 Jan 2015 - Initial Version  
!---------------------------------------------------------------------------
module ASTG_FormatString_mod
   use ASTG_FormatParser_mod
   use ASTG_WrapArray_mod
   use ASTG_Exception_mod
   implicit none
   private

   public :: FormatString
#ifndef __GFORTRAN__
   public :: operator(.fmt.)
#endif

   interface FormatString
      module procedure formatMap
      module procedure formatVector
   end interface FormatString

#ifndef __GFORTRAN__
   interface operator(.fmt.)
      module procedure formatMap
      module procedure formatVector
   end interface operator(.fmt.)
#endif

   character(len=*), parameter :: LIST_DIRECTED_FORMAT = '*'

contains


   function formatMap(fmt, dictionary) result(string)
      use ASTG_FormatToken_mod
      use ASTG_FormatTokenVector_mod, only: TokenVector => Vector
      use ASTG_FormatTokenVector_mod, only: TokenVectorIterator => VectorIterator
      use ASTG_CIStringUnlimitedMap_mod, only: Map
      use ASTG_CIStringUnlimitedMap_mod, only: MapIterator
      character(len=:), allocatable :: string
      character(len=*), intent(in) :: fmt
      class(Map), intent(in) :: dictionary

      type (FormatParser) :: p
      type (TokenVectorIterator) :: tokenIter
      type (FormatToken), pointer :: token
      type (MapIterator) :: dictionaryIter
      class (*), pointer :: arg

      call p%parse(fmt)
      
      tokenIter = p%begin()
      string = ''

      do while (tokenIter /= p%end())
         token => tokenIter%get()
         select case (token%type)

         case (TEXT)
            string = string // token%text

         case (KEYWORD)
            dictionaryIter = dictionary%find(token%text)
            if (dictionaryIter == dictionary%end()) then
               call throw('FormatString::formatMap() - no such keyword: <' // token%text // '> in "extra".')
               return
            end if
            arg => dictionaryIter%value()
            string = string // handleScalar(arg, token%editDescriptor)

         case (POSITION)
            call throw('FormatString::formatMap() - position arguments not allowed.')
            return
         end select

         call tokenIter%next()

      end do

   end function formatMap


   function formatVector(fmt, args) result(string)
      use ASTG_FormatToken_mod
      use ASTG_FormatTokenVector_mod, only: TokenVector => Vector
      use ASTG_FormatTokenVector_mod, only: TokenVectorIterator => VectorIterator
      use ASTG_UnlimitedVector_mod, only: Vector
      use ASTG_UnlimitedVector_mod, only: VectorIterator
      character(len=:), allocatable :: string
      character(len=*), intent(in) :: fmt
      class(Vector), intent(in) :: args

      type (FormatParser) :: p
      type (TokenVectorIterator) :: tokenIter
      type (VectorIterator) :: argIter
      type (FormatToken), pointer :: token
      class (*), pointer :: arg

      string = ''
      ! Workaround for Gfortran problem with empty vectors
      if (args%size() == 0) then
         string = fmt
         return
      end if

      call p%parse(fmt)

      tokenIter = p%begin()
      argIter = args%begin()

      do while (tokenIter /= p%end())
         token => tokenIter%get()

         select case (token%type)

         case (TEXT)
            string = string // token%text

         case (POSITION)
            if (argIter == args%end()) then
               ! check other ranks
               call throw('FormatString::formatVector() - not enough values for format string.')
               return
            else
               arg => argIter%get()
               string = string // handleScalar(arg, token%editDescriptor)
               call argIter%next()
            end if

         case (KEYWORD)
            call throw('FormatString::formatVector() - keyword arguments not allowed.')
            return
         end select

         call tokenIter%next()

      end do

      if (argIter /= args%end()) then
         call throw('FormatString::formatVector() - not all arguments converted during string formatting.')
      end if

   end function formatVector


   !---------------------------------------------------------------------------  
   ! FUNCTION: 
   ! handleScalar
   !
   ! DESCRIPTION:
   ! This function is used by format to deal with all unlimited polymorphic
   ! scalar variables passed to it.
   !---------------------------------------------------------------------------
   function handleScalar(arg, fmt) result(str)
      use ASTG_DynamicBuffer_mod
      use iso_fortran_env, only: int8, int16, int32, int64, real32, real64, real128
      use ASTG_String_mod
      character(len=:), allocatable :: str
      class (*), intent(in) :: arg
      character(len=*), intent(in) :: fmt

      type (DynamicBuffer) :: buffer
      integer :: iostat
      logical :: intrinsic

      iostat = -1
      call buffer%allocate()
      do while (iostat /= 0)

         select type (arg)
         type is (String)
            if (fmt == LIST_DIRECTED_FORMAT) then
               write(buffer%buffer,*,iostat=iostat) arg%str
            else
               write(buffer%buffer,'(' // fmt // ')',iostat=iostat) arg%str
            end if
            intrinsic = .true.
         class default
            include 'write_if_intrinsic.inc'
         end select

         if (.not. intrinsic) then ! try wrapped array
            select type (arg)
            type is (WrapArray1D)
               call handleArray1D(arg%array, fmt, buffer, iostat=iostat)
            type is (WrapArray2D)
               call handleArray2D(arg%array, fmt, buffer, iostat=iostat)
            type is (WrapArray3D)
               call handleArray3D(arg%array, fmt, buffer, iostat=iostat)
            type is (WrapArray4D)
               call handleArray4D(arg%array, fmt, buffer, iostat=iostat)
            type is (WrapArray5D)
               call handleArray5D(arg%array, fmt, buffer, iostat=iostat)
            class default ! other
               buffer%buffer(1) = 'FormatParser::handleScalar() :: unsupported type'
               iostat = 0
            end select
         end if
         
         if (iostat == 0) exit
         if (iostat == INTERNAL_FILE_EOR) then
            call buffer%growRecordSize()
            cycle
         end if
         if (iostat == INTERNAL_FILE_EOF) then
            call buffer%growNumRecords()
            cycle
         end if
         
         ! unrecoverable iostat
         call throw('FormatString::format*() - bad format "'//fmt//'"')
         str=''
         return
         
      end do

      str = buffer%concatenate()

   end function handleScalar

   !---------------------------------------------------------------------------  
   ! FUNCTION: 
   ! handleArray1D
   !
   ! DESCRIPTION: 
   ! This function is used by format to deal with all unlimited polymorphic
   ! 1D vector variables passed to it.
   !---------------------------------------------------------------------------
   subroutine handleArray1D(arg, fmt, buffer, iostat)
      use ASTG_DynamicBuffer_mod
      use iso_fortran_env, only: int32, real32, int64, real64, real128
      use iso_fortran_env, only: int8, int16, int32, int64, real32, real64, real128

      class (*), intent(in) :: arg(:)
      character(len=*), intent(in) :: fmt
      type (DynamicBuffer), intent(inout) :: buffer
      integer, intent(inout) :: iostat

      logical :: intrinsic

      include 'write_if_intrinsic.inc'
      
      if (.not. intrinsic) then
         buffer%buffer(1) = 'FormatParser::handleScalar() :: unsupported type'
         iostat = 0
      end if

   end subroutine handleArray1D


   !---------------------------------------------------------------------------  
   ! FUNCTION: 
   ! handleArray2D
   !
   ! DESCRIPTION: 
   ! This function is used by format to deal with all unlimited polymorphic
   ! 2D vector variables passed to it.
   !---------------------------------------------------------------------------
   subroutine handleArray2D(arg, fmt, buffer, iostat)
      use ASTG_DynamicBuffer_mod
      use iso_fortran_env, only: int8, int16, int32, int64, real32, real64, real128
      class (*), intent(in) :: arg(:,:)
      character(len=*), intent(in) :: fmt
      type (DynamicBuffer), intent(inout) :: buffer
      integer, intent(inout) :: iostat

      logical :: intrinsic

      include 'write_if_intrinsic.inc'

      if (.not. intrinsic) then
         buffer%buffer(1) = 'FormatParser::handleScalar() :: unsupported type'
         iostat = 0
      end if

   end subroutine handleArray2D


   !---------------------------------------------------------------------------  
   ! FUNCTION: 
   ! handleArray3D
   !
   ! DESCRIPTION: 
   ! This function is used by format to deal with all unlimited polymorphic
   ! 3D vector variables passed to it.
   !---------------------------------------------------------------------------
   subroutine handleArray3D(arg, fmt, buffer, iostat)
      use ASTG_DynamicBuffer_mod
      use iso_fortran_env, only: int8, int16, int32, int64, real32, real64, real128
      class (*), intent(in) :: arg(:,:,:)
      character(len=*), intent(in) :: fmt
      type (DynamicBuffer), intent(inout) :: buffer
      integer, intent(inout) :: iostat

      logical :: intrinsic

      include 'write_if_intrinsic.inc'

      if (.not. intrinsic) then
         buffer%buffer(1) = 'FormatParser::handleScalar() :: unsupported type'
         iostat = 0
      end if

   end subroutine handleArray3D


   !---------------------------------------------------------------------------  
   ! FUNCTION: 
   ! handleArray4D
   !
   ! DESCRIPTION: 
   ! This function is used by format to deal with all unlimited polymorphic
   ! 4D vector variables passed to it.
   !---------------------------------------------------------------------------
   subroutine handleArray4D(arg, fmt, buffer, iostat)
      use ASTG_DynamicBuffer_mod
      use iso_fortran_env, only: int8, int16, int32, int64, real32, real64, real128
      class (*), intent(in) :: arg(:,:,:,:)
      character(len=*), intent(in) :: fmt
      type (DynamicBuffer), intent(inout) :: buffer
      integer, intent(inout) :: iostat

      logical :: intrinsic

      include 'write_if_intrinsic.inc'

      if (.not. intrinsic) then
         buffer%buffer(1) = 'FormatParser::handleScalar() :: unsupported type'
         iostat = 0
      end if

   end subroutine handleArray4D


   !---------------------------------------------------------------------------  
   ! FUNCTION: 
   ! handleArray5D
   !
   ! DESCRIPTION: 
   ! This function is used by format to deal with all unlimited polymorphic
   ! 5D vector variables passed to it.
   !---------------------------------------------------------------------------
   subroutine handleArray5D(arg, fmt, buffer, iostat)
      use ASTG_DynamicBuffer_mod
      use iso_fortran_env, only: int8, int16, int32, int64, real32, real64, real128
      class (*), intent(in) :: arg(:,:,:,:,:)
      character(len=*), intent(in) :: fmt
      type (DynamicBuffer), intent(inout) :: buffer
      integer, intent(inout) :: iostat

      logical :: intrinsic

      include 'write_if_intrinsic.inc'

      if (.not. intrinsic) then
         buffer%buffer(1) = 'FormatParser::handleScalar() :: unsupported type'
         iostat = 0
      end if

   end subroutine handleArray5D


end module ASTG_FormatString_mod
