!------------------------------------------------------------------------------
! NASA/GSFC, CISTO, Code 606, Advanced Software Technology Group
!------------------------------------------------------------------------------
!
!*MODULE: PFL_FormatString_mod
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
module PFL_FormatString_mod
   use PFL_FormatParser_mod
   use PFL_WrapArray_mod
   use PFL_Exception_mod
   implicit none
   private

   public :: FormatString
#ifndef __GFORTRAN__
   public :: operator(.fmt.)
#endif

   interface FormatString
      module procedure format_map
      module procedure format_vector
      module procedure format_preparsed
   end interface FormatString

#ifndef __GFORTRAN__
   interface operator(.fmt.)
      module procedure format_map
      module procedure format_vector
      module procedure format_preparsed
   end interface operator(.fmt.)
#endif

   character(len=*), parameter :: LIST_DIRECTED_FORMAT = '*'

contains


   function format_map(fmt, dictionary) result(string)
      use PFL_FormatToken_mod
      use PFL_StringUnlimitedMap_mod, only: Map
      character(len=:), allocatable :: string
      character(len=*), intent(in) :: fmt
      class(Map), intent(in) :: dictionary

      type (FormatParser) :: p

      call p%parse(fmt)
      string = format_preparsed(p, dictionary)

   end function format_map

   function format_preparsed(parsed, dictionary) result(string)
      use PFL_FormatToken_mod
      use PFL_FormatTokenVector_mod, only: TokenVector => Vector
      use PFL_FormatTokenVector_mod, only: TokenVectorIterator => VectorIterator
      use PFL_StringUnlimitedMap_mod, only: Map
      use PFL_StringUnlimitedMap_mod, only: MapIterator
      character(len=:), allocatable :: string
      type (FormatParser), intent(in) :: parsed
      class(Map), intent(in) :: dictionary

      type (TokenVectorIterator) :: tokenIter
      type (FormatToken), pointer :: token
      type (MapIterator) :: dictionaryIter
      class (*), pointer :: arg

      tokenIter = parsed%begin()
      string = ''

      do while (tokenIter /= parsed%end())
         token => tokenIter%get()
         select case (token%type)

         case (TEXT)
            string = string // token%text

         case (KEYWORD)
            dictionaryIter = dictionary%find(token%text)
            if (dictionaryIter == dictionary%end()) then
               call throw('FormatString::format_map() - no such keyword: <' // token%text // '> in "extra".')
               return
            end if
            arg => dictionaryIter%value()
            string = string // handleScalar(arg, token%edit_descriptor)

         case (POSITION)
            call throw('FormatString::format_map() - position arguments not allowed.')
            return
         end select

         call tokenIter%next()

      end do

   end function format_preparsed


   function format_vector(fmt, args) result(string)
      use PFL_FormatToken_mod
      use PFL_FormatTokenVector_mod, only: TokenVector => Vector
      use PFL_FormatTokenVector_mod, only: TokenVectorIterator => VectorIterator
      use PFL_UnlimitedVector_mod, only: Vector
      use PFL_UnlimitedVector_mod, only: VectorIterator
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
               call throw('FormatString::format_vector() - not enough values for format string.')
               return
            else
               arg => argIter%get()
               string = string // handleScalar(arg, token%edit_descriptor)
               call argIter%next()
            end if

         case (KEYWORD)
            call throw('FormatString::format_vector() - keyword arguments not allowed.')
            return
         end select

         call tokenIter%next()

      end do

      if (argIter /= args%end()) then
         call throw('FormatString::format_vector() - not all arguments converted during string formatting.')
      end if

   end function format_vector


   !---------------------------------------------------------------------------  
   ! FUNCTION: 
   ! handleScalar
   !
   ! DESCRIPTION:
   ! This function is used by format to deal with all unlimited polymorphic
   ! scalar variables passed to it.
   !---------------------------------------------------------------------------
   function handleScalar(arg, fmt) result(str)
      use PFL_DynamicBuffer_mod
      use iso_fortran_env, only: int8, int16, int32, int64, real32, real64, real128
      use FTL
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
               write(buffer%buffer,*,iostat=iostat) arg%get(), achar(003)
            else
               write(buffer%buffer,'(' // fmt // ',"'//achar(003)//'")',iostat=iostat) arg%get()
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
            call buffer%grow_record_size()
            cycle
         end if
         if (iostat == INTERNAL_FILE_EOF) then
            call buffer%grow_num_records()
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
      use PFL_DynamicBuffer_mod
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
      use PFL_DynamicBuffer_mod
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
      use PFL_DynamicBuffer_mod
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
      use PFL_DynamicBuffer_mod
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
      use PFL_DynamicBuffer_mod
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


end module PFL_FormatString_mod
