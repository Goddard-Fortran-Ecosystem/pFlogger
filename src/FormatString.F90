#include "error_handling_macros.fh"
!------------------------------------------------------------------------------
! NASA/GSFC, CISTO, Code 606, Advanced Software Technology Group
!------------------------------------------------------------------------------
!
!*MODULE: PFL_FormatString
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
module PFL_FormatString
   use PFL_FormatParser
   use PFL_WrapArray
   use PFL_Exception
   use PFL_FormatToken
   use gFTL_StringUnlimitedMap
   use yaFyaml, only: String
   use PFL_KeywordEnforcer
   implicit none
   private

   public :: FormatString

   interface FormatString
      module procedure format_map
      module procedure format_vector
      module procedure format_preparsed
   end interface FormatString

   character(len=*), parameter :: LIST_DIRECTED_FORMAT = '*'

contains


   function format_map(fmt, dictionary, unusable, rc) result(string)
      character(len=:), allocatable :: string
      character(len=*), intent(in) :: fmt
      class(StringUnlimitedMap), intent(in) :: dictionary
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      type (FormatParser) :: p
      integer :: status

      _UNUSED_DUMMY(unusable)
      
      call p%parse(fmt)
      string = format_preparsed(p, dictionary, rc=status)
      _VERIFY(status,'',rc)
      _RETURN(_SUCCESS,rc)

   end function format_map

   function format_preparsed(parsed, dictionary, unusable, rc) result(string)
      use PFL_FormatTokenVector, only: TokenVectorIterator => VectorIterator
      character(len=:), allocatable :: string
      type (FormatParser), target, intent(in) :: parsed
      class(StringUnlimitedMap), target, intent(in) :: dictionary
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      type (TokenVectorIterator) :: tokenIter
      type (FormatToken), pointer :: token
      type (StringUnlimitedMapIterator) :: dictionaryIter
      class (*), pointer :: arg

      _UNUSED_DUMMY(unusable)

      tokenIter = parsed%begin()
      string = ''

      do while (tokenIter /= parsed%end())
         token => tokenIter%get()
         select case (token%type)

         case (TEXT)
            string = string // token%text

         case (KEYWORD)
            dictionaryIter = dictionary%find(token%text)
#            define _NO_KEYWORD(text) 'format_preparsed() - no such keyword: <'//text//'> in "extra".'
            _ASSERT(.not.(dictionaryIter == dictionary%end()),_NO_KEYWORD(token%text), rc)

            arg => dictionaryIter%value()
            string = string // handleScalar(arg, token%edit_descriptor)

         case (POSITION)
            _ASSERT(.false.,'format_map() - position arguments not allowed.', rc)
         end select

         call tokenIter%next()

      end do

      _RETURN(_SUCCESS,rc)
   end function format_preparsed


   function format_vector(fmt, args, unusable, rc) result(string)
      use PFL_FormatToken
      use PFL_FormatTokenVector, only: TokenVectorIterator => VectorIterator
      use gFTL_UnlimitedVector
      character(len=:), allocatable :: string
      character(len=*), intent(in) :: fmt
      type(UnlimitedVector), intent(in) :: args
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      type (FormatParser), save :: p
      character(len=:), save, allocatable :: old_fmt

      type (TokenVectorIterator) :: tokenIter
      type (UnlimitedVectorIterator) :: argIter
      type (FormatToken), pointer :: token
      class (*), pointer :: arg

      _UNUSED_DUMMY(unusable)

      if (.not. allocated(old_fmt)) then
         old_fmt = fmt
         call p%parse(fmt)
      end if

      string = ''
      if (fmt /= old_fmt) then
         old_fmt = fmt
         call p%reset()
         call p%parse(fmt)
      end if

      tokenIter = p%begin()
      argIter = args%begin()

      do while (tokenIter /= p%end())
         token => tokenIter%get()

         select case (token%type)

         case (TEXT)
            string = string // token%text

         case (POSITION)
            
#           define _END_OF_VALUES 'format_vector() - not enough position arguments for format string.'
            _ASSERT(.not.(argIter == args%end()), _END_OF_VALUES, rc)
            arg => argIter%get()
            string = string // handleScalar(arg, token%edit_descriptor)
            call argIter%next()

         case (KEYWORD)
            _ASSERT(.false.,'format_vector() - keyword arguments not allowed.',rc)
         end select

         call tokenIter%next()

      end do

      if (argIter /= args%end()) then
#        define _EXTRA_ARGS
         _ASSERT(.false., 'format_vector() - additional unprocessed arguments.',rc)
      end if

      _RETURN(_SUCCESS,rc)
   end function format_vector


   !---------------------------------------------------------------------------  
   ! FUNCTION: 
   ! handleScalar
   !
   ! DESCRIPTION:
   ! This function is used by format to deal with all unlimited polymorphic
   ! scalar variables passed to it.
   !---------------------------------------------------------------------------
   function handleScalar(arg, fmt, unusable, rc) result(str)
      use PFL_DynamicBuffer
      use iso_fortran_env, only: int8, int16, int32, int64, real32, real64, real128
      character(len=:), allocatable :: str
      class (*), intent(in) :: arg
      character(len=*), intent(in) :: fmt
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      type (DynamicBuffer) :: buffer
      integer :: iostat
      logical :: intrinsic
      logical, parameter :: SCALAR = .true.
      integer :: status

      _UNUSED_DUMMY(unusable)

      iostat = -1
      call buffer%allocate()
      do while (iostat /= 0)

         select type (arg)
         type is (String)
            if (fmt(1:1) == LIST_DIRECTED_FORMAT) then
               write(buffer%buffer,'(a)',iostat=iostat) arg%get()
            else
               write(buffer%buffer,fmt,iostat=iostat) arg%get()
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
               buffer%buffer = 'handleScalar() :: unsupported type (possibly rank > 5?)'
               iostat = 0
            end select
         end if
         
         if (iostat == 0) exit
         if (iostat == INTERNAL_FILE_EOR) then
            call buffer%grow_record_size(rc=status)
            _VERIFY(status,'',rc)
            cycle
         end if
         if (iostat == INTERNAL_FILE_EOF) then
            call buffer%grow_num_records(rc=status)
            _VERIFY(status,'',rc)
            cycle
         end if
         
         ! unrecoverable iostat
         str=''
         _ASSERT(.false.,'handleScalar() - bad format "'//fmt//'"',rc)
         
      end do

      str = buffer%concatenate()
      _RETURN(_SUCCESS,rc)

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
      use PFL_DynamicBuffer
      use iso_fortran_env, only: int32, real32, int64, real64, real128
      use iso_fortran_env, only: int8, int16, int32, int64, real32, real64, real128

      class (*), intent(in) :: arg(:)
      character(len=*), intent(in) :: fmt
      type (DynamicBuffer), intent(inout) :: buffer
      integer, intent(inout) :: iostat

      logical :: intrinsic
      logical, parameter :: SCALAR = .false.

      include 'write_if_intrinsic.inc'
      
      if (.not. intrinsic) then
         buffer%buffer = 'handleScalar() - unsupported type'
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
      use PFL_DynamicBuffer
      use iso_fortran_env, only: int8, int16, int32, int64, real32, real64, real128
      class (*), intent(in) :: arg(:,:)
      character(len=*), intent(in) :: fmt
      type (DynamicBuffer), intent(inout) :: buffer
      integer, intent(inout) :: iostat

      logical :: intrinsic
      logical, parameter :: SCALAR = .false.

      include 'write_if_intrinsic.inc'

      if (.not. intrinsic) then
         buffer%buffer = 'handleScalar() - unsupported type'
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
      use PFL_DynamicBuffer
      use iso_fortran_env, only: int8, int16, int32, int64, real32, real64, real128
      class (*), intent(in) :: arg(:,:,:)
      character(len=*), intent(in) :: fmt
      type (DynamicBuffer), intent(inout) :: buffer
      integer, intent(inout) :: iostat

      logical :: intrinsic
      logical, parameter :: SCALAR = .false.

      include 'write_if_intrinsic.inc'

      if (.not. intrinsic) then
         buffer%buffer = 'handleScalar() - unsupported type'
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
      use PFL_DynamicBuffer
      use iso_fortran_env, only: int8, int16, int32, int64, real32, real64, real128
      class (*), intent(in) :: arg(:,:,:,:)
      character(len=*), intent(in) :: fmt
      type (DynamicBuffer), intent(inout) :: buffer
      integer, intent(inout) :: iostat

      logical :: intrinsic
      logical, parameter :: SCALAR = .false.

      include 'write_if_intrinsic.inc'

      if (.not. intrinsic) then
         buffer%buffer = 'handleScalar() - unsupported type'
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
      use PFL_DynamicBuffer
      use iso_fortran_env, only: int8, int16, int32, int64, real32, real64, real128
      class (*), intent(in) :: arg(:,:,:,:,:)
      character(len=*), intent(in) :: fmt
      type (DynamicBuffer), intent(inout) :: buffer
      integer, intent(inout) :: iostat

      logical :: intrinsic
      logical, parameter :: SCALAR = .false.

      include 'write_if_intrinsic.inc'

      if (.not. intrinsic) then
         buffer%buffer = 'handleScalar() - unsupported type'
         iostat = 0
      end if

   end subroutine handleArray5D


end module PFL_FormatString
