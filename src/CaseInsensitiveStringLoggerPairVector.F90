
module FTL_CaseInsensitiveStringLoggerPairVector_mod
      use FTL_CaseInsensitiveStringLoggerPair_mod, only: CaseInsensitiveStringLoggerPair
      
   implicit none
   private

   public :: CaseInsensitiveStringLoggerPairVector
   public :: CaseInsensitiveStringLoggerPairVectorIterator
   public :: CaseInsensitiveStringLoggerPairVectorReverseIterator
   ! external functions
   public :: swap


   integer, parameter :: UNINITIALIZED = -1
   type :: CaseInsensitiveStringLoggerPairVector
      private
      
      type(CaseInsensitiveStringLoggerPair), allocatable :: elements(:)
      integer :: numElements = 0

   contains

      procedure :: copyVector
      procedure :: copyFromArray
      generic :: assignment(=) => copyVector, copyFromArray

      procedure :: size => getSize
      procedure :: capacity
      procedure :: empty

      procedure :: at
      procedure :: front
      procedure :: back



      procedure :: data

      procedure :: push_back_T
      generic :: push_back => push_back_T
      procedure :: pop_back
      procedure :: insert_T
      generic :: insert => insert_T


      procedure :: reserve
      procedure :: resize
      procedure :: clear

      procedure :: begin
      procedure :: end
      procedure :: rBegin
      procedure :: rEnd

   end type CaseInsensitiveStringLoggerPairVector

   type CaseInsensitiveStringLoggerPairVectorIterator
      type(CaseInsensitiveStringLoggerPair), pointer :: elements(:) => null()
      integer :: index = UNINITIALIZED
   contains
      procedure :: get

      procedure :: next
      procedure :: previous
      procedure :: atDefault
      procedure :: atOffset
      generic :: at => atDefault, atOffset
      
      procedure :: equalIters
      procedure :: notEqualIters
      generic :: operator(==) => equalIters
      generic :: operator(/=) => notEqualIters

      procedure :: lessThanIter
      procedure :: lessThanOrEqualIter
      procedure :: greaterThanIter
      procedure :: greaterThanOrEqualIter
      generic :: operator(<) => lessThanIter
      generic :: operator(<=) => lessThanOrEqualIter
      generic :: operator(>) => greaterThanIter
      generic :: operator(>=) => greaterThanOrEqualIter
      
      procedure :: add
      generic :: operator(+) => add

   end type CaseInsensitiveStringLoggerPairVectorIterator

   type CaseInsensitiveStringLoggerPairVectorReverseIterator
      type(CaseInsensitiveStringLoggerPair), pointer :: elements(:) => null()
      integer :: index = UNINITIALIZED
   contains
      procedure :: get => getRIter

      procedure :: next => rNext
      procedure :: previous => rPrevious
      procedure :: rAtDefault
      procedure :: rAtOffset
      generic :: at => rAtDefault, rAtOffset

      procedure :: equalRIters
      procedure :: notEqualRIters
      generic :: operator(==) => equalRIters
      generic :: operator(/=) => notEqualRIters

      procedure :: rLessThanIter
      procedure :: rLessThanOrEqualIter
      procedure :: rGreaterThanIter
      procedure :: rGreaterThanOrEqualIter
      generic :: operator(<) => rLessThanIter
      generic :: operator(<=) => rLessThanOrEqualIter
      generic :: operator(>) => rGreaterThanIter
      generic :: operator(>=) => rGreaterThanOrEqualIter
   end type CaseInsensitiveStringLoggerPairVectorReverseIterator


   interface CaseInsensitiveStringLoggerPairVector
      module procedure constructor_empty
   end interface CaseInsensitiveStringLoggerPairVector


   interface swap
      module procedure swapVector
   end interface swap


contains


   ! Returns an empty array.   Note that reserve() may 
   ! preallocate some memory even for an empty Vector.
   function constructor_empty() result(v)
      type (CaseInsensitiveStringLoggerPairVector) :: v

      call v%reserve(0)
      v%numElements = 0

   end function constructor_empty

!----------------------------------------------
! Create a Vector by copying from another
!----------------------------------------------
   subroutine copyVector(this, other)
      class (CaseInsensitiveStringLoggerPairVector), intent(inout) :: this
      type (CaseInsensitiveStringLoggerPairVector), intent(in) :: other

      integer :: i, n

      n = other%size()
      call reserve(this, n)
      this%numElements = n

      do i = 1, n
         this%elements(i) = other%elements(i)
      end do

   end subroutine copyVector


!----------------------------------------------
! Create a Vector from a standard Fortran array.
!----------------------------------------------
   subroutine copyFromArray(this, array)
      class (CaseInsensitiveStringLoggerPairVector), intent(inout) :: this
      type(CaseInsensitiveStringLoggerPair), intent(in) :: array(:)

      integer :: i, n

      n = size(array)

      call reserve(this, n)
      this%numElements = n
      do i = 1, n
         this%elements(i) = array(i)
      end do

   end subroutine copyFromArray


!----------------------------------------------
! Return the number of active elements in the vector.
! Note that the internal array may be larger. (See reserve().)
!----------------------------------------------
   integer function getSize(this) 
      class (CaseInsensitiveStringLoggerPairVector), intent(in) :: this
      getSize = this%numElements
   end function getSize


!----------------------------------------------
! Return the actual length of the internal storage array.
! Note this is different than the size of the Vector.
!----------------------------------------------
   integer function capacity(this) 
      class (CaseInsensitiveStringLoggerPairVector), intent(in) :: this
      capacity = size(this%elements)
   end function capacity


!----------------------------------------------
! Return true if vector is currently size 0.
!----------------------------------------------
   logical function empty(this)
      class (CaseInsensitiveStringLoggerPairVector), intent(in) :: this
      empty = (this%numElements == 0)
   end function empty

!---------------------------------------------------
! Return _reference_ to the ith element of Vector.
!---------------------------------------------------
   function at(this, i) result(ptr)
      class (CaseInsensitiveStringLoggerPairVector), target, intent(in) :: this
      integer, intent(in) :: i
      type(CaseInsensitiveStringLoggerPair), pointer :: ptr

      ptr => this%elements(i)

   end function at

!---------------------------------------------------
! Return reference to 1st element of vector.
!---------------------------------------------------
   function front(this) result(ptr)
      class (CaseInsensitiveStringLoggerPairVector), target, intent(in) :: this
      type(CaseInsensitiveStringLoggerPair), pointer :: ptr

      ptr => this%elements(1)

   end function front


!---------------------------------------------------
! Return reference to last element of vector
!---------------------------------------------------
   function back(this) result(ptr)
      class (CaseInsensitiveStringLoggerPairVector), target, intent(in) :: this
      type(CaseInsensitiveStringLoggerPair), pointer :: ptr

      ptr => this%elements(this%numElements)

   end function back




!-----------------------------------------------------------------
! Return a reference  to the active portion of the internal array.
!-----------------------------------------------------------------
   function data(this) result(d)
      class (CaseInsensitiveStringLoggerPairVector), target :: this
      type(CaseInsensitiveStringLoggerPair), pointer :: d(:)

      d => this%elements(1:this%numElements)

   end function data


!---------------------------------------------------
! Reserve storage for at least n active elements.
! Will not shrink storage below the current number
! of active elements.
!---------------------------------------------------
   subroutine reserve(this, n)
      class (CaseInsensitiveStringLoggerPairVector), intent(inout) :: this
      integer, intent(in) :: n

      type(CaseInsensitiveStringLoggerPair), allocatable :: tmp(:)
      integer :: i, nOld

      if (.not. allocated(this%elements)) then
         allocate(this%elements(0))
      end if

      nOld = size(this%elements)
      if (n > nOld) then
         call move_alloc(from=this%elements, to=tmp)
         allocate(this%elements(n))
         do i = 1, nOld
            this%elements(i) = tmp(i)
         end do
         deallocate(tmp)
      end if

   end subroutine reserve


!---------------------------------------------------
! Resize active elements.  The optional 'value' argument can
! be used to provide an initial value to new elements.  Otherwise
! the values are undefined.   If the new size is smaller than the old
! size, elements are effectively lost.
!---------------------------------------------------
   subroutine resize(this, n, value)
      class (CaseInsensitiveStringLoggerPairVector), intent(inout) :: this
      integer, intent(in) :: n
      type(CaseInsensitiveStringLoggerPair), optional, intent(in) :: value

      integer :: i
      integer :: nOld

      nOld = this%numElements
      this%numElements = n

      if (n < nOld) then

!!         do i= n+1, nOld
!!            (T.free('this%elements(i)'))
!!         end do

      else if (n > nOld) then

         call this%reserve(n)
         do i = nOld+1, n
            if (present(value)) then
               this%elements(i) = value
            else
!TODO: something should be done here
!               $if(T.defaultValue())
!	       $(T.assign('this%elements(i)', T.defaultValue()))
!               $endif
            end if
         end do

      end if

   end subroutine resize


!---------------------------------------------------
!  Set number of elements to 0.  Note that internal 
!  storage may not be released.
!---------------------------------------------------
   subroutine clear(this)
      class (CaseInsensitiveStringLoggerPairVector), intent(inout) :: this
      call this%resize(0)
   end subroutine clear



!---------------------------------------------------
!  Extend vector by one element and set it to <value>.
!---------------------------------------------------
   subroutine push_back_T(this, value)
      class (CaseInsensitiveStringLoggerPairVector), intent(inout) :: this
      type(CaseInsensitiveStringLoggerPair), intent(in) :: value

      integer :: n

      n = this%numElements + 1
      call this%reserve(n)
      
      this%numElements = n
      this%elements(n) = value

   end subroutine push_back_T


!---------------------------------------------------
! Shrink vector by one element from the end.
!---------------------------------------------------
   subroutine pop_back(this)
      class (CaseInsensitiveStringLoggerPairVector), intent(inout) :: this

      integer :: n

      n = this%numElements - 1
      call this%reserve(n)
      
      this%numElements = n

!!      $(T.free('this%elements(n+1)'))

   end subroutine pop_back


!---------------------------------------------------
!  Insert <value> at position i.  Extends vector by one element.
!---------------------------------------------------
   subroutine insert_T(this, i, value)
      class (CaseInsensitiveStringLoggerPairVector), intent(inout) :: this
      integer, intent(in) :: i
      type(CaseInsensitiveStringLoggerPair), intent(in) :: value

      integer :: j, n

      n = this%numElements
      call this%reserve(n+1)

      do j = n, i, -1
         this%elements(j+1) = this%elements(j)
      end do
  
      this%elements(i) = value

      this%numElements = n + 1

   end subroutine insert_T


   
!---------------------------------------------------
! Swap contents of two vectors.
!---------------------------------------------------
   subroutine swapVector(this, a)
      type (CaseInsensitiveStringLoggerPairVector), intent(inout) :: this
      type (CaseInsensitiveStringLoggerPairVector), intent(inout) :: a

      type(CaseInsensitiveStringLoggerPair), allocatable :: tmp(:)
      integer :: nTmp

      ! swap elements
      call move_alloc(from=this%elements, to=tmp)
      call move_alloc(from=a%elements, to=this%elements)
      call move_alloc(from=tmp, to=a%elements)
      
      ! swap metadata
      nTmp = this%numElements
      this%numElements = a%numElements
      a%numElements = nTmp

   end subroutine swapVector


!------------------------------------------------------
!  Construct a forward iterator, initially set to 1st
!  element of vector.
!------------------------------------------------------
   function begin(this) result(iter)
      class (CaseInsensitiveStringLoggerPairVector), target, intent(in) :: this
      type (CaseInsensitiveStringLoggerPairVectorIterator) :: iter
      
      iter%elements => this%elements
      iter%index = 1
      
   end function begin


!------------------------------------------------------
!  Construct  forward iterator, initially set to just
!  after last element of vector.
!------------------------------------------------------
   function end(this) result(iter)
      class (CaseInsensitiveStringLoggerPairVector), target, intent(in) :: this
      type (CaseInsensitiveStringLoggerPairVectorIterator) :: iter
      
      iter%elements => this%elements
      iter%index = this%size() + 1 ! past the end
      
   end function end
   

!------------------------------------------------------
!  Construct a reverse iterator, initially set to last
!  element of vector.
!------------------------------------------------------
   function rbegin(this) result(iter)
      class (CaseInsensitiveStringLoggerPairVector), target, intent(in) :: this
      type (CaseInsensitiveStringLoggerPairVectorReverseIterator) :: iter
      
      iter%elements => this%elements
      iter%index = this%size()
      
   end function rbegin


!------------------------------------------------------
!  Construct a reverse iterator, initially set to just
!  before 1st element of vector.
!------------------------------------------------------
   function rend(this) result(iter)
      class (CaseInsensitiveStringLoggerPairVector), target, intent(in) :: this
      type (CaseInsensitiveStringLoggerPairVectorReverseIterator) :: iter
      
      iter%elements => this%elements
      iter%index = 0
      
   end function rend



!-----------------------------------------------------
! Dereference iterator.
!-----------------------------------------------------
      function get(this) result(ptr)
         type(CaseInsensitiveStringLoggerPair), pointer :: ptr
         class (CaseInsensitiveStringLoggerPairVectorIterator), intent(in) :: this

         ptr => this%elements(this%index)

      end function get
      



      subroutine next(this)
         class (CaseInsensitiveStringLoggerPairVectorIterator), intent(inout) :: this
         this%index = this%index + 1
      end subroutine next


      subroutine previous(this)
         class (CaseInsensitiveStringLoggerPairVectorIterator), intent(inout) :: this
         this%index = this%index - 1
      end subroutine previous


      logical function equalIters(this, other)
         class (CaseInsensitiveStringLoggerPairVectorIterator), intent(in) :: this
         class (CaseInsensitiveStringLoggerPairVectorIterator), intent(in) :: other

         equalIters = (this%index == other%index)
         
      end function equalIters


      logical function notEqualIters(this, other)
         class (CaseInsensitiveStringLoggerPairVectorIterator), intent(in) :: this
         class (CaseInsensitiveStringLoggerPairVectorIterator), intent(in) :: other

         notEqualIters = .not. (this == other)
         
      end function notEqualIters


      ! Illegal to use these unless both arguments reference the
      ! same vector.
      logical function lessThanIter(this, other)
         class (CaseInsensitiveStringLoggerPairVectorIterator), intent(in) :: this
         class (CaseInsensitiveStringLoggerPairVectorIterator), intent(in) :: other
         lessThanIter = (this%index < other%index)
      end function lessThanIter

      logical function lessThanOrEqualIter(this, other)
         class (CaseInsensitiveStringLoggerPairVectorIterator), intent(in) :: this
         class (CaseInsensitiveStringLoggerPairVectorIterator), intent(in) :: other
         lessThanOrEqualIter = (this%index <= other%index)
      end function lessThanOrEqualIter

      logical function greaterThanIter(this, other)
         class (CaseInsensitiveStringLoggerPairVectorIterator), intent(in) :: this
         class (CaseInsensitiveStringLoggerPairVectorIterator), intent(in) :: other
         greaterThanIter = (this%index > other%index)
      end function greaterThanIter

      logical function greaterThanOrEqualIter(this, other)
         class (CaseInsensitiveStringLoggerPairVectorIterator), intent(in) :: this
         class (CaseInsensitiveStringLoggerPairVectorIterator), intent(in) :: other
         greaterThanOrEqualIter = (this%index >= other%index)
      end function greaterThanOrEqualIter


      function atDefault(this) result(ptr)
         type(CaseInsensitiveStringLoggerPair), pointer :: ptr
         class (CaseInsensitiveStringLoggerPairVectorIterator), intent(in) :: this

         ptr => this%elements(this%index)
         
      end function atDefault


      function atOffset(this, i) result(ptr)
         type(CaseInsensitiveStringLoggerPair), pointer :: ptr
         class (CaseInsensitiveStringLoggerPairVectorIterator), intent(in) :: this
         integer, intent(in) :: i

         ptr => this%elements(this%index + i)
         
      end function atOffset

      function add(this, n) result(newIter)
         type (CaseInsensitiveStringLoggerPairVectorIterator) :: newIter
         class (CaseInsensitiveStringLoggerPairVectorIterator), intent(in) :: this
         integer, intent(in) :: n

         newIter%index = this%index + n
         newIter%elements => this%elements

      end function add

      ! Dereference iterator
      function getRIter(this) result(ptr)
         class (CaseInsensitiveStringLoggerPairVectorReverseIterator), intent(in) :: this
         type(CaseInsensitiveStringLoggerPair), pointer :: ptr

         ptr => this%elements(this%index)

      end function getRIter




      subroutine rNext(this)
         class (CaseInsensitiveStringLoggerPairVectorReverseIterator), intent(inout) :: this
         this%index = this%index - 1
      end subroutine rNext


      subroutine rPrevious(this)
         class (CaseInsensitiveStringLoggerPairVectorReverseIterator), intent(inout) :: this
         this%index = this%index + 1
      end subroutine rPrevious


      logical function equalRIters(this, other)
         class (CaseInsensitiveStringLoggerPairVectorReverseIterator), intent(in) :: this
         class (CaseInsensitiveStringLoggerPairVectorReverseIterator), intent(in) :: other

         equalRIters = &
              associated(this%elements, other%elements) .and. &
              (this%index == other%index)
         
      end function equalRIters


      logical function notEqualRIters(this, other)
         class (CaseInsensitiveStringLoggerPairVectorReverseIterator), intent(in) :: this
         class (CaseInsensitiveStringLoggerPairVectorReverseIterator), intent(in) :: other

         notEqualRIters = .not. (this == other)
         
      end function notEqualRIters



      ! Reverse iterator:
      ! Illegal to use these unless both arguments reference the
      ! same vector.
      logical function rLessThanIter(this, other)
         class (CaseInsensitiveStringLoggerPairVectorReverseIterator), intent(in) :: this
         class (CaseInsensitiveStringLoggerPairVectorReverseIterator), intent(in) :: other
         rLessThanIter = (this%index > other%index)
      end function rLessThanIter

      logical function rLessThanOrEqualIter(this, other)
         class (CaseInsensitiveStringLoggerPairVectorReverseIterator), intent(in) :: this
         class (CaseInsensitiveStringLoggerPairVectorReverseIterator), intent(in) :: other
         rLessThanOrEqualIter = (this%index >= other%index)
      end function rLessThanOrEqualIter

      logical function rGreaterThanIter(this, other)
         class (CaseInsensitiveStringLoggerPairVectorReverseIterator), intent(in) :: this
         class (CaseInsensitiveStringLoggerPairVectorReverseIterator), intent(in) :: other
         rGreaterThanIter = (this%index < other%index)
      end function rGreaterThanIter

      logical function rGreaterThanOrEqualIter(this, other)
         class (CaseInsensitiveStringLoggerPairVectorReverseIterator), intent(in) :: this
         class (CaseInsensitiveStringLoggerPairVectorReverseIterator), intent(in) :: other
         rGreaterThanOrEqualIter = (this%index <= other%index)
      end function rGreaterThanOrEqualIter


      function rAtDefault(this) result(ptr)
         type(CaseInsensitiveStringLoggerPair), pointer :: ptr
         class (CaseInsensitiveStringLoggerPairVectorReverseIterator), intent(in) :: this

         ptr => this%elements(this%index)
         
      end function rAtDefault


      function rAtOffset(this, i) result(ptr)
         type(CaseInsensitiveStringLoggerPair), pointer :: ptr
         class (CaseInsensitiveStringLoggerPairVectorReverseIterator), intent(in) :: this
         integer, intent(in) :: i

         ptr => this%elements(this%index - i)
         
      end function rAtOffset

end module FTL_CaseInsensitiveStringLoggerPairVector_mod




