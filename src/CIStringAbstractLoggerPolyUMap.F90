

#define PAIR  CIStringAbstractLoggerPolyPair
#define BUCKET  CIStringAbstractLoggerPolyPairVec
#define BUCKET_VECTOR CIStringAbstractLoggerPolyPairVecVec
#define BUCKET_ITERATOR CIStringAbstractLoggerPolyPairVecIter
#define BUCKET_VECTOR_ITERATOR CIStringAbstractLoggerPolyPairVecVecIter
#define ITERATOR CIStringAbstractLoggerPolyUMapIter
#define UNORDERED_MAP CIStringAbstractLoggerPolyUMap

module FTL_CIStringAbstractLoggerPolyUMap_mod
   use FTL_CIStringAbstractLoggerPolyPair_mod, only: CIStringAbstractLoggerPolyPair
   use FTL_CIStringAbstractLoggerPolyPairVec_mod, only: CIStringAbstractLoggerPolyPairVec
   use FTL_CIStringAbstractLoggerPolyPairVec_mod, only: CIStringAbstractLoggerPolyPairVecIter
   use FTL_CIStringAbstractLoggerPolyPairVec_mod, only: CIStringAbstractLoggerPolyPairVecReverseIter
   use FTL_CIStringAbstractLoggerPolyPairVec_mod, only: swap 

   use FTL_CIStringAbstractLoggerPolyPairVecVec_mod, only: CIStringAbstractLoggerPolyPairVecVec
   use FTL_CIStringAbstractLoggerPolyPairVecVec_mod, only: CIStringAbstractLoggerPolyPairVecVecIter
   use FTL_CIStringAbstractLoggerPolyPairVecVec_mod, only: CIStringAbstractLoggerPolyPairVecVecReverseIter
   use FTL_CIStringAbstractLoggerPolyPairVecVec_mod, only: swap 

   use FTL_HashFunction_mod
   use FTL_Exception_mod

      use FTL_CIString_mod
      
   
   
   
      use ASTG_AbstractLogger_mod, only: AbstractLogger
   
   implicit none

   private

   public :: CIStringAbstractLoggerPolyUMap
   public :: ITERATOR
   public :: MAX_BUCKET_COUNT

   ! Not sure what a good value for this is
   integer, parameter :: MAX_BUCKET_COUNT = 10000
   
   type :: UNORDERED_MAP
      private
      type (BUCKET_VECTOR) :: bucketVector
      integer :: size_
      real :: maxLoadFactor = 1.0
   contains
      procedure :: get_max_load_factor
      procedure :: get_load_factor
      procedure :: set_max_load_factor
      procedure :: get_bucket_count

      procedure :: rehash
      procedure :: reserve

      procedure, nopass :: get_max_bucket_count
      procedure :: size
      procedure :: insert

      procedure :: emplace_keyValue
      procedure :: emplace_pair
      generic :: emplace => emplace_keyValue, emplace_pair
      procedure :: find_K
      generic :: find => find_K
      procedure :: count_K
      generic :: count => count_K
      procedure :: emplace_keyValue_alt
      generic :: emplace => emplace_keyValue_alt
      procedure :: find_alt
      generic :: find => find_alt
      procedure :: count_alt
      generic :: count => count_alt

      procedure :: empty
      procedure :: at_K
      generic :: at => at_K
      procedure :: at_alt
      generic :: at => at_alt

#if defined(__INTEL_COMPILER) | defined(__GFORTRAN__)
      procedure :: copy
      generic :: assignment(=) => copy
#endif
      procedure :: begin
      procedure :: end
   end type UNORDERED_MAP

   interface UNORDERED_MAP
      module procedure newMap_default
      module procedure newMap_numBuckets
   end interface UNORDERED_MAP

   integer, parameter :: DEFAULT_NUM_BUCKETS = 100

   type ITERATOR
      private
      type (BUCKET_VECTOR), pointer :: bucketVector => null()
      type (BUCKET_VECTOR_ITERATOR) :: bucketIterator
      type (BUCKET_ITERATOR) :: subIterator
   contains
      procedure :: next
      procedure :: get
      procedure :: first
      procedure :: second
      procedure :: equalIter
      generic :: operator(==) => equalIter
      procedure :: notEqualIter
      generic :: operator(/=) => notEqualIter
   end type ITERATOR


contains


   function newMap_default() result(map)
      type (UNORDERED_MAP) :: map

      map = UNORDERED_MAP(DEFAULT_NUM_BUCKETS)

   end function newMap_default


   function newMap_numBuckets(numBuckets) result(map)
      type (UNORDERED_MAP) :: map
      integer, intent(in) :: numBuckets

      integer :: i

      map%bucketVector = BUCKET_VECTOR()
      call map%bucketVector%resize(numBuckets, BUCKET())

      map%size_ = 0

   end function newMap_numBuckets

#if defined(__INTEL_COMPILER) | defined(__GFORTRAN__)
    subroutine copy(a, b)
       class (UNORDERED_MAP), intent(inout) :: a
       type (UNORDERED_MAP), intent(in) :: b

       a%bucketVector = b%bucketVector
       a%size_ = b%size_
    end subroutine copy
#endif

   integer function size(this)
      class (CIStringAbstractLoggerPolyUMap), intent(in) :: this
      
      size = this%size_
   end function size


   recursive function insert(this, value) result(iter)
      type (ITERATOR) :: iter
      class (UNORDERED_MAP), target, intent(inout) :: this
      type (PAIR), intent(in) :: value

      class (BUCKET), pointer :: ptr
      class (PAIR), pointer :: item
      type (BUCKET_ITERATOR) :: bIter
      integer :: idx

      idx = 1 + mod(hashFunction(value%first), this%bucketVector%size())
      ptr => this%bucketVector%at(idx)

      bIter = ptr%begin()
      do while (bIter /= ptr%end())
         item => bIter%at()

         if (item%first == value%first) then
            call throwDuplicateKey(value%first)
            return
         end if
         call bIter%next()

      end do

      call ptr%push_back(value)
      this%size_ = this%size_ + 1

      if (this%get_load_factor() > this%get_max_load_factor()) then
         call this%rehash(ceiling( this%size_ / this%get_max_load_factor()))
      end if

      idx = 1 + mod(hashFunction(value%first), this%bucketVector%size())
      ptr => this%bucketVector%at(idx)

      bIter = ptr%end()
      call bIter%previous()

      iter%bucketVector => this%bucketVector
      ! TODO: C convention for index would much improve the use of the "+" operator
      iter%bucketIterator = this%bucketVector%begin() + (idx - 1)
      iter%subIterator = bIter

   end function insert


   ! Note this implementation does not do construction in place.
   ! Possibly not possible with Fortran?
   function emplace_keyValue(this, key, value ) result(iter)
      type (ITERATOR) :: iter
      class (UNORDERED_MAP), intent(inout) :: this
      type(CIString), intent(in) :: key
      class(AbstractLogger), intent(in) :: value

      ! If implementation is altered to not use insert() directly,
      ! then a check must be added for load factor.
      iter = this%insert(PAIR(key,value))

   end function emplace_keyValue


   ! Note this implementation does not do construction in place.
   ! Possibly not possible with Fortran?
   function emplace_pair(this, p) result(iter)
      type (ITERATOR) :: iter
      class (UNORDERED_MAP), intent(inout) :: this
      type (PAIR), intent(in) :: p

      ! If implementation is altered to not use insert() directly,
      ! then a check must be added for load factor.
      iter = this%insert(p)

   end function emplace_pair

   
   function find_K(this, key) result(iter)
      type (ITERATOR) :: iter
      class (UNORDERED_MAP), target, intent(in) :: this
      type(CIString), intent(in) :: key

      class (BUCKET), pointer :: ptr
      class (PAIR), pointer :: item
      type (BUCKET_ITERATOR) :: bIter
      integer :: idx

      idx = 1 + mod(hashFunction(key), this%bucketVector%size())
      ptr => this%bucketVector%at(idx)

      bIter = ptr%begin()

      do while (bIter /= ptr%end())

         item => bIter%at()

         if (item%first == key) then
            iter%bucketVector => null()
            iter%bucketVector => this%bucketVector
            iter%bucketIterator = this%bucketVector%begin() + (idx - 1)
            iter%subIterator = bIter
            return
         end if

         call bIter%next()

      end do

      iter = this%end()

   end function find_K

   ! Note this implementation does not do construction in place.
   ! Possibly not possible with Fortran?
   function emplace_keyValue_alt(this, key, value ) result(iter)
      type (ITERATOR) :: iter
      class (UNORDERED_MAP), intent(inout) :: this
      character(len=*), intent(in) :: key
      class(AbstractLogger), intent(in) :: value

      ! If implementation is altered to not use insert() directly,
      ! then a check must be added for load factor.
      iter = this%emplace(PAIR(CIString(key),value))

   end function emplace_keyValue_alt


   function find_alt(this, key) result(iter)
      type (ITERATOR) :: iter
      class (UNORDERED_MAP), target, intent(in) :: this
      character(len=*), intent(in) :: key

      iter = this%find(CIString(key))

   end function find_alt


   logical function empty(this)
      class (UNORDERED_MAP), intent(in) :: this

      empty = (this%size_ == 0)
   end function empty


   function at_K(this, key) result(ptr)
      class(AbstractLogger), pointer :: ptr
      class (UNORDERED_MAP), intent(in), target :: this
      type(CIString), intent(in) :: key

      class (BUCKET), pointer :: pBucket
      class (PAIR), pointer :: item
      type (BUCKET_ITERATOR) :: iter
      integer :: idx

      idx = 1 + mod(hashFunction(key), this%bucketVector%size())
      pBucket => this%bucketVector%at(idx)

      iter = pBucket%begin()

      do while (iter /= pBucket%end())

         item => iter%at()

         if (item%first == key) then
            ptr => item%second
            return
         end if

         call iter%next()

      end do

      ptr => null()
      
   end function at_K


   integer function count_K(this, key)
      class (UNORDERED_MAP), intent(in) :: this
      type(CIString), intent(in) :: key

      type (ITERATOR),target :: iter
      type(CIString), pointer :: foundKey

      iter = this%begin()

      do while (iter /= this%end())
         foundKey => iter%first()
         if (foundKey == key) then
            count_K = 1
            ! There can only be 1 such key, so we are done.
            return
         end if
         call iter%next()
      end do
      count_K = 0

   end function count_K



   function at_alt(this, key) result(ptr)
      class(AbstractLogger), pointer :: ptr
      class (UNORDERED_MAP), intent(in), target :: this
      character(len=*), intent(in) :: key

      ptr => this%at(CIString(key))
   end function at_alt


   integer function count_alt(this, key)
      class (UNORDERED_MAP), intent(in) :: this
      character(len=*), intent(in) :: key

      count_alt = this%count(CIString(key))

   end function count_alt




   subroutine set_max_load_factor(this, maxLoadFactor)
      class (UNORDERED_MAP), intent(inout) :: this
      real, intent(in) :: maxLoadFactor
      this%maxLoadFactor = maxLoadFactor
   end subroutine set_max_load_factor


   real function get_max_load_factor(this)
      class (UNORDERED_MAP), intent(in) :: this
      get_max_load_factor = this%maxLoadFactor
   end function get_max_load_factor


   real function get_load_factor(this)
      class (UNORDERED_MAP), intent(in) :: this
      get_load_factor = real(this%size()) / this%get_bucket_count()
   end function get_load_factor


   ! static
   integer function get_max_bucket_count()
      get_max_bucket_count = MAX_BUCKET_COUNT
   end function get_max_bucket_count


   integer function get_bucket_count(this)
      class (UNORDERED_MAP), intent(in) :: this
      get_bucket_count = this%bucketVector%size()
   end function get_bucket_count


   subroutine rehash(this, numBuckets)
      class (UNORDERED_MAP), intent(inout) :: this
      integer, intent(in) :: numBuckets

      type (UNORDERED_MAP) :: tmp
      type (ITERATOR) :: iter, tIter
      type (PAIR), pointer :: pair

      ! Only rehash if we need things bigger
      if (numBuckets <= this%get_bucket_count()) return

      tmp = this

      call this%bucketVector%clear()
      this%size_ = 0
      call this%bucketVector%resize(numBuckets, BUCKET())

      iter = tmp%begin()
      do while (iter /= tmp%end())
         pair => iter%get()
         tIter = this%insert(iter%get())
         call iter%next()
      end do
      
   end subroutine rehash

   subroutine reserve(this, numItems)
      class (UNORDERED_MAP), intent(inout) :: this
      integer, intent(in) :: numItems

      integer :: minBuckets

      minBuckets = ceiling(numItems / this%get_max_load_factor())
      
      ! Although rehash() does the same check, it is not required there.

      if (minBuckets > this%get_max_bucket_count()) then
         call throw('exceeded max bucket count for UnorderedMap')
      end if

      if (minBuckets > this%get_bucket_count()) then
         call this%rehash(minBuckets)
      end if

   end subroutine reserve

   
   function begin(this) result(iterator)
      type (ITERATOR) :: iterator
      class (UNORDERED_MAP), target, intent(in) :: this

      type (BUCKET), pointer :: bucket

      iterator%bucketVector => this%bucketVector
      iterator%bucketIterator = this%bucketVector%begin()

      ! First make certain we are NOT on an empty bucket.
      ! If there are no empty buckets, then begin = end.

      bucket => iterator%bucketIterator%get()
      do while (bucket%empty())
         call iterator%bucketIterator%next()
         if (iterator%bucketIterator == this%bucketVector%end()) then
            iterator = this%end()
            return
         end if
         bucket => iterator%bucketIterator%get()
      end do

      iterator%subIterator = bucket%begin()

   end function begin


   function end(this) result(iterator)
      type (ITERATOR) :: iterator
      class (UNORDERED_MAP), target, intent(in) :: this

      type (BUCKET), pointer :: bucket

      iterator%bucketVector => this%bucketVector
      iterator%bucketIterator = this%bucketVector%end()

      bucket => iterator%bucketVector%back()
      iterator%subIterator = bucket%end()

   end function end


   function get(this) result(pair)
      type (PAIR), pointer :: pair
      class (ITERATOR), intent(in) :: this

      pair => this%subIterator%get()
      
   end function get


   function first(this)
      class (ITERATOR), intent(in) :: this
      type(CIString), pointer :: first

      type (PAIR), pointer :: pair

      pair => this%get()
      first => pair%first
      
   end function first

   function second(this)
      class (ITERATOR), intent(in) :: this
      class(AbstractLogger), pointer :: second

      type (PAIR), pointer :: pair
      
      pair => this%get()
      second => pair%second
      
   end function second

   subroutine throwDuplicateKey(key)
      use FTL_StringUtilities_mod
      type(CIString), intent(in) :: key

      call throw('Duplicate (case insensitive) key in UnorderedMap insert for key<' // &
           & toString(key) // '>')

   end subroutine throwDuplicateKey


   logical function equalIter(this, other)
      class (ITERATOR), intent(in) :: this
      type (ITERATOR), intent(in) :: other

      equalIter = (this%bucketIterator == other%bucketIterator)
      if (.not. equalIter) return
      if (this%bucketIterator == this%bucketVector%end()) return

      equalIter = (this%subIterator == other%subIterator)

   end function equalIter


   logical function notEqualIter(this, other)
      class (ITERATOR), intent(in) :: this
      type (ITERATOR), intent(in) :: other

      notEqualIter = .not. (this == other)

   end function notEqualIter

   
   subroutine next(this)
      class (ITERATOR), intent(inout) :: this

      type (BUCKET), pointer :: bucket

      call this%subIterator%next()
      bucket => this%bucketIterator%get()

      if (this%subIterator == bucket%end()) then
         call this%bucketIterator%next()

         if (this%bucketIterator == this%bucketVector%end()) then
            return
         end if

         bucket => this%bucketIterator%get()
         do while (bucket%empty())
            call this%bucketIterator%next()
            if (this%bucketIterator == this%bucketVector%end()) then
               this%subIterator = bucket%end()
               return
            end if
            bucket => this%bucketIterator%get()
         end do

         this%subIterator = bucket%begin()
      end if

   end subroutine next

end module FTL_CIStringAbstractLoggerPolyUMap_mod
