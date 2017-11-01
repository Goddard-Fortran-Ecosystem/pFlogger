module ftl_Config_mod
   use PFL_StringUnlimitedPolyMap_mod
   use PFL_Exception_mod
  implicit none
  private

  public :: Config
  public :: ConfigIterator
  public :: Pair
  public :: P ! convenient for constructing configs

  ! All contained types are either Configs or strings

  type, extends(StringUnlimitedPolyMap) :: Config
  contains
     procedure :: toLogical
     procedure :: toInteger
     procedure :: toReal32
     procedure :: toString
     procedure :: toConfigPtr
  end type Config

  interface Config
     module procedure new_Config_pairs
  end interface Config

  type Unusable
  end type Unusable

contains

   
   function p(key, value)
      type (Pair) :: p
      character(len=*), intent(in) :: key
      class (*), intent(in) :: value

      p%key = key
      allocate(p%value, source=value)

   end function p

   ! Convenient constructor for manually testing cgf
   ! use cases.
   function new_Config_pairs(pairs) result(cfg)
      type (Config) :: cfg
      type (Pair), intent(in) :: pairs(:)

      integer :: i
      do i = 1, size(pairs)
         call cfg%insert(pairs(i))
      end do
      
   end function new_Config_pairs


   logical function toLogical(this, key, unused, default, found) result(flag)
      use PFL_String_mod
      class (Config), intent(in) :: this
      character(len=*), intent(in) :: key
      type (Unusable), optional, intent(in) :: unused
      logical, optional, intent(in) :: default
      logical, optional, intent(out) :: found

      class(*), pointer :: ptr
      integer :: iostat

      logical, parameter :: DEFAULT_LOGICAL = .false.
      character(len=:), allocatable :: str

      if (present(unused)) print*,shape(unused)
    
      ptr => this%at(key)
      if (associated(ptr)) then

         select type (ptr)
         type is (character(len=*))
            read(ptr,*,iostat=iostat) flag
            if (iostat /= 0) then
               flag = DEFAULT_LOGICAL
               if (.not. present(found)) then
                  call throw("FTL::Config::toLogical() - failure converting "//key//": '"//ptr//"' to logical.")
               else
                  found = .false.
               end if
            else
               if (present(found)) found = .true.
            end if
         type is (String)
            str = ptr%toString()
            read(str,*,iostat=iostat) flag
            if (iostat /= 0) then
               flag = DEFAULT_LOGICAL
               if (.not. present(found)) then
                  call throw("FTL::Config::toLogical() - failure converting "// &
                       & key//": '"//str//"' to logical.")
               else
                  found = .false.
               end if
            else
               if (present(found)) found = .true.
            end if
         class default
            if (present(found)) found = .true.
            flag = DEFAULT_LOGICAL
            call throw("FTL::Config::toLogical() - type of "//key//" must be logical.")
         end select

      else

         if (present(found)) found = .false.
         if (present(default)) then
            flag = default
         else
            flag = DEFAULT_LOGICAL
            if (.not. present(found)) then
               call throw("FTL::Config::toLogical() - '" // key // "' not found and default not provided.")
            end if
         end if

      end if
      
   end function toLogical



   integer function toInteger(this, key, unused, default, found) result(i)
      use PFL_String_mod
     class (Config), intent(in) :: this
     character(len=*), intent(in) :: key
     type (Unusable), optional, intent(in) :: unused
     integer, optional, intent(in) :: default
     logical, optional, intent(out) :: found
     
     class(*), pointer :: ptr
     integer :: iostat

     integer, parameter :: DEFAULT_INTEGER = -HUGE(1)

     if (present(unused)) print*,shape(unused)
     
     ptr => this%at(key)
     if (associated(ptr)) then
        
        select type (ptr)
#ifdef __GFORTRAN__
        type is (String) ! extract raw string
           i = read_integer(ptr%toString(), found)
#endif
        type is (character(len=*))
           i = read_integer(ptr, found)
        type is (integer)
           i = ptr
           if (present(found)) found = .true.
        class default
           if (present(found)) found = .true.
           i = DEFAULT_INTEGER
           call throw("FTL::Config::toInteger() - type of "//key//" must be integer.")
        end select

     else
        
        if (present(found)) found = .false.
        
        if (present(default)) then
           i = default
        else
           i = DEFAULT_INTEGER
           if (.not. present(found)) then
               call throw("FTL::Config::toInteger() - '" // key // "' not found and default not provided.")
            end if
         end if
      end if

   contains

      integer function read_integer(str, found) result(i)
         character(len=*), intent(in) :: str
         logical, optional, intent(out) :: found
              
         read(str,*,iostat=iostat) i
         if (iostat /= 0) then
            i = DEFAULT_INTEGER
            if (.not. present(found)) then
               call throw("FTL::Config::toInteger() - failure converting " // &
                    & key //": '"//str//"' to integer.")
            else
               found = .false.
            end if
         else
            if (present(found)) found = .true.
         end if
      end function read_integer
      
   end function toInteger



   function toReal32(this, key, unused, default, found) result(x)
      use PFL_String_mod
      use iso_fortran_env, only: REAL32
      use, intrinsic :: IEEE_ARITHMETIC
      real(kind=REAL32) :: x
      class (Config), intent(in) :: this
      character(len=*), intent(in) :: key
      type (Unusable), optional, intent(in) :: unused
      real(kind=REAL32), optional, intent(in) :: default
      logical, optional, intent(out) :: found
      
      class(*), pointer :: ptr
      integer :: iostat

      ! Cannot make this a parameter - IEEE_VALUE cannot be used for init expr.
      real(kind=REAL32) :: DEFAULT_REAL32
      
      if (present(unused)) print*,shape(unused)

      DEFAULT_REAL32 = IEEE_VALUE(DEFAULT_REAL32, IEEE_QUIET_NAN)
      
      ptr => this%at(key)
      if (associated(ptr)) then
        
        select type (ptr)
#ifdef __GFORTRAN__
        type is (String) ! extract raw string
           x = read_real32(ptr%toString(), found)
#endif
        type is (character(len=*))
           x = read_real32(ptr, found)
        type is (real(kind=REAL32))
           x = ptr
           if (present(found)) found = .true.
        class default
           if (present(found)) found = .true.
           x = DEFAULT_REAL32
           call throw("FTL::Config::toReal32() - type of "//key//" must be REAL32.")
        end select

     else
        
        if (present(found)) found = .false.
        
        if (present(default)) then
           x = default
        else
           x = DEFAULT_REAL32
           if (.not. present(found)) then
               call throw("FTL::Config::toReal32() - '" // key // "' not found and default not provided.")
            end if
         end if
      end if

   contains

      function read_real32(str, found) result(x)
         real(kind=REAL32) :: x
         character(len=*), intent(in) :: str
         logical, optional, intent(out) :: found
              
         read(str,*,iostat=iostat) x
         if (iostat /= 0) then
            x = DEFAULT_REAL32
            if (.not. present(found)) then
               call throw("FTL::Config::toREAL32() - failure converting " // &
                    & key //": '"//str//"' to REAL32.")
            else
               found = .false.
            end if
         else
            if (present(found)) found = .true.
         end if
      end function read_real32
      
   end function toReal32



   function toString(this, key, unused, default, found) result(str)
      use PFL_String_mod, only:  String
      
      character(len=:), allocatable :: str
      class (Config), intent(in) :: this
      character(len=*), intent(in) :: key
      type (Unusable), optional, intent(in) :: unused
      character(len=*), optional, intent(in) :: default
      logical, optional, intent(out) :: found
      
      class(*), pointer :: ptr
      character(len=*), parameter :: DEFAULT_STRING = ''

      if (present(unused)) print*,shape(unused)

      ptr => this%at(key)
      if (associated(ptr)) then

         select type (ptr)
         type is (character(len=*))
            str = ptr
            if (present(found)) then
               found = .true.
            end if
         type is (String)
            str = ptr%toString()
            if (present(found)) then
               found = .true.
            end if
         class default
            if (present(found)) then
               found = .true.
            end if
            str = DEFAULT_STRING
            call throw("FTL::Config::toString() - type of '"//key//"' is not string.")
         end select
         
      else
         
         if (present(found)) found = .false.
         
         if (present(default)) then
            str = default
         else
            str = DEFAULT_STRING
            if (.not. present(found)) then
               call throw("FTL::Config::toString() - '" // key // "' not found and default not provided.")
            end if
         end if
      end if
      
   end function toString


  function toConfigPtr(this, key, found) result(cfgPtr)
     type (Config), pointer :: cfgPtr
     class (Config), target, intent(in) :: this
     character(len=*), intent(in) :: key
     logical, optional, intent(out) :: found

    class(*), pointer :: ptr
    
    ptr => this%at(key)
    if (associated(ptr)) then

       select type (ptr)
       type is (Config)
          cfgPtr => ptr
          if (present(found)) found = .true.
       class default
          cfgPtr => null()
          if (present(found)) found = .false.
           call throw("FTL::Config::toConfigPtr() - type of '"//key//"' is not a Config.")
       end select
       
    else

       cfgPtr => null()
       if (present(found)) then
          found = .false.
       else
          call throw("FTL::Config::toConfigPtr() - '" // key // "' not found.")
       end if

    end if

   end function toConfigPtr
  

end module ftl_Config_mod
