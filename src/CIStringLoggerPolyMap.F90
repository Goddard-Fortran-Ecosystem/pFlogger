module  CIStringLoggerPolyMap_mod
   use ASTG_AbstractLogger_mod
#define _key_string_deferred
#define _key_equal_defined
#define _KEY_LESS_THAN(x,y) caseInsensitiveLessThan(x,y)

#define _value class(AbstractLogger)
#define _value_polymorphic
#define _value_equal_defined


#include "templates/map.inc"

   logical function caseInsensitiveLessThan(x,y) result(less)
      character(len=*), intent(in) :: x
      character(len=*), intent(in) :: y

      integer :: i
      character(1) :: cx, cy

      integer, parameter :: UPPER_LOWER_DELTA = iachar('A') - iachar('a')
      
      do i = 1, min(len(x),len(y))
         cx = x(i:i)
         cy = y(i:i)

         if (cx >= 'A' .and. cx <= 'Z') then
            cx = achar(iachar(cx) - UPPER_LOWER_DELTA)
         end if

         if (cy >= 'A' .and. cy <= 'Z') then
            cy = achar(iachar(cy) - UPPER_LOWER_DELTA)
         end if

         less = (cx < cy)
         if (cx /= cy) then
            return
         end if

      end do

      less = (len(x) < len(y))

   end function caseInsensitiveLessThan

end module CIStringLoggerPolyMap_mod

