!------------------------------------------------------------------------------
! NASA/GSFC, CISTO, Code 606, Advanced Software Technology Group
!------------------------------------------------------------------------------
!
!*MODULE: PFL_StringHandlerMap
!
!> @brief Case insensitive abstract logger map.
!
!> @author ASTG staff
!> @date 01 Jan 2015 - Initial Version
!------------------------------------------------------------------------------
module  PFL_StringHandlerMap
   use PFL_AbstractHandler
#define _map HandlerMap
#define _pair HandlerPair
#define _iterator HandlerIterator
   
#include "types/key_deferredLengthString.inc"

#define _value class(AbstractHandler)
#define _value_allocatable

#define _ASSIGN(dest,src) allocate(dest%key,source=src%key); if(allocated(src%value)) allocate(dest%value,source=src%value)
#define _MOVE(dest,src) call move_alloc(from=src%key,to=dest%key); if (allocated(src%value)) call move_alloc(from=src%value,to=dest%value)
#define _FREE(x) deallocate(x%key,x%value)

#define _alt
#include "templates/map.inc"

end module PFL_StringHandlerMap

