module astg_AbstractLock_mod
   implicit none
   private

   public :: AbstractLock

   type, abstract :: AbstractLock
   contains
      procedure(lock), deferred :: acquire
      procedure(lock), deferred :: release
   end type AbstractLock

   abstract interface
      subroutine lock(this)
         import AbstractLock
         class (AbstractLock), intent(inout) :: this
      end subroutine lock
   end interface

end module astg_AbstractLock_mod
