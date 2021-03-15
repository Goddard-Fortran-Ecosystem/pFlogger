module PFL_AbstractLock
   implicit none
   private

   public :: AbstractLock

   type, abstract :: AbstractLock
   contains
      procedure(lock), deferred :: acquire
      procedure(lock), deferred :: release
      procedure(lock), deferred :: init
      procedure(lock), deferred :: destroy
      procedure(is_initialized), deferred :: is_initialized
   end type AbstractLock

   abstract interface
      subroutine lock(this, rc)
         import AbstractLock
         class (AbstractLock), intent(inout) :: this
         integer, optional, intent(out) :: rc
      end subroutine lock

      function is_initialized(this) result(init)
         import AbstractLock
         class (AbstractLock), intent(in) :: this
         logical :: init
      end function
   end interface
end module PFL_AbstractLock
