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
   end type AbstractLock

   abstract interface
      subroutine lock(this)
         import AbstractLock
         class (AbstractLock), intent(inout) :: this
      end subroutine lock
   end interface

end module PFL_AbstractLock
