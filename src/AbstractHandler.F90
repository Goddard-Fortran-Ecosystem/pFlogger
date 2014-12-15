module ASTG_AbstractHandler_mod
   use ASTG_SeverityLevels_mod, only: levelToString
   implicit none
   private

   public :: AbstractHandler
   type, abstract :: AbstractHandler
      private
      integer :: level
   contains
      procedure(emitMessage), deferred :: emitMessage
      procedure :: emit
      procedure(close), deferred :: close
      procedure :: setLevel
      procedure :: getLevel
   end type AbstractHandler

   abstract interface

      subroutine emitMessage(this, levelString, message)
         import AbstractHandler
         class (AbstractHandler), intent(in) :: this
         character(len=*), intent(in) :: levelString
         character(len=*), intent(in) :: message
      end subroutine emitMessage

      subroutine close(this)
         import AbstractHandler
         class(AbstractHandler), intent(inout) :: this
      end subroutine close

   end interface

   
contains

   subroutine emit(this, level, message)
      class(AbstractHandler), intent(in) :: this
      integer, intent(in) :: level
      character(len=*), intent(in) :: message
      
      if (level >= this%getLevel()) then
        call this%emitMessage(levelToString(level), message)
      end if
      
   end subroutine emit
   
   subroutine setLevel(this, level)
      class (AbstractHandler), intent(inout) :: this
      integer, intent(in) :: level
      
      this%level = level
     
   end subroutine setLevel

   
   integer function getLevel(this)
      class (AbstractHandler), intent(in) :: this
      
      getLevel = this%level
      
   end function getLevel
 
end module ASTG_AbstractHandler_mod
