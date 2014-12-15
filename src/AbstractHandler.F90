module ASTG_AbstractHandler_mod
   ! Abstract class for handlers. This is a placeholder to define specific
   ! handler interfaces. Instances of this class define how logging events 
   ! are dispatched to specific destinations. 
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
      procedure(flush), deferred :: flush
      procedure :: setLevel
      procedure :: getLevel
   end type AbstractHandler

   abstract interface

      ! This version is intended to be implemented by subclasses
      subroutine emitMessage(this, levelString, message)
         import AbstractHandler
         class (AbstractHandler), intent(in) :: this
         character(len=*), intent(in) :: levelString
         character(len=*), intent(in) :: message
      end subroutine emitMessage

      ! This version is intended to be implemented by subclasses
      subroutine close(this)
         import AbstractHandler
         class(AbstractHandler), intent(inout) :: this
      end subroutine close

      ! This version is intended to be implemented by subclasses
      subroutine flush(this)
         import AbstractHandler
         class(AbstractHandler), intent(inout) :: this
      end subroutine flush

   end interface

   
contains

   
   subroutine emit(this, level, message)
      ! Log a specified message with severity 'level'
      class(AbstractHandler), intent(in) :: this
      integer, intent(in) :: level
      character(len=*), intent(in) :: message
      
      if (level >= this%getLevel()) then
        call this%emitMessage(levelToString(level), message)
      end if
      
   end subroutine emit

   
   subroutine setLevel(this, level)
      ! Set the logging level of this handler
      class (AbstractHandler), intent(inout) :: this
      integer, intent(in) :: level
      
      this%level = level
     
   end subroutine setLevel

   
   integer function getLevel(this)
      ! Get the logging level of this handler
      class (AbstractHandler), intent(in) :: this
      
      getLevel = this%level
      
   end function getLevel
 
end module ASTG_AbstractHandler_mod
