module ASTG_AbstractHandler_mod
   implicit none
   private

   public :: AbstractHandler
   public :: NOTSET
   public :: DEBUG
   public :: INFO
   public :: WARNING
   public :: ERROR
   public :: CRITICAL
   
   type, abstract :: AbstractHandler
      private
      integer :: level
   contains
      procedure(emit), deferred :: emit
      procedure(close), deferred :: close
      procedure :: setLevel
      procedure :: getLevel
   end type AbstractHandler

   abstract interface

      subroutine emit(this, level, message)
         import AbstractHandler
         class(AbstractHandler), intent(in) :: this
         integer, intent(in) :: level
         character(len=*), intent(in) :: message
      end subroutine emit


      subroutine close(this)
         import AbstractHandler
         class(AbstractHandler), intent(inout) :: this
      end subroutine close

   end interface

   enum, bind(c)
      enumerator :: &
           & NOTSET   =  0, &
           & DEBUG    = 10, &
           & INFO     = 20, &
           & WARNING  = 30, &
           & ERROR    = 40, &
           & CRITICAL = 50
   end enum

   
 contains

    
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
