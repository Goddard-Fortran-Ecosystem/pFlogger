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

   subroutine emit(this, level, message)
      class(AbstractHandler), intent(in) :: this
      integer, intent(in) :: level
      character(len=*), intent(in) :: message
      
      if (level >= this%getLevel()) then
        call this%emitMessage(levelToString(level), message)
      end if
      
   end subroutine emit
   
   function levelToString(level) result(string)
      character(len=:), allocatable :: string
      integer, intent(in) :: level

      select case (level)
      case (DEBUG)
        string = 'DEBUG'
      case (INFO)
        string = 'INFO'
      case (WARNING)
        string = 'WARNING'
      case (ERROR)
        string = 'ERROR'
      case (CRITICAL)
        string = 'CRITICAL'
      end select
      
   end function levelToString

    
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
