module ASTG_AbstractLogger_mod
   implicit none
   private

   public :: AbstractLogger
   
   type, abstract :: AbstractLogger
      private
      logical :: isOpen_ = .false.
      integer :: unit
      character(len=:), allocatable :: fileName
   contains
      procedure :: setUnit
      procedure :: getUnit
      procedure :: isOpen
      procedure :: open
      procedure :: close
      procedure :: setFileName
      procedure :: getFileName

      procedure(toString_integer), deferred :: toString_integer
      generic :: toString => toString_integer

      procedure :: log_integer
      generic :: log => log_integer
   end type AbstractLogger

   abstract interface

      function toString_integer(this, i) result(string)
         import AbstractLogger
         character(len=:), allocatable :: string
         class (AbstractLogger), intent(inout) :: this
         integer, intent(in) :: i

      end function toString_integer

   end interface


contains


   subroutine log_integer(this, i)
      class (AbstractLogger), intent(inout) :: this
      integer, intent(in) :: i
      
      if (.not. this%isOpen()) call this%open()
      write(this%unit,'(a)') this%toString(i)
      call flush(this%getUnit())

   end subroutine log_integer


   logical function isOpen(this)
      class (AbstractLogger), intent(in) :: this
      isOpen = .false.
   end function isOpen

   subroutine open(this)
      class (AbstractLogger), intent(inout) :: this

      integer :: unit

      if (this%isOpen()) return

      open(newunit=unit, file=this%getFileName(), &
           & status='unknown', form='formatted', access='append')

      call this%setUnit(unit)
      this%isOpen_ = .true.
         
   end subroutine open


   subroutine close(this)
      class (AbstractLogger), intent(inout) :: this

      close(this%getUnit())
      this%isOpen_ = .false.

   end subroutine close


   subroutine setUnit(this, unit)
      class (AbstractLogger), intent(inout) :: this
      integer, intent(in) :: unit

      this%unit = unit

   end subroutine setUnit
   

   integer function getUnit(this) result(unit)
      class (AbstractLogger), intent(in) :: this
      unit = this%unit
   end function getUnit


   function getFileName(this) result(fileName)
      class (AbstractLogger), intent(in) :: this
      character(len=:), allocatable :: fileName

      fileName = this%fileName
   end function getFileName

   subroutine setFileName(this, fileName)
      class (AbstractLogger), intent(inout) :: this
      character(len=*), intent(in) :: fileName

      this%fileName = fileName

   end subroutine setFileName

end module ASTG_AbstractLogger_mod
      
