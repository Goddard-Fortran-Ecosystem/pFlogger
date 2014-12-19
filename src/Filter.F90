module ASTG_Filter_mod
   use ASTG_LogRecord_mod
   implicit none
   private

   public :: Filter

   type Filter
      private
      character(len=:), allocatable :: name
   contains
      procedure :: filter => filter_ ! name conflict
   end type Filter

   interface Filter
      module procedure newFilter
   end interface Filter


contains


   function newFilter(name) result(f)
      type (Filter) :: f
      character(len=*), intent(in) :: name
      f%name = name
   end function newFilter


   logical function filter_(this, record)
      class (Filter), intent(in) :: this
      class (LogRecord), intent(inout) :: record

      character(len=:), allocatable :: recordName

      recordName = record%getName()
      if (this%name == recordName(1:len(this%name))) then
         filter_ = .true.  ! do emit
      else
         filter_ = .false. ! do NOT emit
      end if

   end function filter_

end module ASTG_Filter_mod
