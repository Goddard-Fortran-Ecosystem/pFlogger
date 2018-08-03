module ConfigurableFilter_mod
   use PFL_AbstractFilter_mod
   implicit none
   private

   public :: ConfigurableFilter

   type, extends(AbstractFilter) :: ConfigurableFilter
      logical :: filterOn = .false.
   contains
      procedure :: do_filter
      procedure :: equal
   end type ConfigurableFilter

contains


   logical function do_filter(this, record)
      use PFL_LogRecord_mod
      class (ConfigurableFilter), intent(in) :: this
      class (LogRecord), intent(inout) :: record

      do_filter = this%filterOn

   end function do_filter

   logical function equal(a, b)
      use iso_fortran_env, only: IOSTAT_END
      class (ConfigurableFilter), intent(in) :: a
      class (AbstractFilter), intent(in) :: b

      select type (b)
      class is (ConfigurableFilter)
         equal = (a%filterOn .eqv. b%filterOn)
      class default
         equal = .false.
      end select
   end function equal

end module ConfigurableFilter_mod
