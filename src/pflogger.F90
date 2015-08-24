module pflogger_mod
   implicit none
   private

   public :: initialize
   public :: finalize

contains

   subroutine initialize()
      use ASTG_SeverityLevels_mod
      call initialize_severity_levels()
   end subroutine initialize

   subroutine finalize()
      use ASTG_SeverityLevels_mod
      call finalize_severity_levels()
   end subroutine finalize

end module pflogger_mod
