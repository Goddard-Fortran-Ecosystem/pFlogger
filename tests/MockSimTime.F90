module MockSimTime_mod
   use PFL_StringUnlimitedMap
   private

   public :: mockSimTime

contains

   subroutine mockSimTime(dict)
      type (Map), intent(out) :: dict

      call dict%insert('Y',1234)
      call dict%insert('M',3)
      call dict%insert('D',21)

   end subroutine mockSimTime

end module MockSimTime_Mod
