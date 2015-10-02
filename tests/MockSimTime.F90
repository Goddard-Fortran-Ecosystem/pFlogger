module MockSimTime_mod
   use ASTG_StringUnlimitedMap_mod
   private

   public :: mockSimTime

contains

   subroutine mockSimTime(dict)
      type (Map), intent(out) :: dict

      call dict%insert('Y',1234)
      call dict%insert('M',3)
      call dict%insert('D',21)

   end subroutine mockSimTime

end module MockSimTime_mod
