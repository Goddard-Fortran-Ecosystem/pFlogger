module MockDateFormat_mod
   use ASTG_Formatter_mod
   use ASTG_LogRecord_mod
   use ASTG_CIStringUnlimitedMap_mod, only: CIStringUnlimitedMap => Map
   implicit none
   private

   public :: MockDateFormat
   public :: mockdf
   

   type, extends (Formatter) :: MockDateFormat
      ! These components must be public for testing purposes
      integer :: Y
      integer :: M
      integer :: D
      integer :: HH
      integer :: MM
      integer :: SS
      integer :: MS
      type (Formatter) :: f
   contains
      procedure :: fillDateAndTime
   end type MockDateFormat

   type(MockDateFormat) :: mockdf

contains

   
   subroutine fillDateAndTime(this, rec)
      class(MockDateFormat), intent(in) :: this
      type(LogRecord), intent(inout) :: rec
      integer,dimension(8) :: values
      
      call rec%extra%insert('Y', this%Y)
      call rec%extra%insert('M', this%M)
      call rec%extra%insert('D', this%D)
      call rec%extra%insert('HH', this%HH)
      call rec%extra%insert('MM', this%MM)
      call rec%extra%insert('SS', this%SS)
      call rec%extra%insert('MS', this%MS)
      
   end subroutine fillDateAndTime

   
end module MockDateFormat_mod


