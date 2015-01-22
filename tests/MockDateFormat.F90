module MockDateFormat_mod
   use ASTG_Formatter_mod
   use ASTG_LogRecord_mod
   use FTL_CIStringXUMap_mod
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
   contains
      procedure :: fillDateAndTime
   end type MockDateFormat

   type(MockDateFormat) :: mockdf

contains

   
   subroutine fillDateAndTime(this, rec)
      class(MockDateFormat), intent(in) :: this
      type(LogRecord), intent(inout) :: rec
      integer,dimension(8) :: values
      type (CIStringXUMapIter) :: iter
      
      iter = rec%extra%emplace('Y', this%Y)
      iter = rec%extra%emplace('M', this%M)
      iter = rec%extra%emplace('D', this%D)
      iter = rec%extra%emplace('HH', this%HH)
      iter = rec%extra%emplace('MM', this%MM)
      iter = rec%extra%emplace('SS', this%SS)
      iter = rec%extra%emplace('MS', this%MS)
      
   end subroutine fillDateAndTime

   
end module MockDateFormat_mod


