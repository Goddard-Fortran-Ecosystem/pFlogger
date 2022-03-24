module MockDateFormat_mod
   use PFL_Formatter
   use PFL_LogRecord
   use gFTL2_StringUnlimitedMap
   implicit none
   private

   public :: MockDateFormat
   public :: mockdf
   

   type, extends (Formatter) :: MockDateFormat
      ! These components must be public for testing purposes
      type (Formatter) :: f

      integer :: Y = 0
      integer :: M = 0
      integer :: D = 0
      integer :: HH = 0
      integer :: MM = 0
      integer :: SS = 0
      integer :: MS = 0
   contains
      procedure :: fillDateAndTime
   end type MockDateFormat

   type(MockDateFormat) :: mockdf

contains

   
   subroutine fillDateAndTime(this, rec)
      class(MockDateFormat), intent(in) :: this
      type(LogRecord), intent(inout) :: rec
      integer,dimension(8) :: values

      rec%time_fields(1) = this%Y
      rec%time_fields(2) = this%M
      rec%time_fields(3) = this%D
      rec%time_fields(5) = this%HH
      rec%time_fields(6) = this%MM
      rec%time_fields(7) = this%SS
      rec%time_fields(8) = this%MS

   end subroutine fillDateAndTime

   
end module MockDateFormat_Mod


