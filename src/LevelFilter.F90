module PFL_LevelFilter
   use PFL_AbstractFilter
   use PFL_LogRecord
   implicit none
   private

   public :: LevelFilter

   type, extends(AbstractFilter) :: LevelFilter
      private
      integer :: min_level
      integer :: max_level
   contains
      procedure :: do_filter
      procedure :: equal
   end type LevelFilter


   interface LevelFilter
      module procedure new_LevelFilter
    end interface LevelFilter


contains
   

   function new_LevelFilter(min_level, max_level) result(filter)
      type (LevelFilter) :: filter
      integer, intent(in) :: min_level
      integer, intent(in) :: max_level

      filter%min_level = min_level
      filter%max_level = max_level

   end function new_LevelFilter


   !---------------------------------------------------------------------------  
   ! FUNCTION: 
   ! do_filter
   !
   ! DESCRIPTION: 
   ! Determine if LogRecord can be logged.
   ! Is the specified record to be logged? Returns FALSE for no, TRUE for
   ! yes.
   !---------------------------------------------------------------------------
   logical function do_filter(this, record)
      class (LevelFilter), intent(in) :: this
      class (LogRecord), intent(in) :: record

      integer :: n

      integer :: level

      level = record%get_level()
      
      if (this%min_level <= level .and. level <= this%max_level) then
         do_filter = .true.  ! do emit
      else
         do_filter = .false.
      end if

      
   end function do_filter
   

   !---------------------------------------------------------------------------  
   ! FUNCTION: 
   ! equal
   !
   ! DESCRIPTION: 
   ! Overloads 'equal' operation for Levelfilters.
   !---------------------------------------------------------------------------  
   logical function equal(a, b)
      class(LevelFilter), intent(in) :: a
      class(AbstractFilter), intent(in) :: b

      select type (b)
      type is (LevelFilter)
         equal = (a%min_level == b%min_level) .and. (a%max_level == b%max_level)
      class default
         equal = .false.
      end select

   end function equal

   
end module PFL_LevelFilter

