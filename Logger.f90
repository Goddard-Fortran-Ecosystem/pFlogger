module Logger_mod
   implicit none
   
   private
   public Logger, New, Delete, Assignment(=), LogMessage
   public DEBUG_LOGGING_LEVEL,   &
          TRACE_LOGGING_LEVEL,   &
          WARNING_LOGGING_LEVEL, &
          ERROR_LOGGING_LEVEL
   save
  
   type Logger
      private
      integer                                     :: level 
   end type
   
   ! Overloaded procedure interfaces
   interface New               
      module procedure NewPrivate, NewCopyPrivate
   end interface
      

   interface Delete               
      module procedure DeletePrivate
   end interface
   
   interface Assignment(=)
      module procedure AssignPrivate
   end interface
   
   interface OutputProperties  
      module procedure OutputPropertiesPrivate
   end interface

   interface LogMessage        
      module procedure LogMessageReal, LogMessageInteger, LogMessageIntegerArray
      module procedure LogMessageRealArray, LogMessageTextOnly 
   end interface

   ! Definitions
   integer, parameter                             :: DEBUG_LOGGING_LEVEL   = 1
   integer, parameter                             :: TRACE_LOGGING_LEVEL   = 2
   integer, parameter                             :: WARNING_LOGGING_LEVEL = 3 
   integer, parameter                             :: ERROR_LOGGING_LEVEL   = 4

contains

   ! ------------------------
   ! Standard methods: Construction, Destruction, Copying, and Assignment.
   ! ------------------------
   subroutine NewPrivate(this,level)
      type (Logger), intent(out)                        :: this
      integer, intent(in)                               :: level
      this%level = level
   end subroutine

   subroutine NewCopyPrivate(this, other)
      type (Logger), intent(out)                        :: this
      type (Logger), intent(in)                         :: other
      this%level = other%level
   end subroutine
  
   subroutine DeletePrivate(this)
      type (Logger), intent(inout)                      :: this
   end subroutine
  
   subroutine AssignPrivate(this, other)
      type (Logger), intent(inout)                      :: this
      type (Logger), intent(in)                         :: other
      this%level = other%level
   end subroutine
  
   ! ------------------------
   ! Accessors.
   ! ------------------------
   subroutine SetLevel(this, level)
      type (Logger), intent(inout)                      :: this
      integer, intent(in)                               :: level
      this%level = level
   end subroutine

   function GetLevel(this)
      type (Logger), intent(in)                         :: this
      integer                                           :: GetLevel
      GetLevel = this%level
   end function

   ! ------------------------
   ! Other methods.
   ! ------------------------
   subroutine LogMessageTextOnly(this, level, text)
      type (Logger)                                     :: this
      integer                                           :: level
      character(len=*)                                  :: text
      if ( level >= this%level ) print *, 'Log: ', text
   end subroutine

   subroutine LogMessageReal(this, level, key, val)
      type (Logger)                                     :: this
      integer                                           :: level
      character(len=*)                                  :: key
      real                                              :: val
      if ( level >= this%level ) print *, 'Log: ' // trim(key), val
   end subroutine

   subroutine LogMessageInteger(this, level, key, val)
      type (Logger)                                     :: this
      integer                                           :: level
      character(len=*)                                  :: key
      integer                                           :: val
      if ( level >= this%level )  print *, 'Log: ' // trim(key), val
   end subroutine

   subroutine LogMessageIntegerArray(this, level, key, vals)
      type (Logger)                                     :: this
      integer                                           :: level
      character(len=*)                                  :: key
      integer                                           :: vals(:)
      if ( level >= this%level ) then
         print *, 'Log: ' // trim(key)
         print *, vals
      endif
   end subroutine

   subroutine LogMessageRealArray(this, level, key, vals)
      type (Logger)                                     :: this
      integer                                           :: level
      character(len=*)                                  :: key
      real                                              :: vals(:)
      if ( level >= this%level ) then
         print *, 'Log: ' // trim(key)
         print *, vals
      endif
   end subroutine
    
end module Logger_mod


