module NumberKinds
   implicit none
  
   integer, parameter                                   :: KREAL = kind(0.d0)
   integer, parameter                                   :: KINT  = kind(1)

   character(len=*), parameter                          :: date = __DATE__

end module

module LoggerModule
   use NumberKinds
   !use OutputWriterModule
   implicit none
   
   private
   public Logger, New, Delete, Assignment(=), LogMessage
   public DEBUG_LOGGING_LEVEL, TRACE_LOGGING_LEVEL, WARNING_LOGGING_LEVEL, ERROR_LOGGING_LEVEL
   save
  
   type Logger
      private
      integer(KINT)                                     :: level 
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
   
   !interface OutputProperties  
   !   module procedure OutputPropertiesPrivate
   !end interface

   interface LogMessage        
      module procedure LogMessageReal, LogMessageInteger, LogMessageIntegerArray
      module procedure LogMessageRealArray, LogMessageTextOnly 
   end interface

   ! Definitions
   integer(KINT), parameter                             :: DEBUG_LOGGING_LEVEL   = 1
   integer(KINT), parameter                             :: TRACE_LOGGING_LEVEL   = 2
   integer(KINT), parameter                             :: WARNING_LOGGING_LEVEL = 3 
   integer(KINT), parameter                             :: ERROR_LOGGING_LEVEL   = 4

contains

   ! ------------------------
   ! Standard ADT Methods. Construction, Destruction, Copying, and Assignment.
   ! ------------------------
   subroutine NewPrivate(self,level)
      type (Logger), intent(out)                        :: self
      integer(KINT), intent(in)                         :: level
      self%level = level
   end subroutine

   subroutine NewCopyPrivate(self, other)
      type (Logger), intent(out)                        :: self
      type (Logger), intent(in)                         :: other
      self%level = other%level
   end subroutine
  
   subroutine DeletePrivate(self)
      type (Logger), intent(inout)                      :: self
   end subroutine
  
   subroutine AssignPrivate(self, other)
      type (Logger), intent(inout)                      :: self
      type (Logger), intent(in)                         :: other
      self%level = other%level
   end subroutine
  
   ! ------------------------
   ! Accessors.
   ! ------------------------
   subroutine SetLevel(self, level)
      type (Logger), intent(inout)                      :: self
      integer(KINT), intent(in)                         :: level
      self%level = level
   end subroutine

   function GetLevel(self)
      type (Logger), intent(in)                         :: self
      integer(KINT)                                     :: GetLevel
      GetLevel = self%level
   end function

   ! ------------------------
   ! Other methods.
   ! ------------------------
   subroutine LogMessageTextOnly(self, level, text)
      type (Logger)                                     :: self
      integer(KINT)                                     :: level
      character(len=*)                                  :: text
      if ( level >= self%level ) print *, 'Log: ', text
   end subroutine

   subroutine LogMessageReal(self, level, key, val)
      type (Logger)                                     :: self
      integer(KINT)                                     :: level
      character(len=*)                                  :: key
      real(KREAL)                                       :: val
      if ( level >= self%level ) print *, 'Log: ' // trim(key), val
   end subroutine

   subroutine LogMessageInteger(self, level, key, val)
      type (Logger)                                     :: self
      integer(KINT)                                     :: level
      character(len=*)                                  :: key
      integer(KINT)                                     :: val
      if ( level >= self%level )  print *, 'Log: ' // trim(key), val
   end subroutine

   subroutine LogMessageIntegerArray(self, level, key, vals)
      type (Logger)                                     :: self
      integer(KINT)                                     :: level
      character(len=*)                                  :: key
      integer(KINT)                                     :: vals(:)
      if ( level >= self%level ) then
         print *, 'Log: ' // trim(key)
         print *, vals
      endif
   end subroutine

   subroutine LogMessageRealArray(self, level, key, vals)
      type (Logger)                                     :: self
      integer(KINT)                                     :: level
      character(len=*)                                  :: key
      real(KREAL)                                       :: vals(:)
      if ( level >= self%level ) then
         print *, 'Log: ' // trim(key)
         print *, vals
      endif
   end subroutine
  
   ! ------------------------
   ! Output properties.
   ! ------------------------
   !subroutine OutputPropertiesPrivate(self, writer)
   !   type (Logger), intent(in)                         :: self
   !   type (OutputWriter), intent(inout)                :: writer
   ! 
   !   call Write(writer, 'Logging Level', self%level)
   !
   !end subroutine
  
end module


