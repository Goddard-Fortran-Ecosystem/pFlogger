module ASTG_Logger_mod
   use ASTG_SeverityLevels_mod
   use ASTG_AbstractHandler_mod
   use FTL_AbstracthandlerPolyWrap_mod
   use FTL_AbstracthandlerPolyWrapVector_mod

   implicit none
   private

   public :: Logger

   type :: Logger
      private
      integer :: level
      type (AbstractHandlerPolyWrapVector) :: handlers
   contains
      procedure :: log
      procedure :: addHandler
      procedure :: removeHandler
      procedure :: getHandlers
      procedure :: setLevel
   end type Logger

   interface Logger
      module procedure newLogger
   end interface Logger

contains

   
   function newLogger() result(alog)
      use ASTG_StreamHandler_mod
      type (Logger) :: alog

! TODO: Need to NOTSET when inheritance is working
      alog%level = INFO
      alog%handlers = AbstractHandlerPolyWrapVector()
      call alog%addHandler(StreamHandler())
      
   end function newLogger

   
   subroutine addHandler(this, handler)
      class (Logger), intent(inout) :: this
      class (AbstractHandler), intent(in) :: handler
      
      call this%handlers%push_back(AbstractHandlerPolyWrap(handler))
      
   end subroutine addHandler


   subroutine removeHandler(this, handler)
      class (Logger), intent(inout) :: this
      class (AbstractHandler), intent(in) :: handler

      call this%handlers%pop_back()
      
   end subroutine removeHandler


   subroutine log(this, level, message)
      class (Logger), intent(inout) :: this
      integer, intent(in) :: level
      character(len=*), intent(in) :: message
      
      type (AbstractHandlerPolyWrapVectorIterator) :: iter
      type (AbstractHandlerPolyWrap), pointer :: handlerWrap
      class (AbstractHandler), pointer :: handler

      iter = this%handlers%begin()
      do while (iter /= this%handlers%end())
         handlerWrap => iter%get()
         handler => handlerWrap%get()
         call handler%emit(level, message)
         call iter%next()
      end do

   end subroutine log


   function getHandlers(this) result(handlers)
      class (Logger), target, intent(in) :: this
      type (AbstractHandlerPolyWrapVector), pointer :: handlers
      
      handlers => this%handlers
      
   end function getHandlers


   subroutine setLevel(this, level)
      class (Logger), intent(inout) :: this
      integer, intent(in) :: level
      this%level = level
   end subroutine setLevel

end module ASTG_Logger_mod


