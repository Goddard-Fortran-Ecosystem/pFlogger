module ASTG_Logger_mod
   use ASTG_AbstractHandler_mod
   use FTL_AbstracthandlerPolyWrap_mod
   use FTL_AbstracthandlerPolyWrapVector_mod

   implicit none
   private

   public :: Logger
   public :: newLogger

   type :: Logger
      type (AbstractHandlerPolyWrapVector) :: handlers

   contains
      procedure :: log
      procedure :: addHandler
      procedure :: getHandlers
   end type Logger


   interface newLogger
      module procedure newFileLogger
   end interface newLogger


contains


   function newFileLogger(name) result(log)
      use ASTG_FileHandler_mod
      character(len=*), intent(in) :: name
      type (Logger) :: log

      log%handlers = AbstractHandlerPolyWrapVector()
      call log%addHandler(FileHandler(fileName=name))

   end function newFileLogger


   subroutine addHandler(this, handler)
      class (Logger), intent(inout) :: this
      class (AbstractHandler), intent(in) :: handler
      call this%handlers%push_back(AbstractHandlerPolyWrap(handler))
   end subroutine addHandler


   subroutine log(this, message)
      class (Logger), intent(inout) :: this
      character(len=*), intent(in) :: message

      type (AbstractHandlerPolyWrapVectorIterator) :: iter
      type (AbstractHandlerPolyWrap), pointer :: handlerWrap
      class (AbstractHandler), pointer :: handler

      iter = this%handlers%begin()
      do while (iter /= this%handlers%end())
         handlerWrap => iter%get()
         handler => handlerWrap%get()
         call handler%emit(message)
         call iter%next()
      end do

   end subroutine log


   function getHandlers(this) result(handlers)
      class (Logger), target, intent(in) :: this
      type (AbstractHandlerPolyWrapVector), pointer :: handlers
      
      handlers => this%handlers
   end function getHandlers

end module ASTG_Logger_mod


