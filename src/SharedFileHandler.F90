! Delegates all operations to wrapped FileHandler.
! A lock is used to ensure atomic emitMessage() actions.

module astg_SharedFileHandler_mod
   use astg_AbstractLock_mod
   use astg_AbstractHandler_mod
   use astg_FileHandler_mod
   implicit none
   private

   public :: SharedFileHandler

   type, extends(FileHandler) :: SharedFileHandler
      class (FileHandler), allocatable :: handler
      class (AbstractLock), allocatable :: lock
   contains
      procedure :: emitMessage
      procedure :: close
      procedure :: open
      procedure :: flush
      procedure :: setFormatter
      procedure :: format
      procedure :: setLevel
      procedure :: getLevel
      procedure :: equal
   end type SharedFileHandler

   interface SharedFileHandler
      module procedure newSharedFileHandler
   end interface SharedFileHandler

contains


   function newSharedFileHandler(handler, lock) result(h)
      type (SharedFileHandler) :: h
      class (FileHandler), intent(in) :: handler
      class (AbstractLock), intent(in) :: lock

      allocate(h%handler, source=handler)
      allocate(h%lock, source=lock)
      call h%close()


   end function newSharedFileHandler

   subroutine emitMessage(this, record)
      use astg_LogRecord_mod
      class (SharedFileHandler), intent(inout) :: this
      type (LogRecord), intent(in) :: record

      include 'mpif.h'
      integer :: rank, ier
      call mpi_comm_rank(MPI_COMM_WORLD, rank, ier)
      call this%lock%acquire()
      call this%handler%emitMessage(record)
      call this%close()
      call this%lock%release()
      
   end subroutine emitMessage


   subroutine close(this)
      class (SharedFileHandler), intent(inout) :: this

      call this%handler%close()

   end subroutine close


   subroutine open(this)
      class (SharedFileHandler), intent(inout) :: this

      call this%handler%open()

   end subroutine open
   

   subroutine flush(this)
      ! Flushes Fortran unit currently open for output.
      class(SharedFileHandler), intent(in) :: this

      call this%handler%flush()

   end subroutine flush


   logical function equal(a, b)
      class (SharedFileHandler), intent(in) :: a
      class (AbstractHandler), intent(in) :: b

      equal = (a%handler == b)

   end function equal


   !---------------------------------------------------------------------------  
   ! FUNCTION: 
   ! format
   !
   ! DESCRIPTION: 
   ! Format a record using specified formatter.
   !---------------------------------------------------------------------------
   function format(this, record) result(message)
      use ASTG_LogRecord_mod
      class(SharedFileHandler), intent(in) :: this
      type(LogRecord), intent(in) :: record
      character(len=:), allocatable :: message

      message = this%handler%format(record)
      
   end function format

   
   !---------------------------------------------------------------------------  
   ! ROUTINE: 
   ! setFormatter
   !
   ! DESCRIPTION: 
   ! Set the formatter for this handler.
   !---------------------------------------------------------------------------
   subroutine setFormatter(this, fmt)
      use ASTG_Formatter_mod
      class (SharedFileHandler), intent(inout) :: this
      class (Formatter) :: fmt

      call this%handler%setFormatter(fmt)

   end subroutine setFormatter

   
   !---------------------------------------------------------------------------  
   ! ROUTINE: 
   ! setLevel
   !
   ! DESCRIPTION: 
   ! Set the logging level of this handler.
   !---------------------------------------------------------------------------  
   subroutine setLevel(this, level)
      class (SharedFileHandler), intent(inout) :: this
      integer, intent(in) :: level
      
      call this%handler%setLevel(level)
     
   end subroutine setLevel

   
   !---------------------------------------------------------------------------  
   ! FUNCTION: 
   ! getLevel
   !
   ! DESCRIPTION: 
   ! Get the logging level of this handler.
   !---------------------------------------------------------------------------  
   integer function getLevel(this)
      class (SharedFileHandler), intent(in) :: this
      
      getLevel = this%handler%getLevel()
      
   end function getLevel


   !---------------------------------------------------------------------------  
   ! FUNCTION: 
   ! getFileName
   !
   ! DESCRIPTION: 
   ! Get the file name associated with this handler.
   !---------------------------------------------------------------------------  
   function getFileName(this) result(fileName)
      class (SharedFileHandler), intent(in) :: this
      character(len=:), allocatable :: fileName

      fileName = this%handler%getFileName()

   end function getFileName


   !---------------------------------------------------------------------------  
   ! ROUTINE: 
   ! setFileName
   !
   ! DESCRIPTION: 
   ! Set the file name associated with this handler.
   !---------------------------------------------------------------------------  
   subroutine setFileName(this, fileName)
      class (SharedFileHandler), intent(inout) :: this
      character(len=*), intent(in) :: fileName

      call this%handler%setFileName(fileName)

   end subroutine setFileName

   

end module astg_SharedFileHandler_mod
