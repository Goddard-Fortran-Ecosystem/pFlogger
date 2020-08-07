#include "error_handling_macros.fh"
!------------------------------------------------------------------------------
! NASA/GSFC, CISTO, Code 606, Advanced Software Technology Group
!------------------------------------------------------------------------------
!
!*MODULE: PFL_FileHandler
!
!> @brief A handler class which writes logging events to disk files
!
!> @author ASTG staff
!> @date 01 Jan 2015 - Initial Version
!------------------------------------------------------------------------------
module PFL_FileHandler
   use PFL_StreamHandler, only: StreamHandler
   use PFL_AbstractLock, only: AbstractLock
   use PFL_LogRecord, only: LogRecord
   use PFL_KeywordEnforcer
   use PFL_Exception
   implicit none
   private

   public :: FileHandler

   type, extends(StreamHandler) :: FileHandler
      private
      logical :: is_open_ = .false.
      character(len=:), allocatable :: file_name
      class (AbstractLock), allocatable :: lock
   contains
      procedure :: is_open
      procedure :: open
      procedure :: close
      procedure :: set_file_name
      procedure :: get_file_name
      procedure :: emit_message
      procedure :: atomic_emit_message
      procedure :: equal
      procedure :: set_lock
      procedure :: is_lockable
      procedure :: free
   end type FileHandler

   interface FileHandler
      module procedure newFileHandler
   end interface

contains

    
   !---------------------------------------------------------------------------  
   ! FUNCTION: 
   ! newFileHandler
   !
   ! DESCRIPTION: 
   ! Instantiate a file handler with a given file name. Optionally
   ! set a level and a delay. If a delay is set to true then we
   ! don't open the stream.
   !---------------------------------------------------------------------------
   function newFileHandler(file_name, unusable, delay) result(handler)
      type (FileHandler) :: handler
      character(len=*), intent(in) :: file_name
      class (KeywordEnforcer), optional, intent(in) :: unusable
      logical, optional, intent(in) :: delay

      logical :: delay_
      logical :: opened
      
      if (present(delay)) then
         delay_ = delay
      else
         delay_ = .false. ! backward compatibility
      end if

      call handler%set_file_name(file_name)

      if (.not. delay_) call handler%open()

   end function newFileHandler

    
   !---------------------------------------------------------------------------  
   ! ROUTINE: 
   ! emit_message
   !
   ! DESCRIPTION: 
   ! Write a formatted string to a file.
   !---------------------------------------------------------------------------  
   subroutine emit_message(this, record, unusable, rc)
      class (FileHandler), intent(inout) :: this
      type(LogRecord), intent(in) :: record
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status
      if (this%is_lockable()) then
         call this%lock%acquire()
         call this%open()
      end if

      call this%atomic_emit_message(record, rc=status)
      _VERIFY(status,'',rc)

      if (this%is_lockable()) then
         call this%close()
         call this%lock%release()
      end if
      _RETURN(_SUCCESS,rc)

   end subroutine emit_message


   subroutine atomic_emit_message(this, record, unusable, rc)
      class (FileHandler), intent(inout) :: this
      type(LogRecord), intent(in) :: record
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status

      if (.not. this%is_open()) then
         call this%open(rc=status)
         _VERIFY(status,'',rc)
      end if
      call this%StreamHandler%emit_message(record, rc=status)
      _VERIFY(status,'',rc)

      _RETURN(_SUCCESS,rc)

   end subroutine atomic_emit_message
   

    
   !---------------------------------------------------------------------------  
   ! FUNCTION: 
   ! is_open
   !
   ! DESCRIPTION: 
   ! Check if file unit is open. 
   !---------------------------------------------------------------------------  
   logical function is_open(this)
      class (FileHandler), intent(in) :: this
       
      is_open = this%is_open_

   end function is_open


   !---------------------------------------------------------------------------  
   ! ROUTINE: 
   ! open
   !
   ! DESCRIPTION: 
   ! Open the disk file used by this handler.
   !---------------------------------------------------------------------------  
   subroutine open(this, unusable, rc)
      class (FileHandler), intent(inout) :: this
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: unit
      logical :: opened
      integer :: status

      if (this%is_open()) return

      open(newunit=unit, file=this%get_file_name(), &
           & status='unknown', form='formatted', position='append',iostat=status)
      _VERIFY(status,'could not open '//this%get_file_name(),rc)

      this%is_open_ = .true.

      call this%set_unit(unit)
      _RETURN(_SUCCESS,rc)
       
   end subroutine open


   !---------------------------------------------------------------------------  
   ! ROUTINE: 
   ! close
   !
   ! DESCRIPTION: 
   ! Closes the disk file used by this handler.
   !---------------------------------------------------------------------------  
   subroutine close(this)
      class (FileHandler), intent(inout) :: this
      integer :: unit

      call this%flush()
      call this%StreamHandler%close()
      ! workaround for NAG 6.0
!!$      inquire(file=this%get_file_name(), number=unit)
      unit = this%StreamHandler%get_unit()
      close(unit)
      this%is_open_ = .false.

   end subroutine close


   !---------------------------------------------------------------------------  
   ! FUNCTION: 
   ! get_file_name
   !
   ! DESCRIPTION: 
   ! Get the file name associated with this handler.
   !---------------------------------------------------------------------------  
   function get_file_name(this) result(file_name)
      class (FileHandler), intent(in) :: this
      character(len=:), allocatable :: file_name

      file_name = this%file_name

   end function get_file_name


   !---------------------------------------------------------------------------  
   ! ROUTINE: 
   ! set_file_name
   !
   ! DESCRIPTION: 
   ! Set the file name associated with this handler.
   !---------------------------------------------------------------------------  
   subroutine set_file_name(this, file_name)
      class (FileHandler), intent(inout) :: this
      character(len=*), intent(in) :: file_name

      this%file_name = file_name

   end subroutine set_file_name


   !---------------------------------------------------------------------------  
   ! FUNCTION: 
   ! equal
   !
   ! DESCRIPTION: 
   ! Overloads 'equal' operation for file handlers.
   !---------------------------------------------------------------------------  
   logical function equal(a, b)
      use PFL_AbstractHandler, only: AbstractHandler
      class (FileHandler), intent(in) :: a
      class (AbstractHandler), intent(in) :: b

      select type (b)
      class is (FileHandler)
         equal = (a%StreamHandler == b%StreamHandler) .and. (a%file_name == b%file_name)
      class default
         equal = .false.
      end select

   end function equal


   logical function is_lockable(this)
      class (FileHandler), intent(in) :: this

      is_lockable = .false.
      if (allocated(this%lock)) is_lockable = this%lock%is_initialized()

   end function is_lockable



   subroutine set_lock(this, lock)
      class (FileHandler), intent(inout) :: this
      class (AbstractLock), intent(in) :: lock
      
      if (this%is_lockable()) then
         call this%lock%destroy()
         deallocate(this%lock)
      end if

      if (this%is_open()) call this%close()

      allocate(this%lock, source=lock)
      call this%lock%init()

   end subroutine set_lock

   subroutine free(this)
      class (FileHandler), intent(inout) :: this

      if (this%is_lockable()) then
         call this%lock%destroy()
         deallocate(this%lock)
      end if

   end subroutine free

end module PFL_FileHandler
