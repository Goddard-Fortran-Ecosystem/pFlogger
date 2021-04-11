module pflogger
   use, intrinsic :: iso_fortran_env, only: OUTPUT_UNIT
   use Pfl_Exception
   use PFL_LoggerManager
   use PFL_AbstractHandlerPtrVector
   use PFL_AbstractHandlerPolyVector
   use PFL_Logger
   use PFL_RootLogger
   use PFL_AbstractHandler
   use PFL_StreamHandler
   use PFL_FileHandler
   use PFL_SeverityLevels
   use PFL_Formatter
   use PFL_FastFormatter
   use PFL_WrapArray
#ifdef _LOGGER_USE_MPI
#  ifdef SUPPORT_FOR_MPI_ALLOC_MEM_CPTR
   use PFL_MpiLock
   use PFL_MpiFilter
   use PFL_MpiFormatter
#  endif   
#endif
   use PFL_RotatingFileHandler
   use gFTL_StringUnlimitedMap
   implicit none
   private

   public :: initialize
   public :: finalize

   public :: logging
   public :: Logger
   public :: RootLogger
   public :: WrapArray

   public :: AbstractHandler
   public :: HandlerPtrVector, HandlerPtrVectorIterator
   public :: HandlerVector, HandlerVectorIterator
   public :: StreamHandler
   public :: FileHandler
   public :: RotatingFileHandler
   public :: newRotatingFileHandler

   public :: Formatter
   public :: newFormatter
   public :: FastFormatter

   public :: NOTSET
   public :: DEBUG
   public :: INFO
   public :: WARNING
   public :: ERROR
   public :: CRITICAL

   public :: level_to_name
   public :: name_to_level

   public :: initialize_severity_levels
   public :: finalize_severity_levels

#ifdef _LOGGER_USE_MPI
#  ifdef SUPPORT_FOR_MPI_ALLOC_MEM_CPTR
   public :: MpiLock
   public :: MpiFilter
   public :: MpiFormatter
#  endif
#endif   

   public :: set_throw_fptr
   public :: StringUnlimitedMap

contains

   subroutine initialize(unusable,comm,logging_config,logger_name, rc)
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(in) :: comm
      character(len=*), optional,intent(in) :: logging_config
      character(len=*), optional,intent(in) :: logger_name
      integer, optional, intent(out) :: rc

      type (HandlerVector) :: handlers
      type (StreamHandler) :: console
      type (FileHandler) :: file_handler
      integer :: level,rank,status
      character(:), allocatable :: logging_configuration_file
      character(:), allocatable :: logger_default_name
      integer :: comm_world
      type(Logger), pointer :: lgr

      _UNUSED_DUMMY(unusable)
      if (present(logging_config)) then
         logging_configuration_file=logging_config
      else
         logging_configuration_file=''
      end if
      if (present(comm)) then
         call MPI_Comm_dup(comm,comm_world,status)
         _VERIFY(status)
      else
         comm_world=MPI_COMM_WORLD
      end if

      call initialize_severity_levels()
      call initialize_logger_manager()
      call set_last_resort(StreamHandler())

      if (logging_configuration_file /= '') then
         call logging%load_file(logging_configuration_file)
      else

         if (present(logger_name)) then
            call MPI_COMM_Rank(comm_world,rank,status)
            console = StreamHandler(OUTPUT_UNIT)
            call console%set_level(INFO)
            call console%set_formatter(MpiFormatter(comm_world, fmt='%(short_name)a10~: %(message)a'))
            call handlers%push_back(console)

            file_handler = FileHandler('warnings_and_errors.log')
            call file_handler%set_level(WARNING)
            call file_handler%set_formatter(MpiFormatter(comm_world, fmt='pe=%(mpi_rank)i5.5~: %(short_name)a~: %(message)a'))
            call file_handler%set_lock(MpiLock(comm_world))
            call handlers%push_back(file_handler)

            if (rank == 0) then
               level = INFO
            else
               level = WARNING
            end if

            call logging%basic_config(level=level, handlers=handlers, rc=status)
            _VERIFY(status)

            if (rank == 0) then
               lgr => logging%get_logger(logger_name)
               call lgr%warning('No configure file specified for logging layer.  Using defaults.')
            end if
         endif
      end if
      _RETURN(_SUCCESS)

   end subroutine initialize
   
   subroutine finalize()

      call finalize_severity_levels()

   end subroutine finalize

end module pflogger
