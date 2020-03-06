!------------------------------------------------------------------------------
! NASA/GSFC, CISTO, Code 606, Advanced Software Technology Group
!------------------------------------------------------------------------------
!
! MODULE: PFL_LoggerManager
!
!> @brief A manager instance that holds the hierarchy of loggers. 
!
!> @author ASTG staff
!> @date 01 Jan 2015 - Initial Version  
!---------------------------------------------------------------------------
module PFL_LoggerManager
   use PFL_RootLogger, only: RootLogger
   use PFL_StringAbstractLoggerPolyMap
   use PFL_SeverityLevels
   use PFL_Logger, only: Logger, newLogger
   use PFL_AbstractLogger
   use PFL_LoggerPolyVector
#ifdef _LOGGER_USE_MPI
   use mpi
#endif
   use PFL_Exception, only: throw
   use PFL_YAML_Parser, only: SUCCESS, YAML_load_file => load_file
   use FTL_Config, only: Config
   use PFL_Config, only: ConfigElements, build_logger, check_schema_version
   implicit none
   private

   public :: LoggerManager
   public :: initialize_logger_manager
   public :: logging ! singleton instance

!!$   type, extends(Object) :: LoggerManager
   type :: LoggerManager
      private
      type (RootLogger) :: root_node
      type (LoggerMap) :: loggers
   contains
      procedure :: get_logger
      procedure, private :: fixup_ancestors
      procedure, private :: fixup_children
      procedure, nopass :: get_parent_prefix

      procedure :: load_file
      procedure :: load_config
      procedure :: build_loggers
      procedure :: build_root_logger

   end type LoggerManager
   
   
   interface LoggerManager
      module procedure new_LoggerManager
   end interface LoggerManager
   
   type (LoggerManager), target, save :: logging

   ! Private type - only used internally
   integer :: counter = 0
   type, extends(AbstractLogger) :: Placeholder
      private
      integer :: counter
      type (LoggerVector) :: children
   end type Placeholder

   type Unusable
   end type Unusable
   

contains


   !---------------------------------------------------------------------------  
   ! FUNCTION: 
   ! new_LoggerManager
   !
   ! DESCRIPTION: 
   ! Initialize with the root node of the logger hierarchy.
   !---------------------------------------------------------------------------
   function new_LoggerManager(root_node) result(manager)
      type (LoggerManager), target :: manager
      type (RootLogger), intent(in) :: root_node

      manager%root_node = root_node

   end function new_LoggerManager


   !---------------------------------------------------------------------------  
   ! FUNCTION: 
   ! get_logger
   !
   ! DESCRIPTION: 
   ! Get a logger with the specified 'name', creating it if necessary.
   ! Note that:
   ! 1) 'name' is a dot-separated hierarchical name such as 'A','A.B','A.B.C',
   !    etc.
   ! 2) 'name' is case insensitive.
   !---------------------------------------------------------------------------
   function get_logger(this, name) result(lgr)
      class (Logger), pointer :: lgr
      class (LoggerManager), target, intent(inout) :: this
      character(len=*), intent(in) :: name

      class (AbstractLogger), pointer :: tmp, tmp2

      tmp => this%loggers%at(name)

      if (associated(tmp)) then

         ! cast to Logger
         select type (tmp)
         type is (Logger)
            lgr => tmp
         type is (Placeholder)
            block
              type (Placeholder) :: ph

              ph%children = tmp%children
              allocate(lgr, source=newLogger(name))
              call this%loggers%set(name, lgr)

              tmp2 => this%loggers%at(name)
              select type (tmp2)
              type is (Logger)
                 lgr => tmp2
                 call this%fixup_children(ph, lgr)
                 call this%fixup_ancestors(lgr)
                 class default
                 call throw(__FILE__,__LINE__,'should not get here')
              end select
            end block
            class default
            lgr => null()
            call throw(__FILE__,__LINE__,'LoggerManager::get_logger() - Illegal type of logger <' &
                 & // name // '>')
         end select
         return

      else ! new logger

         allocate(lgr, source=newLogger(name))
         call this%loggers%insert(name, lgr)
         tmp => this%loggers%at(name)
         ! cast to Logger
         select type (tmp)
         class is (Logger)
            lgr => tmp
            call this%fixup_ancestors(lgr)
         class default
            lgr => null()
            call throw(__FILE__,__LINE__,'LoggerManager::get_logger() - Illegal type of logger <' &
                 & // name // '>')
         end select

      end if

   end function get_logger

   subroutine fixup_ancestors(this, lgr)
      class (LoggerManager), target, intent(inout) :: this
      class (Logger), intent(inout), pointer :: lgr

      integer :: i
      character(len=:), allocatable :: name
      character(len=:), allocatable :: ancestor_name
      class (Logger), pointer :: ancestor
      class (AbstractLogger), pointer :: tmp

      name = lgr%get_name()
      ancestor_name = this%get_parent_prefix(name)
      ancestor => null()

      do while (ancestor_name /= '' .and. (.not. associated(ancestor)))
         tmp => this%loggers%at(ancestor_name)
         if (associated(tmp)) then ! ancestor exists
            select type (tmp)
               class is (Logger)
               ancestor => tmp
               call lgr%set_parent(ancestor)
            type is (Placeholder)
               call tmp%children%push_back(lgr)
               class default
               call throw(__FILE__,__LINE__,"LoggerManager::fixup_ancestors() - illegal type for name '"//ancestor_name//"'.")
            end select
         else ! create placeholder
            block
              type (Placeholder), pointer :: ph
              allocate(ph)
              counter = counter + 1
              ph%counter = counter
              call this%loggers%insert(ancestor_name, ph)
              tmp => this%loggers%at(ancestor_name)
              select type (tmp)
              class is (PlaceHolder)
                 ph => tmp
              end select
              ! lgr pointer may be invalidated by insert() call above
              ! find lgr again ...
              tmp => this%loggers%at(name)
              select type (tmp)
              class is (Logger)
                 lgr => tmp
              end select
              call ph%children%push_back(lgr)
            end block
         end if

         ancestor_name = this%get_parent_prefix(ancestor_name)
      end do

      if (.not. associated(ancestor)) then
         ancestor => this%root_node
      end if

      call lgr%set_parent(ancestor)

   end subroutine fixup_ancestors


   subroutine fixup_children(this, ph, lgr)
      class (LoggerManager), target, intent(in) :: this
      type (Placeholder), intent(in) :: ph
      class (Logger), intent(inout) :: lgr

      type (LoggerVecIterator) :: iter
      class (Logger), pointer :: child
      class (Logger), pointer :: child_ancestor
      character(len=:), allocatable :: name, child_ancestor_name
      integer :: n

      iter = ph%children%begin()
      name = lgr%get_name()
      n = len(name)

      do while (iter /= ph%children%end())
         child => iter%get()
         child_ancestor => child%get_parent()
         child_ancestor_name = child_ancestor%get_name()

         if (child_ancestor_name(1:n) /= name) then
            call lgr%set_parent(child_ancestor)
            call child%set_parent(lgr)
         end if
         call iter%next()
      end do

   end subroutine fixup_children

   !---------------------------------------------------------------------------  
   ! FUNCTION: 
   ! get_parent_prefix
   !
   ! DESCRIPTION: 
   ! In the logger hierarchy, the parent prefix is the string preceding the
   ! last logger in the hierarchy. For example: the parent prefix of c in the
   ! logger hierarchy a.b.c is a.b
   !---------------------------------------------------------------------------
   function get_parent_prefix(name) result(prefix)
      character(len=*), intent(in) :: name
      character(len=:), allocatable :: prefix

      integer :: idx

      idx = index(name, '.', back=.true.)
      prefix = name(1:idx-1)

   end function get_parent_prefix


   subroutine initialize_logger_manager()
      
      logging = LoggerManager(RootLogger(WARNING))

   end subroutine initialize_logger_manager



   subroutine load_file(this, file_name, unused, extra, comm)
      class (LoggerManager), target, intent(inout) :: this
      character(len=*), intent(in) :: file_name
      type (Unusable), optional, intent(in) :: unused
      type (Config), optional, intent(in) :: extra
      integer, optional, intent(in) :: comm

      type (Config) :: cfg
      integer :: rc
      type (Config) :: extra_

      if (present(extra)) then
         extra_ = extra
      end if

      if (present(comm)) then
         call extra_%insert('_GLOBAL_COMMUNICATOR',comm)
      end if


      cfg = YAML_load_file(file_name, rc)
      if (rc /= SUCCESS) then
         call throw(__FILE__,__LINE__,'PFL_LoggerManager::load_file() - Failure opening file: <'//file_name//'>')
         return
      end if
      call this%load_config(cfg, extra=extra_, comm=comm)
      
   end subroutine load_file



   subroutine load_config(this, cfg, unused, extra, comm)
      class (LoggerManager), target, intent(inout) :: this
      type (Config), intent(in) :: cfg
      type (Unusable), optional, intent(in) :: unused
      type (Config), optional, intent(in) :: extra
      integer, optional, intent(in) :: comm

      type (ConfigElements) :: elements
      
      logical :: found
      type (Config), pointer :: subcfg

      call check_schema_version(cfg)

      call elements%set_global_communicator(comm)

      subcfg => cfg%toConfigPtr('locks', found=found)
      if (found) call elements%build_locks(subcfg, extra=extra)

      subcfg => cfg%toConfigPtr('filters', found=found)
      if (found) call elements%build_filters(subcfg, extra=extra)
      
      subcfg => cfg%toConfigPtr('formatters', found=found)
      if (found) call elements%build_formatters(subcfg, extra=extra)

      subcfg => cfg%toConfigPtr('handlers', found=found)
      if (found)call elements%build_handlers(subcfg, extra=extra)

      call this%build_loggers(cfg, elements, extra=extra)
      call this%build_root_logger(cfg, elements, extra=extra)

   end subroutine load_config


   subroutine build_loggers(this, cfg, elements, unused, extra)
      use PFL_StringUnlimitedPolyMap, only: ConfigIterator
      class (LoggerManager), target, intent(inout) :: this
      type (Config), intent(in) :: cfg
      type (ConfigElements), intent(in) :: elements
      type (Unusable), optional, intent(in) :: unused
      type (Config), optional, intent(in) :: extra

      type (Config), pointer :: lgrs_cfg, lgr_cfg
      
      logical :: found
      type (ConfigIterator) :: iter
      character(len=:), allocatable :: name
      type (Logger), pointer :: lgr

      lgrs_cfg => cfg%toConfigPtr('loggers', found=found)
      if (found) then
         ! Loop over contained loggers
         iter = lgrs_cfg%begin()
         do while (iter /= lgrs_cfg%end())
            name = iter%key()
            lgr => this%get_logger(name)
            lgr_cfg => lgrs_cfg%toConfigPtr(name)
            call build_logger(lgr, lgr_cfg, elements, extra=extra)
            call iter%next()
         end do
      end if

   end subroutine build_loggers


   subroutine build_root_logger(this, cfg, elements, unused, extra)
      use PFL_StringUnlimitedPolyMap, only: ConfigIterator
      class (LoggerManager), intent(inout) :: this
      type (Config), intent(in) :: cfg
      type (ConfigElements), intent(in) :: elements
      type (Unusable), optional, intent(in) :: unused
      type (Config), optional, intent(in) :: extra

      type (Config), pointer :: root_cfg
      
      logical :: found
      character(len=:), allocatable :: name

      root_cfg => cfg%toConfigPtr('root', found=found)
      if (found) then
         call build_logger(this%root_node, root_cfg, elements, extra=extra)
      end if

   end subroutine build_root_logger

end module PFL_LoggerManager
