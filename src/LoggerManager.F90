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
   use gFTL_StringUnlimitedMap
   use yafyaml, only: Parser
   use yafyaml, only: Configuration, ConfigurationIterator
   use yafyaml, only: FileStream
   use yafyaml, only: yayfaml_SUCCESS => SUCCESS
   use PFL_RootLogger, only: RootLogger
   use PFL_SeverityLevels
   use PFL_Logger, only: Logger, newLogger
   use PFL_AbstractLogger
   use PFL_LoggerPolyVector
   use PFL_StringAbstractLoggerPolyMap
   use PFL_StringHandlerMap
   use PFL_AbstractHandler
   use PFL_KeywordEnforcer
#ifdef _LOGGER_USE_MPI
   use mpi
#endif
   use PFL_Exception, only: throw
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
      type (ConfigElements) :: elements 
   contains
      procedure :: get_logger_name
      procedure :: get_logger_root
      generic :: get_logger => get_logger_name
      generic :: get_logger => get_logger_root
      procedure, private :: fixup_ancestors
      procedure, private :: fixup_children
      procedure, nopass :: get_parent_prefix

      procedure :: load_file
      procedure :: load_config
      procedure :: build_loggers
      procedure :: build_root_logger
      procedure :: basic_config

      procedure :: free
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
   ! get_logger_name
   !
   ! DESCRIPTION: 
   ! Get a logger with the specified 'name', creating it if necessary.
   ! Note that:
   ! 1) 'name' is a dot-separated hierarchical name such as 'A','A.B','A.B.C',
   !    etc.
   ! 2) 'name' is case insensitive.
   !---------------------------------------------------------------------------
   function get_logger_root(this) result(lgr)
      class (Logger), pointer :: lgr
      class (LoggerManager), target, intent(inout) :: this
      
      lgr => this%root_node
   end function get_logger_root

   !---------------------------------------------------------------------------  
   ! FUNCTION: 
   ! get_logger_name
   !
   ! DESCRIPTION: 
   ! Get a logger with the specified 'name', creating it if necessary.
   ! Note that:
   ! 1) 'name' is a dot-separated hierarchical name such as 'A','A.B','A.B.C',
   !    etc.
   ! 2) 'name' is case insensitive.
   !---------------------------------------------------------------------------
   function get_logger_name(this, name) result(lgr)
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

   end function get_logger_name

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
      type (StringUnlimitedMap), optional, intent(in) :: extra
      integer, optional, intent(in) :: comm

      type (Configuration) :: cfg
      integer :: rc
      type (StringUnlimitedMap) :: extra_

      type(Configuration) :: c
      type(Parser) :: p

      if (present(extra)) then
         extra_ = extra
      end if

      if (present(comm)) then
         call extra_%insert('_GLOBAL_COMMUNICATOR',comm)
      end if


      p = Parser('Core')
      c = p%load(FileStream(file_name))

      call this%load_config(c, extra=extra_, comm=comm)
      
   end subroutine load_file



   subroutine load_config(this, cfg, unused, extra, comm)
      class (LoggerManager), target, intent(inout) :: this
      type (Configuration), intent(in) :: cfg
      type (Unusable), optional, intent(in) :: unused
      type (StringUnlimitedMap), optional, intent(in) :: extra
      integer, optional, intent(in) :: comm

      logical :: found
      type (Configuration) :: subcfg

      call check_schema_version(cfg)
  
      associate (elements => this%elements)
         call elements%set_global_communicator(comm)

         call cfg%get(subcfg, 'locks')
         if (.not. subcfg%is_none()) call elements%build_locks(subcfg, extra=extra)

         call cfg%get(subcfg, 'filters')
         if (.not. subcfg%is_none()) call elements%build_filters(subcfg, extra=extra)
         
         call cfg%get(subcfg, 'formatters')
         if (.not. subcfg%is_none()) call elements%build_formatters(subcfg, extra=extra)

         call cfg%get(subcfg, 'handlers')
         if (.not. subcfg%is_none()) call elements%build_handlers(subcfg, extra=extra)
      end associate

      call this%build_loggers(cfg, extra=extra)
      call this%build_root_logger(cfg, extra=extra)
   end subroutine load_config


   subroutine build_loggers(this, cfg, unused, extra)
      class (LoggerManager), target, intent(inout) :: this
      type (Configuration), intent(in) :: cfg
      type (Unusable), optional, intent(in) :: unused
      type (StringUnlimitedMap), optional, intent(in) :: extra

      type (Configuration) :: lgrs_cfg, lgr_cfg
      
      logical :: is_present
      type (ConfigurationIterator) :: iter
      character(len=:), allocatable :: name
      type (Logger), pointer :: lgr

      call cfg%get(lgrs_cfg, 'loggers')

      if (.not. lgrs_cfg%is_mapping()) then
         ! TODO: raise an exception instead of stopping
         error stop
      end if
      if (.not. lgrs_cfg%is_none()) then
         ! Loop over contained loggers
         iter = lgrs_cfg%begin()
         do while (iter /= lgrs_cfg%end())
            name = iter%key()
            lgr => this%get_logger(name)
            call lgrs_cfg%get(lgr_cfg, name)
            call build_logger(lgr, lgr_cfg, this%elements, extra=extra)
            call iter%next()
         end do
      end if

   end subroutine build_loggers


   subroutine build_root_logger(this, cfg, unused, extra)
      class (LoggerManager), intent(inout) :: this
      type (Configuration), intent(in) :: cfg
      type (Unusable), optional, intent(in) :: unused
      type (StringUnlimitedMap), optional, intent(in) :: extra

      type (Configuration) :: root_cfg
      
      logical :: found
      character(len=:), allocatable :: name

      call cfg%get(root_cfg, 'root')
      if (.not. root_cfg%is_none()) then
         call build_logger(this%root_node, root_cfg, this%elements, extra=extra)
      end if

   end subroutine build_root_logger

   subroutine basic_config(this, unusable, filename, level, stream, force, handlers, handler_refs, rc)
      use pfl_StreamHandler
      use pfl_FileHandler
      use pfl_AbstractHandlerPtrVector
      use pfl_AbstractHandlerPolyVector
      use pfl_AbstractHandler
      class(LoggerManager), intent(inout) :: this
      class(KeywordEnforcer), optional, intent(in) :: unusable
      character(*), optional, intent(in) :: filename
      integer, optional, intent(in) :: level
      type (StreamHandler), target, optional, intent(in) :: stream
      logical, optional, intent(in) :: force
      type(HandlerVector), optional, intent(inout) :: handlers
      type(HandlerPtrVector), optional, intent(in) :: handler_refs
      integer, optional :: rc

      type(HandlerPtrVector), pointer :: existing_handlers
      type(HandlerVectorIterator) :: iter
      type(HandlerPtrVectorIterator) :: iter_ptr
      class(AbstractHandler), pointer :: h
      class(HandlerMap), pointer :: hdlMapPtr
      character(:), allocatable :: h_name

      existing_handlers => this%root_node%get_handlers()
      
      if (existing_handlers%size() > 0) then
         if (present(force)) then
            if (.not. force) then
               if (present(rc)) rc = 0 ! success - do nothing
               return
            end if
         else ! force not present
            if (present(rc)) rc = 0 ! success - do nothing
            return
         end if
      end if

      ! Else ...

      ! Check that conflicting arguments are not present
      if (count([present(filename),present(stream),present(handlers), present(handler_refs)]) > 1) then
         rc = -1 ! conflicting arguments
         return
      end if

      if (present(level)) then
         call this%root_node%set_level(level)
      end if

      hdlMapPtr => this%elements%get_handlers()
      
      if (present(filename)) then
         h_name = get_hname()
         call hdlMapPtr%insert(h_name, FileHandler(filename))
         h => hdlMapPtr%at(h_name)
         call this%root_node%add_handler(h)
      end if

      if (present(stream)) then
         call this%root_node%add_handler(stream)
      end if
         
      if (present(handlers)) then
         iter = handlers%begin()
         do while (iter /= handlers%end())
            h_name = get_hname()
            call hdlMapPtr%insert(h_name, iter%get())
            h => hdlMapPtr%at(h_name)
            call this%root_node%add_handler(h)
            call iter%next()
         end do
         call handlers%erase(handlers%begin(), handlers%end())
      end if

      if (present(handler_refs)) then
         iter_ptr = handler_refs%begin()
         do while (iter_ptr /= handler_refs%end())
            h => iter_ptr%get()
            call this%root_node%add_handler(h)
            call iter_ptr%next()
         end do
      end if

      if (present(rc)) rc = 0 ! success
   contains
      function get_hname() result(h_name)
         character(:), allocatable :: h_name
         integer :: i
         character(4) :: n_name
         i = hdlMapPtr%size() + 1
         write(n_name, '(I4.4)') i
         h_name = "__internal__from_basic__"//n_name
      end function 
   end subroutine basic_config

   subroutine free(this)
      class(LoggerManager), intent(inout) :: this
      character(len=:), allocatable :: name
      type (HandlerMap), pointer :: hdlMapPtr
      class (AbstractHandler), pointer :: hdlPtr
      type (HandlerIterator) :: iter

      hdlMapPtr => this%elements%get_handlers()
      iter =  hdlMapPtr%begin()
      do while (iter /= hdlMapPtr%end())
         hdlPtr => iter%value()
         call hdlPtr%free()
         call iter%next()
      enddo

   end subroutine free

end module PFL_LoggerManager
