! Note that several modules are combined into one file here.  The
! reason is to minimize complexit for those desiring to just use this
! layer.
module PFL_ParserLevel_mod
  use FTL_Config_mod
  implicit none
  private

  public :: ParserLevel

  type :: ParserLevel
     type (Config), pointer :: config => null()
     integer :: indentation
  end type ParserLevel

  interface ParserLevel
     module procedure new_ParserLevel
  end interface ParserLevel

contains

  function new_ParserLevel(indentation, cfg) result(pLevel)
    type (ParserLevel) :: pLevel
    integer, intent(in) :: indentation
    type(Config), target, intent(in) :: cfg

    pLevel%indentation = indentation
    pLevel%config => cfg

  end function new_ParserLevel
  
end module PFL_ParserLevel_mod


module PFL_ParserLevelStack_mod
  use PFL_ParserLevel_mod
#define _vector ParserLevelStack
#define _type type (ParserLevel)
#include "templates/vector.inc"
end module PFL_ParserLevelStack_mod


module PFL_YAML_Line_mod
  implicit none
  private

  
  public :: YAML_Line
  public :: is_comment
  
  type :: YAML_Line
     character(len=:), allocatable :: key
     character(len=:), allocatable :: value
     integer :: indentation
   contains
     procedure :: starts_dictionary
  end type YAML_Line

  interface YAML_Line
     procedure new_YAML_Line
  end interface YAML_Line

  character(len=*), parameter :: SEPARATOR = ':'
  character(len=*), parameter :: COMMENT_CHARS= '#'

contains


  function new_YAML_Line(buffer) result(line)
    type (YAML_Line) :: line
    character(len=*), intent(in) :: buffer

    line%indentation = verify(buffer, ' ') - 1
    block
      integer :: idx
      idx = scan(buffer, SEPARATOR)
      if (idx > 0) then
         line%key = trim(adjustl(buffer(1:idx-1)))

         line%value = buffer(idx+1:len_trim(buffer))
         idx = scan(line%value, COMMENT_CHARS)
         if (idx > 0) then
            line%value = line%value(1:idx-1)
         end if
         line%value = trim(adjustl(line%value))
      end if
    end block

  end function new_YAML_Line


  logical function starts_dictionary(this)
    class (YAML_Line), intent(in) :: this

    starts_dictionary = (this%value == '') ! empty value

  end function starts_dictionary


  logical function is_comment(buffer)
    character(len=*), intent(in) :: buffer

    if (len_trim(buffer) == 0) then ! empty line
       is_comment = .true.
       return
    else
       block
         integer :: idx
         idx = scan(buffer,COMMENT_CHARS)
         if (idx > 0 .and. len_trim(buffer(1:idx-1)) == 0) then
            is_comment = .true.
            return
         end if
       end block
    end if
    is_comment = .false.

  end function is_comment

end module PFL_YAML_Line_mod


module PFL_YAML_Parser_mod
  use PFL_YAML_Line_mod
  use FTL_Config_mod
  use PFL_ParserLevel_mod
  use PFL_ParserLevelStack_mod
  implicit none
  private

  public :: load_file
  public :: SUCCESS, FAIL

  type :: YAML_Parser
     private
     integer :: io_unit
     type (ParserLevelStack) :: levels
     type (Config), pointer :: parent_config => null()
   contains
     ! helper procedures
     procedure :: open
     procedure :: close
     procedure :: parse_one_line
     procedure :: read_line
     procedure :: set_new_indentation
  end type YAML_Parser

  integer, parameter :: SUCCESS = 0
  integer, parameter :: DONE = 1
  integer, parameter :: FAIL = 2

  integer, parameter :: MAX_LEN_LINE = 80


contains


  function load_file(file_name, rc) result(cfg)
    type (Config), target :: cfg
    character(len=*), intent(in) :: file_name
    integer, intent(out) :: rc

    type (YAML_Parser), target :: p

    rc = p%open(file_name)
    if (rc /= SUCCESS) return
    call p%levels%push_back(ParserLevel(1, cfg)) ! with empty current
    
    do while (rc == SUCCESS)
       rc = p%parse_one_line()
    end do

    if (rc /= DONE) then
       rc = FAIL
       return
    else
       rc = SUCCESS
    end if

    call p%close()
    call p%levels%clear()

  end function load_file

  

  integer function open(this, file_name) result(rc)
    class (YAML_Parser), intent(inout) :: this
    character(len=*), intent(in) :: file_name

    integer :: iostat

    open(file=file_name, newunit=this%io_unit, status='old', form='formatted', &
         & iostat=iostat)
    rc = iostat

  end function open


  subroutine close(this)
    class (YAML_Parser), intent(inout) :: this

    close(this%io_unit)

  end subroutine close

  
  integer function parse_one_line(this) result(rc)
     use PFL_String_Mod
    class (YAML_Parser), target, intent(inout) :: this
    type (YAML_Line) :: line
    character(len=MAX_LEN_LINE) :: buffer

    type (ParserLevel), pointer :: current_level
    type (Config) :: empty
    

    rc = SUCCESS ! unless
    
    call this%read_line(buffer, rc)
    if (rc /= SUCCESS .or. is_comment(buffer)) return

    line = YAML_Line(buffer)

    rc = this%set_new_indentation(line%indentation)
    if (rc /= SUCCESS) return

    current_level => this%levels%back()

    if (line%starts_dictionary()) then
       call current_level%config%insert(line%key, empty)
       this%parent_config => current_level%config%toConfigPtr(line%key)
    else
#ifdef __GFORTRAN__
       call current_level%config%insert(line%key, String(line%value))
#else
       call current_level%config%insert(line%key, line%value)
#endif
    end if

  end function parse_one_line

  
  subroutine read_line(this, buffer, rc)
    use iso_fortran_env, only: IOSTAT_END, IOSTAT_EOR
    class (YAML_Parser), intent(in) :: this
    character(len=*), intent(inout) :: buffer
    integer, intent(out) :: rc
    
    integer :: iostat
    
    read(this%io_unit,'(a)', iostat=iostat) buffer

    select case (iostat)
    case (IOSTAT_END) ! done
       rc = DONE
    case (IOSTAT_EOR)
       rc = FAIL
    case (0)
       rc = SUCCESS
    case default ! not sure how this can happen, but ...
       rc = FAIL
    end select
    
  end subroutine read_line
  
  
  integer function set_new_indentation(this, indentation) result(rc)
    class (YAML_Parser), intent(inout) :: this
    integer, intent(in) :: indentation

    type (ParserLevel), pointer :: level

    rc = SUCCESS ! unless
    
    level => this%levels%back()

    if (indentation == level%indentation) return

    if (indentation > level%indentation) then ! start new dictionary
       if (.not. associated(this%parent_config)) then
          rc = FAIL
          return
       end if
       call this%levels%push_back(ParserLevel(indentation, this%parent_config))
       this%parent_config => null()
       return
    end if

    ! Search back up the stack
       
    do while (this%levels%size() > 1)
       call this%levels%pop_back()
       level => this%levels%back()
       
       if (indentation == level%indentation) exit

       if (indentation > level%indentation) then
          ! illegal indentation pattern
          rc = FAIL
       end if

    end do

  end function set_new_indentation

  
end module PFL_YAML_Parser_mod
