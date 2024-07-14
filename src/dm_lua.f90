! Author:  Philipp Engel
! Licence: ISC
module dm_lua
    !! Lua abstraction layer that provides procedures for reading from and
    !! writing to the Lua stack.
    !!
    !! The following example creates a new Lua state, then pushes an
    !! observation to and pulls it from the stack:
    !!
    !! ```fortran
    !! integer              :: rc
    !! type(lua_state_type) :: lua
    !! type(observ_type)    :: observ1, observ2
    !!
    !! rc = dm_lua_init(lua)          ! Initialise Lua interpreter.
    !! call dm_lua_from(lua, observ1) ! Push observation onto stack.
    !! rc = dm_lua_to(lua, observ2)   ! Read observation back from stack.
    !! call dm_lua_destroy(lua)       ! Destroy Lua interpreter.
    !! ```
    use, intrinsic :: iso_c_binding
    use :: lua
    use :: dm_error
    use :: dm_file
    use :: dm_kind
    use :: dm_string
    use :: dm_util
    implicit none (type, external)
    private

    abstract interface
        ! int *lua_CFunction(lua_State *L)
        function dm_lua_callback(ptr) bind(c)
            !! C-interoperable Lua callback function.
            import :: c_int, c_ptr
            implicit none
            type(c_ptr), intent(in), value :: ptr             !! Lua state pointer.
            integer(kind=c_int)            :: dm_lua_callback !! Return value.
        end function dm_lua_callback
    end interface

    interface dm_lua_field
        !! Pushes table element on stack and optionally returns value.
        module procedure :: lua_field_array_int32
        module procedure :: lua_field_array_int64
        module procedure :: lua_field_int32
        module procedure :: lua_field_int64
        module procedure :: lua_field_logical
        module procedure :: lua_field_real64
        module procedure :: lua_field_stack
        module procedure :: lua_field_string
    end interface dm_lua_field

    interface dm_lua_get
        !! Pushes table index on stack and optionally returns value.
        module procedure :: lua_get_int32
        module procedure :: lua_get_int64
        module procedure :: lua_get_logical
        module procedure :: lua_get_real64
        module procedure :: lua_get_stack
        module procedure :: lua_get_string
    end interface dm_lua_get

    interface dm_lua_from
        !! Converts derived types to Lua table on stack.
        module procedure :: lua_from_observ
        module procedure :: lua_from_request
    end interface dm_lua_from

    interface dm_lua_read
        !! Pushes global variable on stack and optionally returns value.
        module procedure :: lua_read_array_int32
        module procedure :: lua_read_array_int64
        module procedure :: lua_read_int32
        module procedure :: lua_read_int64
        module procedure :: lua_read_logical
        module procedure :: lua_read_real64
        module procedure :: lua_read_stack
        module procedure :: lua_read_string
    end interface dm_lua_read

    interface dm_lua_set
        module procedure :: lua_set_int32
    end interface dm_lua_set

    interface dm_lua_to
        !! Converts Lua table to Fortran derived type.
        module procedure :: lua_to_job
        module procedure :: lua_to_job_list
        module procedure :: lua_to_jobs
        module procedure :: lua_to_observ
        module procedure :: lua_to_observs
        module procedure :: lua_to_report
        module procedure :: lua_to_request
    end interface dm_lua_to

    type, public :: lua_state_type
        !! Lua state type that stores the Lua pointer.
        type(c_ptr) :: ptr = c_null_ptr !! C pointer to Lua interpreter.
    end type lua_state_type

    ! Public procedures.
    public :: dm_lua_call
    public :: dm_lua_callback
    public :: dm_lua_destroy
    public :: dm_lua_dump_stack
    public :: dm_lua_error
    public :: dm_lua_escape
    public :: dm_lua_eval
    public :: dm_lua_exec
    public :: dm_lua_field
    public :: dm_lua_from
    public :: dm_lua_get
    public :: dm_lua_read
    public :: dm_lua_init
    public :: dm_lua_is_function
    public :: dm_lua_is_nil
    public :: dm_lua_is_opened
    public :: dm_lua_is_table
    public :: dm_lua_last_error
    public :: dm_lua_open
    public :: dm_lua_pop
    public :: dm_lua_register
    public :: dm_lua_set
    public :: dm_lua_table
    public :: dm_lua_table_size
    public :: dm_lua_to
    public :: dm_lua_to_int32
    public :: dm_lua_to_int64
    public :: dm_lua_to_logical
    public :: dm_lua_to_real32
    public :: dm_lua_to_real64
    public :: dm_lua_to_string
    public :: dm_lua_unescape
    public :: dm_lua_version
    public :: dm_lua_version_number

    ! Private procedures.
    private :: lua_field_array_int32
    private :: lua_field_array_int64
    private :: lua_field_int32
    private :: lua_field_int64
    private :: lua_field_logical
    private :: lua_field_real64
    private :: lua_field_stack
    private :: lua_field_string

    private :: lua_from_observ
    private :: lua_from_request

    private :: lua_get_int32
    private :: lua_get_int64
    private :: lua_get_logical
    private :: lua_get_real64
    private :: lua_get_stack
    private :: lua_get_string

    private :: lua_read_array_int32
    private :: lua_read_array_int64
    private :: lua_read_int32
    private :: lua_read_int64
    private :: lua_read_logical
    private :: lua_read_real64
    private :: lua_read_stack
    private :: lua_read_string

    private :: lua_set_int32

    private :: lua_to_job
    private :: lua_to_job_list
    private :: lua_to_jobs
    private :: lua_to_observ
    private :: lua_to_observs
    private :: lua_to_report
    private :: lua_to_request
contains
    ! ******************************************************************
    ! PUBLIC FUNCTIONS.
    ! ******************************************************************
    integer function dm_lua_call(lua, nargs, nresults) result(rc)
        !! Calls Lua function on top of stack.
        type(lua_state_type), intent(inout) :: lua      !! Lua type.
        integer,              intent(in)    :: nargs    !! Number of arguments.
        integer,              intent(in)    :: nresults !! Number of results.

        rc = dm_lua_error(lua_pcall(lua%ptr, nargs, nresults, 0))
    end function dm_lua_call

    integer function dm_lua_error(lua_error) result(rc)
        !! Converts Lua error code to DMPACK error code.
        integer, intent(in) :: lua_error !! Lua error code.

        select case (lua_error)
            case (LUA_OK)
                rc = E_NONE
            case (LUA_YIELD)
                rc = E_LUA_YIELD
            case (LUA_ERRRUN)
                rc = E_LUA_RUNTIME
            case (LUA_ERRSYNTAX)
                rc = E_LUA_SYNTAX
            case (LUA_ERRMEM)
                rc = E_LUA_MEM
            case (LUA_ERRERR)
                rc = E_LUA_ERROR
            case (LUA_ERRFILE)
                rc = E_LUA_FILE
            case default
                rc = E_LUA
        end select
    end function dm_lua_error

    function dm_lua_escape(str) result(res)
        !! Escapes passed character string by replacing each occurance of `\`
        !! with `\\`.
        character(len=*), intent(in)  :: str !! String to escape.
        character(len=:), allocatable :: res !! Escaped string.

        integer :: i

        res = ''

        do i = 1, len_trim(str)
            if (str(i:i) == '\') then
                res = res // '\\'
                cycle
            end if
            res = res // str(i:i)
        end do
    end function dm_lua_escape

    integer function dm_lua_eval(lua, command) result(rc)
        !! Executes Lua command passed in character string `command`.
        type(lua_state_type), intent(inout) :: lua     !! Lua type.
        character(len=*),     intent(in)    :: command !! Lua command to evaluate.

        rc = dm_lua_error(lual_dostring(lua%ptr, command))
    end function dm_lua_eval

    integer function dm_lua_exec(lua, file_path) result(rc)
        !! Executes Lua script.
        type(lua_state_type), intent(inout) :: lua       !! Lua type.
        character(len=*),     intent(in)    :: file_path !! Path to Lua script file.

        rc = dm_lua_error(lual_dofile(lua%ptr, trim(file_path)))
    end function dm_lua_exec

    integer function dm_lua_init(lua, libs) result(rc)
        !! Initialises Lua interpreter and opens libraries, unless `libs` is
        !! `.false.`. Returns `E_INVALID` if the Lua pointer is already
        !! associated, and `E_LUA` if one of the Lua calls failed.
        type(lua_state_type), intent(inout)        :: lua  !! Lua type.
        logical,              intent(in), optional :: libs !! Open Lua libraries.

        logical :: libs_

        libs_ = .true.
        if (present(libs)) libs_ = libs

        rc = E_INVALID
        if (c_associated(lua%ptr)) return

        rc = E_LUA
        lua%ptr = lual_newstate()
        if (.not. c_associated(lua%ptr)) return

        if (libs_) call lual_openlibs(lua%ptr)
        rc = E_NONE
    end function dm_lua_init

    logical function dm_lua_is_function(lua) result(is_function)
        !! Returns `.true.` if element on top of stack is of type function.
        type(lua_state_type), intent(inout) :: lua  !! Lua type.

        is_function = (lua_isfunction(lua%ptr, -1) == 1)
    end function dm_lua_is_function

    logical function dm_lua_is_nil(lua) result(is_nil)
        !! Returns `.true.` if element on top of stack is nil.
        type(lua_state_type), intent(inout) :: lua  !! Lua type.

        is_nil = (lua_isnil(lua%ptr, -1) == 1)
    end function dm_lua_is_nil

    logical function dm_lua_is_opened(lua) result(is_opened)
        !! Returns `.true.` if pointer to Lua interpreter is associated.
        type(lua_state_type), intent(inout) :: lua  !! Lua type.

        is_opened = c_associated(lua%ptr)
    end function dm_lua_is_opened

    logical function dm_lua_is_table(lua) result(is_table)
        !! Returns `.true.` if element on top of stack is of type table.
        type(lua_state_type), intent(inout) :: lua  !! Lua type.

        is_table = (lua_istable(lua%ptr, -1) == 1)
    end function dm_lua_is_table

    function dm_lua_last_error(lua) result(str)
        !! Returns last error message as allocatable character string.
        type(lua_state_type), intent(inout) :: lua !! Lua type.
        character(len=:), allocatable       :: str !! Last error message.

        if (lua_isstring(lua%ptr, -1) == 1) then
            str = lua_tostring(lua%ptr, -1)
            return
        end if

        str = ''
    end function dm_lua_last_error

    integer function dm_lua_open(lua, file_path, eval) result(rc)
        !! Opens Lua script and executes it by default.
        type(lua_state_type), intent(inout)        :: lua       !! Lua type.
        character(len=*),     intent(in)           :: file_path !! Path to Lua script.
        logical,              intent(in), optional :: eval      !! Evaluate script once.

        logical :: eval_

        rc = E_INVALID
        if (.not. c_associated(lua%ptr)) return

        rc = E_NOT_FOUND
        if (.not. dm_file_exists(file_path)) return

        eval_ = .true.
        if (present(eval)) eval_ = eval

        if (eval_) then
            rc = dm_lua_error(lual_dofile(lua%ptr, trim(file_path)))
            return
        end if

        rc = dm_lua_error(lual_loadfile(lua%ptr, trim(file_path)))
    end function dm_lua_open

    integer function dm_lua_table(lua, name, n) result(rc)
        !! Loads global table of given name.
        type(lua_state_type), intent(inout)         :: lua  !! Lua type.
        character(len=*),     intent(in)            :: name !! Name of table.
        integer,              intent(out), optional :: n    !! Number of elements in table.

        if (present(n)) n = 0

        rc = E_NOT_FOUND
        if (lua_getglobal(lua%ptr, trim(name)) == LUA_TNIL) return

        rc = E_TYPE
        if (lua_istable(lua%ptr, -1) == 0) then
            call lua_pop(lua%ptr, 1)
            return
        end if

        if (present(n)) n = dm_lua_table_size(lua)
        rc = E_NONE
    end function dm_lua_table

    integer function dm_lua_table_size(lua) result(n)
        !! Returns size of table on stack.
        type(lua_state_type), intent(inout) :: lua !! Lua type.

        n = int(lua_rawlen(lua%ptr, -1), kind=i4)
    end function dm_lua_table_size

    integer(kind=i4) function dm_lua_to_int32(lua, idx) result(value)
        !! Returns 4-byte integer from Lua stack at position `idx`.
        type(lua_state_type), intent(inout) :: lua !! Lua type.
        integer,              intent(in)    :: idx !! Stack index.

        value = int(lua_tointeger(lua%ptr, idx), kind=i4)
    end function dm_lua_to_int32

    integer(kind=i8) function dm_lua_to_int64(lua, idx) result(value)
        !! Returns 8-byte integer from Lua stack at position `idx`.
        type(lua_state_type), intent(inout) :: lua !! Lua type.
        integer,              intent(in)    :: idx !! Stack index.

        value = lua_tointeger(lua%ptr, idx)
    end function dm_lua_to_int64

    logical function dm_lua_to_logical(lua, idx) result(value)
        !! Returns 8-byte integer from Lua stack at position `idx`.
        type(lua_state_type), intent(inout) :: lua !! Lua type.
        integer,              intent(in)    :: idx !! Stack index.

        value = lua_toboolean(lua%ptr, idx)
    end function dm_lua_to_logical

    real(kind=r4) function dm_lua_to_real32(lua, idx) result(value)
        !! Returns 4-byte real from Lua stack at position `idx`.
        type(lua_state_type), intent(inout) :: lua !! Lua type.
        integer,              intent(in)    :: idx !! Stack index.

        value = real(lua_tonumber(lua%ptr, idx), kind=r4)
    end function dm_lua_to_real32

    real(kind=r8) function dm_lua_to_real64(lua, idx) result(value)
        !! Returns 8-byte real from Lua stack at position `idx`.
        type(lua_state_type), intent(inout) :: lua !! Lua type.
        integer,              intent(in)    :: idx !! Stack index.

        value = lua_tonumber(lua%ptr, idx)
    end function dm_lua_to_real64

    function dm_lua_to_string(lua, idx) result(value)
        !! Returns allocatable character string from Lua stack at position `idx`.
        type(lua_state_type), intent(inout) :: lua   !! Lua type.
        integer,              intent(in)    :: idx   !! Stack index.
        character(len=:), allocatable       :: value !! String value.

        value = lua_tostring(lua%ptr, idx)
    end function dm_lua_to_string

    function dm_lua_unescape(str) result(res)
        !! Unescapes passed character string by replacing each occurance of
        !! `\\` with `\`.
        character(len=*), intent(in)  :: str !! String to escape.
        character(len=:), allocatable :: res !! Unescaped string.

        integer :: i, n

        res = ''

        i = 1
        n = len_trim(str)

        do
            if (i > n) exit
            if (i < n) then
                if (str(i:i + 1) == '\\') then
                    res = res // '\'
                    i = i + 2
                    cycle
                end if
            end if
            res = res // str(i:i)
            i = i + 1
        end do
    end function dm_lua_unescape

    function dm_lua_version(name) result(version)
        !! Returns Lua version as allocatable string of the form `5.4` or
        !! `liblua/5.4` if argument `name` is `.true.`.
        logical, intent(in), optional :: name !! Add prefix `liblua/`.
        character(len=:), allocatable :: version

        character(len=3)     :: v
        integer              :: major, minor, rc
        logical              :: name_
        type(lua_state_type) :: lua

        name_ = .false.
        if (present(name)) name_ = name

        v  = '0.0'
        rc = dm_lua_init(lua)

        if (dm_is_ok(rc)) then
            call dm_lua_version_number(lua, major, minor)
            write (v, '(i1, ".", i1)') major, minor
        end if

        call dm_lua_destroy(lua)

        if (name_) then
            version = 'liblua/' // v
        else
            version = v
        end if
    end function dm_lua_version

    ! ******************************************************************
    ! PUBLIC SUBROUTINES.
    ! ******************************************************************
    subroutine dm_lua_destroy(lua)
        !! Closes Lua.
        type(lua_state_type), intent(inout) :: lua !! Lua type.

        if (.not. c_associated(lua%ptr)) return
        call lua_close(lua%ptr)
        lua%ptr = c_null_ptr
    end subroutine dm_lua_destroy

    subroutine dm_lua_dump_stack(lua, unit)
        !! Dumps stack to standard output or file unit.
        type(lua_state_type), intent(inout)        :: lua  !! Lua type.
        integer,              intent(in), optional :: unit !! File unit.

        integer :: i, top, type, unit_

        unit_ = stdout
        if (present(unit)) unit_ = unit

        top = lua_gettop(lua%ptr)

        do i = 1, top
            type = lua_type(lua%ptr, i)
            write (unit_, '(tr1, i0, tr1, a, tr1)', advance='no') i, lua_typename(lua%ptr, type)

            select case (type)
                case (LUA_TNIL)
                    write (unit_, '("nil")')
                case (LUA_TBOOLEAN)
                    write (unit_, '(l1)')   lua_toboolean(lua%ptr, i)
                case (LUA_TNUMBER)
                    write (unit_, '(f0.1)') lua_tonumber(lua%ptr, i)
                case (LUA_TSTRING)
                    write (unit_, '(a)')    lua_tostring(lua%ptr, i)
                case default
                    write (unit_, *)
            end select
        end do
    end subroutine dm_lua_dump_stack

    subroutine dm_lua_pop(lua, n)
        !! Pops element on stack.
        type(lua_state_type), intent(inout)        :: lua !! Lua type.
        integer,              intent(in), optional :: n   !! Stack position.

        integer :: n_

        n_ = 1
        if (present(n)) n_ = n
        if (.not. c_associated(lua%ptr)) return
        call lua_pop(lua%ptr, n_)
    end subroutine dm_lua_pop

    subroutine dm_lua_register(lua, name, proc)
        !! Registers a new Lua command.
        type(lua_state_type), intent(inout) :: lua  !! Lua type.
        character(len=*),     intent(in)    :: name !! Lua procedure name.
        procedure(dm_lua_callback)          :: proc !! C-interoperable subroutine to call.

        call lua_register(lua%ptr, trim(name), c_funloc(proc))
    end subroutine dm_lua_register

    subroutine dm_lua_version_number(lua, major, minor)
        !! Returns Lua version number.
        type(lua_state_type), intent(inout) :: lua   !! Lua type.
        integer,              intent(out)   :: major !! Major version number.
        integer,              intent(out)   :: minor !! Minor version number.

        real :: version

        version = real(lua_version(lua%ptr))
        major   = floor(version / 100)
        minor   = floor(version - (major * 100))
    end subroutine dm_lua_version_number

    ! ******************************************************************
    ! PRIVATE FUNCTIONS.
    ! ******************************************************************
    integer function lua_field_array_int32(lua, name, values) result(rc)
        !! Returns allocatable 4-byte integer array from table field `name` in
        !! `values`.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_ALLOC` if array allocation failed.
        !! * `E_EMPTY` if the field of given name is null.
        !! * `E_TYPE` if the field is not an integer array.
        !!
        !! On error, `values` will be allocated but empty.
        type(lua_state_type),          intent(inout) :: lua       !! Lua type.
        character(len=*),              intent(in)    :: name      !! Table field name.
        integer(kind=i4), allocatable, intent(out)   :: values(:) !! Table field values.

        lua_block: block
            integer :: i, n, stat

            rc = E_EMPTY
            if (lua_getfield(lua%ptr, -1, name) <= 0) exit lua_block

            rc = E_TYPE
            if (lua_istable(lua%ptr, -1) == 0) exit lua_block

            n = dm_lua_table_size(lua)

            rc = E_ALLOC
            allocate (values(n), stat=stat)
            if (stat /= 0) exit lua_block

            do i = 1, n
                rc = dm_lua_get(lua, i, values(i))
                if (dm_is_error(rc)) exit lua_block
            end do

            rc = E_NONE
        end block lua_block

        call lua_pop(lua%ptr, 1)
        if (.not. allocated(values)) allocate (values(0))
    end function lua_field_array_int32

    integer function lua_field_array_int64(lua, name, values) result(rc)
        !! Returns allocatable 8-byte integer array from table field `name` in
        !! `values`.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_ALLOC` if array allocation failed.
        !! * `E_EMPTY` if the field of given name is null.
        !! * `E_TYPE` if the field is not an integer array.
        !!
        !! On error, `values` will be allocated but empty.
        type(lua_state_type),          intent(inout) :: lua       !! Lua type.
        character(len=*),              intent(in)    :: name      !! Table field name.
        integer(kind=i8), allocatable, intent(out)   :: values(:) !! Table field values.

        lua_block: block
            integer :: i, n, stat

            rc = E_EMPTY
            if (lua_getfield(lua%ptr, -1, name) <= 0) exit lua_block

            rc = E_TYPE
            if (lua_istable(lua%ptr, -1) == 0) exit lua_block

            n = dm_lua_table_size(lua)

            rc = E_ALLOC
            allocate (values(n), stat=stat)
            if (stat /= 0) exit lua_block

            do i = 1, n
                rc = dm_lua_get(lua, i, values(i))
                if (dm_is_error(rc)) exit lua_block
            end do

            rc = E_NONE
        end block lua_block

        call lua_pop(lua%ptr, 1)
        if (.not. allocated(values)) allocate (values(0))
    end function lua_field_array_int64

    integer function lua_field_int32(lua, name, value) result(rc)
        !! Returns 4-byte integer from table field `name` in `value`.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_EMPTY` if the field of given name is null.
        !! * `E_TYPE` if the field is not of type integer.
        !!
        !! On error, `value` will not be overwritten.
        type(lua_state_type), intent(inout) :: lua   !! Lua type.
        character(len=*),     intent(in)    :: name  !! Table field name.
        integer(kind=i4),     intent(inout) :: value !! Table field value.

        lua_block: block
            rc = E_EMPTY
            if (lua_getfield(lua%ptr, -1, name) <= 0) exit lua_block

            rc = E_TYPE
            if (lua_isinteger(lua%ptr, -1) /= 1) exit lua_block

            rc = E_NONE
            value = int(lua_tointeger(lua%ptr, -1), kind=i4)
        end block lua_block

        call lua_pop(lua%ptr, 1)
    end function lua_field_int32

    integer function lua_field_int64(lua, name, value) result(rc)
        !! Returns 8-byte integer from table field `name` in `value`.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_EMPTY` if the field of given name is null.
        !! * `E_TYPE` if the field is not of type integer.
        !!
        !! On error, `value` will not be overwritten.
        type(lua_state_type), intent(inout) :: lua   !! Lua type.
        character(len=*),     intent(in)    :: name  !! Table field name.
        integer(kind=i8),     intent(inout) :: value !! Table field value.

        lua_block: block
            rc = E_EMPTY
            if (lua_getfield(lua%ptr, -1, name) <= 0) exit lua_block

            rc = E_TYPE
            if (lua_isinteger(lua%ptr, -1) /= 1) exit lua_block

            rc = E_NONE
            value = lua_tointeger(lua%ptr, -1)
        end block lua_block

        call lua_pop(lua%ptr, 1)
    end function lua_field_int64

    integer function lua_field_logical(lua, name, value) result(rc)
        !! Returns logical from table field `name` in `value`.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_EMPTY` if the field of given name is null.
        !! * `E_TYPE` if the field is not of type boolean.
        !!
        !! On error, `value` will not be overwritten.
        type(lua_state_type), intent(inout) :: lua   !! Lua type.
        character(len=*),     intent(in)    :: name  !! Table field name.
        logical,              intent(inout) :: value !! Table field value.

        lua_block: block
            rc = E_EMPTY
            if (lua_getfield(lua%ptr, -1, name) <= 0) exit lua_block

            rc = E_TYPE
            if (lua_isboolean(lua%ptr, -1) /= 1) exit lua_block

            rc = E_NONE
            value = lua_toboolean(lua%ptr, -1)
        end block lua_block

        call lua_pop(lua%ptr, 1)
    end function lua_field_logical

    integer function lua_field_real64(lua, name, value) result(rc)
        !! Returns 8-byte real from table field `name` in `value`.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_EMPTY` if the field of given name is null.
        !! * `E_TYPE` if the field is not of type number.
        !!
        !! On error, `value` will not be overwritten.
        type(lua_state_type), intent(inout) :: lua   !! Lua type.
        character(len=*),     intent(in)    :: name  !! Table field name.
        real(kind=r8),        intent(inout) :: value !! Table field value.

        lua_block: block
            rc = E_EMPTY
            if (lua_getfield(lua%ptr, -1, name) <= 0) exit lua_block

            rc = E_TYPE
            if (lua_isnumber(lua%ptr, -1) /= 1) exit lua_block

            rc = E_NONE
            value = lua_tonumber(lua%ptr, -1)
        end block lua_block

        call lua_pop(lua%ptr, 1)
    end function lua_field_real64

    integer function lua_field_stack(lua, name) result(rc)
        !! Pushes table field of given name on stack.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_EMPTY` if the field of given name is null.
        !!
        type(lua_state_type), intent(inout) :: lua  !! Lua type.
        character(len=*),     intent(in)    :: name !! Field name.

        rc = E_EMPTY
        if (lua_getfield(lua%ptr, -1, name) == LUA_TNIL) return
        rc = E_NONE
    end function lua_field_stack

    integer function lua_field_string(lua, name, value, unescape) result(rc)
        !! Returns character string from table field `name` in `value`. If
        !! `unescape` is passed and `.true.`, the returned string will have all
        !! occurences of `\\` replaced by `\`.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_EMPTY` if the field of given name is null.
        !! * `E_LIMIT` if the actual string length is greater than the length of
        !!    the passed value argument.
        !! * `E_TYPE` if the field is not of type string.
        !!
        !! On error, `value` will not be overwritten.
        type(lua_state_type), intent(inout)        :: lua      !! Lua type.
        character(len=*),     intent(in)           :: name     !! Table field name.
        character(len=*),     intent(inout)        :: value    !! Table field value.
        logical,              intent(in), optional :: unescape !! Unescape the string.

        character(len=:), allocatable :: str
        logical                       :: unescape_

        unescape_ = .false.
        if (present(unescape)) unescape_ = unescape

        lua_block: block
            rc = E_EMPTY
            if (lua_getfield(lua%ptr, -1, name) <= 0) exit lua_block

            rc = E_TYPE
            if (lua_isstring(lua%ptr, -1) /= 1) exit lua_block

            str = lua_tostring(lua%ptr, -1)

            if (unescape_) then
                value = dm_lua_unescape(str)
            else
                value = str
            end if

            rc = E_LIMIT
            if (len(str) > len(value)) exit lua_block

            rc = E_NONE
        end block lua_block

        call lua_pop(lua%ptr, 1)
    end function lua_field_string

    integer function lua_get_int32(lua, i, value) result(rc)
        !! Returns 4-byte integer from table element `i` in `value`.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_EMPTY` if the table element of given name is null.
        !! * `E_INVALID` if the element on top of the stack is not a table.
        !! * `E_TYPE` if the table element is not of type integer.
        !!
        !! On error, `value` will not be overwritten.
        type(lua_state_type), intent(inout) :: lua   !! Lua type.
        integer,              intent(in)    :: i     !! Variable index.
        integer,              intent(inout) :: value !! Variable value.

        lua_block: block
            rc = E_INVALID
            if (lua_istable(lua%ptr, -1) == 0) return

            rc = E_EMPTY
            if (lua_rawgeti(lua%ptr, -1, int(i, kind=lua_integer)) == LUA_TNIL) exit lua_block

            rc = E_TYPE
            if (lua_isinteger(lua%ptr, -1) /= 1) exit lua_block

            rc = E_NONE
            value = int(lua_tointeger(lua%ptr, -1), kind=i4)
        end block lua_block

        call lua_pop(lua%ptr, 1)
    end function lua_get_int32

    integer function lua_get_int64(lua, i, value) result(rc)
        !! Returns 8-byte integer from table element `i` in `value`.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_EMPTY` if the table element of given name is null.
        !! * `E_INVALID` if the element on top of the stack is not a table.
        !! * `E_TYPE` if the table element is not of type integer.
        !!
        !! On error, `value` will not be overwritten.
        type(lua_state_type), intent(inout) :: lua   !! Lua type.
        integer,              intent(in)    :: i     !! Variable index.
        integer(kind=i8),     intent(inout) :: value !! Variable value.

        lua_block: block
            rc = E_INVALID
            if (lua_istable(lua%ptr, -1) == 0) return

            rc = E_EMPTY
            if (lua_rawgeti(lua%ptr, -1, int(i, kind=lua_integer)) == LUA_TNIL) exit lua_block

            rc = E_TYPE
            if (lua_isinteger(lua%ptr, -1) /= 1) exit lua_block

            rc = E_NONE
            value = lua_tointeger(lua%ptr, -1)
        end block lua_block

        call lua_pop(lua%ptr, 1)
    end function lua_get_int64

    integer function lua_get_logical(lua, i, value) result(rc)
        !! Returns logical from table element `i` in `value`.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_EMPTY` if the table element of given name is null.
        !! * `E_INVALID` if the element on top of the stack is not a table.
        !! * `E_TYPE` if the table element is not of type boolean.
        !!
        !! On error, `value` will not be overwritten.
        type(lua_state_type), intent(inout) :: lua   !! Lua type.
        integer,              intent(in)    :: i     !! Variable index.
        logical,              intent(inout) :: value !! Variable value.

        lua_block: block
            rc = E_INVALID
            if (lua_istable(lua%ptr, -1) == 0) return

            rc = E_EMPTY
            if (lua_rawgeti(lua%ptr, -1, int(i, kind=lua_integer)) == LUA_TNIL) exit lua_block

            rc = E_TYPE
            if (lua_isboolean(lua%ptr, -1) /= 1) exit lua_block

            rc = E_NONE
            value = lua_toboolean(lua%ptr, -1)
        end block lua_block

        call lua_pop(lua%ptr, 1)
    end function lua_get_logical

    integer function lua_get_real64(lua, i, value) result(rc)
        !! Returns 8-byte real from table element `i` in `value`.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_EMPTY` if the table element of given name is null.
        !! * `E_INVALID` if the element on top of the stack is not a table.
        !! * `E_TYPE` if the table element is not of type number.
        !!
        !! On error, `value` will not be overwritten.
        type(lua_state_type), intent(inout) :: lua   !! Lua type.
        integer,              intent(in)    :: i     !! Variable index.
        real(kind=r8),        intent(inout) :: value !! Variable value.

        lua_block: block
            rc = E_INVALID
            if (lua_istable(lua%ptr, -1) == 0) return

            rc = E_EMPTY
            if (lua_rawgeti(lua%ptr, -1, int(i, kind=lua_integer)) == LUA_TNIL) exit lua_block

            rc = E_TYPE
            if (lua_isnumber(lua%ptr, -1) /= 1) exit lua_block

            rc = E_NONE
            value = lua_tonumber(lua%ptr, -1)
        end block lua_block

        call lua_pop(lua%ptr, 1)
    end function lua_get_real64

    integer function lua_get_stack(lua, i) result(rc)
        !! Pushes table element at index `i` on stack.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_INVALID` if the element on top of the stack is not a table.
        !! * `E_EMPTY` if the table is empty.
        !!
        type(lua_state_type), intent(inout) :: lua !! Lua type.
        integer,              intent(in)    :: i   !! Variable index.

        rc = E_INVALID
        if (lua_istable(lua%ptr, -1) == 0) return
        rc = E_EMPTY
        if (lua_rawgeti(lua%ptr, -1, int(i, kind=lua_integer)) == LUA_TNIL) return
        rc = E_NONE
    end function lua_get_stack

    integer function lua_get_string(lua, i, value, unescape) result(rc)
        !! Returns character string from table element `i` in `value`.  If
        !! `unescape` is passed and `.true.`, the returned string will have all
        !! occurences of `\\` replaced by `\`.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_EMPTY` if the table element of given name is null.
        !! * `E_INVALID` if the element on top of the stack is not a table.
        !! * `E_LIMIT` if the actual string length is greater than the length of
        !!    the passed value argument.
        !! * `E_TYPE` if the table element is not of type string.
        !!
        !! On error, `value` will not be overwritten.
        type(lua_state_type), intent(inout)        :: lua      !! Lua type.
        integer,              intent(in)           :: i        !! Variable index.
        character(len=*),     intent(inout)        :: value    !! Variable value.
        logical,              intent(in), optional :: unescape !! Unescape string.

        character(len=:), allocatable :: str
        logical                       :: unescape_

        unescape_ = .false.
        if (present(unescape)) unescape_ = unescape

        lua_block: block
            rc = E_INVALID
            if (lua_istable(lua%ptr, -1) == 0) return

            rc = E_EMPTY
            if (lua_rawgeti(lua%ptr, -1, int(i, kind=lua_integer)) == LUA_TNIL) exit lua_block

            rc = E_TYPE
            if (lua_isstring(lua%ptr, -1) /= 1) exit lua_block

            str = lua_tostring(lua%ptr, -1)

            if (unescape_) then
                value = dm_lua_unescape(str)
            else
                value = str
            end if

            rc = E_LIMIT
            if (len(str) > len(value)) exit lua_block

            rc = E_NONE
        end block lua_block

        call lua_pop(lua%ptr, 1)
    end function lua_get_string

    integer function lua_read_array_int32(lua, name, values) result(rc)
        !! Returns the values of global variable as 4-byte integers.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_ALLOC` if array allocation failed.
        !! * `E_TYPE` if variable is not an integer table.
        !!
        type(lua_state_type),          intent(inout) :: lua       !! Lua type.
        character(len=*),              intent(in)    :: name      !! Variable name.
        integer(kind=i4), allocatable, intent(out)   :: values(:) !! Variable values.

        lua_block: block
            integer :: i, n, stat

            rc = E_TYPE
            if (lua_getglobal(lua%ptr, name) /= LUA_TTABLE) exit lua_block

            n = dm_lua_table_size(lua)

            rc = E_ALLOC
            allocate (values(n), stat=stat)
            if (stat /= 0) exit lua_block

            do i = 1, n
                rc = dm_lua_get(lua, i, values(i))
                if (dm_is_error(rc)) exit lua_block
            end do

            rc = E_NONE
        end block lua_block

        call lua_pop(lua%ptr, 1)
        if (.not. allocated(values)) allocate (values(0))
    end function lua_read_array_int32

    integer function lua_read_array_int64(lua, name, values) result(rc)
        !! Returns the values of global variable as 8-byte integers.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_ALLOC` if array allocation failed.
        !! * `E_TYPE` if variable is not an integer table.
        !!
        type(lua_state_type),          intent(inout) :: lua       !! Lua type.
        character(len=*),              intent(in)    :: name      !! Variable name.
        integer(kind=i8), allocatable, intent(out)   :: values(:) !! Variable values.

        lua_block: block
            integer :: i, n, stat

            rc = E_TYPE
            if (lua_getglobal(lua%ptr, name) /= LUA_TTABLE) exit lua_block

            n = dm_lua_table_size(lua)

            rc = E_ALLOC
            allocate (values(n), stat=stat)
            if (stat /= 0) exit lua_block

            do i = 1, n
                rc = dm_lua_get(lua, i, values(i))
                if (dm_is_error(rc)) exit lua_block
            end do

            rc = E_NONE
        end block lua_block

        call lua_pop(lua%ptr, 1)
        if (.not. allocated(values)) allocate (values(0))
    end function lua_read_array_int64

    integer function lua_read_int32(lua, name, value) result(rc)
        !! Returns the value of global variable as 4-byte integer. The
        !! function returns `E_TYPE` if the variable is not of type integer.
        type(lua_state_type), intent(inout) :: lua   !! Lua type.
        character(len=*),     intent(in)    :: name  !! Variable name.
        integer(kind=i4),     intent(inout) :: value !! Variable value.

        rc = E_TYPE
        if (lua_getglobal(lua%ptr, name) == LUA_TNUMBER) then
            value = int(lua_tointeger(lua%ptr, -1), kind=i4)
            rc = E_NONE
        end if
        call lua_pop(lua%ptr, 1)
    end function lua_read_int32

    integer function lua_read_int64(lua, name, value) result(rc)
        !! Returns the value of global variable as 8-byte integer. The
        !! function returns `E_TYPE` if the variable is not of type integer.
        type(lua_state_type), intent(inout) :: lua   !! Lua type.
        character(len=*),     intent(in)    :: name  !! Variable name.
        integer(kind=i8),     intent(inout) :: value !! Variable value.

        rc = E_TYPE
        if (lua_getglobal(lua%ptr, name) == LUA_TNUMBER) then
            value = lua_tointeger(lua%ptr, -1)
            rc = E_NONE
        end if
        call lua_pop(lua%ptr, 1)
    end function lua_read_int64

    integer function lua_read_logical(lua, name, value) result(rc)
        !! Returns the value of global variable as logical. The function
        !! returns `E_TYPE` if the variable is not of type boolean.
        type(lua_state_type), intent(inout) :: lua   !! Lua type.
        character(len=*),     intent(in)    :: name  !! Variable name.
        logical,              intent(inout) :: value !! Variable value.

        rc = E_TYPE
        if (lua_getglobal(lua%ptr, name) == LUA_TBOOLEAN) then
            value = lua_toboolean(lua%ptr, -1)
            rc = E_NONE
        end if
        call lua_pop(lua%ptr, 1)
    end function lua_read_logical

    integer function lua_read_real64(lua, name, value) result(rc)
        !! Returns the value of global variable as 8-byte real. The function
        !! returns `E_TYPE` if the variable is not of type number.
        type(lua_state_type), intent(inout) :: lua   !! Lua type.
        character(len=*),     intent(in)    :: name  !! Variable name.
        real(kind=r8),        intent(inout) :: value !! Variable value.

        rc = E_TYPE
        if (lua_getglobal(lua%ptr, name) == LUA_TNUMBER) then
            value = lua_tonumber(lua%ptr, -1)
            rc = E_NONE
        end if
        call lua_pop(lua%ptr, 1)
    end function lua_read_real64

    integer function lua_read_stack(lua, name) result(rc)
        !! Pushes global variable on stack. Returns `E_EMPTY` if the variable
        !! does not exist.
        type(lua_state_type), intent(inout) :: lua  !! Lua type.
        character(len=*),     intent(in)    :: name !! Variable name.

        rc = E_EMPTY
        if (lua_getglobal(lua%ptr, name) <= 0) return
        rc = E_NONE
    end function lua_read_stack

    integer function lua_read_string(lua, name, value) result(rc)
        !! Returns the value of global variable as allocatable string. The
        !! function returns `E_TYPE` if the variable is not of type string.
        type(lua_state_type), intent(inout) :: lua   !! Lua type.
        character(len=*),     intent(in)    :: name  !! Variable name.
        character(len=*),     intent(inout) :: value !! Variable value.

        rc = E_TYPE
        if (lua_getglobal(lua%ptr, name) == LUA_TSTRING) then
            value = lua_tostring(lua%ptr, -1)
            rc = E_NONE
        end if
        call lua_pop(lua%ptr, 1)
    end function lua_read_string

    integer function lua_set_int32(lua, name, value) result(rc)
        !! Sets 4-byte integer variable of given name.
        type(lua_state_type), intent(inout) :: lua   !! Lua type.
        character(len=*),     intent(in)    :: name  !! Name of variable
        integer(kind=i4),     intent(in)    :: value !! Value of variable.

        rc = dm_lua_eval(lua, name // ' = ' // dm_itoa(value))
    end function lua_set_int32

    integer function lua_to_job(lua, job) result(rc)
        !! Reads Lua table into Fortran job type. The table has to be on top of
        !! the stack and will be removed once finished.
        use :: dm_job

        type(lua_state_type), intent(inout) :: lua !! Lua type.
        type(job_type),       intent(out)   :: job !! Job type.

        lua_block: block
            rc = E_TYPE
            if (.not. dm_lua_is_table(lua)) exit lua_block

            ! Ignore error codes, just assume defaults if missing.
            rc = dm_lua_field(lua, 'delay',    job%delay)
            rc = dm_lua_field(lua, 'disabled', job%disabled)
            rc = dm_lua_field(lua, 'onetime',  job%onetime)
            rc = dm_lua_field(lua, 'observation')

            rc = lua_to_observ(lua, job%observ)
            job%valid = (rc == E_NONE)
        end block lua_block

        call dm_lua_pop(lua)
        rc = E_NONE
    end function lua_to_job

    integer function lua_to_job_list(lua, job_list) result(rc)
        !! Reads Lua table into Fortran job list. The table has to be on
        !! top of the stack and will be removed once finished.
        use :: dm_job

        type(lua_state_type), intent(inout) :: lua      !! Lua type.
        type(job_list_type),  intent(out)   :: job_list !! Job list type.

        integer        :: i, sz
        type(job_type) :: job

        lua_block: block
            rc = E_TYPE
            if (.not. dm_lua_is_table(lua)) exit lua_block

            sz = dm_lua_table_size(lua)
            rc = dm_job_list_init(job_list, sz)
            if (dm_is_error(rc)) exit lua_block

            rc = E_EMPTY
            if (sz == 0) exit lua_block

            do i = 1, sz
                rc = dm_lua_get(lua, i)
                rc = dm_lua_to(lua, job)
                if (dm_is_error(rc)) exit lua_block
                rc = dm_job_list_add(job_list, job)
            end do

            rc = E_NONE
        end block lua_block

        call dm_lua_pop(lua)
    end function lua_to_job_list

    integer function lua_to_jobs(lua, jobs) result(rc)
        !! Reads Lua table into Fortran jobs type array. The table has to be on
        !! top of the stack and will be removed once finished.
        !!
        !! The functions returns the following error codes:
        !!
        !! * `E_ALLOC` if the array allocation failed.
        !! * `E_EMPTY` if the table is empty.
        !! * `E_TYPE` if the stack element is not a table.
        !!
        use :: dm_job

        type(lua_state_type),        intent(inout) :: lua     !! Lua type.
        type(job_type), allocatable, intent(out)   :: jobs(:) !! Job type array.

        integer :: i, stat, sz

        lua_block: block
            rc = E_TYPE
            if (.not. dm_lua_is_table(lua)) exit lua_block

            rc = E_ALLOC
            sz = dm_lua_table_size(lua)
            allocate (jobs(sz), stat=stat)
            if (stat /= 0) exit lua_block

            rc = E_EMPTY
            if (sz == 0) exit lua_block

            do i = 1, sz
                rc = dm_lua_get(lua, i)
                rc = lua_to_job(lua, jobs(i))
                if (dm_is_error(rc)) exit lua_block
            end do

            rc = E_NONE
        end block lua_block

        call dm_lua_pop(lua)
    end function lua_to_jobs

    integer function lua_to_observ(lua, observ) result(rc)
        !! Reads Lua table into Fortran observation type. The table has to be on
        !! top of the stack and will be removed once finished.
        use :: dm_observ

        type(lua_state_type), intent(inout) :: lua    !! Lua type.
        type(observ_type),    intent(out)   :: observ !! Observation type.

        integer :: i, nrec, nreq, sz

        observ_block: block
            rc = E_TYPE
            if (.not. dm_lua_is_table(lua)) exit observ_block

            ! Read observation attributes.
            rc = dm_lua_field(lua, 'node_id',    observ%node_id)
            rc = dm_lua_field(lua, 'sensor_id',  observ%sensor_id)
            rc = dm_lua_field(lua, 'target_id',  observ%target_id)
            rc = dm_lua_field(lua, 'id',         observ%id)
            rc = dm_lua_field(lua, 'name',       observ%name)
            rc = dm_lua_field(lua, 'timestamp',  observ%timestamp)
            rc = dm_lua_field(lua, 'source',     observ%source)
            rc = dm_lua_field(lua, 'device',     observ%device, unescape=.true.)
            rc = dm_lua_field(lua, 'priority',   observ%priority)
            rc = dm_lua_field(lua, 'error',      observ%error)
            rc = dm_lua_field(lua, 'next',       observ%next)
            rc = dm_lua_field(lua, 'nreceivers', observ%nreceivers)
            rc = dm_lua_field(lua, 'nrequests',  observ%nrequests)

            ! Read receivers.
            rc = dm_lua_field(lua, 'receivers')
            sz = dm_lua_table_size(lua)

            nrec = min(OBSERV_MAX_NRECEIVERS, int(sz))
            observ%nreceivers = nrec

            do i = 1, nrec
                rc = dm_lua_get(lua, i, observ%receivers(i))
                if (dm_is_error(rc)) exit
            end do

            call dm_lua_pop(lua) ! receivers

            ! Read requests.
            rc = dm_lua_field(lua, 'requests')
            sz = dm_lua_table_size(lua)

            nreq = min(OBSERV_MAX_NREQUESTS, int(sz))
            observ%nrequests = nreq

            req_loop: do i = 1, nreq
                ! Load element.
                rc = dm_lua_get(lua, i)
                if (dm_is_error(rc)) exit req_loop
                ! Read element.
                rc = lua_to_request(lua, observ%requests(i))
                if (dm_is_error(rc)) exit req_loop
            end do req_loop

            call dm_lua_pop(lua)
        end block observ_block

        call dm_lua_pop(lua)
    end function lua_to_observ

    integer function lua_to_observs(lua, observs) result(rc)
        !! Reads Lua table into Fortran observation type array. The table has to
        !! be on top of the stack and will be removed once finished.
        use :: dm_observ

        type(lua_state_type),           intent(inout) :: lua        !! Lua type.
        type(observ_type), allocatable, intent(out)   :: observs(:) !! Observation type array.

        integer :: i, stat, sz

        lua_block: block
            rc = E_TYPE
            if (.not. dm_lua_is_table(lua)) exit lua_block

            rc = E_ALLOC
            sz = dm_lua_table_size(lua)
            allocate (observs(sz), stat=stat)
            if (stat /= 0) exit lua_block

            rc = E_EMPTY
            if (sz == 0) exit lua_block

            do i = 1, sz
                ! Load element.
                rc = dm_lua_get(lua, i)
                if (dm_is_error(rc)) exit lua_block
                ! Read element.
                rc = lua_to_observ(lua, observs(i))
                if (dm_is_error(rc)) exit lua_block
            end do

            rc = E_NONE
        end block lua_block

        call dm_lua_pop(lua)
    end function lua_to_observs

    integer function lua_to_report(lua, report) result(rc)
        !! Reads Lua table into Fortran report type. The table has to
        !! be on top of the stack and will be removed once finished.
        use :: dm_report

        type(lua_state_type), intent(inout) :: lua    !! Lua type.
        type(report_type),    intent(out)   :: report !! Report type.

        integer :: i, sz, stat

        lua_block: block
            rc = E_TYPE
            if (.not. dm_lua_is_table(lua)) exit lua_block

            rc = dm_lua_field(lua, 'node',     report%node)
            rc = dm_lua_field(lua, 'from',     report%from)
            rc = dm_lua_field(lua, 'to',       report%to)
            rc = dm_lua_field(lua, 'output',   report%output)
            rc = dm_lua_field(lua, 'title',    report%title)
            rc = dm_lua_field(lua, 'subtitle', report%subtitle)
            rc = dm_lua_field(lua, 'meta',     report%meta)
            rc = dm_lua_field(lua, 'style',    report%style)

            ! Plots table.
            plots_block: block
                rc = dm_lua_field(lua, 'plots')

                if (dm_is_error(rc)) then
                    report%plot%disabled = .true.
                    exit plots_block
                end if

                rc = dm_lua_field(lua, 'disabled', report%plot%disabled)
                rc = dm_lua_field(lua, 'database', report%plot%database)
                rc = dm_lua_field(lua, 'title',    report%plot%title)
                rc = dm_lua_field(lua, 'meta',     report%plot%meta)
                rc = dm_lua_field(lua, 'observations')

                if (dm_is_ok(rc)) then
                    rc = E_ALLOC
                    sz = dm_lua_table_size(lua)

                    allocate (report%plot%observs(sz), stat=stat)

                    if (stat /= 0) then
                        call dm_lua_pop(lua)
                        exit plots_block
                    end if

                    observs_loop: do i = 1, sz
                        rc = dm_lua_get(lua, i)
                        if (dm_is_error(rc)) exit observs_loop

                        rc = dm_lua_field(lua, 'format',   report%plot%observs(i)%format)
                        rc = dm_lua_field(lua, 'sensor',   report%plot%observs(i)%sensor)
                        rc = dm_lua_field(lua, 'target',   report%plot%observs(i)%target)
                        rc = dm_lua_field(lua, 'response', report%plot%observs(i)%response)
                        rc = dm_lua_field(lua, 'unit',     report%plot%observs(i)%unit)
                        rc = dm_lua_field(lua, 'title',    report%plot%observs(i)%title)
                        rc = dm_lua_field(lua, 'subtitle', report%plot%observs(i)%subtitle)
                        rc = dm_lua_field(lua, 'meta',     report%plot%observs(i)%meta)
                        rc = dm_lua_field(lua, 'color',    report%plot%observs(i)%color)
                        rc = dm_lua_field(lua, 'width',    report%plot%observs(i)%width)
                        rc = dm_lua_field(lua, 'height',   report%plot%observs(i)%height)

                        call dm_to_lower(report%plot%observs(i)%format)
                        call dm_lua_pop(lua) ! table element
                    end do observs_loop

                    call dm_lua_pop(lua) ! table
                end if

                call dm_lua_pop(lua) ! table
                rc = E_NONE
            end block plots_block

            if (dm_is_error(rc)) exit lua_block

            ! Logs table.
            logs_block: block
                rc = dm_lua_field(lua, 'logs')

                if (dm_is_error(rc)) then
                    report%log%disabled = .true.
                    exit logs_block
                end if

                rc = dm_lua_field(lua, 'disabled', report%log%disabled)
                rc = dm_lua_field(lua, 'minlevel', report%log%min_level)
                rc = dm_lua_field(lua, 'maxlevel', report%log%max_level)
                rc = dm_lua_field(lua, 'database', report%log%database)
                rc = dm_lua_field(lua, 'title',    report%log%title)
                rc = dm_lua_field(lua, 'meta',     report%log%meta)

                call dm_lua_pop(lua) ! table
            end block logs_block

            rc = E_NONE
        end block lua_block

        call dm_lua_pop(lua) ! table
    end function lua_to_report

    integer function lua_to_request(lua, request) result(rc)
        !! Reads Lua table into Fortran request type. The table has to be on
        !! top of the stack and will be removed once finished.
        use :: dm_request

        type(lua_state_type), intent(inout) :: lua     !! Lua type.
        type(request_type),   intent(out)   :: request !! Request type.

        integer :: i, n, sz

        request_block: block
            rc = E_TYPE
            if (.not. dm_lua_is_table(lua)) exit request_block

            rc = dm_lua_field(lua, 'name',       request%name)
            rc = dm_lua_field(lua, 'timestamp',  request%timestamp)
            rc = dm_lua_field(lua, 'request',    request%request,   unescape=.true.)
            rc = dm_lua_field(lua, 'response',   request%response,  unescape=.true.)
            rc = dm_lua_field(lua, 'delimiter',  request%delimiter, unescape=.true.)
            rc = dm_lua_field(lua, 'pattern',    request%pattern,   unescape=.true.)
            rc = dm_lua_field(lua, 'delay',      request%delay)
            rc = dm_lua_field(lua, 'error',      request%error)
            rc = dm_lua_field(lua, 'mode',       request%mode)
            rc = dm_lua_field(lua, 'retries',    request%retries)
            rc = dm_lua_field(lua, 'state',      request%state)
            rc = dm_lua_field(lua, 'timeout',    request%timeout)
            rc = dm_lua_field(lua, 'nresponses', request%nresponses)

            ! Read responses.
            rc = dm_lua_field(lua, 'responses')
            sz = dm_lua_table_size(lua)

            n = min(REQUEST_MAX_NRESPONSES, int(sz))
            request%nresponses = n

            res_loop: do i = 1, n
                rc = dm_lua_get(lua, i)
                if (dm_is_error(rc)) exit res_loop

                rc = dm_lua_field(lua, 'name',  request%responses(i)%name)
                rc = dm_lua_field(lua, 'unit',  request%responses(i)%unit)
                rc = dm_lua_field(lua, 'type',  request%responses(i)%type)
                rc = dm_lua_field(lua, 'error', request%responses(i)%error)
                rc = dm_lua_field(lua, 'value', request%responses(i)%value)

                call dm_lua_pop(lua) ! table element
            end do res_loop

            call dm_lua_pop(lua) ! responses
            rc = E_NONE
        end block request_block

        call dm_lua_pop(lua) ! table
    end function lua_to_request

    ! ******************************************************************
    ! PRIVATE SUBROUTINES.
    ! ******************************************************************
    subroutine lua_from_observ(lua, observ)
        !! Pushes observation on Lua stack.
        use :: dm_observ

        type(lua_state_type), intent(inout) :: lua    !! Lua type.
        type(observ_type),    intent(inout) :: observ !! Observation type.

        integer     :: i
        type(c_ptr) :: ptr

        call lua_createtable(lua%ptr, 0, 15)

        ptr = lua_pushstring(lua%ptr, trim(observ%node_id))
        call lua_setfield(lua%ptr, -2, 'node_id')

        ptr = lua_pushstring(lua%ptr, trim(observ%sensor_id))
        call lua_setfield(lua%ptr, -2, 'sensor_id')

        ptr = lua_pushstring(lua%ptr, trim(observ%target_id))
        call lua_setfield(lua%ptr, -2, 'target_id')

        ptr = lua_pushstring(lua%ptr, trim(observ%id))
        call lua_setfield(lua%ptr, -2, 'id')

        ptr = lua_pushstring(lua%ptr, trim(observ%name))
        call lua_setfield(lua%ptr, -2, 'name')

        ptr = lua_pushstring(lua%ptr, trim(observ%timestamp))
        call lua_setfield(lua%ptr, -2, 'timestamp')

        ptr = lua_pushstring(lua%ptr, trim(observ%source))
        call lua_setfield(lua%ptr, -2, 'source')

        ptr = lua_pushstring(lua%ptr, dm_lua_escape(observ%device))
        call lua_setfield(lua%ptr, -2, 'device')

        call lua_pushinteger(lua%ptr, int(observ%priority, kind=lua_integer))
        call lua_setfield(lua%ptr, -2, 'priority')

        call lua_pushinteger(lua%ptr, int(observ%error, kind=lua_integer))
        call lua_setfield(lua%ptr, -2, 'error')

        call lua_pushinteger(lua%ptr, int(observ%next, kind=lua_integer))
        call lua_setfield(lua%ptr, -2, 'next')

        call lua_pushinteger(lua%ptr, int(observ%nreceivers, kind=lua_integer))
        call lua_setfield(lua%ptr, -2, 'nreceivers')

        call lua_pushinteger(lua%ptr, int(observ%nrequests, kind=lua_integer))
        call lua_setfield(lua%ptr, -2, 'nrequests')

        ! Receivers.
        call lua_createtable(lua%ptr, observ%nreceivers, 0)

        do i = 1, observ%nreceivers
            call lua_pushinteger(lua%ptr, int(i, kind=lua_integer))
            ptr = lua_pushstring(lua%ptr, trim(observ%receivers(i)))
            call lua_settable(lua%ptr, -3)
        end do

        call lua_setfield(lua%ptr, -2, 'receivers')

        ! Requests.
        call lua_createtable(lua%ptr, observ%nrequests, 0)

        do i = 1, observ%nrequests
            call lua_pushinteger(lua%ptr, int(i, kind=lua_integer))
            call lua_from_request(lua, observ%requests(i))
            call lua_settable(lua%ptr, -3)
        end do

        call lua_setfield(lua%ptr, -2, 'requests')
    end subroutine lua_from_observ

    subroutine lua_from_request(lua, request)
        !! Pushes request on Lua stack.
        use :: dm_request

        type(lua_state_type), intent(inout) :: lua     !! Lua type.
        type(request_type),   intent(inout) :: request !! Request type.

        integer     :: i
        type(c_ptr) :: ptr

        call lua_createtable(lua%ptr, 0, 14)

        ptr = lua_pushstring(lua%ptr, trim(request%name))
        call lua_setfield(lua%ptr, -2, 'name')

        ptr = lua_pushstring(lua%ptr, trim(request%timestamp))
        call lua_setfield(lua%ptr, -2, 'timestamp')

        ptr = lua_pushstring(lua%ptr, dm_lua_escape(request%request))
        call lua_setfield(lua%ptr, -2, 'request')

        ptr = lua_pushstring(lua%ptr, dm_lua_escape(request%response))
        call lua_setfield(lua%ptr, -2, 'response')

        ptr = lua_pushstring(lua%ptr, dm_lua_escape(request%delimiter))
        call lua_setfield(lua%ptr, -2, 'delimiter')

        ptr = lua_pushstring(lua%ptr, dm_lua_escape(request%pattern))
        call lua_setfield(lua%ptr, -2, 'pattern')

        call lua_pushinteger(lua%ptr, int(request%delay, kind=lua_integer))
        call lua_setfield(lua%ptr, -2, 'delay')

        call lua_pushinteger(lua%ptr, int(request%error, kind=lua_integer))
        call lua_setfield(lua%ptr, -2, 'error')

        call lua_pushinteger(lua%ptr, int(request%mode, kind=lua_integer))
        call lua_setfield(lua%ptr, -2, 'mode')

        call lua_pushinteger(lua%ptr, int(request%retries, kind=lua_integer))
        call lua_setfield(lua%ptr, -2, 'retries')

        call lua_pushinteger(lua%ptr, int(request%state, kind=lua_integer))
        call lua_setfield(lua%ptr, -2, 'state')

        call lua_pushinteger(lua%ptr, int(request%timeout, kind=lua_integer))
        call lua_setfield(lua%ptr, -2, 'timeout')

        call lua_pushinteger(lua%ptr, int(request%nresponses, kind=lua_integer))
        call lua_setfield(lua%ptr, -2, 'nresponses')

        ! Responses.
        call lua_createtable(lua%ptr, request%nresponses, 0)

        do i = 1, request%nresponses
            call lua_pushinteger(lua%ptr, int(i, kind=lua_integer))
            call lua_createtable(lua%ptr, 0, 5)

            ptr = lua_pushstring(lua%ptr, trim(request%responses(i)%name))
            call lua_setfield(lua%ptr, -2, 'name')

            ptr = lua_pushstring(lua%ptr, trim(request%responses(i)%unit))
            call lua_setfield(lua%ptr, -2, 'unit')

            call lua_pushinteger(lua%ptr, int(request%responses(i)%type, kind=lua_integer))
            call lua_setfield(lua%ptr, -2, 'type')

            call lua_pushinteger(lua%ptr, int(request%responses(i)%error, kind=lua_integer))
            call lua_setfield(lua%ptr, -2, 'error')

            call lua_pushnumber(lua%ptr, request%responses(i)%value)
            call lua_setfield(lua%ptr, -2, 'value')

            call lua_settable(lua%ptr, -3)
        end do

        call lua_setfield(lua%ptr, -2, 'responses')
    end subroutine lua_from_request
end module dm_lua
