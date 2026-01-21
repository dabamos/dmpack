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

    type, public :: lua_state_type
        !! Lua state type that stores the Lua pointer.
        type(c_ptr) :: ctx = c_null_ptr !! C pointer to Lua interpreter.
    end type lua_state_type

    abstract interface
        ! int *lua_CFunction(lua_State *L)
        function dm_lua_callback(ptr) bind(c)
            !! C-interoperable Lua callback function.
            import :: c_int, c_ptr
            implicit none
            type(c_ptr), intent(in), value :: ptr             !! Lua state pointer.
            integer(c_int)                 :: dm_lua_callback !! Return value.
        end function dm_lua_callback
    end interface

    interface dm_lua_field
        !! Pushes table element on stack and optionally returns value.
        module procedure :: lua_field_array_int32
        module procedure :: lua_field_array_int64
        module procedure :: lua_field_array_string
        module procedure :: lua_field_int16
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
        module procedure :: lua_from_response
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
        module procedure :: lua_to_group
        module procedure :: lua_to_job
        module procedure :: lua_to_job_list
        module procedure :: lua_to_jobs
        module procedure :: lua_to_observ
        module procedure :: lua_to_observs
        module procedure :: lua_to_report
    end interface dm_lua_to

    ! Public abstract interfaces.
    public :: dm_lua_callback

    ! Public procedures.
    public :: dm_lua_call
    public :: dm_lua_destroy
    public :: dm_lua_dump_stack
    public :: dm_lua_error
    public :: dm_lua_error_message
    public :: dm_lua_escape
    public :: dm_lua_eval
    public :: dm_lua_exec
    public :: dm_lua_field
    public :: dm_lua_from
    public :: dm_lua_get
    public :: dm_lua_init
    public :: dm_lua_is_function
    public :: dm_lua_is_nil
    public :: dm_lua_is_opened
    public :: dm_lua_is_table
    public :: dm_lua_open
    public :: dm_lua_pop
    public :: dm_lua_read
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
    private :: lua_field_array_string
    private :: lua_field_int32
    private :: lua_field_int64
    private :: lua_field_logical
    private :: lua_field_real64
    private :: lua_field_stack
    private :: lua_field_string

    private :: lua_from_observ
    private :: lua_from_response

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

    private :: lua_to_group
    private :: lua_to_job
    private :: lua_to_job_list
    private :: lua_to_jobs
    private :: lua_to_observ
    private :: lua_to_observs
    private :: lua_to_report
contains
    ! **************************************************************************
    ! PUBLIC FUNCTIONS.
    ! **************************************************************************
    integer function dm_lua_call(lua, nargs, nresults) result(rc)
        !! Calls Lua function on top of stack.
        type(lua_state_type), intent(inout) :: lua      !! Lua state.
        integer,              intent(in)    :: nargs    !! Number of arguments.
        integer,              intent(in)    :: nresults !! Number of results.

        rc = E_NULL
        if (.not. c_associated(lua%ctx)) return
        rc = dm_lua_error(lua_pcall(lua%ctx, nargs, nresults, 0))
    end function dm_lua_call

    integer function dm_lua_error(lua_error) result(rc)
        !! Converts Lua error code to DMPACK error code.
        integer, intent(in) :: lua_error !! Lua error code.

        select case (lua_error)
            case (LUA_OK);        rc = E_NONE
            case (LUA_YIELD);     rc = E_LUA_YIELD
            case (LUA_ERRRUN);    rc = E_LUA_RUNTIME
            case (LUA_ERRSYNTAX); rc = E_LUA_SYNTAX
            case (LUA_ERRMEM);    rc = E_LUA_MEM
            case (LUA_ERRERR);    rc = E_LUA_ERROR
            case (LUA_ERRFILE);   rc = E_LUA_FILE
            case default;         rc = E_LUA
        end select
    end function dm_lua_error

    function dm_lua_error_message(lua) result(message)
        !! Returns last error message as allocatable character string or an
        !! empty string if no message is available.
        type(lua_state_type), intent(inout) :: lua     !! Lua state.
        character(:), allocatable           :: message !! Last error message.

        lua_block: block
            if (.not. c_associated(lua%ctx))    exit lua_block
            if (lua_isstring(lua%ctx, -1) == 0) exit lua_block
            message = lua_tostring(lua%ctx, -1)
            return
        end block lua_block

        if (.not. allocated(message)) message = ''
    end function dm_lua_error_message

    function dm_lua_escape(string) result(escaped)
        !! Escapes passed character string by replacing each occurance of `\`
        !! with `\\`.
        character(*), intent(in)  :: string  !! String to escape.
        character(:), allocatable :: escaped !! Escaped string.

        integer :: i

        escaped = ''

        do i = 1, len_trim(string)
            if (string(i:i) == '\') then
                escaped = escaped // '\\'
                cycle
            end if

            escaped = escaped // string(i:i)
        end do
    end function dm_lua_escape

    integer function dm_lua_eval(lua, command) result(rc)
        !! Executes Lua command passed in character string `command`.
        type(lua_state_type), intent(inout) :: lua     !! Lua state.
        character(*),         intent(in)    :: command !! Lua command to evaluate.

        rc = E_NULL
        if (.not. c_associated(lua%ctx)) return
        rc = dm_lua_error(lual_dostring(lua%ctx, command))
    end function dm_lua_eval

    integer function dm_lua_exec(lua, file_path) result(rc)
        !! Executes Lua script.
        type(lua_state_type), intent(inout) :: lua       !! Lua state.
        character(*),         intent(in)    :: file_path !! Path to Lua script file.

        rc = E_NULL
        if (.not. c_associated(lua%ctx)) return
        rc = dm_lua_error(lual_dofile(lua%ctx, trim(file_path)))
    end function dm_lua_exec

    integer function dm_lua_init(lua, libs) result(rc)
        !! Initialises Lua interpreter and opens libraries, unless `libs` is
        !! `.false.`. Returns `E_EXIST` if the Lua pointer is already
        !! associated, and `E_LUA` if one of the Lua calls failed.
        type(lua_state_type), intent(inout)        :: lua  !! Lua state.
        logical,              intent(in), optional :: libs !! Open Lua libraries.

        rc = E_EXIST
        if (c_associated(lua%ctx)) return

        rc = E_LUA
        lua%ctx = lual_newstate()
        if (.not. c_associated(lua%ctx)) return

        rc = E_NONE
        if (dm_present(libs, .true.)) call lual_openlibs(lua%ctx)
    end function dm_lua_init

    logical function dm_lua_is_function(lua) result(is_function)
        !! Returns `.true.` if element on top of stack is of type function.
        type(lua_state_type), intent(inout) :: lua  !! Lua state.

        is_function = .false.
        if (.not. c_associated(lua%ctx)) return
        is_function = (lua_isfunction(lua%ctx, -1) == 1)
    end function dm_lua_is_function

    logical function dm_lua_is_nil(lua) result(is_nil)
        !! Returns `.true.` if element on top of stack is nil.
        type(lua_state_type), intent(inout) :: lua  !! Lua state.

        is_nil = .true.
        if (.not. c_associated(lua%ctx)) return
        is_nil = (lua_isnil(lua%ctx, -1) == 1)
    end function dm_lua_is_nil

    logical function dm_lua_is_opened(lua) result(is_opened)
        !! Returns `.true.` if pointer to Lua interpreter is associated.
        type(lua_state_type), intent(inout) :: lua  !! Lua state.

        is_opened = c_associated(lua%ctx)
    end function dm_lua_is_opened

    logical function dm_lua_is_table(lua) result(is_table)
        !! Returns `.true.` if element on top of stack is of type table.
        type(lua_state_type), intent(inout) :: lua  !! Lua state.

        is_table = .false.
        if (.not. c_associated(lua%ctx)) return
        is_table = (lua_istable(lua%ctx, -1) == 1)
    end function dm_lua_is_table

    integer function dm_lua_open(lua, file_path, eval) result(rc)
        !! Opens Lua script and executes it by default. The function returns the
        !! following error codes:
        !!
        !! * `E_LUA` on internal Lua error.
        !! * `E_NOT_FOUND` if the file could not be found.
        !! * `E_NULL` if the Lua interpreter is not initialised.
        !!
        type(lua_state_type), intent(inout)        :: lua       !! Lua state.
        character(*),         intent(in)           :: file_path !! Path to Lua script.
        logical,              intent(in), optional :: eval      !! Evaluate script once.

        integer :: stat

        rc = E_NULL
        if (.not. c_associated(lua%ctx)) return

        rc = E_NOT_FOUND
        if (.not. dm_file_exists(file_path)) return

        if (dm_present(eval, .true.)) then
            stat = lual_dofile(lua%ctx, trim(file_path))
        else
            stat = lual_loadfile(lua%ctx, trim(file_path))
        end if

        rc = dm_lua_error(stat)
    end function dm_lua_open

    integer function dm_lua_table(lua, name, n) result(rc)
        !! Loads global table of given name. The function returns the following
        !! error codes:
        !!
        !! * `E_NOT_FOUND` if the name does not exist.
        !! * `E_NULL` if the Lua interpreter is not initialised.
        !! * `E_TYPE` if variable on stack is not a table.
        !!
        type(lua_state_type), intent(inout)         :: lua  !! Lua state.
        character(*),         intent(in)            :: name !! Name of table.
        integer,              intent(out), optional :: n    !! Number of elements in table.

        if (present(n)) n = 0

        rc = E_NULL
        if (.not. c_associated(lua%ctx)) return

        rc = E_NOT_FOUND
        if (lua_getglobal(lua%ctx, trim(name)) == LUA_TNIL) return

        rc = E_TYPE
        if (lua_istable(lua%ctx, -1) == 0) then
            call lua_pop(lua%ctx, 1)
            return
        end if

        rc = E_NONE
        if (present(n)) n = dm_lua_table_size(lua)
    end function dm_lua_table

    integer function dm_lua_table_size(lua) result(n)
        !! Returns size of table on stack. Returns `-1` on error.
        type(lua_state_type), intent(inout) :: lua !! Lua state.

        n = -1
        if (.not. c_associated(lua%ctx)) return
        n = int(lua_rawlen(lua%ctx, -1), kind=i4)
    end function dm_lua_table_size

    integer(i4) function dm_lua_to_int32(lua, idx) result(value)
        !! Returns 4-byte integer from Lua stack at position `idx`.
        type(lua_state_type), intent(inout) :: lua !! Lua state.
        integer,              intent(in)    :: idx !! Stack index.

        value = 0_i4
        if (.not. c_associated(lua%ctx)) return
        value = int(lua_tointeger(lua%ctx, idx), kind=i4)
    end function dm_lua_to_int32

    integer(i8) function dm_lua_to_int64(lua, idx) result(value)
        !! Returns 8-byte integer from Lua stack at position `idx`.
        type(lua_state_type), intent(inout) :: lua !! Lua state.
        integer,              intent(in)    :: idx !! Stack index.

        value = 0_i8
        if (.not. c_associated(lua%ctx)) return
        value = lua_tointeger(lua%ctx, idx)
    end function dm_lua_to_int64

    logical function dm_lua_to_logical(lua, idx) result(value)
        !! Returns 8-byte integer from Lua stack at position `idx`.
        type(lua_state_type), intent(inout) :: lua !! Lua state.
        integer,              intent(in)    :: idx !! Stack index.

        value = .false.
        if (.not. c_associated(lua%ctx)) return
        value = lua_toboolean(lua%ctx, idx)
    end function dm_lua_to_logical

    real(r4) function dm_lua_to_real32(lua, idx) result(value)
        !! Returns 4-byte real from Lua stack at position `idx`.
        type(lua_state_type), intent(inout) :: lua !! Lua state.
        integer,              intent(in)    :: idx !! Stack index.

        value = 0.0_r4
        if (.not. c_associated(lua%ctx)) return
        value = real(lua_tonumber(lua%ctx, idx), kind=r4)
    end function dm_lua_to_real32

    real(r8) function dm_lua_to_real64(lua, idx) result(value)
        !! Returns 8-byte real from Lua stack at position `idx`.
        type(lua_state_type), intent(inout) :: lua !! Lua state.
        integer,              intent(in)    :: idx !! Stack index.

        value = 0.0_r8
        if (.not. c_associated(lua%ctx)) return
        value = lua_tonumber(lua%ctx, idx)
    end function dm_lua_to_real64

    function dm_lua_to_string(lua, idx) result(value)
        !! Returns allocatable character string from Lua stack at position `idx`.
        type(lua_state_type), intent(inout) :: lua   !! Lua state.
        integer,              intent(in)    :: idx   !! Stack index.
        character(:), allocatable           :: value !! String value.

        if (.not. c_associated(lua%ctx)) then
            value = ''
            return
        end if

        value = lua_tostring(lua%ctx, idx)
    end function dm_lua_to_string

    function dm_lua_unescape(string) result(unescaped)
        !! Unescapes passed character string by replacing each occurance of
        !! `\\` with `\`.
        character(*), intent(in)  :: string    !! String to escape.
        character(:), allocatable :: unescaped !! Unescaped string.

        integer :: i, n

        unescaped = ''

        i = 1
        n = len_trim(string)

        do
            if (i > n) exit
            if (i < n) then
                if (string(i:i + 1) == '\\') then
                    unescaped = unescaped // '\'
                    i = i + 2
                    cycle
                end if
            end if
            unescaped = unescaped // string(i:i)
            i = i + 1
        end do
    end function dm_lua_unescape

    function dm_lua_version(name) result(version)
        !! Returns Lua version as allocatable string of the form `5.4` or
        !! `liblua/5.4` if argument `name` is `.true.`.
        logical, intent(in), optional :: name !! Add prefix `liblua/`.
        character(:), allocatable     :: version

        character(3)         :: v
        integer              :: major, minor, rc
        type(lua_state_type) :: lua

        v  = '0.0'
        rc = dm_lua_init(lua)

        if (dm_is_ok(rc)) then
            call dm_lua_version_number(lua, major, minor)
            write (v, '(i1, ".", i1)') major, minor
        end if

        call dm_lua_destroy(lua)

        if (dm_present(name, .false.)) then
            version = 'liblua/' // v
        else
            version = v
        end if
    end function dm_lua_version

    ! **************************************************************************
    ! PUBLIC SUBROUTINES.
    ! **************************************************************************
    subroutine dm_lua_destroy(lua)
        !! Closes Lua.
        type(lua_state_type), intent(inout) :: lua !! Lua state.

        if (.not. c_associated(lua%ctx)) return
        call lua_close(lua%ctx)
        lua%ctx = c_null_ptr
    end subroutine dm_lua_destroy

    subroutine dm_lua_dump_stack(lua, unit)
        !! Dumps stack to standard output or file unit.
        type(lua_state_type), intent(inout)        :: lua  !! Lua state.
        integer,              intent(in), optional :: unit !! File unit.

        integer :: i, top, type, unit_

        unit_ = dm_present(unit, stdout)
        top   = lua_gettop(lua%ctx)

        do i = 1, top
            type = lua_type(lua%ctx, i)
            write (unit_, '(tr1, i0, tr1, a, tr1)', advance='no') i, lua_typename(lua%ctx, type)

            select case (type)
                case (LUA_TNIL);     write (unit_, '("nil")')
                case (LUA_TBOOLEAN); write (unit_, '(l1)')   lua_toboolean(lua%ctx, i)
                case (LUA_TNUMBER);  write (unit_, '(f0.1)') lua_tonumber(lua%ctx, i)
                case (LUA_TSTRING);  write (unit_, '(a)')    lua_tostring(lua%ctx, i)
                case default;        write (unit_, *)
            end select
        end do
    end subroutine dm_lua_dump_stack

    subroutine dm_lua_pop(lua, n)
        !! Pops element on stack.
        type(lua_state_type), intent(inout)        :: lua !! Lua state.
        integer,              intent(in), optional :: n   !! Stack position.

        integer :: n_

        n_ = dm_present(n, 1)
        if (.not. c_associated(lua%ctx)) return
        call lua_pop(lua%ctx, n_)
    end subroutine dm_lua_pop

    subroutine dm_lua_register(lua, name, proc)
        !! Registers a new Lua command.
        type(lua_state_type), intent(inout) :: lua  !! Lua state.
        character(*),         intent(in)    :: name !! Lua procedure name.
        procedure(dm_lua_callback)          :: proc !! C-interoperable subroutine to call.

        call lua_register(lua%ctx, trim(name), c_funloc(proc))
    end subroutine dm_lua_register

    subroutine dm_lua_version_number(lua, major, minor)
        !! Returns Lua version number.
        type(lua_state_type), intent(inout) :: lua   !! Lua state.
        integer,              intent(out)   :: major !! Major version number.
        integer,              intent(out)   :: minor !! Minor version number.

        real :: version

        version = real(lua_version(lua%ctx))
        major   = floor(version / 100)
        minor   = floor(version - (major * 100))
    end subroutine dm_lua_version_number

    ! **************************************************************************
    ! PRIVATE FUNCTIONS.
    ! **************************************************************************
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
        type(lua_state_type),     intent(inout) :: lua       !! Lua state.
        character(*),             intent(in)    :: name      !! Table field name.
        integer(i4), allocatable, intent(out)   :: values(:) !! Table field values.

        lua_block: block
            integer :: i, n, stat

            rc = E_EMPTY
            if (lua_getfield(lua%ctx, -1, name) <= 0) exit lua_block

            rc = E_TYPE
            if (lua_istable(lua%ctx, -1) == 0) exit lua_block

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

        call lua_pop(lua%ctx, 1)
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
        type(lua_state_type),     intent(inout) :: lua       !! Lua state.
        character(*),             intent(in)    :: name      !! Table field name.
        integer(i8), allocatable, intent(out)   :: values(:) !! Table field values.

        lua_block: block
            integer :: i, n, stat

            rc = E_EMPTY
            if (lua_getfield(lua%ctx, -1, name) <= 0) exit lua_block

            rc = E_TYPE
            if (lua_istable(lua%ctx, -1) == 0) exit lua_block

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

        call lua_pop(lua%ctx, 1)
        if (.not. allocated(values)) allocate (values(0))
    end function lua_field_array_int64

    integer function lua_field_array_string(lua, name, values, unescape) result(rc)
        !! Returns allocatable character array from table field `name` in
        !! `values`. If `values` is allocated, it will be deallocated first.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_ALLOC` if array allocation failed.
        !! * `E_EMPTY` if the field of given name is null.
        !! * `E_BOUNDS` if a string length is greater than the array length.
        !! * `E_TYPE` if not a table or not a string element.
        !!
        !! On error, `values` will be allocated but empty.
        type(lua_state_type),      intent(inout)        :: lua       !! Lua state.
        character(*),              intent(in)           :: name      !! Table field name.
        character(*), allocatable, intent(inout)        :: values(:) !! Table field values.
        logical,                   intent(in), optional :: unescape  !! Unescape strings.

        if (allocated(values)) deallocate (values)

        lua_block: block
            integer :: i, n, stat

            rc = E_EMPTY
            if (lua_getfield(lua%ctx, -1, name) <= 0) exit lua_block

            rc = E_TYPE
            if (lua_istable(lua%ctx, -1) == 0) exit lua_block

            n = dm_lua_table_size(lua)

            rc = E_ALLOC
            allocate (values(n), stat=stat)
            if (stat /= 0) exit lua_block

            do i = 1, n
                rc = dm_lua_get(lua, i, values(i), unescape)
                if (dm_is_error(rc)) exit lua_block
            end do

            rc = E_NONE
        end block lua_block

        call lua_pop(lua%ctx, 1)
        if (.not. allocated(values)) allocate (values(0))
    end function lua_field_array_string

    integer function lua_field_int16(lua, name, value) result(rc)
        !! Returns 2-byte integer from table field `name` in `value`.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_EMPTY` if the field of given name is null.
        !! * `E_TYPE` if the field is not of type integer.
        !!
        !! On error, `value` will not be overwritten.
        type(lua_state_type), intent(inout) :: lua   !! Lua state.
        character(*),         intent(in)    :: name  !! Table field name.
        integer(i2),          intent(inout) :: value !! Table field value.

        lua_block: block
            rc = E_EMPTY
            if (lua_getfield(lua%ctx, -1, name) <= 0) exit lua_block

            rc = E_TYPE
            if (lua_isinteger(lua%ctx, -1) /= 1) exit lua_block

            rc = E_NONE
            value = int(lua_tointeger(lua%ctx, -1), kind=i2)
        end block lua_block

        call lua_pop(lua%ctx, 1)
    end function lua_field_int16

    integer function lua_field_int32(lua, name, value) result(rc)
        !! Returns 4-byte integer from table field `name` in `value`.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_EMPTY` if the field of given name is null.
        !! * `E_TYPE` if the field is not of type integer.
        !!
        !! On error, `value` will not be overwritten.
        type(lua_state_type), intent(inout) :: lua   !! Lua state.
        character(*),         intent(in)    :: name  !! Table field name.
        integer(i4),          intent(inout) :: value !! Table field value.

        lua_block: block
            rc = E_EMPTY
            if (lua_getfield(lua%ctx, -1, name) <= 0) exit lua_block

            rc = E_TYPE
            if (lua_isinteger(lua%ctx, -1) /= 1) exit lua_block

            rc = E_NONE
            value = int(lua_tointeger(lua%ctx, -1), kind=i4)
        end block lua_block

        call lua_pop(lua%ctx, 1)
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
        type(lua_state_type), intent(inout) :: lua   !! Lua state.
        character(*),         intent(in)    :: name  !! Table field name.
        integer(i8),          intent(inout) :: value !! Table field value.

        lua_block: block
            rc = E_EMPTY
            if (lua_getfield(lua%ctx, -1, name) <= 0) exit lua_block

            rc = E_TYPE
            if (lua_isinteger(lua%ctx, -1) /= 1) exit lua_block

            rc = E_NONE
            value = lua_tointeger(lua%ctx, -1)
        end block lua_block

        call lua_pop(lua%ctx, 1)
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
        type(lua_state_type), intent(inout) :: lua   !! Lua state.
        character(*),         intent(in)    :: name  !! Table field name.
        logical,              intent(inout) :: value !! Table field value.

        lua_block: block
            rc = E_EMPTY
            if (lua_getfield(lua%ctx, -1, name) <= 0) exit lua_block

            rc = E_TYPE
            if (lua_isboolean(lua%ctx, -1) /= 1) exit lua_block

            rc = E_NONE
            value = lua_toboolean(lua%ctx, -1)
        end block lua_block

        call lua_pop(lua%ctx, 1)
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
        type(lua_state_type), intent(inout) :: lua   !! Lua state.
        character(*),         intent(in)    :: name  !! Table field name.
        real(r8),             intent(inout) :: value !! Table field value.

        lua_block: block
            rc = E_EMPTY
            if (lua_getfield(lua%ctx, -1, name) <= 0) exit lua_block

            rc = E_TYPE
            if (lua_isnumber(lua%ctx, -1) /= 1) exit lua_block

            rc = E_NONE
            value = lua_tonumber(lua%ctx, -1)
        end block lua_block

        call lua_pop(lua%ctx, 1)
    end function lua_field_real64

    integer function lua_field_stack(lua, name) result(rc)
        !! Pushes table field of given name on stack.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_EMPTY` if the field of given name is null.
        !!
        type(lua_state_type), intent(inout) :: lua  !! Lua state.
        character(*),         intent(in)    :: name !! Field name.

        rc = E_EMPTY
        if (lua_getfield(lua%ctx, -1, name) == LUA_TNIL) return
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
        !! * `E_BOUNDS` if the actual string length is greater than the length
        !!    of the passed value argument.
        !! * `E_TYPE` if the field is not of type string.
        !!
        !! On error, `value` will not be overwritten.
        type(lua_state_type), intent(inout)        :: lua      !! Lua state.
        character(*),         intent(in)           :: name     !! Table field name.
        character(*),         intent(inout)        :: value    !! Table field value.
        logical,              intent(in), optional :: unescape !! Unescape the string.

        lua_block: block
            character(:), allocatable :: string

            rc = E_EMPTY
            if (lua_getfield(lua%ctx, -1, name) <= 0) exit lua_block

            rc = E_TYPE
            if (lua_isstring(lua%ctx, -1) /= 1) exit lua_block

            string = lua_tostring(lua%ctx, -1)

            if (dm_present(unescape, .false.)) then
                value = dm_lua_unescape(string)
            else
                value = string
            end if

            rc = E_BOUNDS
            if (len(string) > len(value)) exit lua_block

            rc = E_NONE
        end block lua_block

        call lua_pop(lua%ctx, 1)
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
        type(lua_state_type), intent(inout) :: lua   !! Lua state.
        integer,              intent(in)    :: i     !! Variable index.
        integer,              intent(inout) :: value !! Variable value.

        lua_block: block
            rc = E_INVALID
            if (lua_istable(lua%ctx, -1) == 0) return

            rc = E_EMPTY
            if (lua_rawgeti(lua%ctx, -1, int(i, kind=lua_integer)) == LUA_TNIL) exit lua_block

            rc = E_TYPE
            if (lua_isinteger(lua%ctx, -1) /= 1) exit lua_block

            rc = E_NONE
            value = int(lua_tointeger(lua%ctx, -1), kind=i4)
        end block lua_block

        call lua_pop(lua%ctx, 1)
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
        type(lua_state_type), intent(inout) :: lua   !! Lua state.
        integer,              intent(in)    :: i     !! Variable index.
        integer(i8),          intent(inout) :: value !! Variable value.

        lua_block: block
            rc = E_INVALID
            if (lua_istable(lua%ctx, -1) == 0) return

            rc = E_EMPTY
            if (lua_rawgeti(lua%ctx, -1, int(i, kind=lua_integer)) == LUA_TNIL) exit lua_block

            rc = E_TYPE
            if (lua_isinteger(lua%ctx, -1) /= 1) exit lua_block

            rc = E_NONE
            value = lua_tointeger(lua%ctx, -1)
        end block lua_block

        call lua_pop(lua%ctx, 1)
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
        type(lua_state_type), intent(inout) :: lua   !! Lua state.
        integer,              intent(in)    :: i     !! Variable index.
        logical,              intent(inout) :: value !! Variable value.

        lua_block: block
            rc = E_INVALID
            if (lua_istable(lua%ctx, -1) == 0) return

            rc = E_EMPTY
            if (lua_rawgeti(lua%ctx, -1, int(i, kind=lua_integer)) == LUA_TNIL) exit lua_block

            rc = E_TYPE
            if (lua_isboolean(lua%ctx, -1) /= 1) exit lua_block

            rc = E_NONE
            value = lua_toboolean(lua%ctx, -1)
        end block lua_block

        call lua_pop(lua%ctx, 1)
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
        type(lua_state_type), intent(inout) :: lua   !! Lua state.
        integer,              intent(in)    :: i     !! Variable index.
        real(r8),             intent(inout) :: value !! Variable value.

        lua_block: block
            rc = E_INVALID
            if (lua_istable(lua%ctx, -1) == 0) return

            rc = E_EMPTY
            if (lua_rawgeti(lua%ctx, -1, int(i, kind=lua_integer)) == LUA_TNIL) exit lua_block

            rc = E_TYPE
            if (lua_isnumber(lua%ctx, -1) /= 1) exit lua_block

            rc = E_NONE
            value = lua_tonumber(lua%ctx, -1)
        end block lua_block

        call lua_pop(lua%ctx, 1)
    end function lua_get_real64

    integer function lua_get_stack(lua, i) result(rc)
        !! Pushes table element at index `i` on stack.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_INVALID` if the element on top of the stack is not a table.
        !! * `E_EMPTY` if the table is empty.
        !!
        type(lua_state_type), intent(inout) :: lua !! Lua state.
        integer,              intent(in)    :: i   !! Variable index.

        rc = E_INVALID
        if (lua_istable(lua%ctx, -1) == 0) return
        rc = E_EMPTY
        if (lua_rawgeti(lua%ctx, -1, int(i, kind=lua_integer)) == LUA_TNIL) return
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
        !! * `E_BOUNDS` if the actual string length is greater than the length
        !!    of the passed value argument.
        !! * `E_TYPE` if the table element is not of type string.
        !!
        !! On error, `value` will not be overwritten.
        type(lua_state_type), intent(inout)        :: lua      !! Lua state.
        integer,              intent(in)           :: i        !! Variable index.
        character(*),         intent(inout)        :: value    !! Variable value.
        logical,              intent(in), optional :: unescape !! Unescape string.

        lua_block: block
            character(:), allocatable :: string

            rc = E_INVALID
            if (lua_istable(lua%ctx, -1) == 0) return

            rc = E_EMPTY
            if (lua_rawgeti(lua%ctx, -1, int(i, kind=lua_integer)) == LUA_TNIL) exit lua_block

            rc = E_TYPE
            if (lua_isstring(lua%ctx, -1) /= 1) exit lua_block

            string = lua_tostring(lua%ctx, -1)

            if (dm_present(unescape, .false.)) then
                value = dm_lua_unescape(string)
            else
                value = string
            end if

            rc = E_BOUNDS
            if (len(string) > len(value)) exit lua_block

            rc = E_NONE
        end block lua_block

        call lua_pop(lua%ctx, 1)
    end function lua_get_string

    integer function lua_read_array_int32(lua, name, values) result(rc)
        !! Returns the values of global variable as 4-byte integers.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_ALLOC` if array allocation failed.
        !! * `E_TYPE` if variable is not an integer table.
        !!
        type(lua_state_type),     intent(inout) :: lua       !! Lua state.
        character(*),             intent(in)    :: name      !! Variable name.
        integer(i4), allocatable, intent(out)   :: values(:) !! Variable values.

        lua_block: block
            integer :: i, n, stat

            rc = E_TYPE
            if (lua_getglobal(lua%ctx, name) /= LUA_TTABLE) exit lua_block

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

        call lua_pop(lua%ctx, 1)
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
        type(lua_state_type),     intent(inout) :: lua       !! Lua state.
        character(*),             intent(in)    :: name      !! Variable name.
        integer(i8), allocatable, intent(out)   :: values(:) !! Variable values.

        lua_block: block
            integer :: i, n, stat

            rc = E_TYPE
            if (lua_getglobal(lua%ctx, name) /= LUA_TTABLE) exit lua_block

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

        call lua_pop(lua%ctx, 1)
        if (.not. allocated(values)) allocate (values(0))
    end function lua_read_array_int64

    integer function lua_read_int32(lua, name, value) result(rc)
        !! Returns the value of global variable as 4-byte integer. The
        !! function returns `E_TYPE` if the variable is not of type integer.
        type(lua_state_type), intent(inout) :: lua   !! Lua state.
        character(*),         intent(in)    :: name  !! Variable name.
        integer(i4),          intent(inout) :: value !! Variable value.

        rc = E_TYPE
        if (lua_getglobal(lua%ctx, name) == LUA_TNUMBER) then
            value = int(lua_tointeger(lua%ctx, -1), kind=i4)
            rc = E_NONE
        end if
        call lua_pop(lua%ctx, 1)
    end function lua_read_int32

    integer function lua_read_int64(lua, name, value) result(rc)
        !! Returns the value of global variable as 8-byte integer. The
        !! function returns `E_TYPE` if the variable is not of type integer.
        type(lua_state_type), intent(inout) :: lua   !! Lua state.
        character(*),         intent(in)    :: name  !! Variable name.
        integer(i8),          intent(inout) :: value !! Variable value.

        rc = E_TYPE
        if (lua_getglobal(lua%ctx, name) == LUA_TNUMBER) then
            value = lua_tointeger(lua%ctx, -1)
            rc = E_NONE
        end if
        call lua_pop(lua%ctx, 1)
    end function lua_read_int64

    integer function lua_read_logical(lua, name, value) result(rc)
        !! Returns the value of global variable as logical. The function
        !! returns `E_TYPE` if the variable is not of type boolean.
        type(lua_state_type), intent(inout) :: lua   !! Lua state.
        character(*),         intent(in)    :: name  !! Variable name.
        logical,              intent(inout) :: value !! Variable value.

        rc = E_TYPE
        if (lua_getglobal(lua%ctx, name) == LUA_TBOOLEAN) then
            value = lua_toboolean(lua%ctx, -1)
            rc = E_NONE
        end if
        call lua_pop(lua%ctx, 1)
    end function lua_read_logical

    integer function lua_read_real64(lua, name, value) result(rc)
        !! Returns the value of global variable as 8-byte real. The function
        !! returns `E_TYPE` if the variable is not of type number.
        type(lua_state_type), intent(inout) :: lua   !! Lua state.
        character(*),         intent(in)    :: name  !! Variable name.
        real(r8),             intent(inout) :: value !! Variable value.

        rc = E_TYPE
        if (lua_getglobal(lua%ctx, name) == LUA_TNUMBER) then
            value = lua_tonumber(lua%ctx, -1)
            rc = E_NONE
        end if
        call lua_pop(lua%ctx, 1)
    end function lua_read_real64

    integer function lua_read_stack(lua, name) result(rc)
        !! Pushes global variable on stack. Returns `E_EMPTY` if the variable
        !! does not exist.
        type(lua_state_type), intent(inout) :: lua  !! Lua state.
        character(*),         intent(in)    :: name !! Variable name.

        rc = E_EMPTY
        if (lua_getglobal(lua%ctx, name) <= 0) return
        rc = E_NONE
    end function lua_read_stack

    integer function lua_read_string(lua, name, value) result(rc)
        !! Returns the value of global variable as allocatable string. The
        !! function returns `E_TYPE` if the variable is not of type string.
        type(lua_state_type), intent(inout) :: lua   !! Lua state.
        character(*),         intent(in)    :: name  !! Variable name.
        character(*),         intent(inout) :: value !! Variable value.

        rc = E_TYPE
        if (lua_getglobal(lua%ctx, name) == LUA_TSTRING) then
            value = lua_tostring(lua%ctx, -1)
            rc = E_NONE
        end if
        call lua_pop(lua%ctx, 1)
    end function lua_read_string

    integer function lua_set_int32(lua, name, value) result(rc)
        !! Sets 4-byte integer variable of given name.
        type(lua_state_type), intent(inout) :: lua   !! Lua state.
        character(*),         intent(in)    :: name  !! Name of variable
        integer(i4),          intent(in)    :: value !! Value of variable.

        rc = dm_lua_eval(lua, name // ' = ' // dm_itoa(value))
    end function lua_set_int32

    integer function lua_to_group(lua, group) result(rc)
        !! Reads Lua table into Fortran observation group. The table has to be
        !! on top of the stack and will be removed once finished.
        use :: dm_group
        use :: dm_observ

        type(lua_state_type), intent(inout) :: lua   !! Lua state.
        type(group_type),     intent(out)   :: group !! Observation group.

        lua_block: block
            integer           :: i, n
            type(observ_type) :: observ

            rc = E_TYPE
            if (.not. dm_lua_is_table(lua)) exit lua_block

            n = dm_lua_table_size(lua)

            rc = dm_group_create(group, n)
            if (dm_is_error(rc)) exit lua_block

            rc = E_EMPTY
            if (n == 0) exit lua_block

            do i = 1, n
                ! Load element.
                rc = dm_lua_get(lua, i)
                if (dm_is_error(rc)) exit lua_block

                ! Read element.
                rc = lua_to_observ(lua, observ)
                if (dm_is_error(rc)) exit lua_block

                ! Add to group.
                rc = dm_group_add(group, observ)
                if (dm_is_error(rc)) exit lua_block
            end do

            rc = E_NONE
        end block lua_block

        call dm_lua_pop(lua)
    end function lua_to_group

    integer function lua_to_job(lua, job) result(rc)
        !! Reads Lua table into Fortran job. The table has to be on top of the
        !! stack and will be removed once finished.
        use :: dm_job

        type(lua_state_type), intent(inout) :: lua !! Lua state.
        type(job_type),       intent(out)   :: job !! Job.

        lua_block: block
            rc = E_TYPE
            if (.not. dm_lua_is_table(lua)) exit lua_block

            ! Ignore error codes, just assume defaults if missing.
            rc = dm_lua_field(lua, 'delay',    job%delay)
            rc = dm_lua_field(lua, 'disabled', job%disabled)
            rc = dm_lua_field(lua, 'onetime',  job%onetime)
            rc = dm_lua_field(lua, 'group')

            rc = lua_to_group(lua, job%group)
        end block lua_block

        call dm_lua_pop(lua)
    end function lua_to_job

    integer function lua_to_job_list(lua, job_list) result(rc)
        !! Reads Lua table into Fortran job list. The table has to be on
        !! top of the stack and will be removed once finished.
        use :: dm_job
        use :: dm_job_list

        type(lua_state_type), intent(inout) :: lua      !! Lua state.
        type(job_list_type),  intent(out)   :: job_list !! Job list.

        lua_block: block
            integer        :: i, n
            type(job_type) :: job

            rc = E_TYPE
            if (.not. dm_lua_is_table(lua)) exit lua_block

            n = dm_lua_table_size(lua)

            rc = dm_job_list_create(job_list, n)
            if (dm_is_error(rc)) exit lua_block

            rc = E_EMPTY
            if (n == 0) exit lua_block

            do i = 1, n
                rc = dm_lua_get(lua, i)
                if (dm_is_error(rc)) exit lua_block

                rc = dm_lua_to(lua, job)
                if (dm_is_error(rc)) exit lua_block

                rc = dm_job_list_add(job_list, job)
                if (dm_is_error(rc)) exit lua_block
            end do

            rc = E_NONE
        end block lua_block

        call dm_lua_pop(lua)
    end function lua_to_job_list

    integer function lua_to_jobs(lua, jobs) result(rc)
        !! Reads Lua table into Fortran job array. The table has to be on
        !! top of the stack and will be removed once finished.
        !!
        !! The functions returns the following error codes:
        !!
        !! * `E_ALLOC` if the array allocation failed.
        !! * `E_EMPTY` if the table is empty.
        !! * `E_TYPE` if the stack element is not a table.
        !!
        use :: dm_job

        type(lua_state_type),        intent(inout) :: lua     !! Lua state.
        type(job_type), allocatable, intent(out)   :: jobs(:) !! Job type array.

        lua_block: block
            integer :: i, n, stat

            rc = E_TYPE
            if (.not. dm_lua_is_table(lua)) exit lua_block

            n = dm_lua_table_size(lua)

            rc = E_ALLOC
            allocate (jobs(n), stat=stat)
            if (stat /= 0) exit lua_block

            rc = E_EMPTY
            if (n == 0) exit lua_block

            do i = 1, n
                rc = dm_lua_get(lua, i)
                if (dm_is_error(rc)) exit lua_block

                rc = lua_to_job(lua, jobs(i))
                if (dm_is_error(rc)) exit lua_block
            end do

            rc = E_NONE
        end block lua_block

        call dm_lua_pop(lua)
    end function lua_to_jobs

    integer function lua_to_observ(lua, observ) result(rc)
        !! Reads Lua table into Fortran observation. The table has to be on top
        !! of the stack and will be removed once finished.
        use :: dm_observ

        type(lua_state_type), intent(inout) :: lua    !! Lua state.
        type(observ_type),    intent(out)   :: observ !! Observation.

        observ_block: block
            integer :: i, n, nreceivers, nresponses

            rc = E_TYPE
            if (.not. dm_lua_is_table(lua)) exit observ_block

            ! Read observation attributes.
            rc = dm_lua_field(lua, 'id',         observ%id)
            rc = dm_lua_field(lua, 'group_id',   observ%group_id)
            rc = dm_lua_field(lua, 'node_id',    observ%node_id)
            rc = dm_lua_field(lua, 'sensor_id',  observ%sensor_id)
            rc = dm_lua_field(lua, 'target_id',  observ%target_id)
            rc = dm_lua_field(lua, 'timestamp',  observ%timestamp)
            rc = dm_lua_field(lua, 'name',       observ%name)
            rc = dm_lua_field(lua, 'source',     observ%source)
            rc = dm_lua_field(lua, 'device',     observ%device,    unescape=.true.)
            rc = dm_lua_field(lua, 'request',    observ%request,   unescape=.true.)
            rc = dm_lua_field(lua, 'response',   observ%response,  unescape=.true.)
            rc = dm_lua_field(lua, 'delimiter',  observ%delimiter, unescape=.true.)
            rc = dm_lua_field(lua, 'pattern',    observ%pattern,   unescape=.true.)
            rc = dm_lua_field(lua, 'delay',      observ%delay)
            rc = dm_lua_field(lua, 'error',      observ%error)
            rc = dm_lua_field(lua, 'mode',       observ%mode)
            rc = dm_lua_field(lua, 'next',       observ%next)
            rc = dm_lua_field(lua, 'priority',   observ%priority)
            rc = dm_lua_field(lua, 'retries',    observ%retries)
            rc = dm_lua_field(lua, 'state',      observ%state)
            rc = dm_lua_field(lua, 'timeout',    observ%timeout)
            rc = dm_lua_field(lua, 'nreceivers', observ%nreceivers)
            rc = dm_lua_field(lua, 'nresponses', observ%nresponses)

            ! Read receivers.
            rc = dm_lua_field(lua, 'receivers')
            n  = dm_lua_table_size(lua)

            nreceivers = min(OBSERV_MAX_NRECEIVERS, n)
            observ%nreceivers = nreceivers

            do i = 1, nreceivers
                rc = dm_lua_get(lua, i, observ%receivers(i))
                if (dm_is_error(rc)) exit
            end do

            call dm_lua_pop(lua) ! receivers

            ! Read responses.
            rc = dm_lua_field(lua, 'responses')
            n  = dm_lua_table_size(lua)

            nresponses = min(OBSERV_MAX_NRESPONSES, n)
            observ%nresponses = nresponses

            do i = 1, nresponses
                rc = dm_lua_get(lua, i)
                if (dm_is_error(rc)) exit

                rc = dm_lua_field(lua, 'name',  observ%responses(i)%name)
                rc = dm_lua_field(lua, 'unit',  observ%responses(i)%unit)
                rc = dm_lua_field(lua, 'type',  observ%responses(i)%type)
                rc = dm_lua_field(lua, 'error', observ%responses(i)%error)
                rc = dm_lua_field(lua, 'value', observ%responses(i)%value)

                call dm_lua_pop(lua) ! table element
            end do

            rc = E_NONE
            call dm_lua_pop(lua) ! table responses
        end block observ_block

        call dm_lua_pop(lua) ! table observ
    end function lua_to_observ

    integer function lua_to_observs(lua, observs) result(rc)
        !! Reads Lua table into Fortran observation type array. The table has to
        !! be on top of the stack and will be removed once finished.
        use :: dm_observ

        type(lua_state_type),           intent(inout) :: lua        !! Lua state.
        type(observ_type), allocatable, intent(out)   :: observs(:) !! Observation array.

        lua_block: block
            integer :: i, n, stat

            rc = E_TYPE
            if (.not. dm_lua_is_table(lua)) exit lua_block

            n = dm_lua_table_size(lua)

            rc = E_ALLOC
            allocate (observs(n), stat=stat)
            if (stat /= 0) exit lua_block

            rc = E_EMPTY
            if (n == 0) exit lua_block

            do i = 1, n
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

        type(lua_state_type), intent(inout) :: lua    !! Lua state.
        type(report_type),    intent(out)   :: report !! Report.

        lua_block: block
            character(REPORT_FORMAT_NAME_LEN) :: format
            integer                           :: i, sz, stat

            rc = E_TYPE
            if (.not. dm_lua_is_table(lua)) exit lua_block

            rc = dm_lua_field(lua, 'node',     report%node)
            rc = dm_lua_field(lua, 'from',     report%from)
            rc = dm_lua_field(lua, 'to',       report%to)
            rc = dm_lua_field(lua, 'output',   report%output)
            rc = dm_lua_field(lua, 'style',    report%style)
            rc = dm_lua_field(lua, 'title',    report%title)
            rc = dm_lua_field(lua, 'subtitle', report%subtitle)
            rc = dm_lua_field(lua, 'author',   report%author)
            rc = dm_lua_field(lua, 'meta',     report%meta)
            rc = dm_lua_field(lua, 'format',   format)
            rc = dm_lua_field(lua, 'verbose',  report%verbose)

            report%format = dm_report_format_from_name(format)

            ! Plots table.
            plots_block: block
                rc = dm_lua_field(lua, 'plots')

                if (dm_is_error(rc)) then
                    report%plot%disabled = .true.
                    exit plots_block
                end if

                rc = dm_lua_field(lua, 'database', report%plot%database)
                rc = dm_lua_field(lua, 'title',    report%plot%title)
                rc = dm_lua_field(lua, 'meta',     report%plot%meta)
                rc = dm_lua_field(lua, 'disabled', report%plot%disabled)
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
                        associate (observ => report%plot%observs(i))
                            rc = dm_lua_get(lua, i)
                            if (dm_is_error(rc)) exit observs_loop

                            rc = dm_lua_field(lua, 'format',    observ%format)
                            rc = dm_lua_field(lua, 'sensor',    observ%sensor)
                            rc = dm_lua_field(lua, 'target',    observ%target)
                            rc = dm_lua_field(lua, 'response',  observ%response)
                            rc = dm_lua_field(lua, 'unit',      observ%unit)
                            rc = dm_lua_field(lua, 'title',     observ%title)
                            rc = dm_lua_field(lua, 'subtitle',  observ%subtitle)
                            rc = dm_lua_field(lua, 'meta',      observ%meta)
                            rc = dm_lua_field(lua, 'color',     observ%color)
                            rc = dm_lua_field(lua, 'width',     observ%width)
                            rc = dm_lua_field(lua, 'height',    observ%height)
                            rc = dm_lua_field(lua, 'disabled',  observ%disabled)
                            rc = dm_lua_field(lua, 'pagebreak', observ%pagebreak)
                            rc = dm_lua_field(lua, 'scale',     observ%scale)

                            call dm_lower(observ%format)
                            call dm_lua_pop(lua) ! table element
                        end associate
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

    ! **************************************************************************
    ! PRIVATE SUBROUTINES.
    ! **************************************************************************
    subroutine lua_from_observ(lua, observ)
        !! Pushes observation on Lua stack.
        use :: dm_observ

        type(lua_state_type), intent(inout) :: lua    !! Lua state.
        type(observ_type),    intent(inout) :: observ !! Observation.

        integer     :: i
        type(c_ptr) :: ptr

        call lua_createtable(lua%ctx, 0, 25)

        ptr = lua_pushstring(lua%ctx, trim(observ%id))
        call lua_setfield(lua%ctx, -2, 'id')

        ptr = lua_pushstring(lua%ctx, trim(observ%group_id))
        call lua_setfield(lua%ctx, -2, 'group_id')

        ptr = lua_pushstring(lua%ctx, trim(observ%node_id))
        call lua_setfield(lua%ctx, -2, 'node_id')

        ptr = lua_pushstring(lua%ctx, trim(observ%sensor_id))
        call lua_setfield(lua%ctx, -2, 'sensor_id')

        ptr = lua_pushstring(lua%ctx, trim(observ%target_id))
        call lua_setfield(lua%ctx, -2, 'target_id')

        ptr = lua_pushstring(lua%ctx, trim(observ%timestamp))
        call lua_setfield(lua%ctx, -2, 'timestamp')

        ptr = lua_pushstring(lua%ctx, trim(observ%name))
        call lua_setfield(lua%ctx, -2, 'name')

        ptr = lua_pushstring(lua%ctx, trim(observ%source))
        call lua_setfield(lua%ctx, -2, 'source')

        ptr = lua_pushstring(lua%ctx, dm_lua_escape(observ%device))
        call lua_setfield(lua%ctx, -2, 'device')

        ptr = lua_pushstring(lua%ctx, dm_lua_escape(observ%request))
        call lua_setfield(lua%ctx, -2, 'request')

        ptr = lua_pushstring(lua%ctx, dm_lua_escape(observ%response))
        call lua_setfield(lua%ctx, -2, 'response')

        ptr = lua_pushstring(lua%ctx, dm_lua_escape(observ%delimiter))
        call lua_setfield(lua%ctx, -2, 'delimiter')

        ptr = lua_pushstring(lua%ctx, dm_lua_escape(observ%pattern))
        call lua_setfield(lua%ctx, -2, 'pattern')

        call lua_pushinteger(lua%ctx, int(observ%delay, kind=lua_integer))
        call lua_setfield(lua%ctx, -2, 'delay')

        call lua_pushinteger(lua%ctx, int(observ%error, kind=lua_integer))
        call lua_setfield(lua%ctx, -2, 'error')

        call lua_pushinteger(lua%ctx, int(observ%mode, kind=lua_integer))
        call lua_setfield(lua%ctx, -2, 'mode')

        call lua_pushinteger(lua%ctx, int(observ%next, kind=lua_integer))
        call lua_setfield(lua%ctx, -2, 'next')

        call lua_pushinteger(lua%ctx, int(observ%priority, kind=lua_integer))
        call lua_setfield(lua%ctx, -2, 'priority')

        call lua_pushinteger(lua%ctx, int(observ%retries, kind=lua_integer))
        call lua_setfield(lua%ctx, -2, 'retries')

        call lua_pushinteger(lua%ctx, int(observ%state, kind=lua_integer))
        call lua_setfield(lua%ctx, -2, 'state')

        call lua_pushinteger(lua%ctx, int(observ%timeout, kind=lua_integer))
        call lua_setfield(lua%ctx, -2, 'timeout')

        call lua_pushinteger(lua%ctx, int(observ%nreceivers, kind=lua_integer))
        call lua_setfield(lua%ctx, -2, 'nreceivers')

        call lua_pushinteger(lua%ctx, int(observ%nresponses, kind=lua_integer))
        call lua_setfield(lua%ctx, -2, 'nresponses')

        ! Receivers.
        call lua_createtable(lua%ctx, observ%nreceivers, 0)

        do i = 1, observ%nreceivers
            call lua_pushinteger(lua%ctx, int(i, kind=lua_integer))
            ptr = lua_pushstring(lua%ctx, trim(observ%receivers(i)))
            call lua_settable(lua%ctx, -3)
        end do

        call lua_setfield(lua%ctx, -2, 'receivers')

        ! Responses.
        call lua_createtable(lua%ctx, observ%nresponses, 0)

        do i = 1, observ%nresponses
            call lua_pushinteger(lua%ctx, int(i, kind=lua_integer))
            call lua_from_response(lua, observ%responses(i))
        end do

        call lua_setfield(lua%ctx, -2, 'responses')
    end subroutine lua_from_observ

    subroutine lua_from_response(lua, response)
        !! Pushes response on Lua stack.
        use :: dm_response

        type(lua_state_type), intent(inout) :: lua      !! Lua state.
        type(response_type),  intent(inout) :: response !! response.

        type(c_ptr) :: ptr

        call lua_createtable(lua%ctx, 0, 5)

        ptr = lua_pushstring(lua%ctx, trim(response%name))
        call lua_setfield(lua%ctx, -2, 'name')

        ptr = lua_pushstring(lua%ctx, trim(response%unit))
        call lua_setfield(lua%ctx, -2, 'unit')

        call lua_pushinteger(lua%ctx, int(response%type, kind=lua_integer))
        call lua_setfield(lua%ctx, -2, 'type')

        call lua_pushinteger(lua%ctx, int(response%error, kind=lua_integer))
        call lua_setfield(lua%ctx, -2, 'error')

        call lua_pushnumber(lua%ctx, response%value)
        call lua_setfield(lua%ctx, -2, 'value')

        call lua_settable(lua%ctx, -3)
    end subroutine lua_from_response
end module dm_lua
