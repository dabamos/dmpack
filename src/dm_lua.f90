! Author:  Philipp Engel
! Licence: ISC
module dm_lua
    !! Lua abstraction layer.
    use, intrinsic :: iso_c_binding
    use :: lua
    use :: dm_error
    use :: dm_file
    use :: dm_kind
    use :: dm_job
    use :: dm_observ
    use :: dm_report
    use :: dm_request
    use :: dm_string
    use :: dm_util
    implicit none (type, external)
    private

    type, public :: lua_state_type
        !! Opaque Lua type.
        private
        type(c_ptr) :: ptr = c_null_ptr !! C pointer to Lua interpreter.
    end type lua_state_type

    abstract interface
        ! int *lua_CFunction(lua_State *L)
        function dm_lua_callback(ptr) bind(c)
            !! Abstract Lua callback function.
            import :: c_int, c_ptr
            implicit none
            type(c_ptr), intent(in), value :: ptr
            integer(kind=c_int)            :: dm_lua_callback
        end function dm_lua_callback
    end interface

    interface dm_lua_field
        !! Pushes table element on stack and optionally returns value.
        module procedure :: lua_field_
        module procedure :: lua_field_a
        module procedure :: lua_field_i4
        module procedure :: lua_field_i8
        module procedure :: lua_field_l
        module procedure :: lua_field_r8
    end interface

    interface dm_lua_get
        !! Pushes table index on stack and optionally returns value.
        module procedure :: lua_get_
        module procedure :: lua_get_a
        module procedure :: lua_get_i4
        module procedure :: lua_get_i8
        module procedure :: lua_get_l
        module procedure :: lua_get_r8
    end interface

    interface dm_lua_from
        !! Pushes derived types on stack.
        module procedure :: lua_from_observ
    end interface

    interface dm_lua_global
        !! Pushes global variable on stack and optionally returns value.
        module procedure :: lua_global_
        module procedure :: lua_global_a
        module procedure :: lua_global_i4
        module procedure :: lua_global_i8
        module procedure :: lua_global_l
        module procedure :: lua_global_r8
    end interface

    interface dm_lua_to
        !! Converts Lua table to Fortran derived type.
        module procedure :: lua_to_job
        module procedure :: lua_to_job_list
        module procedure :: lua_to_jobs
        module procedure :: lua_to_observ
        module procedure :: lua_to_observs
        module procedure :: lua_to_report
    end interface dm_lua_to

    ! Public procedures.
    public :: dm_lua_call
    public :: dm_lua_callback
    public :: dm_lua_destroy
    public :: dm_lua_dump_stack
    public :: dm_lua_error
    public :: dm_lua_escape
    public :: dm_lua_exec
    public :: dm_lua_field
    public :: dm_lua_from
    public :: dm_lua_get
    public :: dm_lua_global
    public :: dm_lua_init
    public :: dm_lua_is_function
    public :: dm_lua_is_nil
    public :: dm_lua_is_table
    public :: dm_lua_open
    public :: dm_lua_pop
    public :: dm_lua_register
    public :: dm_lua_table
    public :: dm_lua_table_size
    public :: dm_lua_to
    public :: dm_lua_version

    ! Private procedures.
    private :: lua_field_
    private :: lua_field_a
    private :: lua_field_i4
    private :: lua_field_i8
    private :: lua_field_l
    private :: lua_field_r8

    private :: lua_from_observ

    private :: lua_get_
    private :: lua_get_a
    private :: lua_get_i4
    private :: lua_get_i8
    private :: lua_get_l
    private :: lua_get_r8

    private :: lua_global_
    private :: lua_global_a
    private :: lua_global_i4
    private :: lua_global_i8
    private :: lua_global_l
    private :: lua_global_r8

    private :: lua_to_job
    private :: lua_to_job_list
    private :: lua_to_jobs
    private :: lua_to_observ
    private :: lua_to_observs
    private :: lua_to_report
contains
    ! ******************************************************************
    ! PUBLIC PROCEDURES.
    ! ******************************************************************
    integer function dm_lua_call(lua, nargs, nresults) result(rc)
        !! Calls Lua function on top of stack.
        type(lua_state_type), intent(inout) :: lua      !! Lua type.
        integer,              intent(in)    :: nargs    !! Number of arguments.
        integer,              intent(in)    :: nresults !! Number of results.

        rc = E_LUA
        if (lua_pcall(lua%ptr, nargs, nresults, 0) /= 0) return
        rc = E_NONE
    end function dm_lua_call

    function dm_lua_error(lua) result(str)
        !! Returns last error message as allocatable character string.
        type(lua_state_type), intent(inout) :: lua !! Lua type.
        character(len=:), allocatable       :: str !! Last error message.

        if (lua_isstring(lua%ptr, -1) == 1) then
            str = lua_tostring(lua%ptr, -1)
            return
        end if

        str = ''
    end function dm_lua_error

    function dm_lua_escape(str) result(esc)
        !! Escapes passed character string by replacing each occurance of `\`
        !! with `\\`.
        character(len=*), intent(in)  :: str !! String to escape.
        character(len=:), allocatable :: esc !! Escaped string.
        integer                       :: i

        esc = ''

        do i = 1, len_trim(str)
            if (str(i:i) == '\') then
                esc = esc // '\\'
                cycle
            end if
            esc = esc // str(i:i)
        end do
    end function dm_lua_escape

    integer function dm_lua_exec(lua, file_path) result(rc)
        !! Executes Lua script.
        type(lua_state_type), intent(inout) :: lua       !! Lua type.
        character(len=*),     intent(in)    :: file_path !! Path to Lua script file.

        rc = E_LUA
        if (lual_dofile(lua%ptr, trim(file_path)) /= 0) return
        rc = E_NONE
    end function dm_lua_exec

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

    logical function dm_lua_is_table(lua) result(is_table)
        !! Returns `.true.` if element on top of stack is of type table.
        type(lua_state_type), intent(inout) :: lua  !! Lua type.

        is_table = (lua_istable(lua%ptr, -1) == 1)
    end function dm_lua_is_table

    integer function dm_lua_init(lua) result(rc)
        !! Initialises Lua interpreter.
        type(lua_state_type), intent(inout) :: lua !! Lua type.

        rc = E_INVALID
        if (c_associated(lua%ptr)) return

        rc = E_LUA
        lua%ptr = lual_newstate()
        if (.not. c_associated(lua%ptr)) return
        call lual_openlibs(lua%ptr)

        rc = E_NONE
    end function dm_lua_init

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

        rc = E_LUA
        eval_ = .true.
        if (present(eval)) eval_ = eval

        if (eval_) then
            if (lual_dofile(lua%ptr, trim(file_path)) /= 0) return
            rc = E_NONE
            return
        end if

        if (lual_loadfile(lua%ptr, trim(file_path)) /= 0) return
        rc = E_NONE
    end function dm_lua_open

    integer function dm_lua_register(lua, name, proc) result(rc)
        !! Registers a new Lua command.
        type(lua_state_type), intent(inout) :: lua  !! Lua type.
        character(len=*),     intent(in)    :: name !! Lua procedure name.
        procedure(dm_lua_callback)          :: proc !! C-interoperable subroutine to call.

        rc = E_LUA
        call lua_register(lua%ptr, trim(name), c_funloc(proc))
        if (lua_iscfunction(lua%ptr, -1) == 0) return
        rc = E_NONE
    end function dm_lua_register

    integer function dm_lua_table(lua, name, n) result(rc)
        !! Loads global table of given name.
        type(lua_state_type), intent(inout)         :: lua  !! Lua type.
        character(len=*),     intent(in)            :: name !! Name of table.
        integer,              intent(out), optional :: n    !! Number of elements in table.

        if (present(n)) n = 0

        rc = E_NOT_FOUND
        if (lua_getglobal(lua%ptr, trim(name)) == LUA_TNIL) return

        rc = E_TYPE
        if (lua_istable(lua%ptr, -1) /= 1) then
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

    real function dm_lua_version(lua) result(v)
        !! Returns Lua version number as 4-byte real.
        type(lua_state_type), intent(inout) :: lua !! Lua type.

        v = real(lua_version(lua%ptr))
    end function dm_lua_version

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
                    write (unit_, '(l)')  lua_toboolean(lua%ptr, i)
                case (LUA_TNUMBER)
                    write (unit_, '(i0)') lua_tonumber(lua%ptr, i)
                case (LUA_TSTRING)
                    write (unit_, '(a)')  lua_tostring(lua%ptr, i)
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

    ! ******************************************************************
    ! PRIVATE PROCEDURES.
    ! ******************************************************************
    integer function lua_field_(lua, name) result(rc)
        !! Pushes table field of given name on stack.
        type(lua_state_type), intent(inout) :: lua  !! Lua type.
        character(len=*),     intent(in)    :: name !! Field name.

        rc = E_EMPTY
        if (lua_getfield(lua%ptr, -1, name) == LUA_TNIL) return
        rc = E_NONE
    end function lua_field_

    integer function lua_field_a(lua, name, value) result(rc)
        !! Returns character string from table field `name` in `value`.
        type(lua_state_type), intent(inout) :: lua   !! Lua type.
        character(len=*),     intent(in)    :: name  !! Table field name.
        character(len=*),     intent(inout) :: value !! Table field value.

        rc = E_EMPTY
        if (lua_getfield(lua%ptr, -1, name) > 0) then
            rc = E_TYPE
            if (lua_isstring(lua%ptr, -1) == 1) then
                value = lua_tostring(lua%ptr, -1)
                rc = E_NONE
            end if
        end if
        call lua_pop(lua%ptr, 1)
    end function lua_field_a

    integer function lua_field_i4(lua, name, value) result(rc)
        !! Returns 4-byte integer from table field `name` in `value`.
        type(lua_state_type), intent(inout) :: lua   !! Lua type.
        character(len=*),     intent(in)    :: name  !! Table field name.
        integer(kind=i4),     intent(inout) :: value !! Table field value.

        rc = E_EMPTY
        if (lua_getfield(lua%ptr, -1, name) > 0) then
            rc = E_TYPE
            if (lua_isinteger(lua%ptr, -1) == 1) then
                value = int(lua_tointeger(lua%ptr, -1), kind=i4)
                rc = E_NONE
            end if
        end if
        call lua_pop(lua%ptr, 1)
    end function lua_field_i4

    integer function lua_field_i8(lua, name, value) result(rc)
        !! Returns 8-byte integer from table field `name` in `value`.
        type(lua_state_type), intent(inout) :: lua   !! Lua type.
        character(len=*),     intent(in)    :: name  !! Table field name.
        integer(kind=i8),     intent(inout) :: value !! Table field value.

        rc = E_EMPTY
        if (lua_getfield(lua%ptr, -1, name) > 0) then
            rc = E_TYPE
            if (lua_isinteger(lua%ptr, -1) == 1) then
                value = lua_tointeger(lua%ptr, -1)
                rc = E_NONE
            end if
        end if
        call lua_pop(lua%ptr, 1)
    end function lua_field_i8

    integer function lua_field_l(lua, name, value) result(rc)
        !! Returns logical from table field `name` in `value`.
        type(lua_state_type), intent(inout) :: lua   !! Lua type.
        character(len=*),     intent(in)    :: name  !! Table field name.
        logical,              intent(inout) :: value !! Table field value.

        rc = E_EMPTY
        if (lua_getfield(lua%ptr, -1, name) > 0) then
            rc = E_TYPE
            if (lua_isboolean(lua%ptr, -1) == 1) then
                value = lua_toboolean(lua%ptr, -1)
                rc = E_NONE
            end if
        end if
        call lua_pop(lua%ptr, 1)
    end function lua_field_l

    integer function lua_field_r8(lua, name, value) result(rc)
        !! Returns 8-byte real from table field `name` in `value`.
        type(lua_state_type), intent(inout) :: lua   !! Lua type.
        character(len=*),     intent(in)    :: name  !! Table field name.
        real(kind=r8),        intent(inout) :: value !! Table field value.

        rc = E_EMPTY
        if (lua_getfield(lua%ptr, -1, name) > 0) then
            rc = E_TYPE
            if (lua_isnumber(lua%ptr, -1) == 1) then
                value = lua_tonumber(lua%ptr, -1)
                rc = E_NONE
            end if
        end if
        call lua_pop(lua%ptr, 1)
    end function lua_field_r8

    integer function lua_from_observ(lua, observ) result(rc)
        !! Pushes observation on Lua stack.
        type(lua_state_type), intent(inout) :: lua    !! Lua type.
        type(observ_type),    intent(in)    :: observ !! Observation type.

        integer     :: i, j
        type(c_ptr) :: ptr

        rc = E_LUA
        call lua_createtable(lua%ptr, 0, 14)

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

        ptr = lua_pushstring(lua%ptr, dm_lua_escape(observ%path))
        call lua_setfield(lua%ptr, -2, 'path')

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
            call lua_createtable(lua%ptr, 0, 12)

            ptr = lua_pushstring(lua%ptr, trim(observ%requests(i)%timestamp))
            call lua_setfield(lua%ptr, -2, 'timestamp')

            ptr = lua_pushstring(lua%ptr, dm_lua_escape(observ%requests(i)%request))
            call lua_setfield(lua%ptr, -2, 'request')

            ptr = lua_pushstring(lua%ptr, dm_lua_escape(observ%requests(i)%response))
            call lua_setfield(lua%ptr, -2, 'response')

            ptr = lua_pushstring(lua%ptr, dm_lua_escape(observ%requests(i)%delimiter))
            call lua_setfield(lua%ptr, -2, 'delimiter')

            ptr = lua_pushstring(lua%ptr, dm_lua_escape(observ%requests(i)%pattern))
            call lua_setfield(lua%ptr, -2, 'pattern')

            call lua_pushinteger(lua%ptr, int(observ%requests(i)%delay, kind=lua_integer))
            call lua_setfield(lua%ptr, -2, 'delay')

            call lua_pushinteger(lua%ptr, int(observ%requests(i)%error, kind=lua_integer))
            call lua_setfield(lua%ptr, -2, 'error')

            call lua_pushinteger(lua%ptr, int(observ%requests(i)%retries, kind=lua_integer))
            call lua_setfield(lua%ptr, -2, 'retries')

            call lua_pushinteger(lua%ptr, int(observ%requests(i)%state, kind=lua_integer))
            call lua_setfield(lua%ptr, -2, 'state')

            call lua_pushinteger(lua%ptr, int(observ%requests(i)%timeout, kind=lua_integer))
            call lua_setfield(lua%ptr, -2, 'timeout')

            call lua_pushinteger(lua%ptr, int(observ%requests(i)%nresponses, kind=lua_integer))
            call lua_setfield(lua%ptr, -2, 'nresponses')

            ! Responses.
            call lua_createtable(lua%ptr, observ%requests(i)%nresponses, 0)

            do j = 1, observ%requests(i)%nresponses
                ! Response.
                call lua_pushinteger(lua%ptr, int(j, kind=lua_integer))
                call lua_createtable(lua%ptr, 0, 4)

                ptr = lua_pushstring(lua%ptr, trim(observ%requests(i)%responses(j)%name))
                call lua_setfield(lua%ptr, -2, 'name')

                ptr = lua_pushstring(lua%ptr, trim(observ%requests(i)%responses(j)%unit))
                call lua_setfield(lua%ptr, -2, 'unit')

                call lua_pushinteger(lua%ptr, int(observ%requests(i)%responses(j)%error, kind=lua_integer))
                call lua_setfield(lua%ptr, -2, 'error')

                call lua_pushnumber(lua%ptr, observ%requests(i)%responses(j)%value)
                call lua_setfield(lua%ptr, -2, 'value')

                call lua_settable(lua%ptr, -3)
            end do

            call lua_setfield(lua%ptr, -2, 'responses')
            call lua_settable(lua%ptr, -3)
        end do

        call lua_setfield(lua%ptr, -2, 'requests')

        rc = E_NONE
    end function lua_from_observ

    integer function lua_get_(lua, i) result(rc)
        !! Pushes table element at index `i` on stack.
        type(lua_state_type), intent(inout) :: lua !! Lua type.
        integer,              intent(in)    :: i   !! Variable index.

        rc = E_INVALID
        if (lua_istable(lua%ptr, -1) /= 1) return
        rc = E_EMPTY
        if (lua_rawgeti(lua%ptr, -1, int(i, kind=lua_integer)) == LUA_TNIL) return
        rc = E_NONE
    end function lua_get_

    integer function lua_get_a(lua, i, value) result(rc)
        !! Returns character string from table element `i` in `value`.
        type(lua_state_type), intent(inout) :: lua   !! Lua type.
        integer,              intent(in)    :: i     !! Variable index.
        character(len=*),     intent(inout) :: value !! Variable value.

        rc = E_INVALID
        if (lua_istable(lua%ptr, -1) /= 1) return
        rc = E_EMPTY
        if (lua_rawgeti(lua%ptr, -1, int(i, kind=lua_integer)) > 0) then
            rc = E_TYPE
            if (lua_isstring(lua%ptr, -1) == 1) then
                value = lua_tostring(lua%ptr, -1)
                rc = E_NONE
            end if
        end if
        call lua_pop(lua%ptr, 1)
    end function lua_get_a

    integer function lua_get_i4(lua, i, value) result(rc)
        !! Returns 4-byte integer from table element `i` in `value`.
        type(lua_state_type), intent(inout) :: lua   !! Lua type.
        integer,              intent(in)    :: i     !! Variable index.
        integer,              intent(inout) :: value !! Variable value.

        rc = E_INVALID
        if (lua_istable(lua%ptr, -1) /= 1) return
        rc = E_EMPTY
        if (lua_rawgeti(lua%ptr, -1, int(i, kind=lua_integer)) > 0) then
            rc = E_TYPE
            if (lua_isinteger(lua%ptr, -1) == 1) then
                value = int(lua_tointeger(lua%ptr, -1), kind=i4)
                rc = E_NONE
            end if
        end if
        call lua_pop(lua%ptr, 1)
    end function lua_get_i4

    integer function lua_get_i8(lua, i, value) result(rc)
        !! Returns 8-byte integer from table element `i` in `value`.
        type(lua_state_type), intent(inout) :: lua   !! Lua type.
        integer,              intent(in)    :: i     !! Variable index.
        integer(kind=i8),     intent(inout) :: value !! Variable value.

        rc = E_INVALID
        if (lua_istable(lua%ptr, -1) /= 1) return
        rc = E_EMPTY
        if (lua_rawgeti(lua%ptr, -1, int(i, kind=lua_integer)) > 0) then
            rc = E_TYPE
            if (lua_isinteger(lua%ptr, -1) == 1) then
                value = lua_tointeger(lua%ptr, -1)
                rc = E_NONE
            end if
        end if
        call lua_pop(lua%ptr, 1)
    end function lua_get_i8

    integer function lua_get_l(lua, i, value) result(rc)
        !! Returns logical from table element `i` in `value`.
        type(lua_state_type), intent(inout) :: lua   !! Lua type.
        integer,              intent(in)    :: i     !! Variable index.
        logical,              intent(inout) :: value !! Variable value.
        integer                             :: v

        rc = dm_lua_get(lua, i, v)
        if (rc == E_NONE) value = (v > 0)
    end function lua_get_l

    integer function lua_get_r8(lua, i, value) result(rc)
        !! Returns 8-byte real from table element `i` in `value`.
        type(lua_state_type), intent(inout) :: lua   !! Lua type.
        integer,              intent(in)    :: i     !! Variable index.
        real(kind=r8),        intent(inout) :: value !! Variable value.

        rc = E_INVALID
        if (lua_istable(lua%ptr, -1) /= 1) return
        rc = E_EMPTY
        if (lua_rawgeti(lua%ptr, -1, int(i, kind=lua_integer)) > 0) then
            rc = E_TYPE
            if (lua_isnumber(lua%ptr, -1) == 1) then
                value = lua_tonumber(lua%ptr, -1)
                rc = E_NONE
            end if
        end if
        call lua_pop(lua%ptr, 1)
    end function lua_get_r8

    integer function lua_global_(lua, name) result(rc)
        !! Pushes global variable on stack.
        type(lua_state_type), intent(inout) :: lua  !! Lua type.
        character(len=*),     intent(in)    :: name !! Variable name.

        rc = E_EMPTY
        if (lua_getglobal(lua%ptr, name) <= 0) return
        rc = E_NONE
    end function lua_global_

    integer function lua_global_a(lua, name, value) result(rc)
        !! Returns the value of global variable as allocatable string.
        type(lua_state_type), intent(inout) :: lua   !! Lua type.
        character(len=*),     intent(in)    :: name  !! Variable name.
        character(len=*),     intent(inout) :: value !! Variable value.

        rc = E_TYPE
        if (lua_getglobal(lua%ptr, name) == LUA_TSTRING) then
            value = lua_tostring(lua%ptr, -1)
            rc = E_NONE
        end if
        call lua_pop(lua%ptr, 1)
    end function lua_global_a

    integer function lua_global_i4(lua, name, value) result(rc)
        !! Returns the value of global variable as 4-byte integer.
        type(lua_state_type), intent(inout) :: lua   !! Lua type.
        character(len=*),     intent(in)    :: name  !! Variable name.
        integer(kind=i4),     intent(inout) :: value !! Variable value.

        rc = E_TYPE
        if (lua_getglobal(lua%ptr, name) == LUA_TNUMBER) then
            value = int(lua_tointeger(lua%ptr, -1), kind=i4)
            rc = E_NONE
        end if
        call lua_pop(lua%ptr, 1)
    end function lua_global_i4

    integer function lua_global_i8(lua, name, value) result(rc)
        !! Returns the value of global variable as 8-byte integer.
        type(lua_state_type), intent(inout) :: lua   !! Lua type.
        character(len=*),     intent(in)    :: name  !! Variable name.
        integer(kind=i8),     intent(inout) :: value !! Variable value.

        rc = E_TYPE
        if (lua_getglobal(lua%ptr, name) == LUA_TNUMBER) then
            value = lua_tointeger(lua%ptr, -1)
            rc = E_NONE
        end if
        call lua_pop(lua%ptr, 1)
    end function lua_global_i8

    integer function lua_global_l(lua, name, value) result(rc)
        !! Returns the value of global variable as logical.
        type(lua_state_type), intent(inout) :: lua   !! Lua type.
        character(len=*),     intent(in)    :: name  !! Variable name.
        logical,              intent(inout) :: value !! Variable value.
        integer                             :: v

        rc = dm_lua_global(lua, name, v)
        if (rc == E_NONE) value = (v > 0)
    end function lua_global_l

    integer function lua_global_r8(lua, name, value) result(rc)
        !! Returns the value of global variable as 8-byte real.
        type(lua_state_type), intent(inout) :: lua   !! Lua type.
        character(len=*),     intent(in)    :: name  !! Variable name.
        real(kind=r8),        intent(inout) :: value !! Variable value.

        rc = E_TYPE
        if (lua_getglobal(lua%ptr, name) == LUA_TNUMBER) then
            value = lua_tonumber(lua%ptr, -1)
            rc = E_NONE
        end if
        call lua_pop(lua%ptr, 1)
    end function lua_global_r8

    integer function lua_to_job(lua, job) result(rc)
        !! Reads Lua table into Fortran job type. The table has to be on top of
        !! the stack and will be removed once finished.
        type(lua_state_type), intent(inout) :: lua !! Lua type.
        type(job_type),       intent(out)   :: job !! Job type.

        lua_block: block
            rc = E_TYPE
            if (.not. dm_lua_is_table(lua)) exit lua_block

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
        type(lua_state_type), intent(inout) :: lua    !! Lua type.
        type(observ_type),    intent(out)   :: observ !! Observation type.

        integer :: i, j, sz
        integer :: nrec, nreq, nres

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
            rc = dm_lua_field(lua, 'path',       observ%path)
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
                rc = dm_lua_get(lua, i)
                if (dm_is_error(rc)) exit req_loop

                rc = dm_lua_field(lua, 'timestamp',  observ%requests(i)%timestamp)
                rc = dm_lua_field(lua, 'request',    observ%requests(i)%request)
                rc = dm_lua_field(lua, 'response',   observ%requests(i)%response)
                rc = dm_lua_field(lua, 'delimiter',  observ%requests(i)%delimiter)
                rc = dm_lua_field(lua, 'pattern',    observ%requests(i)%pattern)
                rc = dm_lua_field(lua, 'delay',      observ%requests(i)%delay)
                rc = dm_lua_field(lua, 'error',      observ%requests(i)%error)
                rc = dm_lua_field(lua, 'retries',    observ%requests(i)%retries)
                rc = dm_lua_field(lua, 'state',      observ%requests(i)%state)
                rc = dm_lua_field(lua, 'timeout',    observ%requests(i)%timeout)
                rc = dm_lua_field(lua, 'nresponses', observ%requests(i)%nresponses)

                ! Read responses.
                rc = dm_lua_field(lua, 'responses')
                sz = dm_lua_table_size(lua)

                nres = min(REQUEST_MAX_NRESPONSES, int(sz))
                observ%requests(i)%nresponses = nres

                res_loop: do j = 1, nres
                    rc = dm_lua_get(lua, j)
                    if (dm_is_error(rc)) exit res_loop

                    rc = dm_lua_field(lua, 'name',  observ%requests(i)%responses(j)%name)
                    rc = dm_lua_field(lua, 'unit',  observ%requests(i)%responses(j)%unit)
                    rc = dm_lua_field(lua, 'error', observ%requests(i)%responses(j)%error)
                    rc = dm_lua_field(lua, 'value', observ%requests(i)%responses(j)%value)

                    call dm_lua_pop(lua) ! table element
                end do res_loop

                call dm_lua_pop(lua) ! responses
                call dm_lua_pop(lua) ! table element

                rc = E_NONE
            end do req_loop

            call dm_lua_pop(lua)
        end block observ_block

        call dm_lua_pop(lua)
    end function lua_to_observ

    integer function lua_to_observs(lua, observs) result(rc)
        !! Reads Lua table into Fortran observation type array. The table has to
        !! be on top of the stack and will be removed once finished.
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
                rc = dm_lua_get(lua, i)
                if (dm_is_error(rc)) exit lua_block
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
end module dm_lua
