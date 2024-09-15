! Author:  Philipp Engel
! Licence: ISC
module dm_config
    !! Module for loading Lua configuration files.
    use :: dm_error
    use :: dm_id
    use :: dm_kind
    use :: dm_lua
    use :: dm_string
    implicit none (type, external)
    private

    type, public :: config_type
        !! Opaque file configuration type.
        private
        type(lua_state_type) :: lua !! Lua type.
    end type config_type

    interface dm_config_get
        !! Generic interface to return configuration value by name.
        module procedure :: config_get_array_int32
        module procedure :: config_get_array_int64
        module procedure :: config_get_int32
        module procedure :: config_get_int64
        module procedure :: config_get_job_list
        module procedure :: config_get_logical
        module procedure :: config_get_real32
        module procedure :: config_get_real64
        module procedure :: config_get_report
        module procedure :: config_get_string
    end interface dm_config_get

    public :: dm_config_close
    public :: dm_config_field
    public :: dm_config_get
    public :: dm_config_open
    public :: dm_config_remove
    public :: dm_config_size

    private :: config_error
    private :: config_get_array_int32
    private :: config_get_array_int64
    private :: config_get_int32
    private :: config_get_int64
    private :: config_get_job_list
    private :: config_get_logical
    private :: config_get_real32
    private :: config_get_real64
    private :: config_get_report
    private :: config_get_string
contains
    ! ******************************************************************
    ! PUBLIC PROCEDURES.
    ! ******************************************************************
    integer function dm_config_field(config, name) result(rc)
        !! Loads field value on to Lua stack.
        type(config_type), intent(inout) :: config !! Config type.
        character(len=*),  intent(in)    :: name   !! Setting name.

        rc = dm_lua_field(config%lua, name)
        if (dm_is_error(rc)) rc = E_CONFIG
    end function dm_config_field

    integer function dm_config_open(config, path, name, geocom) result(rc)
        !! Opens configuration file and optionally loads the table of the given
        !! name if the argument has been passed.
        use :: dm_file,       only: dm_file_exists
        use :: dm_lua_api,    only: dm_lua_api_register
        use :: dm_lua_geocom, only: dm_lua_geocom_register

        type(config_type), intent(inout)        :: config !! Config type.
        character(len=*),  intent(in)           :: path   !! Path to config file.
        character(len=*),  intent(in), optional :: name   !! Name of table. Passed name implies table loading.
        logical,           intent(in), optional :: geocom !! Register GeoCOM API for Lua.

        logical :: geocom_

        geocom_ = .false.
        if (present(geocom)) geocom_ = geocom

        rc = E_INVALID
        if (len_trim(path) == 0) then
            call dm_error_out(rc, 'missing path to configuration file')
            return
        end if

        rc = E_NOT_FOUND
        if (.not. dm_file_exists(path)) then
            call dm_error_out(rc, 'configuration file ' // trim(path) // ' not found')
            return
        end if

        lua_block: block
            ! Initialise Lua interpreter.
            rc = dm_lua_init(config%lua, libs=.true.)
            if (dm_is_error(rc)) exit lua_block

            ! Register DMPACK API for Lua.
            rc = dm_lua_api_register(config%lua)
            if (dm_is_error(rc)) exit lua_block

            ! Register GeoCOM API for Lua.
            if (geocom_) then
                rc = dm_lua_geocom_register(config%lua, procedures=.true., errors=.true.)
                if (dm_is_error(rc)) exit lua_block
            end if

            ! Load and evaluate Lua script.
            rc = dm_lua_open(config%lua, path, eval=.true.)
            if (dm_is_error(rc)) exit lua_block

            ! Load Lua table onto stack.
            if (.not. dm_string_is_present(name)) exit lua_block
            rc = dm_lua_table(config%lua, name)
        end block lua_block

        if (dm_is_ok(rc)) return

        if (rc >= E_LUA .and. rc <= E_LUA_FILE) then
            call dm_error_out(rc, dm_lua_error_string(config%lua))
        else if (present(name)) then
            call dm_error_out(rc, 'failed to read configuration ' // trim(name) // ' from file ' // path)
        else
            call dm_error_out(rc, 'failed to read configuration from file ' // path)
        end if
    end function dm_config_open

    integer function dm_config_size(config) result(n)
        !! Returns size of configuration table.
        type(config_type), intent(inout) :: config !! Config type.

        n = 0
        if (.not. dm_lua_is_table(config%lua)) return
        n = dm_lua_table_size(config%lua)
    end function dm_config_size

    subroutine dm_config_close(config)
        !! Closes configuration file.
        type(config_type), intent(inout) :: config !! Config type.

        ! Remove last table from stack.
        if (.not. dm_lua_is_nil(config%lua)) call dm_lua_pop(config%lua)
        call dm_lua_destroy(config%lua)
    end subroutine dm_config_close

    subroutine dm_config_remove(config)
        !! Removes last stack element.
        type(config_type), intent(inout) :: config !! Config type.

        call dm_lua_pop(config%lua)
    end subroutine dm_config_remove

    ! ******************************************************************
    ! PRIVATE PROCEDURES.
    ! ******************************************************************
    integer function config_error(error, param) result(rc)
        !! Returns `E_CONFIG` on error and prints error message to standard
        !! error.
        integer,          intent(in)           :: error !! Error code.
        character(len=*), intent(in), optional :: param !! Lua table name.

        rc = E_NONE
        if (error == E_NONE)  return
        if (error == E_EMPTY) return

        rc = E_CONFIG
        if (dm_string_is_present(param)) then
            call dm_error_out(error, 'invalid parameter ' // trim(param) // ' in configuration')
        else
            call dm_error_out(error, 'invalid parameter in configuration', extra=.true.)
        end if
    end function config_error

    subroutine config_get_array_int32(config, name, values, error)
        !! Returns configuration values as 4-byte integer array in `values`.
        type(config_type),             intent(inout)         :: config    !! Config type.
        character(len=*),              intent(in)            :: name      !! Setting name.
        integer(kind=i4), allocatable, intent(out)           :: values(:) !! Setting values.
        integer,                       intent(out), optional :: error     !! Error code.

        integer :: rc

        rc = dm_lua_field(config%lua, name, values)
        rc = config_error(rc, param=name)
        if (present(error)) error = rc
    end subroutine config_get_array_int32

    subroutine config_get_array_int64(config, name, values, error)
        !! Returns configuration values as 8-byte integer array in `values`.
        type(config_type),             intent(inout)         :: config    !! Config type.
        character(len=*),              intent(in)            :: name      !! Setting name.
        integer(kind=i8), allocatable, intent(out)           :: values(:) !! Setting values.
        integer,                       intent(out), optional :: error     !! Error code.

        integer :: rc

        rc = dm_lua_field(config%lua, name, values)
        rc = config_error(rc, param=name)
        if (present(error)) error = rc
    end subroutine config_get_array_int64

    subroutine config_get_int32(config, name, value, error)
        !! Returns configuration value as 4-byte integer in `value`.
        type(config_type), intent(inout)         :: config !! Config type.
        character(len=*),  intent(in)            :: name   !! Setting name.
        integer(kind=i4),  intent(inout)         :: value  !! Setting value.
        integer,           intent(out), optional :: error  !! Error code.

        integer :: rc

        rc = dm_lua_field(config%lua, name, value)
        rc = config_error(rc, param=name)
        if (present(error)) error = rc
    end subroutine config_get_int32

    subroutine config_get_int64(config, name, value, error)
        !! Returns configuration value as 8-byte integer in `value`.
        type(config_type), intent(inout)         :: config !! Config type.
        character(len=*),  intent(in)            :: name   !! Setting name.
        integer(kind=i8),  intent(inout)         :: value  !! Setting value.
        integer,           intent(out), optional :: error  !! Error code.

        integer :: rc

        rc = dm_lua_field(config%lua, name, value)
        rc = config_error(rc, param=name)
        if (present(error)) error = rc
    end subroutine config_get_int64

    subroutine config_get_job_list(config, name, value, error, field)
        !! Returns configuration value as job list in `value`.
        use :: dm_job

        type(config_type),   intent(inout)         :: config !! Config type.
        character(len=*),    intent(in)            :: name   !! Setting name.
        type(job_list_type), intent(out)           :: value  !! Setting value.
        integer,             intent(out), optional :: error  !! Error code.
        logical,             intent(in),  optional :: field  !! Read from table field.

        integer :: rc
        logical :: field_

        field_ = .true.
        if (present(field)) field_ = field
        if (field_) rc = dm_lua_field(config%lua, name)
        rc = dm_lua_to(config%lua, value)
        rc = config_error(rc, param=name)
        if (present(error)) error = rc
    end subroutine config_get_job_list

    subroutine config_get_logical(config, name, value, error)
        !! Returns configuration value as logical in `value` (if 0 or 1).
        type(config_type), intent(inout)         :: config !! Config type.
        character(len=*),  intent(in)            :: name   !! Setting name.
        logical,           intent(inout)         :: value  !! Setting value.
        integer,           intent(out), optional :: error  !! Error code.

        integer :: rc

        rc = dm_lua_field(config%lua, name, value)
        rc = config_error(rc, param=name)
        if (present(error)) error = rc
    end subroutine config_get_logical

    subroutine config_get_real32(config, name, value, error)
        !! Returns configuration value as 4-byte real in `value`.
        type(config_type), intent(inout)         :: config !! Config type.
        character(len=*),  intent(in)            :: name   !! Setting name.
        real(kind=r4),     intent(inout)         :: value  !! Setting value.
        integer,           intent(out), optional :: error  !! Error code.

        integer       :: rc
        real(kind=r8) :: value_

        call dm_config_get(config, name, value_, rc)
        if (present(error)) error = rc
        if (dm_is_error(rc)) return
        value = real(value_, kind=r4)
    end subroutine config_get_real32

    subroutine config_get_real64(config, name, value, error)
        !! Returns configuration value as 8-byte real in `value`.
        type(config_type), intent(inout)         :: config !! Config type.
        character(len=*),  intent(in)            :: name   !! Setting name.
        real(kind=r8),     intent(inout)         :: value  !! Setting value.
        integer,           intent(out), optional :: error  !! Error code.

        integer :: rc

        rc = dm_lua_field(config%lua, name, value)
        rc = config_error(rc, param=name)
        if (present(error)) error = rc
    end subroutine config_get_real64

    subroutine config_get_report(config, name, value, error, field)
        !! Returns configuration value as report in `value`.
        use :: dm_report

        type(config_type), intent(inout)         :: config !! Config type.
        character(len=*),  intent(in)            :: name   !! Setting name.
        type(report_type), intent(out)           :: value  !! Setting value.
        integer,           intent(out), optional :: error  !! Error code.
        logical,           intent(in),  optional :: field  !! Read from table field.

        integer :: rc
        logical :: field_

        field_ = .true.
        if (present(field)) field_ = field
        if (field_) rc = dm_lua_field(config%lua, name)
        rc = dm_lua_to(config%lua, value)
        rc = config_error(rc, param=name)
        if (present(error)) error = rc
    end subroutine config_get_report

    subroutine config_get_string(config, name, value, error)
        !! Returns configuration value as character string in `value`. The
        !! string is unescaped by default (`\\` is converted to `\`).
        type(config_type), intent(inout)         :: config !! Config type.
        character(len=*),  intent(in)            :: name   !! Setting name.
        character(len=*),  intent(inout)         :: value  !! Setting value.
        integer,           intent(out), optional :: error  !! Error code.

        integer :: rc

        rc = dm_lua_field(config%lua, name, value, unescape=.true.)
        rc = config_error(rc, param=name)
        if (present(error)) error = rc
    end subroutine config_get_string
end module dm_config
