! Author:  Philipp Engel
! Licence: ISC
module dm_config
    !! Module for loading Lua configuration files.
    use :: dm_error
    use :: dm_id
    use :: dm_kind
    use :: dm_lua
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
        module procedure :: config_get_stack
        module procedure :: config_get_string
    end interface

    public :: dm_config_close
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
    private :: config_get_stack
    private :: config_get_string
contains
    ! ******************************************************************
    ! PUBLIC PROCEDURES.
    ! ******************************************************************
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

        open_block: block
            rc = E_INVALID
            if (len_trim(path) == 0) exit open_block

            rc = E_NOT_FOUND
            if (.not. dm_file_exists(path)) exit open_block

            ! Initialise Lua interpreter.
            rc = dm_lua_init(config%lua, libs=.true.)
            if (dm_is_error(rc)) exit open_block

            ! Register DMPACK API for Lua.
            rc = dm_lua_api_register(config%lua)
            if (dm_is_error(rc)) exit open_block

            ! Register GeoCOM API for Lua.
            if (geocom_) then
                rc = dm_lua_geocom_register(config%lua, procedures=.true., errors=.true.)
                if (dm_is_error(rc)) exit open_block
            end if

            ! Load and evaluate Lua script.
            rc = dm_lua_open(config%lua, path, eval=.true.)
            if (dm_is_error(rc)) exit open_block

            ! Load Lua table onto stack.
            if (present(name)) then
                rc = E_INVALID
                if (len_trim(name) == 0) exit open_block
                rc = dm_lua_table(config%lua, name)
            end if

            rc = config_error(rc, name)
        end block open_block

        if (dm_is_ok(rc)) return
        call dm_error_out(rc, 'failed to read configuration ' // trim(name) // ' from file ' // path)
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
        if (error == E_NONE) return
        if (error == E_EMPTY) return

        rc = E_CONFIG
        if (present(param)) then
            call dm_error_out(error, 'invalid parameter ' // trim(param) // ' in configuration')
            return
        end if

        call dm_error_out(error, 'invalid parameter in configuration')
    end function config_error

    integer function config_get_array_int32(config, name, values) result(rc)
        !! Returns configuration values as 4-byte integer array.
        type(config_type),             intent(inout) :: config    !! Config type.
        character(len=*),              intent(in)    :: name      !! Setting name.
        integer(kind=i4), allocatable, intent(out)   :: values(:) !! Setting values.

        rc = dm_lua_field(config%lua, name, values)
        rc = config_error(rc, param=name)
    end function config_get_array_int32

    integer function config_get_array_int64(config, name, values) result(rc)
        !! Returns configuration values as 8-byte integer array.
        type(config_type),             intent(inout) :: config    !! Config type.
        character(len=*),              intent(in)    :: name      !! Setting name.
        integer(kind=i8), allocatable, intent(out)   :: values(:) !! Setting values.

        rc = dm_lua_field(config%lua, name, values)
        rc = config_error(rc, param=name)
    end function config_get_array_int64

    integer function config_get_int32(config, name, value) result(rc)
        !! Returns configuration value as 4-byte integer.
        type(config_type), intent(inout) :: config !! Config type.
        character(len=*),  intent(in)    :: name   !! Setting name.
        integer(kind=i4),  intent(inout) :: value  !! Setting value.

        rc = dm_lua_field(config%lua, name, value)
        rc = config_error(rc, param=name)
    end function config_get_int32

    integer function config_get_int64(config, name, value) result(rc)
        !! Returns configuration value as 8-byte integer.
        type(config_type), intent(inout) :: config !! Config type.
        character(len=*),  intent(in)    :: name   !! Setting name.
        integer(kind=i8),  intent(inout) :: value  !! Setting value.

        rc = dm_lua_field(config%lua, name, value)
        rc = config_error(rc, param=name)
    end function config_get_int64

    integer function config_get_job_list(config, name, value, field) result(rc)
        !! Returns configuration value as job list.
        use :: dm_job

        type(config_type),   intent(inout)        :: config !! Config type.
        character(len=*),    intent(in)           :: name   !! Setting name.
        type(job_list_type), intent(out)          :: value  !! Setting value.
        logical,             intent(in), optional :: field  !! Read from table field.

        logical :: field_

        field_ = .true.
        if (present(field)) field_ = field
        if (field_) rc = dm_lua_field(config%lua, name)
        rc = dm_lua_to(config%lua, value)
        rc = config_error(rc, param=name)
    end function config_get_job_list

    integer function config_get_logical(config, name, value) result(rc)
        !! Returns configuration value as logical (if 0 or 1).
        type(config_type), intent(inout) :: config !! Config type.
        character(len=*),  intent(in)    :: name   !! Setting name.
        logical,           intent(inout) :: value  !! Setting value.

        rc = dm_lua_field(config%lua, name, value)
        rc = config_error(rc, param=name)
    end function config_get_logical

    integer function config_get_real32(config, name, value) result(rc)
        !! Returns configuration value as 4-byte real.
        type(config_type), intent(inout) :: config !! Config type.
        character(len=*),  intent(in)    :: name   !! Setting name.
        real(kind=r4),     intent(inout) :: value  !! Setting value.
        real(kind=r8)                    :: value_

        rc = dm_config_get(config, name, value_)
        if (dm_is_error(rc)) return
        value = real(value_, kind=r4)
    end function config_get_real32

    integer function config_get_real64(config, name, value) result(rc)
        !! Returns configuration value as 8-byte real.
        type(config_type), intent(inout) :: config !! Config type.
        character(len=*),  intent(in)    :: name   !! Setting name.
        real(kind=r8),     intent(inout) :: value  !! Setting value.

        rc = dm_lua_field(config%lua, name, value)
        rc = config_error(rc, param=name)
    end function config_get_real64

    integer function config_get_report(config, name, value, field) result(rc)
        !! Returns configuration value as report.
        use :: dm_report

        type(config_type), intent(inout)        :: config !! Config type.
        character(len=*),  intent(in)           :: name   !! Setting name.
        type(report_type), intent(out)          :: value  !! Setting value.
        logical,           intent(in), optional :: field  !! Read from table field.

        logical :: field_

        field_ = .true.
        if (present(field)) field_ = field
        if (field_) rc = dm_lua_field(config%lua, name)
        rc = dm_lua_to(config%lua, value)
        rc = config_error(rc, param=name)
    end function config_get_report

    integer function config_get_stack(config, name) result(rc)
        !! Loads field value on to Lua stack.
        type(config_type), intent(inout) :: config !! Config type.
        character(len=*),  intent(in)    :: name   !! Setting name.

        rc = dm_lua_field(config%lua, name)
        rc = config_error(rc, param=name)
    end function config_get_stack

    integer function config_get_string(config, name, value) result(rc)
        !! Returns configuration value as character string. The string is
        !! unescaped by default (`\\` is converted to `\`).
        type(config_type), intent(inout) :: config !! Config type.
        character(len=*),  intent(in)    :: name   !! Setting name.
        character(len=*),  intent(inout) :: value  !! Setting value.

        rc = dm_lua_field(config%lua, name, value, unescape=.true.)
        rc = config_error(rc, param=name)
    end function config_get_string
end module dm_config
