! Author:  Philipp Engel
! Licence: ISC
module dm_config
    !! Class for loading Lua-based configuration files.
    !!
    !! The following configuration file `myapp.conf` is given as an example for
    !! a program `myapp`:
    !!
    !! ```lua
    !! -- myapp.conf
    !! myapp = {
    !!     database = "observ.sqlite",
    !!     node = "dummy-node",
    !!     verbose = true
    !! }
    !! ```
    !!
    !! In Fortran, open the configuration file and read the settings with class
    !! method `get()`:
    !!
    !! ```fortran
    !! character(:), allocatable :: database, node
    !! integer                   :: rc
    !! logical                   :: verbose
    !! type(config_class)        :: config
    !!
    !! rc = config%open('myapp.conf', 'myapp')
    !!
    !! if (dm_is_ok(rc)) then
    !!     call config%get('database', database)
    !!     call config%get('node',     node)
    !!     call config%get('verbose',  verbose)
    !! end if
    !!
    !! call config%close()
    !! ```
    use :: dm_error
    use :: dm_id
    use :: dm_kind
    use :: dm_lua
    use :: dm_string
    use :: dm_util
    implicit none (type, external)
    private

    type, public :: config_class
        !! Opaque file configuration class.
        private
        type(lua_state_type) :: lua !! Lua context type.
    contains
        private
        ! Private methods.
        procedure :: get_array_int32  => config_get_array_int32
        procedure :: get_array_int64  => config_get_array_int64
        procedure :: get_array_string => config_get_array_string
        procedure :: get_int32        => config_get_int32
        procedure :: get_int64        => config_get_int64
        procedure :: get_job_list     => config_get_job_list
        procedure :: get_logical      => config_get_logical
        procedure :: get_real32       => config_get_real32
        procedure :: get_real64       => config_get_real64
        procedure :: get_report       => config_get_report
        procedure :: get_string       => config_get_string
        ! Public methods.
        procedure, public :: close    => config_close
        procedure, public :: field    => config_field
        generic,   public :: get      => get_array_int32,  &
                                         get_array_int64,  &
                                         get_array_string, &
                                         get_int32,        &
                                         get_int64,        &
                                         get_job_list,     &
                                         get_logical,      &
                                         get_real32,       &
                                         get_real64,       &
                                         get_report,       &
                                         get_string
        procedure, public :: open     => config_open
        procedure, public :: remove   => config_remove
        procedure, public :: size     => config_size
    end type config_class

    private :: config_close
    private :: config_field
    private :: config_get_array_int32
    private :: config_get_array_int64
    private :: config_get_array_string
    private :: config_get_int32
    private :: config_get_int64
    private :: config_get_job_list
    private :: config_get_logical
    private :: config_get_real32
    private :: config_get_real64
    private :: config_get_report
    private :: config_get_string
    private :: config_open
    private :: config_remove
    private :: config_size

    private :: config_error
contains
    ! **************************************************************************
    ! PRIVATE CLASS FUNCTIONS.
    ! **************************************************************************
    integer function config_field(this, name) result(rc)
        !! Loads field value on to Lua stack. Returns `E_CONFIG` on error.
        class(config_class), intent(inout) :: this !! Config object.
        character(*),        intent(in)    :: name !! Setting name.

        rc = dm_lua_field(this%lua, name)
        if (dm_is_error(rc)) rc = E_CONFIG
    end function config_field

    integer function config_open(this, path, name, geocom) result(rc)
        !! Opens configuration file and optionally loads the table of the given
        !! name if the argument has been passed.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_INVALID` if the file path is empty.
        !! * `E_LUA` if a Lua error occured.
        !! * `E_NOT_FOUND` if the configuration file is not found.
        !! * `E_TYPE` if the configuration `name` is not a Lua table.
        !!
        use :: dm_file,       only: dm_file_exists
        use :: dm_lua_api,    only: dm_lua_api_register
        use :: dm_lua_geocom, only: dm_lua_geocom_register

        class(config_class), intent(inout)        :: this   !! Config object.
        character(*),        intent(in)           :: path   !! Path to config file.
        character(*),        intent(in), optional :: name   !! Name of table. Passed name implies table loading.
        logical,             intent(in), optional :: geocom !! Register GeoCOM API for Lua.

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
            rc = dm_lua_init(this%lua, libs=.true.)
            if (dm_is_error(rc)) exit lua_block

            ! Register DMPACK API for Lua.
            rc = dm_lua_api_register(this%lua)
            if (dm_is_error(rc)) exit lua_block

            ! Register GeoCOM API for Lua.
            if (dm_present(geocom, .false.)) then
                rc = dm_lua_geocom_register(this%lua, procedures=.true., errors=.true.)
                if (dm_is_error(rc)) exit lua_block
            end if

            ! Load and evaluate Lua script.
            rc = dm_lua_open(this%lua, path, eval=.true.)
            if (dm_is_error(rc)) exit lua_block

            ! Load Lua table onto stack.
            if (.not. dm_string_is_present(name)) exit lua_block
            rc = dm_lua_table(this%lua, name)
        end block lua_block

        if (dm_is_ok(rc)) return

        if (rc >= E_LUA .and. rc <= E_LUA_FILE) then
            call dm_error_out(rc, dm_lua_error_message(this%lua))
        else if (present(name)) then
            call dm_error_out(rc, 'failed to read configuration ' // trim(name) // ' from file ' // path)
        else
            call dm_error_out(rc, 'failed to read configuration from file ' // path)
        end if
    end function config_open

    integer function config_size(this) result(n)
        !! Returns size of configuration table.
        class(config_class), intent(inout) :: this !! Config object.

        n = 0
        if (.not. dm_lua_is_table(this%lua)) return
        n = dm_lua_table_size(this%lua)
    end function config_size

    ! **************************************************************************
    ! PRIVATE CLASS SUBROUTINES.
    ! **************************************************************************
    subroutine config_close(this)
        !! Closes configuration file.
        class(config_class), intent(inout) :: this !! Config object.

        ! Remove last table from stack.
        if (.not. dm_lua_is_nil(this%lua)) call dm_lua_pop(this%lua)
        call dm_lua_destroy(this%lua)
    end subroutine config_close

    subroutine config_remove(this)
        !! Removes last stack element.
        class(config_class), intent(inout) :: this !! Config object.

        call dm_lua_pop(this%lua)
    end subroutine config_remove

    subroutine config_get_array_int32(this, name, values, error)
        !! Returns configuration values as 4-byte integer array in `values`.
        class(config_class),      intent(inout)         :: this      !! Config object.
        character(*),             intent(in)            :: name      !! Setting name.
        integer(i4), allocatable, intent(out)           :: values(:) !! Setting values.
        integer,                  intent(out), optional :: error     !! Error code.

        integer :: rc

        rc = dm_lua_field(this%lua, name, values)
        rc = config_error(rc, param=name)
        if (present(error)) error = rc
    end subroutine config_get_array_int32

    subroutine config_get_array_int64(this, name, values, error)
        !! Returns configuration values as 8-byte integer array in `values`.
        class(config_class),      intent(inout)         :: this      !! Config object.
        character(*),             intent(in)            :: name      !! Setting name.
        integer(i8), allocatable, intent(out)           :: values(:) !! Setting values.
        integer,                  intent(out), optional :: error     !! Error code.

        integer :: rc

        rc = dm_lua_field(this%lua, name, values)
        rc = config_error(rc, param=name)
        if (present(error)) error = rc
    end subroutine config_get_array_int64

    subroutine config_get_array_string(this, name, values, error)
        !! Returns configuration values as string array in `values`.
        class(config_class),       intent(inout)         :: this      !! Config object.
        character(*),              intent(in)            :: name      !! Setting name.
        character(*), allocatable, intent(inout)         :: values(:) !! Setting values.
        integer,                   intent(out), optional :: error     !! Error code.

        integer :: rc

        rc = dm_lua_field(this%lua, name, values)
        rc = config_error(rc, param=name)
        if (present(error)) error = rc
    end subroutine config_get_array_string

    subroutine config_get_int32(this, name, value, error)
        !! Returns configuration value as 4-byte integer in `value`.
        class(config_class), intent(inout)         :: this  !! Config object.
        character(*),        intent(in)            :: name  !! Setting name.
        integer(i4),         intent(inout)         :: value !! Setting value.
        integer,             intent(out), optional :: error !! Error code.

        integer :: rc

        rc = dm_lua_field(this%lua, name, value)
        rc = config_error(rc, param=name)
        if (present(error)) error = rc
    end subroutine config_get_int32

    subroutine config_get_int64(this, name, value, error)
        !! Returns configuration value as 8-byte integer in `value`.
        class(config_class), intent(inout)         :: this  !! Config object.
        character(*),        intent(in)            :: name  !! Setting name.
        integer(i8),         intent(inout)         :: value !! Setting value.
        integer,             intent(out), optional :: error !! Error code.

        integer :: rc

        rc = dm_lua_field(this%lua, name, value)
        rc = config_error(rc, param=name)
        if (present(error)) error = rc
    end subroutine config_get_int64

    subroutine config_get_job_list(this, name, value, error, field)
        !! Returns configuration value as job list in `value`.
        use :: dm_job

        class(config_class), intent(inout)         :: this  !! Config object.
        character(*),        intent(in)            :: name  !! Setting name.
        type(job_list_type), intent(out)           :: value !! Setting value.
        integer,             intent(out), optional :: error !! Error code.
        logical,             intent(in),  optional :: field !! Read from table field.

        integer :: rc

        if (dm_present(field, .true.)) rc = dm_lua_field(this%lua, name)
        rc = dm_lua_to(this%lua, value)
        rc = config_error(rc, param=name)
        if (present(error)) error = rc
    end subroutine config_get_job_list

    subroutine config_get_logical(this, name, value, error)
        !! Returns configuration value as logical in `value` (if 0 or 1).
        class(config_class), intent(inout)         :: this  !! Config object.
        character(*),        intent(in)            :: name  !! Setting name.
        logical,             intent(inout)         :: value !! Setting value.
        integer,             intent(out), optional :: error !! Error code.

        integer :: rc

        rc = dm_lua_field(this%lua, name, value)
        rc = config_error(rc, param=name)
        if (present(error)) error = rc
    end subroutine config_get_logical

    subroutine config_get_real32(this, name, value, error)
        !! Returns configuration value as 4-byte real in `value`.
        class(config_class), intent(inout)         :: this  !! Config object.
        character(*),        intent(in)            :: name  !! Setting name.
        real(r4),            intent(inout)         :: value !! Setting value.
        integer,             intent(out), optional :: error !! Error code.

        integer       :: rc
        real(r8) :: value_

        call this%get(name, value_, rc)
        if (present(error)) error = rc
        if (dm_is_error(rc)) return
        value = real(value_, r4)
    end subroutine config_get_real32

    subroutine config_get_real64(this, name, value, error)
        !! Returns configuration value as 8-byte real in `value`.
        class(config_class), intent(inout)         :: this  !! Config object.
        character(*),        intent(in)            :: name  !! Setting name.
        real(r8),            intent(inout)         :: value !! Setting value.
        integer,             intent(out), optional :: error !! Error code.

        integer :: rc

        rc = dm_lua_field(this%lua, name, value)
        rc = config_error(rc, param=name)
        if (present(error)) error = rc
    end subroutine config_get_real64

    subroutine config_get_report(this, name, value, error, field)
        !! Returns configuration value as report in `value`.
        use :: dm_report

        class(config_class), intent(inout)         :: this  !! Config object.
        character(*),        intent(in)            :: name  !! Setting name.
        type(report_type),   intent(out)           :: value !! Setting value.
        integer,             intent(out), optional :: error !! Error code.
        logical,             intent(in),  optional :: field !! Read from table field.

        integer :: rc

        if (dm_present(field, .true.)) rc = dm_lua_field(this%lua, name)
        rc = dm_lua_to(this%lua, value)
        rc = config_error(rc, param=name)
        if (present(error)) error = rc
    end subroutine config_get_report

    subroutine config_get_string(this, name, value, error)
        !! Returns configuration value as character string in `value`. The
        !! string is unescaped by default (`\\` is converted to `\`).
        class(config_class), intent(inout)         :: this  !! Config object.
        character(*),        intent(in)            :: name  !! Setting name.
        character(*),        intent(inout)         :: value !! Setting value.
        integer,             intent(out), optional :: error !! Error code.

        integer :: rc

        rc = dm_lua_field(this%lua, name, value, unescape=.true.)
        rc = config_error(rc, param=name)
        if (present(error)) error = rc
    end subroutine config_get_string

    ! **************************************************************************
    ! PRIVATE UTILITY FUNCTIONS.
    ! **************************************************************************
    integer function config_error(error, param) result(rc)
        !! Returns `E_CONFIG` on error and prints error message to standard
        !! error.
        integer,      intent(in)           :: error !! Error code.
        character(*), intent(in), optional :: param !! Lua table name.

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
end module dm_config
