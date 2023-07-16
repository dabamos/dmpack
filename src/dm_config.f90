! Author:  Philipp Engel
! Licence: ISC
module dm_config
    !! Module for loading Lua configuration files.
    use, intrinsic :: iso_c_binding
    use :: dm_error
    use :: dm_file
    use :: dm_id
    use :: dm_job
    use :: dm_lua
    use :: dm_report
    use :: dm_type
    implicit none (type, external)
    private

    type, public :: config_type
        !! Opaque file configuration type.
        private
        type(lua_state_type) :: lua !! Lua type.
    end type config_type

    interface dm_config_get
        !! Generic interface to return configuration value by name.
        module procedure :: config_get_
        module procedure :: config_get_a
        module procedure :: config_get_i4
        module procedure :: config_get_i8
        module procedure :: config_get_job_list
        module procedure :: config_get_l
        module procedure :: config_get_r4
        module procedure :: config_get_r8
        module procedure :: config_get_report
    end interface

    public :: dm_config_close
    public :: dm_config_get
    public :: dm_config_open
    public :: dm_config_remove
    public :: dm_config_size

    private :: config_error
    private :: config_get_
    private :: config_get_a
    private :: config_get_i4
    private :: config_get_i8
    private :: config_get_job_list
    private :: config_get_l
    private :: config_get_r4
    private :: config_get_r8
    private :: config_get_report
contains
    ! ******************************************************************
    ! PUBLIC PROCEDURES.
    ! ******************************************************************
    integer function dm_config_open(config, path, name) result(rc)
        !! Opens configuration file and optionally loads the table of the given
        !! name if the argument has been passed.
        type(config_type), intent(inout)        :: config !! Config type.
        character(len=*),  intent(in)           :: path   !! Path to config file.
        character(len=*),  intent(in), optional :: name   !! Name of table. Passed name implies table loading.

        open_block: block
            rc = E_INVALID
            if (len_trim(path) == 0) exit open_block

            rc = E_NOT_FOUND
            if (.not. dm_file_exists(path)) exit open_block

            rc = dm_lua_init(config%lua)
            if (dm_is_error(rc)) exit open_block

            rc = dm_lua_open(config%lua, path, eval=.true.)
            if (dm_is_error(rc)) exit open_block

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

        call dm_lua_pop(config%lua)
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
            call dm_error_out(error, 'invalid parameter "' // trim(param) // '" in configuration')
            return
        end if

        call dm_error_out(error, 'invalid parameter in configuration')
    end function config_error

    integer function config_get_(config, name) result(rc)
        !! Loads field value on to stack.
        type(config_type), intent(inout) :: config !! Config type.
        character(len=*),  intent(in)    :: name   !! Setting name.

        rc = dm_lua_field(config%lua, name)
        rc = config_error(rc, param=name)
    end function config_get_

    integer function config_get_a(config, name, value) result(rc)
        !! Returns configuration value as character string.
        type(config_type), intent(inout) :: config !! Config type.
        character(len=*),  intent(in)    :: name   !! Setting name.
        character(len=*),  intent(inout) :: value  !! Setting value.

        rc = dm_lua_field(config%lua, name, value)
        rc = config_error(rc, param=name)
    end function config_get_a

    integer function config_get_i4(config, name, value) result(rc)
        !! Returns configuration value as 4-byte integer.
        type(config_type), intent(inout) :: config !! Config type.
        character(len=*),  intent(in)    :: name   !! Setting name.
        integer(kind=i4),  intent(inout) :: value  !! Setting value.

        rc = dm_lua_field(config%lua, name, value)
        rc = config_error(rc, param=name)
    end function config_get_i4

    integer function config_get_i8(config, name, value) result(rc)
        !! Returns configuration value as 8-byte integer.
        type(config_type), intent(inout) :: config !! Config type.
        character(len=*),  intent(in)    :: name   !! Setting name.
        integer(kind=i8),  intent(inout) :: value  !! Setting value.

        rc = dm_lua_field(config%lua, name, value)
        rc = config_error(rc, param=name)
    end function config_get_i8

    integer function config_get_job_list(config, name, value, field) result(rc)
        !! Returns configuration value as job list.
        type(config_type),   intent(inout)        :: config !! Config type.
        character(len=*),    intent(in)           :: name   !! Setting name.
        type(job_list_type), intent(out)          :: value  !! Setting value.
        logical,             intent(in), optional :: field  !! Read from table field.
        logical                                   :: field_

        field_ = .true.
        if (present(field)) field_ = field
        if (field_) rc = dm_lua_field(config%lua, name)
        rc = dm_lua_to(config%lua, value)
        rc = config_error(rc, param=name)
    end function config_get_job_list

    integer function config_get_l(config, name, value) result(rc)
        !! Returns configuration value as logical (if 0 or 1).
        type(config_type), intent(inout) :: config !! Config type.
        character(len=*),  intent(in)    :: name   !! Setting name.
        logical,           intent(inout) :: value  !! Setting value.

        rc = dm_lua_field(config%lua, name, value)
        rc = config_error(rc, param=name)
    end function config_get_l

    integer function config_get_r4(config, name, value) result(rc)
        !! Returns configuration value as 4-byte real.
        type(config_type), intent(inout) :: config !! Config type.
        character(len=*),  intent(in)    :: name   !! Setting name.
        real(kind=r4),     intent(inout) :: value  !! Setting value.
        real(kind=r8)                    :: value_

        rc = dm_config_get(config, name, value_)
        if (dm_is_error(rc)) return
        value = real(value_, kind=r4)
    end function config_get_r4

    integer function config_get_r8(config, name, value) result(rc)
        !! Returns configuration value as 8-byte real.
        type(config_type), intent(inout) :: config !! Config type.
        character(len=*),  intent(in)    :: name   !! Setting name.
        real(kind=r8),     intent(inout) :: value  !! Setting value.

        rc = dm_lua_field(config%lua, name, value)
        rc = config_error(rc, param=name)
    end function config_get_r8

    integer function config_get_report(config, name, value, field) result(rc)
        !! Returns configuration value as report.
        type(config_type), intent(inout)        :: config !! Config type.
        character(len=*),  intent(in)           :: name   !! Setting name.
        type(report_type), intent(out)          :: value  !! Setting value.
        logical,           intent(in), optional :: field  !! Read from table field.
        logical                                 :: field_

        field_ = .true.
        if (present(field)) field_ = field
        if (field_) rc = dm_lua_field(config%lua, name)
        rc = dm_lua_to(config%lua, value)
        rc = config_error(rc, param=name)
    end function config_get_report
end module dm_config
