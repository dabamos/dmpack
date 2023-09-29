! Author:  Philipp Engel
! Licence: ISC
module dm_arg
    !! Procedures for command-line argument parsing.
    !!
    !! Create an array of argument types, then read and parse the arguments:
    !!
    !! ```fortran
    !! character(len=72) :: input
    !! integer           :: delay, rc
    !! type(arg_type)    :: args(3)
    !!
    !! args = [ &
    !!     arg_type('input',   short='i', type=ARG_TYPE_CHAR, required=.true.), &
    !!     arg_type('delay',   short='x', type=ARG_TYPE_INTEGER), &
    !!     arg_type('verbose', short='V', type=ARG_TYPE_BOOL) &
    !! ]
    !!
    !! rc = dm_arg_read(args, app='myapp', major=1, minor=0)
    !! rc = dm_arg_get(args(1), input)
    !! rc = dm_arg_get(args(2), delay)
    !! rc = dm_arg_get(args(3), verbose)
    !! ```
    !!
    !! Each argument requires name and type. The default type is
    !! `ARG_TYPE_BOOL`. Errors are indicated by the return codes. The
    !! command-line arguments `--help`/-`h` and `--version`/`-v` are processed
    !! automatically by function `dm_arg_read()`.
    use :: dm_app
    use :: dm_ascii
    use :: dm_convert
    use :: dm_error
    use :: dm_file
    use :: dm_id
    use :: dm_kind
    use :: dm_time
    use :: dm_util
    use :: dm_uuid
    use :: dm_version
    implicit none (type, external)
    private

    ! Argument value types.
    integer, parameter, public :: ARG_TYPE_BOOL    = 0 !! Logical argument.
    integer, parameter, public :: ARG_TYPE_CHAR    = 1 !! Character value.
    integer, parameter, public :: ARG_TYPE_INTEGER = 2 !! Integer value.
    integer, parameter, public :: ARG_TYPE_FLOAT   = 3 !! Real value.
    integer, parameter, public :: ARG_TYPE_ID      = 4 !! Valid ID value.
    integer, parameter, public :: ARG_TYPE_UUID    = 5 !! Valid UUID4 value.
    integer, parameter, public :: ARG_TYPE_TIME    = 6 !! Valid ISO 8601 value.
    integer, parameter, public :: ARG_TYPE_FILE    = 7 !! Path to file on file system (must exist).
    integer, parameter, public :: ARG_TYPE_DB      = 8 !! Path to database on file system (must exist).

    integer, parameter, public :: ARG_NAME_LEN  = 32            !! Maximum length of argument name.
    integer, parameter, public :: ARG_VALUE_LEN = FILE_PATH_LEN !! Maximum length of argument value.

    type, public :: arg_type
        !! Argument description type.
        character(len=ARG_NAME_LEN)  :: name     = ' '           !! Identifier of the argument (without leading --).
        character                    :: short    = ASCII_NUL     !! Short argument character.
        character(len=ARG_VALUE_LEN) :: value    = ' '           !! Default and passed value (if any).
        integer                      :: max_len  = ARG_VALUE_LEN !! Maximum argument value length.
        integer                      :: min_len  = 0             !! Minimum argument value length.
        integer                      :: type     = ARG_TYPE_BOOL !! Value data type.
        logical                      :: required = .false.       !! Option is mandatory.
        logical                      :: passed   = .false.       !! Option was passed.
        integer                      :: error    = E_ARG         !! Occured error.
    end type arg_type

    interface dm_arg_get
        !! Returns argument value.
        module procedure :: arg_get_a
        module procedure :: arg_get_b
        module procedure :: arg_get_f
        module procedure :: arg_get_i
    end interface

    public :: dm_arg_get
    public :: dm_arg_has
    public :: dm_arg_help
    public :: dm_arg_parse
    public :: dm_arg_read
    public :: dm_arg_validate

    private :: arg_get_a
    private :: arg_get_b
    private :: arg_get_f
    private :: arg_get_i
contains
    ! ******************************************************************
    ! PUBLIC PROCEDURES.
    ! ****************************************************************** 
    logical function dm_arg_has(name, short) result(has)
        !! Returns `.true.` if argument of given name is passed without value.
        character(len=*), intent(in)           :: name  !! Name of command-line argument.
        character,        intent(in), optional :: short !! Short name.

        type(arg_type) :: args(1)

        has = .false.
        args(1) = arg_type(name=name, type=ARG_TYPE_BOOL)
        if (present(short)) args(1)%short = short
        call dm_arg_parse(args)
        has = (args(1)%error == E_NONE)
    end function dm_arg_has

    integer function dm_arg_read(args, app, major, minor) result(rc)
        !! Reads all arguments from command-line and prints error message if one
        !! is missing. Returns the error code of the first invalid argument.
        !!
        !! The function also parses the command-line arguments for
        !! `-v`/`--version` to display the current application and library
        !! version, and `-h`/`--help` to output all available command-line
        !! arguments. If one of these arguments is passed, `dm_stop(0)` is
        !! called afterwards.
        type(arg_type),   intent(inout)        :: args(:) !! Arguments to match.
        character(len=*), intent(in), optional :: app     !! App name (for `-v`).
        integer,          intent(in), optional :: major   !! Major version number (for `-v`).
        integer,          intent(in), optional :: minor   !! Minor version number (for `-v`).

        integer :: i, n
        integer :: major_, minor_

        rc = E_NONE

        major_ = 0
        minor_ = 0

        if (present(major)) major_ = major
        if (present(minor)) minor_ = minor

        if (dm_arg_has('version', 'v')) then
            ! Print program and library version, then stop.
            if (present(app)) then
                call dm_app_out(app, major_, minor_)
            else
                write (stdout, '("DMPACK ", a3)') DM_VERSION_STRING
            end if

            call dm_stop(0)
        end if

        if (dm_arg_has('help', 'h')) then
            ! Print help, then stop.
            call dm_arg_help(args)
            call dm_stop(0)
        end if

        rc = E_EMPTY
        call dm_arg_parse(args)

        do i = 1, size(args)
            rc = dm_arg_validate(args(i))

            select case (rc)
                case (E_NONE)
                case (E_ARG_NOT_FOUND)
                    cycle

                case (E_ARG_INVALID)
                    call dm_error_out(rc, 'argument --' // trim(args(i)%name) // ' required')

                case (E_ARG_NO_VALUE)
                    call dm_error_out(rc, 'argument --' // trim(args(i)%name) // ' requires value')

                case (E_ARG_TYPE)
                    select case (args(i)%type)
                        case (ARG_TYPE_INTEGER)
                            call dm_error_out(rc, 'argument --' // trim(args(i)%name) // ' not an integer')
                        case (ARG_TYPE_FLOAT)
                            call dm_error_out(rc, 'argument --' // trim(args(i)%name) // ' not a number')
                        case (ARG_TYPE_ID)
                            call dm_error_out(rc, 'argument --' // trim(args(i)%name) // ' not a valid id')
                        case (ARG_TYPE_UUID)
                            call dm_error_out(rc, 'argument --' // trim(args(i)%name) // ' not a valid UUID4')
                        case (ARG_TYPE_TIME)
                            call dm_error_out(rc, 'argument --' // trim(args(i)%name) // ' not in ISO 8601')
                        case (ARG_TYPE_FILE)
                            call dm_error_out(rc, 'file ' // trim(args(i)%value) // ' not found')
                        case (ARG_TYPE_DB)
                            call dm_error_out(rc, 'database ' // trim(args(i)%value) // ' not found')
                    end select

                case (E_ARG_LENGTH)
                    n = len_trim(args(i)%value)
                    if (n > args(i)%max_len) then
                        call dm_error_out(rc, 'argument --' // trim(args(i)%name) // ' too long, max. ' // &
                                          dm_itoa(args(i)%max_len))
                    else if (n < args(i)%min_len) then
                        call dm_error_out(rc, 'argument --' // trim(args(i)%name) // ' too short, min. ' // &
                                          dm_itoa(args(i)%min_len))
                    end if
            end select

            exit
        end do
    end function dm_arg_read

    integer function dm_arg_validate(arg) result(rc)
        !! Validates given argument.
        type(arg_type), intent(inout) :: arg !! Argument to validate.

        integer          :: error
        integer(kind=i8) :: i
        real(kind=r8)    :: f

        validate_block: block
            rc = E_ARG
            if (len_trim(arg%name) == 0) exit validate_block

            rc = E_ARG_INVALID
            if (arg%required .and. .not. arg%passed) exit validate_block

            rc = E_ARG_NOT_FOUND
            if (.not. arg%passed) exit validate_block

            rc = E_ARG_TYPE
            select case (arg%type)
                case (ARG_TYPE_FLOAT)
                    call dm_convert_to(arg%value, f, error)
                    if (dm_is_error(error)) exit validate_block

                case (ARG_TYPE_INTEGER)
                    call dm_convert_to(arg%value, i, error)
                    if (dm_is_error(error)) exit validate_block

                case (ARG_TYPE_ID)
                    if (.not. dm_id_valid(arg%value)) exit validate_block

                case (ARG_TYPE_UUID)
                    if (.not. dm_uuid4_valid(arg%value)) exit validate_block

                case (ARG_TYPE_TIME)
                    if (.not. dm_time_valid(arg%value)) exit validate_block

                case (ARG_TYPE_FILE, ARG_TYPE_DB)
                    if (.not. dm_file_exists(arg%value) .and. arg%required) exit validate_block
            end select

            rc = E_NONE
        end block validate_block

        arg%error = rc
    end function dm_arg_validate

    subroutine dm_arg_help(args)
        !! Prints command-line arguments to standard output if `--help` or `-h`
        !! is passed.
        type(arg_type), intent(inout) :: args(:) !! Arguments array.
        integer                       :: i

        write (stdout, '("Available command-line options:", /)')

        do i = 1, size(args)
            write (stdout, '(4x, "-", a1, ", --", a, 1x)', advance='no') &
                args(i)%short, trim(args(i)%name)

            select case (args(i)%type)
                case (ARG_TYPE_CHAR)
                    write (stdout, '("<string>")')
                case (ARG_TYPE_INTEGER)
                    write (stdout, '("<integer>")')
                case (ARG_TYPE_FLOAT)
                    write (stdout, '("<float>")')
                case (ARG_TYPE_ID, ARG_TYPE_UUID)
                    write (stdout, '("<id>")')
                case (ARG_TYPE_TIME)
                    write (stdout, '("<timestamp>")')
                case (ARG_TYPE_FILE)
                    write (stdout, '("<file>")')
                case (ARG_TYPE_DB)
                    write (stdout, '("<database>")')
                case default
                    write (stdout, *)
            end select
        end do

        write (stdout, '(4x, "-v, --version")')
        write (stdout, '(4x, "-h, --help", /)')
    end subroutine dm_arg_help

    subroutine dm_arg_parse(args)
        !! Parses command-line arguments string `command` for arguments.
        type(arg_type), intent(inout) :: args(:) !! Arguments array.

        character(len=ARG_VALUE_LEN) :: name, value
        integer                      :: i, j, k, n, stat

        n = command_argument_count()

        do i = 1, size(args)
            do j = 1, n
                ! Match argument.
                args(i)%error = E_ARG_NOT_FOUND
                call get_command_argument(j, name)

                if (name(1:1) /= '-') cycle
                if (name(2:2) /= args(i)%short .and. &
                    name(3:)  /= args(i)%name) cycle

                ! No value to expect.
                if (args(i)%type == ARG_TYPE_BOOL) then
                    args(i)%error  = E_NONE
                    args(i)%passed = .true.
                    exit
                end if

                ! No value passed.
                args(i)%error = E_ARG_NO_VALUE
                if (j == n) exit

                call get_command_argument(j + 1, value, length=k, status=stat)
                if (stat /= 0 .or. k == 0 .or. value(1:1) == '-') exit

                ! Minimum and maximum value length.
                args(i)%error = E_ARG_LENGTH
                if (k > args(i)%max_len .or. k < args(i)%min_len) exit

                ! Value found.
                args(i)%value  = dm_ascii_escape(adjustl(value))
                args(i)%passed = .true.
                args(i)%error  = E_NONE

                exit
            end do
        end do
    end subroutine dm_arg_parse

    ! ******************************************************************
    ! PRIVATE PROCEDURES.
    ! ****************************************************************** 
    integer function arg_get_a(arg, value, default, passed) result(rc)
        !! Returns argument value as character string.
        type(arg_type),   intent(inout)         :: arg     !! Arg type.
        character(len=*), intent(inout)         :: value   !! Argument value.
        character(len=*), intent(in),  optional :: default !! Default value.
        logical,          intent(out), optional :: passed  !! Passed or not.

        rc = arg%error

        if (present(passed)) passed = arg%passed

        if (rc == E_ARG_NOT_FOUND) then
            if (present(default)) value = default
            return
        end if

        value = arg%value
    end function arg_get_a

    integer function arg_get_b(arg, value, default, passed) result(rc)
        !! Returns `.true.` if argument has been passed.
        type(arg_type), intent(inout)         :: arg     !! Arg type.
        logical,        intent(inout)         :: value   !! Argument value.
        logical,        intent(in),  optional :: default !! Default value.
        logical,        intent(out), optional :: passed  !! Passed or not.

        rc = arg%error

        if (present(passed)) passed = arg%passed

        if (rc == E_ARG_NOT_FOUND) then
            if (present(default)) value = default
            return
        end if

        value = .true.
    end function arg_get_b

    integer function arg_get_f(arg, value, default, passed) result(rc)
        !! Returns argument value as 8-byte real.
        type(arg_type), intent(inout)         :: arg     !! Arg type.
        real(kind=r8),  intent(inout)         :: value   !! Argument value.
        real(kind=r8),  intent(in),  optional :: default !! Default value.
        logical,        intent(out), optional :: passed  !! Passed or not.

        rc = arg%error

        if (present(passed)) passed = arg%passed

        if (rc == E_ARG_NOT_FOUND) then
            if (present(default)) value = default
            return
        end if

        value = dm_atof(arg%value)
    end function arg_get_f

    integer function arg_get_i(arg, value, default, passed) result(rc)
        !! Returns argument value as 4-byte integer.
        type(arg_type), intent(inout)         :: arg     !! Arg type.
        integer,        intent(inout)         :: value   !! Argument value.
        integer,        intent(in),  optional :: default !! Default value.
        logical,        intent(out), optional :: passed  !! Passed or not.

        rc = arg%error

        if (present(passed)) passed = arg%passed

        if (rc == E_ARG_NOT_FOUND) then
            if (present(default)) value = default
            return
        end if

        value = dm_atoi(arg%value)
    end function arg_get_i
end module dm_arg
