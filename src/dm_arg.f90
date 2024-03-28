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
    !! logical           :: verbose
    !! type(arg_type)    :: args(3)
    !!
    !! args = [ &
    !!     arg_type('input',   short='i', type=ARG_TYPE_STRING, required=.true.), &
    !!     arg_type('delay',   short='x', type=ARG_TYPE_INTEGER), &
    !!     arg_type('verbose', short='V', type=ARG_TYPE_LOGICAL) &
    !! ]
    !!
    !! rc = dm_arg_read(args, app='myapp', major=1, minor=0, patch=0)
    !! rc = dm_arg_get(args(1), input)
    !! rc = dm_arg_get(args(2), delay)
    !! rc = dm_arg_get(args(3), verbose)
    !! ```
    !!
    !! Each argument requires name and type. The default type is
    !! `ARG_TYPE_LOGICAL`. Errors are indicated by the return codes. The
    !! command-line arguments `--help`/-`h` and `--version`/`-v` are processed
    !! automatically by function `dm_arg_read()`.
    use :: dm_ascii
    use :: dm_error
    use :: dm_file
    use :: dm_kind
    use :: dm_util
    implicit none (type, external)
    private

    ! Argument value types.
    integer, parameter, public :: ARG_TYPE_NONE    =  0 !! None type (invalid).
    integer, parameter, public :: ARG_TYPE_LOGICAL =  1 !! Logical argument.
    integer, parameter, public :: ARG_TYPE_INTEGER =  2 !! Integer value.
    integer, parameter, public :: ARG_TYPE_REAL    =  3 !! Real value.
    integer, parameter, public :: ARG_TYPE_CHAR    =  4 !! Single character value.
    integer, parameter, public :: ARG_TYPE_STRING  =  5 !! Character string value.
    integer, parameter, public :: ARG_TYPE_ID      =  6 !! Valid ID value.
    integer, parameter, public :: ARG_TYPE_UUID    =  7 !! Valid UUID4 value.
    integer, parameter, public :: ARG_TYPE_TIME    =  8 !! Valid ISO 8601 value.
    integer, parameter, public :: ARG_TYPE_LEVEL   =  9 !! Log level.
    integer, parameter, public :: ARG_TYPE_FILE    = 10 !! Path to file on file system (must exist).
    integer, parameter, public :: ARG_TYPE_DB      = 11 !! Path to database on file system (must exist).
    integer, parameter, public :: ARG_TYPE_LAST    = 11 !! Never use this.

    integer, parameter, public :: ARG_NAME_LEN  = 32            !! Maximum length of argument name.
    integer, parameter, public :: ARG_VALUE_LEN = FILE_PATH_LEN !! Maximum length of argument value.

    type, public :: arg_type
        !! Argument description type.
        character(len=ARG_NAME_LEN)  :: name     = ' '              !! Identifier of the argument (without leading --).
        character                    :: short    = ASCII_NUL        !! Short argument character.
        character(len=ARG_VALUE_LEN) :: value    = ' '              !! Default and passed value (if any).
        integer                      :: length   = 0                !! Value length.
        integer                      :: max_len  = ARG_VALUE_LEN    !! Maximum argument value length.
        integer                      :: min_len  = 0                !! Minimum argument value length.
        integer                      :: type     = ARG_TYPE_LOGICAL !! Value data type.
        logical                      :: required = .false.          !! Option is mandatory.
        logical                      :: passed   = .false.          !! Option was passed.
        integer                      :: error    = E_NONE           !! Occured error.
    end type arg_type

    interface dm_arg_get
        !! Returns argument value.
        module procedure :: arg_get_int32
        module procedure :: arg_get_logical
        module procedure :: arg_get_real64
        module procedure :: arg_get_string
    end interface

    public :: dm_arg_get
    public :: dm_arg_has
    public :: dm_arg_help
    public :: dm_arg_parse
    public :: dm_arg_read
    public :: dm_arg_validate

    private :: arg_get_int32
    private :: arg_get_logical
    private :: arg_get_real64
    private :: arg_get_string
contains
    ! ******************************************************************
    ! PUBLIC PROCEDURES.
    ! ******************************************************************
    logical function dm_arg_has(name, short) result(has)
        !! Returns `.true.` if argument of given name is passed without value.
        character(len=*), intent(in)           :: name  !! Name of command-line argument.
        character,        intent(in), optional :: short !! Short name.

        integer        :: rc
        type(arg_type) :: args(1)

        has = .false.
        args(1) = arg_type(name=name, type=ARG_TYPE_LOGICAL)
        if (present(short)) args(1)%short = short
        rc = dm_arg_parse(args, ignore_unknown=.true., verbose=.false.)
        has = (args(1)%error == E_NONE)
    end function dm_arg_has

    integer function dm_arg_parse(args, ignore_unknown, verbose) result(rc)
        !! Parses command-line for given arguments. Error messages are printed
        !! to standard error by default, unless `verbose` is `.false.`.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_ARG_INVALID` if an argument has been passed already.
        !! * `E_ARG_NO_VALUE` if an argument has been passed without value.
        !! * `E_ARG_LENGTH` if one of the argument values has wrong length.
        !! * `E_ARG_UNKNOWN` if one of the arguments parsed is not known.
        !!
        type(arg_type), intent(inout)        :: args(:)        !! Arguments array.
        logical,        intent(in), optional :: ignore_unknown !! Allow unknown arguments.
        logical,        intent(in), optional :: verbose        !! Print error messages to stderr.

        character(len=ARG_VALUE_LEN) :: a, value
        integer                      :: i, j, k, n, stat
        logical                      :: ignore_unknown_, verbose_
        logical                      :: exists

        rc = E_NONE

        ! Allow unknown command-line arguments?
        ignore_unknown_ = .false.
        if (present(ignore_unknown)) ignore_unknown_ = ignore_unknown

        ! Show error messages?
        verbose_ = .true.
        if (present(verbose)) verbose_ = verbose

        ! Reset arguments.
        do i = 1, size(args)
            args(i)%passed = .false.
            args(i)%error  = E_ARG_NOT_FOUND
        end do

        ! Cycle through passed command-line arguments.
        i = 1
        n = command_argument_count()
        if (n == 0) return

        do
            if (i > n) return
            call get_command_argument(i, a)

            if (a(1:1) /= '-') then
                ! Argument does not start with `-` and is therefore invalid.
                rc = E_ARG_UNKNOWN
                if (verbose_) call dm_error_out(rc, 'unknown option "' // trim(a) // '"')
                return
            end if

            do j = 1, size(args)
                exists = .false.

                ! Match argument.
                if ((args(j)%short == ASCII_NUL .or. a(2:3) /= args(j)%short // ' ') .and. &
                    (a(2:2) /= '-' .or. a(3:) /= args(j)%name)) cycle

                ! Argument has been passed already.
                if (args(j)%passed) then
                    rc = E_ARG_INVALID
                    if (verbose_) call dm_error_out(rc, 'option ' // trim(a) // ' already set')
                    return
                end if

                ! Argument matches.
                exists = .true.

                ! No value to expect.
                if (args(j)%type == ARG_TYPE_LOGICAL) then
                    args(j)%error  = E_NONE
                    args(j)%passed = .true.
                    exit
                end if

                ! No value passed if last argument.
                args(j)%error = E_ARG_NO_VALUE
                if (i == n) exit

                ! Value found (may be empty).
                i = i + 1

                ! Get value of option.
                call get_command_argument(i, value, length=k, status=stat)
                if (stat /= 0) exit

                ! Minimum and maximum length of value.
                args(j)%length = k
                args(j)%error  = E_ARG_LENGTH
                if (k > args(j)%max_len .or. k < args(j)%min_len) exit

                ! Read value. An empty value is still valid.
                args(j)%value  = dm_ascii_escape(adjustl(value))
                args(j)%passed = .true.
                args(j)%error  = E_NONE

                exit
            end do

            if (.not. exists .and. .not. ignore_unknown_) then
                ! Argument starts with `-` but is unknown or unexpected.
                rc = E_ARG_UNKNOWN
                if (verbose_) call dm_error_out(rc, 'argument ' // trim(a) // ' not allowed')
                return
            end if

            i = i + 1
        end do

        rc = E_NONE
    end function dm_arg_parse

    integer function dm_arg_read(args, app, major, minor, patch) result(rc)
        !! Reads all arguments from command-line and prints error message if one
        !! is missing. Returns the error code of the first invalid argument.
        !!
        !! The function also parses the command-line arguments for
        !! `-v`/`--version` to display the current application and library
        !! version, and `-h`/`--help` to output all available command-line
        !! arguments. If one of these arguments is passed, `dm_stop(0)` is
        !! called afterwards.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_EMPTY` if array of arguments is empty.
        !! * `E_ARG_INVALID` if an required argument has not been passed.
        !! * `E_ARG_NO_VALUE` if an argument has been passed without value.
        !! * `E_ARG_TYPE` if an argument has the wrong type.
        !! * `E_ARG_LENGTH` if the length of the argument is wrong.
        !! * `E_ARG_UNKNOWN` if an unknown argument has been passed.
        !!
        use :: dm_version

        type(arg_type),   intent(inout)        :: args(:) !! Arguments to match.
        character(len=*), intent(in), optional :: app     !! App name (for `-v`).
        integer,          intent(in), optional :: major   !! Major version number (for `-v`).
        integer,          intent(in), optional :: minor   !! Minor version number (for `-v`).
        integer,          intent(in), optional :: patch   !! Patch level (for `-v`).

        integer :: i, n
        integer :: major_, minor_, patch_

        rc = E_NONE

        major_ = 0
        minor_ = 0
        patch_ = 0

        if (present(major)) major_ = major
        if (present(minor)) minor_ = minor
        if (present(patch)) patch_ = patch

        ! Print program and library version, then stop.
        if (dm_arg_has('version', 'v')) then
            if (present(app)) then
                call dm_version_out(app, major_, minor_, patch_)
            else
                write (stdout, '("DMPACK ", a)') DM_VERSION_STRING
            end if

            call dm_stop(STOP_SUCCESS)
        end if

        ! Print help, then stop.
        if (dm_arg_has('help', 'h')) then
            call dm_arg_help(args)
            call dm_stop(STOP_SUCCESS)
        end if

        ! Parse command-line argument and stop on error.
        rc = dm_arg_parse(args, verbose=.true.)

        if (dm_is_error(rc)) then
            print *
            call dm_arg_help(args)
            call dm_stop(STOP_FAILURE)
        end if

        ! Validate passed arguments.
        rc = E_EMPTY

        validate_loop: do i = 1, size(args)
            rc = dm_arg_validate(args(i))

            select case (rc)
                case (E_NONE)
                    cycle validate_loop

                case (E_ARG_NOT_FOUND)
                    ! If the argument is required but not found, `E_ARG_INVALID` is set.
                    ! We can ignore and overwrite this error.
                    rc = E_NONE
                    cycle validate_loop

                case (E_ARG_INVALID)
                    call dm_error_out(rc, 'argument --' // trim(args(i)%name) // ' is required')
                    exit validate_loop

                case (E_ARG_NO_VALUE)
                    call dm_error_out(rc, 'argument --' // trim(args(i)%name) // ' requires value')
                    exit validate_loop

                case (E_ARG_TYPE)
                    select case (args(i)%type)
                        case (ARG_TYPE_INTEGER)
                            call dm_error_out(rc, 'argument --' // trim(args(i)%name) // ' is not an integer')
                        case (ARG_TYPE_REAL)
                            call dm_error_out(rc, 'argument --' // trim(args(i)%name) // ' is not a number')
                        case (ARG_TYPE_CHAR)
                            call dm_error_out(rc, 'argument --' // trim(args(i)%name) // ' is not a single character')
                        case (ARG_TYPE_ID)
                            call dm_error_out(rc, 'argument --' // trim(args(i)%name) // ' is not a valid id')
                        case (ARG_TYPE_UUID)
                            call dm_error_out(rc, 'argument --' // trim(args(i)%name) // ' is not a valid UUID4')
                        case (ARG_TYPE_TIME)
                            call dm_error_out(rc, 'argument --' // trim(args(i)%name) // ' is not in ISO 8601 format')
                        case (ARG_TYPE_LEVEL)
                            call dm_error_out(rc, 'argument --' // trim(args(i)%name) // ' is not a valid log level')
                        case (ARG_TYPE_FILE)
                            call dm_error_out(rc, 'file ' // trim(args(i)%value) // ' not found')
                        case (ARG_TYPE_DB)
                            call dm_error_out(rc, 'database ' // trim(args(i)%value) // ' not found')
                    end select
                    exit validate_loop

                case (E_ARG_LENGTH)
                    n = len_trim(args(i)%value)
                    if (n > args(i)%max_len) then
                        call dm_error_out(rc, 'argument --' // trim(args(i)%name) // ' is too long, must be <= ' // &
                                          dm_itoa(args(i)%max_len))
                    else if (n < args(i)%min_len) then
                        call dm_error_out(rc, 'argument --' // trim(args(i)%name) // ' is too short, must be >= ' // &
                                          dm_itoa(args(i)%min_len))
                    end if
                    exit validate_loop
            end select
        end do validate_loop
    end function dm_arg_read

    integer function dm_arg_validate(arg) result(rc)
        !! Validates given argument. Arguments of type `ARG_TYPE_LEVEL` are
        !! additionally converted to integer if the passed argument value is a
        !! valid log level name. For example, the argument value `warning` is
        !! converted to integer `3`, to match log level `LVL_WARNING`.
        use :: dm_id
        use :: dm_log
        use :: dm_string
        use :: dm_time
        use :: dm_uuid

        type(arg_type), intent(inout) :: arg !! Argument to validate.

        integer       :: error
        integer       :: i, level
        real(kind=r8) :: f

        rc = E_ARG
        if (len_trim(arg%name) == 0) return

        ! Required argument has not been passed.
        rc = E_ARG_INVALID
        if (arg%required .and. .not. arg%passed) return

        ! Exit early if argument has not been passed.
        rc = arg%error
        if (dm_is_error(rc)) return

        ! Validate the type.
        rc = E_ARG_TYPE
        if (arg%type <= ARG_TYPE_NONE .or. arg%type > ARG_TYPE_LAST) return

        select case (arg%type)
            case (ARG_TYPE_INTEGER)
                if (arg%length == 0) return
                call dm_string_to(arg%value, i, error)
                if (dm_is_error(error)) return

            case (ARG_TYPE_REAL)
                if (arg%length == 0) return
                call dm_string_to(arg%value, f, error)
                if (dm_is_error(error)) return

            case (ARG_TYPE_CHAR)
                if (arg%length > 1) return

            case (ARG_TYPE_ID)
                if (.not. dm_id_valid(arg%value)) return

            case (ARG_TYPE_UUID)
                if (.not. dm_uuid4_valid(arg%value)) return

            case (ARG_TYPE_TIME)
                if (.not. dm_time_valid(arg%value)) return

            case (ARG_TYPE_LEVEL)
                if (arg%length == 0) return

                ! Convert string to integer.
                call dm_string_to(arg%value, level, error)

                ! On error, try to read level from level name, and convert the
                ! result back to string. An invalid log level name is turned
                ! into `LVL_NONE`.
                if (dm_is_error(error)) then
                    level     = dm_log_level_from_name(arg%value)
                    arg%value = dm_itoa(level)
                end if

                if (.not. dm_log_valid(level)) return

            case (ARG_TYPE_FILE, ARG_TYPE_DB)
                if (arg%length == 0) return
                if (arg%required .and. .not. dm_file_exists(arg%value)) return
        end select

        rc = E_NONE
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
                case (ARG_TYPE_INTEGER)
                    write (stdout, '("<integer>")')
                case (ARG_TYPE_REAL)
                    write (stdout, '("<float>")')
                case (ARG_TYPE_CHAR)
                    write (stdout, '("<char>")')
                case (ARG_TYPE_STRING)
                    write (stdout, '("<string>")')
                case (ARG_TYPE_ID)
                    write (stdout, '("<id>")')
                case (ARG_TYPE_UUID)
                    write (stdout, '("<uuid>")')
                case (ARG_TYPE_TIME)
                    write (stdout, '("<timestamp>")')
                case (ARG_TYPE_LEVEL)
                    write (stdout, '("<level>")')
                case (ARG_TYPE_FILE)
                    write (stdout, '("<file>")')
                case (ARG_TYPE_DB)
                    write (stdout, '("<database>")')
                case default
                    write (stdout, *)
            end select
        end do

        write (stdout, '(/, 4x, "-h, --help")')
        write (stdout, '(4x, "-v, --version", /)')
    end subroutine dm_arg_help

    ! ******************************************************************
    ! PRIVATE PROCEDURES.
    ! ******************************************************************
    integer function arg_get_int32(arg, value, default, passed) result(rc)
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
    end function arg_get_int32

    integer function arg_get_logical(arg, value, default, passed) result(rc)
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
    end function arg_get_logical

    integer function arg_get_real64(arg, value, default, passed) result(rc)
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
    end function arg_get_real64

    integer function arg_get_string(arg, value, default, passed) result(rc)
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
    end function arg_get_string
end module dm_arg
