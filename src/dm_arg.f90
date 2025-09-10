! Author:  Philipp Engel
! Licence: ISC
module dm_arg
    !! Class for command-line argument parsing.
    !!
    !! Create an arg object, then read and parse the arguments:
    !!
    !! ```fortran
    !! character(72)   :: input
    !! integer         :: delay, rc
    !! logical         :: verbose
    !! type(arg_class) :: arg
    !!
    !! call arg%create()
    !! call arg%add('input',   short='i', type=ARG_TYPE_STRING, required=.true.)
    !! call arg%add('delay',   short='x', type=ARG_TYPE_INTEGER)
    !! call arg%add('verbose', short='V', type=ARG_TYPE_LOGICAL)
    !!
    !! rc = arg%read()
    !! call dm_error_out(rc)
    !!
    !! call arg%get('input',  input)
    !! call arg%get('delay',  delay)
    !! call arg%get('verbose, verbose)
    !! call arg%destroy()
    !! ```
    !!
    !! Each argument requires name and type. The default type is
    !! `ARG_TYPE_LOGICAL`. The command-line arguments `--help`/-`h` and
    !! `--version`/`-v` are processed automatically by method `read()`.
    !!
    !! Additionally, you can pass a callback routine of abstract interface
    !! `dm_arg_version_callback()` to `read()` that outputs the version string.
    use :: dm_ascii
    use :: dm_error
    use :: dm_file
    use :: dm_kind
    use :: dm_util
    implicit none (type, external)
    private

    ! Argument value types.
    integer, parameter, public :: ARG_TYPE_NONE     =  0 !! None type (invalid).
    integer, parameter, public :: ARG_TYPE_LOGICAL  =  1 !! Logical argument.
    integer, parameter, public :: ARG_TYPE_INTEGER  =  2 !! Integer value.
    integer, parameter, public :: ARG_TYPE_REAL     =  3 !! Real value.
    integer, parameter, public :: ARG_TYPE_CHAR     =  4 !! Single character value.
    integer, parameter, public :: ARG_TYPE_STRING   =  5 !! Character string value.
    integer, parameter, public :: ARG_TYPE_ID       =  6 !! Valid ID value.
    integer, parameter, public :: ARG_TYPE_UUID     =  7 !! Valid UUIDv4 value.
    integer, parameter, public :: ARG_TYPE_TIME     =  8 !! Valid ISO 8601 value.
    integer, parameter, public :: ARG_TYPE_LEVEL    =  9 !! Log level (name string or integer value).
    integer, parameter, public :: ARG_TYPE_FILE     = 10 !! Path to file on file system (must exist).
    integer, parameter, public :: ARG_TYPE_DATABASE = 11 !! Path to database on file system (must exist).
    integer, parameter, public :: ARG_TYPE_LAST     = 11 !! Never use this.

    integer, parameter, public :: ARG_NAME_LEN  = 32            !! Maximum length of argument name.
    integer, parameter, public :: ARG_VALUE_LEN = FILE_PATH_LEN !! Maximum length of argument value.

    integer, parameter :: ARG_NARGS_DEFAULT = 64

    abstract interface
        subroutine dm_arg_version_callback()
            !! Callback routine that outputs the version information.
        end subroutine dm_arg_version_callback
    end interface

    type :: arg_type
        !! Private argument description type.
        character(ARG_NAME_LEN)  :: name     = ' '              !! Identifier of the argument (without leading --).
        character                :: short    = ASCII_NUL        !! Short argument character.
        character(ARG_VALUE_LEN) :: value    = ' '              !! Default and passed value (if any).
        integer                  :: length   = 0                !! Value length.
        integer                  :: min_len  = 0                !! Minimum argument value length.
        integer                  :: max_len  = ARG_VALUE_LEN    !! Maximum argument value length.
        integer                  :: type     = ARG_TYPE_LOGICAL !! Value data type.
        logical                  :: required = .false.          !! Option is mandatory.
        logical                  :: passed   = .false.          !! Option was passed.
        integer                  :: error    = E_NONE           !! Occured error.
    end type arg_type

    type, public :: arg_class
        !! Public class to store and parse command-line arguments.
        private
        integer                     :: index = 0 !! Array index.
        type(arg_type), allocatable :: args(:)   !! Arguments array.
    contains
        private
        ! Private methods.
        procedure :: find            => arg_find
        procedure :: get_arg         => arg_get_arg
        procedure :: get_int32       => arg_get_int32
        procedure :: get_logical     => arg_get_logical
        procedure :: get_real64      => arg_get_real64
        procedure :: get_string      => arg_get_string
        ! Public methods.
        procedure, public :: add     => arg_add
        procedure, public :: create  => arg_create
        procedure, public :: destroy => arg_destroy
        generic,   public :: get     => get_int32, get_logical, get_real64, get_string
        procedure, public :: passed  => arg_passed
        procedure, public :: read    => arg_read
    end type arg_class

    ! Public interfaces.
    public :: dm_arg_version_callback

    ! Private methods.
    private :: arg_add
    private :: arg_create
    private :: arg_destroy
    private :: arg_find
    private :: arg_get_int32
    private :: arg_get_logical
    private :: arg_get_real64
    private :: arg_get_string
    private :: arg_passed
    private :: arg_read

    ! Private procedures.
    private :: arg_has
    private :: arg_help
    private :: arg_parse
    private :: arg_type_is_valid
    private :: arg_validate
contains
    ! **************************************************************************
    ! PRIVATE CLASS FUNCTIONS.
    ! **************************************************************************
    integer function arg_find(this, name) result(index)
        !! Returns index of argument in arguments array, or 0 on error.
        class(arg_class), intent(inout) :: this !! Arg object.
        character(*),     intent(in)    :: name !! Argument name.

        integer :: i

        index = 0

        if (.not. allocated(this%args)) return
        if (len_trim(name) == 0)        return

        do i = 1, this%index
            if (this%args(i)%name /= name) cycle
            index = i
            exit
        end do
    end function arg_find

    logical function arg_passed(this, name) result(passed)
        !! Returns `.true.` if argument in arguments array has been passed.
        class(arg_class), intent(inout) :: this !! Arg object.
        character(*),     intent(in)    :: name !! Argument name.

        integer :: i

        passed = .false.
        i = this%find(name)
        if (i == 0) return
        passed = this%args(i)%passed
    end function arg_passed

    integer function arg_read(this, callback) result(rc)
        !! Reads all arguments from command-line and prints error message if one
        !! is missing. Returns the error code of the first invalid argument.
        !!
        !! The function also parses the command-line arguments for
        !! `-v`/`--version` to display the current application and library
        !! version, and `-h`/`--help` to output all available command-line
        !! arguments. If one of these arguments is passed, `dm_stop(0)` is
        !! called afterwards.
        !!
        !! Optional argument `callback` is a routine that outputs the
        !! version information.
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

        class(arg_class), intent(inout)              :: this     !! Arg object.
        procedure(dm_arg_version_callback), optional :: callback !! Version callback.

        integer :: i, n

        rc = E_NONE

        ! Call version callback, then stop.
        if (arg_has('version', 'v')) then
            if (present(callback)) then
                call callback()
            else
                write (stdout, '("DMPACK ", a)') DM_VERSION_STRING
            end if
            call dm_stop(STOP_SUCCESS)
        end if

        ! Print help, then stop.
        if (arg_has('help', 'h')) then
            call arg_help(this%args, this%index)
            call dm_stop(STOP_SUCCESS)
        end if

        ! Parse command-line argument and stop on error.
        rc = arg_parse(this%args, this%index, verbose=.true.)

        if (dm_is_error(rc)) then
            print *
            call arg_help(this%args, this%index)
            call dm_stop(STOP_FAILURE)
        end if

        ! Validate passed arguments.
        rc = E_EMPTY

        validate_loop: &
        do i = 1, this%index
            associate (arg => this%args(i))
                if (.not. arg_type_is_valid(arg%type)) then
                    call dm_error_out(E_TYPE, 'argument --' // trim(arg%name) // ' has no valid type')
                    cycle
                end if

                rc = arg_validate(arg)

                select case (rc)
                    case (E_NONE)
                        cycle validate_loop

                    case (E_ARG_NOT_FOUND)
                        ! If the argument is required but not found, `E_ARG_INVALID` is set.
                        ! We can ignore and overwrite this error.
                        rc = E_NONE
                        cycle validate_loop

                    case (E_ARG_INVALID)
                        call dm_error_out(rc, 'argument --' // trim(arg%name) // ' is required')
                        exit validate_loop

                    case (E_ARG_NO_VALUE)
                        call dm_error_out(rc, 'argument --' // trim(arg%name) // ' requires value')
                        exit validate_loop

                    case (E_ARG_TYPE)
                        select case (arg%type)
                            case (ARG_TYPE_INTEGER);  call dm_error_out(rc, 'argument --' // trim(arg%name)  // ' is not an integer')
                            case (ARG_TYPE_REAL);     call dm_error_out(rc, 'argument --' // trim(arg%name)  // ' is not a number')
                            case (ARG_TYPE_CHAR);     call dm_error_out(rc, 'argument --' // trim(arg%name)  // ' is not a single character')
                            case (ARG_TYPE_ID);       call dm_error_out(rc, 'argument --' // trim(arg%name)  // ' is not a valid id')
                            case (ARG_TYPE_UUID);     call dm_error_out(rc, 'argument --' // trim(arg%name)  // ' is not a valid UUID')
                            case (ARG_TYPE_TIME);     call dm_error_out(rc, 'argument --' // trim(arg%name)  // ' is not in ISO 8601 format')
                            case (ARG_TYPE_LEVEL);    call dm_error_out(rc, 'argument --' // trim(arg%name)  // ' is not a valid log level')
                            case (ARG_TYPE_FILE);     call dm_error_out(rc, 'file '       // trim(arg%value) // ' not found')
                            case (ARG_TYPE_DATABASE); call dm_error_out(rc, 'database '   // trim(arg%value) // ' not found')
                        end select

                        exit validate_loop

                    case (E_ARG_LENGTH)
                        n = len_trim(arg%value)

                        if (arg%max_len == arg%min_len .and. n /= arg%max_len) then
                            call dm_error_out(rc, 'argument --' // trim(arg%name) // ' must be ' // dm_itoa(arg%max_len) // ' characters long')
                        else if (n > arg%max_len) then
                            call dm_error_out(rc, 'argument --' // trim(arg%name) // ' must be <= ' // dm_itoa(arg%max_len))
                        else if (n < arg%min_len) then
                            call dm_error_out(rc, 'argument --' // trim(arg%name) // ' must be >= ' // dm_itoa(arg%min_len))
                        end if

                        exit validate_loop
                end select
            end associate
        end do validate_loop
    end function arg_read

    ! **************************************************************************
    ! PRIVATE CLASS SUBROUTINES.
    ! **************************************************************************
    subroutine arg_add(this, name, short, type, max_len, min_len, required, error)
        class(arg_class), intent(inout)         :: this
        character(*),     intent(in),  optional :: name
        character,        intent(in),  optional :: short
        integer,          intent(in),  optional :: type
        integer,          intent(in),  optional :: max_len
        integer,          intent(in),  optional :: min_len
        logical,          intent(in),  optional :: required
        integer,          intent(out), optional :: error

        integer :: rc

        arg_block: block
            rc = E_CORRUPT
            if (.not. allocated(this%args)) exit arg_block

            rc = E_LIMIT
            if (this%index == size(this%args)) exit arg_block

            rc = E_NONE
            this%index = this%index + 1

            associate (arg => this%args(this%index))
                if (present(name))     arg%name     = name
                if (present(short))    arg%short    = short
                if (present(min_len))  arg%min_len  = min_len
                if (present(max_len))  arg%max_len  = max_len
                if (present(type))     arg%type     = type
                if (present(required)) arg%required = required
            end associate
        end block arg_block

        if (present(error)) error = rc
    end subroutine arg_add

    subroutine arg_create(this, max_size, default)
        !! Initialises arg object and adds default arguments `--help` and
        !! `--version` if `default` is not `.false.`.
        class(arg_class), intent(inout)        :: this     !! Arg object.
        integer,          intent(in), optional :: max_size !! Max. number of arguments to expect.
        logical,          intent(in), optional :: default  !! Add default arguments.

        integer :: n, stat

        n = dm_present(max_size, ARG_NARGS_DEFAULT)
        n = max(n, ARG_NARGS_DEFAULT)
        allocate (this%args(n), stat=stat)

        if (.not. dm_present(default, .true.) .or. n < 2) return
        call this%add('help',    'h', ARG_TYPE_LOGICAL)
        call this%add('version', 'v', ARG_TYPE_LOGICAL)
    end subroutine arg_create

    subroutine arg_destroy(this)
        !! Destroys arg object.
        class(arg_class), intent(inout) :: this !! Arg object.

        this%index = 0
        if (allocated(this%args)) deallocate (this%args)
    end subroutine arg_destroy

    subroutine arg_get_arg(this, name, arg, passed, error)
        !! Returns argument derived type of given name in argument `arg`. If
        !! the argument could not be found, `arg%error` is set to `E_NOT_FOUND.
        class(arg_class), intent(inout)         :: this   !! Arg object.
        character(*),     intent(in)            :: name   !! Argument name.
        type(arg_type),   intent(out)           :: arg    !! Argument type.
        logical,          intent(out), optional :: passed !! Passed or not.
        integer,          intent(out), optional :: error  !! Argument error.

        integer :: i

        arg%error = E_NOT_FOUND

        if (present(passed)) passed = .false.
        if (present(error))  error  = arg%error

        i = this%find(name)
        if (i == 0) return

        arg = this%args(i)

        if (present(passed)) passed = arg%passed
        if (present(error))  error  = arg%error
    end subroutine arg_get_arg

    subroutine arg_get_int32(this, name, value, default, passed, error)
        !! Returns argument value as 4-byte integer.
        class(arg_class), intent(inout)         :: this    !! Arg object.
        character(*),     intent(in)            :: name    !! Argument name.
        integer,          intent(inout)         :: value   !! Argument value.
        integer,          intent(in),  optional :: default !! Default value.
        logical,          intent(out), optional :: passed  !! Passed or not.
        integer,          intent(out), optional :: error   !! Argument error.

        type(arg_type) :: arg

        call this%get_arg(name, arg, passed, error)

        if (dm_is_error(arg%error)) then
            if (present(default)) value = default
            return
        end if

        value = dm_atoi(arg%value)
    end subroutine arg_get_int32

    subroutine arg_get_logical(this, name, value, default, passed, error)
        !! Returns `.true.` if argument has been passed.
        class(arg_class), intent(inout)         :: this    !! Arg object.
        character(*),     intent(in)            :: name    !! Argument name.
        logical,          intent(inout)         :: value   !! Argument value.
        logical,          intent(in),  optional :: default !! Default value.
        logical,          intent(out), optional :: passed  !! Passed or not.
        integer,          intent(out), optional :: error   !! Argument error.

        type(arg_type) :: arg

        call this%get_arg(name, arg, passed, error)

        if (dm_is_error(arg%error)) then
            if (present(default)) value = default
            return
        end if

        value = .true.
    end subroutine arg_get_logical

    subroutine arg_get_real64(this, name, value, default, passed, error)
        !! Returns argument value as 8-byte real.
        class(arg_class), intent(inout)         :: this    !! Arg object.
        character(*),     intent(in)            :: name    !! Argument name.
        real(r8),         intent(inout)         :: value   !! Argument value.
        real(r8),         intent(in),  optional :: default !! Default value.
        logical,          intent(out), optional :: passed  !! Passed or not.
        integer,          intent(out), optional :: error   !! Argument error.

        type(arg_type) :: arg

        call this%get_arg(name, arg, passed, error)

        if (dm_is_error(arg%error)) then
            if (present(default)) value = default
            return
        end if

        value = dm_atof(arg%value)
    end subroutine arg_get_real64

    subroutine arg_get_string(this, name, value, default, passed, error)
        !! Returns argument value as character string.
        class(arg_class), intent(inout)         :: this    !! Arg object.
        character(*),     intent(in)            :: name    !! Argument name.
        character(*),     intent(inout)         :: value   !! Argument value.
        character(*),     intent(in),  optional :: default !! Default value.
        logical,          intent(out), optional :: passed  !! Passed or not.
        integer,          intent(out), optional :: error   !! Argument error.

        type(arg_type) :: arg

        call this%get_arg(name, arg, passed, error)

        if (dm_is_error(arg%error)) then
            if (present(default)) value = default
            return
        end if

        value = arg%value
    end subroutine arg_get_string

    ! **************************************************************************
    ! PRIVATE PROCEDURES.
    ! **************************************************************************
    logical function arg_has(name, short) result(has)
        !! Returns `.true.` if argument of given name is passed without value.
        character(*), intent(in)           :: name  !! Name of command-line argument.
        character,    intent(in), optional :: short !! Short name.

        integer        :: rc
        type(arg_type) :: args(1)

        has = .false.
        args(1) = arg_type(name=name, type=ARG_TYPE_LOGICAL)
        if (present(short)) args(1)%short = short
        rc = arg_parse(args, size(args), ignore_unknown=.true., verbose=.true.)
        has = (args(1)%error == E_NONE)
    end function arg_has

    pure elemental logical function arg_type_is_valid(type) result(valid)
        !! Returns `.true.` if passed type is a valid argument type.
        integer, intent(in) :: type !! Argument type (`ARG_TYPE_*`).

        valid = (type >= ARG_TYPE_NONE .and. type <= ARG_TYPE_LAST)
    end function arg_type_is_valid

    integer function arg_parse(args, nargs, ignore_unknown, verbose) result(rc)
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
        integer,        intent(in), optional :: nargs          !! Number of arguments.
        logical,        intent(in), optional :: ignore_unknown !! Allow unknown arguments.
        logical,        intent(in), optional :: verbose        !! Print error messages to stderr.

        character(ARG_VALUE_LEN) :: a, value
        integer                  :: i, j, k, n, nargs_, stat
        logical                  :: exists, ignore_unknown_, verbose_

        rc = E_NONE

        nargs_          = dm_present(nargs,          size(args)) ! Number of arguments in array.
        ignore_unknown_ = dm_present(ignore_unknown, .false.)    ! Allow unknown command-line arguments?
        verbose_        = dm_present(verbose,        .true.)     ! Show error messages?

        ! Reset arguments.
        args(:)%passed = .false.
        args(:)%error  = E_ARG_NOT_FOUND

        ! Cycle through passed command-line arguments.
        i = 1
        n = command_argument_count()
        if (n == 0) return

        do
            if (i > n) return
            call get_command_argument(i, a)

            if (a(1:1) /= '-' .and. exists) then
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
                    if (verbose_) call dm_error_out(rc, 'option ' // trim(a) // ' is already set')
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
                if (verbose_) call dm_error_out(rc, 'argument ' // trim(a) // ' is not supported')
                return
            end if

            i = i + 1
        end do

        rc = E_NONE
    end function arg_parse

    integer function arg_validate(arg) result(rc)
        !! Validates given argument. Arguments of type `ARG_TYPE_LEVEL` are
        !! additionally converted to integer if the passed argument value is a
        !! valid log level name. For example, the argument value `warning` is
        !! converted to integer `3`, to match log level `LL_WARNING`.
        use :: dm_id
        use :: dm_log
        use :: dm_string
        use :: dm_time
        use :: dm_uuid

        type(arg_type), intent(inout) :: arg !! Argument to validate.

        integer  :: error
        integer  :: i, level
        real(r8) :: r

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
        if (.not. arg_type_is_valid(arg%type)) return

        select case (arg%type)
            case (ARG_TYPE_INTEGER)
                ! 4-byte integer.
                if (arg%length == 0) return
                call dm_string_to(arg%value, i, error)
                if (dm_is_error(error)) return

            case (ARG_TYPE_REAL)
                ! 8-byte real.
                if (arg%length == 0) return
                call dm_string_to(arg%value, r, error)
                if (dm_is_error(error)) return

            case (ARG_TYPE_CHAR)
                ! Character string.
                if (arg%length > 1) return

            case (ARG_TYPE_ID)
                ! DMPACK identifier.
                if (.not. dm_id_is_valid(arg%value)) return

            case (ARG_TYPE_UUID)
                ! UUIDv4.
                if (.not. dm_uuid4_is_valid(arg%value)) return

            case (ARG_TYPE_TIME)
                ! ISO 8601.
                if (.not. dm_time_is_valid(arg%value)) return

            case (ARG_TYPE_LEVEL)
                ! Log level.
                if (arg%length == 0) return
                level = dm_log_level_from_string(arg%value)
                if (.not. dm_log_level_is_valid(level)) return
                ! Set argument value to numeric log level.
                arg%value = dm_itoa(level)

            case (ARG_TYPE_FILE, ARG_TYPE_DATABASE)
                ! File or database.
                if (arg%length == 0) return
                if (arg%required .and. .not. dm_file_exists(arg%value)) return
        end select

        rc = E_NONE
    end function arg_validate

    subroutine arg_help(args, nargs)
        !! Prints command-line arguments to standard output if `--help` or `-h`
        !! is passed.
        type(arg_type), intent(inout)        :: args(:) !! Arguments array.
        integer,        intent(in), optional :: nargs   !! Number of arguments.

        integer :: i, n

        n = dm_present(nargs, size(args))
        write (stdout, '("Available command-line options:", /)')

        do i = 1, n
            write (stdout, '(4x, "-", a1, ", --", a, 1x)', advance='no') &
                args(i)%short, trim(args(i)%name)

            select case (args(i)%type)
                case (ARG_TYPE_INTEGER);  write (stdout, '("<integer>")')
                case (ARG_TYPE_REAL);     write (stdout, '("<real>")')
                case (ARG_TYPE_CHAR);     write (stdout, '("<char>")')
                case (ARG_TYPE_STRING);   write (stdout, '("<string>")')
                case (ARG_TYPE_ID);       write (stdout, '("<id>")')
                case (ARG_TYPE_UUID);     write (stdout, '("<uuid>")')
                case (ARG_TYPE_TIME);     write (stdout, '("<ISO 8601>")')
                case (ARG_TYPE_LEVEL);    write (stdout, '("<log level>")')
                case (ARG_TYPE_FILE);     write (stdout, '("<file path>")')
                case (ARG_TYPE_DATABASE); write (stdout, '("<database path>")')
                case default;             write (stdout, *)
            end select
        end do

        print *
    end subroutine arg_help
end module dm_arg
