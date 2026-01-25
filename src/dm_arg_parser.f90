! Author:  Philipp Engel
! Licence: ISC
module dm_arg_parser
    !! Object-oriented command-line argument parser. The parser is initialised
    !! with the first call to class method `add()` if not created with method
    !! `create()` first. The object is destroyed automatically.
    !!
    !! Read and parse the arguments:
    !!
    !! ``` fortran
    !! character(72)          :: input
    !! integer                :: delay, rc
    !! logical                :: verbose
    !! type(arg_parser_class) :: parser
    !!
    !! call parser%add('input',   short='i', type=ARG_TYPE_STRING, required=.true.)
    !! call parser%add('delay',   short='x', type=ARG_TYPE_INTEGER)
    !! call parser%add('verbose', short='V', type=ARG_TYPE_LOGICAL)
    !!
    !! rc = parser%read()
    !! call dm_error_out(rc)
    !!
    !! call parser%get('input',  input)
    !! call parser%get('delay',  delay)
    !! call parser%get('verbose, verbose)
    !! ```
    !!
    !! Each argument requires name and type. The default type is
    !! `ARG_TYPE_LOGICAL`. The command-line arguments `--help`/-`h` and
    !! `--version`/`-v` are processed automatically by method `read()`.
    !!
    !! Additionally, you can pass a callback routine of abstract interface
    !! `dm_arg_parser_version_callback()` to `read()` that outputs the version
    !! string.
    use :: dm_arg
    use :: dm_error
    use :: dm_kind
    use :: dm_util
    implicit none (type, external)
    private

    integer, parameter :: ARG_PARSER_NARGS = 64

    abstract interface
        subroutine dm_arg_parser_version_callback()
            !! Callback routine that outputs the version information.
        end subroutine dm_arg_parser_version_callback
    end interface

    type, public :: arg_parser_class
        !! Public class to store and parse command-line arguments.
        private
        integer                     :: index = 0 !! Array index.
        type(arg_type), allocatable :: args(:)   !! Arguments array.
    contains
        private
        ! Private methods.
        procedure :: find            => arg_parser_find
        procedure :: get_arg         => arg_parser_get_arg
        procedure :: get_int32       => arg_parser_get_int32
        procedure :: get_logical     => arg_parser_get_logical
        procedure :: get_real64      => arg_parser_get_real64
        procedure :: get_string      => arg_parser_get_string
        procedure :: size            => arg_parser_size
        ! Public methods.
        procedure, public :: add     => arg_parser_add
        procedure, public :: create  => arg_parser_create
        generic,   public :: get     => get_int32,   &
                                        get_logical, &
                                        get_real64,  &
                                        get_string
        procedure, public :: passed  => arg_parser_passed
        procedure, public :: read    => arg_parser_read
        ! Destructor.
        final :: arg_parser_destroy
    end type arg_parser_class

    ! Public interfaces.
    public :: dm_arg_parser_version_callback

    ! Private methods.
    private :: arg_parser_add
    private :: arg_parser_create
    private :: arg_parser_destroy
    private :: arg_parser_find
    private :: arg_parser_get_int32
    private :: arg_parser_get_logical
    private :: arg_parser_get_real64
    private :: arg_parser_get_string
    private :: arg_parser_passed
    private :: arg_parser_read
contains
    ! **************************************************************************
    ! PRIVATE CLASS FUNCTIONS.
    ! **************************************************************************
    integer function arg_parser_find(this, name) result(index)
        !! Returns index of argument in arguments array, or 0 on error.
        class(arg_parser_class), intent(inout) :: this !! Arg parser.
        character(*),            intent(in)    :: name !! Argument name.

        integer :: i

        index = 0

        if (.not. allocated(this%args)) return
        if (len_trim(name) == 0)        return

        do i = 1, this%index
            if (this%args(i)%name /= name) cycle
            index = i
            exit
        end do
    end function arg_parser_find

    logical function arg_parser_passed(this, name) result(passed)
        !! Returns `.true.` if argument in arguments array has been passed.
        class(arg_parser_class), intent(inout) :: this !! Arg parser.
        character(*),            intent(in)    :: name !! Argument name.

        integer :: i

        passed = .false.
        i = this%find(name)
        if (i == 0) return
        passed = this%args(i)%passed
    end function arg_parser_passed

    integer function arg_parser_read(this, callback) result(rc)
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

        class(arg_parser_class), intent(inout)              :: this     !! Arg parser.
        procedure(dm_arg_parser_version_callback), optional :: callback !! Version callback.

        integer :: i, n

        rc = E_NONE

        ! Call version callback, then stop.
        if (dm_arg_has('version', 'v')) then
            if (present(callback)) then
                call callback()
            else
                write (STDOUT, '("DMPACK ", a)') DM_VERSION_STRING
            end if
            call dm_stop(STOP_SUCCESS)
        end if

        ! Print help, then stop.
        if (dm_arg_has('help', 'h')) then
            call dm_arg_help(this%args(:this%index))
            call dm_stop(STOP_SUCCESS)
        end if

        ! Parse command-line argument and stop on error.
        rc = dm_arg_parse(this%args, this%index, verbose=.true.)

        if (dm_is_error(rc)) then
            print *
            call dm_arg_help(this%args(:this%index))
            call dm_stop(STOP_FAILURE)
        end if

        ! Validate passed arguments.
        rc = E_EMPTY

        validate_loop: &
        do i = 1, this%index
            associate (arg => this%args(i))
                if (.not. dm_arg_type_is_valid(arg%type)) then
                    call dm_error_out(E_TYPE, 'argument --' // trim(arg%name) // ' has no valid type')
                    cycle
                end if

                rc = dm_arg_validate(arg)

                select case (rc)
                    case (E_NONE)
                        cycle validate_loop

                    case (E_ARG_NOT_FOUND)
                        ! If the argument is required but not found, `E_ARG_INVALID` is set.
                        ! We can therefore ignore and overwrite this error.
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
                            case (ARG_TYPE_INTEGER);  call dm_error_out(rc, 'argument --' // trim(arg%name) // ' is not an integer')
                            case (ARG_TYPE_REAL);     call dm_error_out(rc, 'argument --' // trim(arg%name) // ' is not a number')
                            case (ARG_TYPE_CHAR);     call dm_error_out(rc, 'argument --' // trim(arg%name) // ' is not a single character')
                            case (ARG_TYPE_ID);       call dm_error_out(rc, 'argument --' // trim(arg%name) // ' is not a valid id')
                            case (ARG_TYPE_UUID);     call dm_error_out(rc, 'argument --' // trim(arg%name) // ' is not a valid UUID')
                            case (ARG_TYPE_TIME);     call dm_error_out(rc, 'argument --' // trim(arg%name) // ' is not in ISO 8601 format')
                            case (ARG_TYPE_LEVEL);    call dm_error_out(rc, 'argument --' // trim(arg%name) // ' is not a valid log level')
                            case (ARG_TYPE_FILE);     call dm_error_out(rc, 'argument --' // trim(arg%name) // ' is not a valid file or directory')
                            case (ARG_TYPE_DATABASE); call dm_error_out(rc, 'argument --' // trim(arg%name) // ' is not a valid database')
                        end select

                        exit validate_loop

                    case (E_ARG_LENGTH)
                        n = len_trim(arg%value)

                        if (arg%max_len == arg%min_len .and. n /= arg%max_len) then
                            call dm_error_out(rc, 'argument --' // trim(arg%name) // ' must be ' // dm_itoa(arg%max_len) // ' characters long')
                        else if (n > arg%max_len) then
                            call dm_error_out(rc, 'argument --' // trim(arg%name) // ' must be shorter or equal ' // dm_itoa(arg%max_len) // ' characters')
                        else if (n < arg%min_len) then
                            call dm_error_out(rc, 'argument --' // trim(arg%name) // ' must be longer or equal ' // dm_itoa(arg%min_len) // ' characters')
                        end if

                        exit validate_loop
                end select
            end associate
        end do validate_loop
    end function arg_parser_read

    integer function arg_parser_size(this) result(n)
        !! Returns size of arguments array.
        class(arg_parser_class), intent(inout) :: this !! Arg parser.

        n = 0
        if (.not. allocated(this%args)) return
        n = size(this%args)
    end function arg_parser_size

    ! **************************************************************************
    ! PRIVATE CLASS SUBROUTINES.
    ! **************************************************************************
    recursive subroutine arg_parser_add(this, name, short, type, max_len, min_len, required, exist)
        !! Adds argument to object. This subroutine is recursive to be able to
        !! call `arg_create()` which calls `arg_add()` for initialisation.
        class(arg_parser_class), intent(inout)         :: this     !! Arg parser.
        character(*),            intent(in),  optional :: name     !! Argument name.
        character,               intent(in),  optional :: short    !! Argument short name.
        integer,                 intent(in),  optional :: type     !! Argument type.
        integer,                 intent(in),  optional :: max_len  !! Argument max. string length.
        integer,                 intent(in),  optional :: min_len  !! Argument min. string length.
        logical,                 intent(in),  optional :: required !! Argument is required.
        logical,                 intent(in),  optional :: exist    !! Argument must exist (file, database).

        integer                     :: n, stat
        type(arg_type), allocatable :: buffer(:)

        n = this%size()

        if (n == 0) then
            call this%create()
        else if (this%index > 0 .and. n == this%index) then
            allocate (buffer(n + 1), stat=stat)

            if (stat /= 0) then
                call dm_error_out(E_ALLOC)
                return
            end if

            buffer(:n) = this%args
            deallocate (this%args)
            call move_alloc(buffer, this%args)
        end if

        this%index = this%index + 1

        associate (arg => this%args(this%index))
            if (present(name))     arg%name     = name
            if (present(short))    arg%short    = short
            if (present(min_len))  arg%min_len  = min_len
            if (present(max_len))  arg%max_len  = max_len
            if (present(type))     arg%type     = type
            if (present(required)) arg%required = required
            if (present(exist))    arg%exist    = exist
        end associate
    end subroutine arg_parser_add

    subroutine arg_parser_create(this, max_size, default)
        !! Initialises argument parser and adds default arguments `--help` and
        !! `--version` unless `default` is `.false.`.
        class(arg_parser_class), intent(inout)        :: this     !! Arg parser.
        integer,                 intent(in), optional :: max_size !! Max. number of arguments to expect.
        logical,                 intent(in), optional :: default  !! Add default arguments.

        integer :: n, stat

        n = dm_present(max_size, ARG_PARSER_NARGS)
        if (allocated(this%args)) deallocate (this%args)
        allocate (this%args(n), stat=stat)
        if (stat /= 0) return

        if (dm_present(default, .true.)) then
            call this%add('help',    'h', ARG_TYPE_LOGICAL)
            call this%add('version', 'v', ARG_TYPE_LOGICAL)
        end if
    end subroutine arg_parser_create

    subroutine arg_parser_destroy(this)
        !! Destroys argument parser object. This subroutine is called
        !! automatically.
        type(arg_parser_class), intent(inout) :: this !! Arg parser.

        if (allocated(this%args)) deallocate (this%args)
    end subroutine arg_parser_destroy

    subroutine arg_parser_get_arg(this, name, arg, passed, error)
        !! Returns argument derived type of given name in argument `arg`. If
        !! the argument could not be found, `arg%error` is set to `E_NOT_FOUND.
        class(arg_parser_class), intent(inout)         :: this   !! Arg parser.
        character(*),            intent(in)            :: name   !! Argument name.
        type(arg_type),          intent(out)           :: arg    !! Argument type.
        logical,                 intent(out), optional :: passed !! Passed or not.
        integer,                 intent(out), optional :: error  !! Argument error.

        integer :: i

        arg%error = E_NOT_FOUND

        if (present(passed)) passed = .false.
        if (present(error))  error  = arg%error

        i = this%find(name)
        if (i == 0) return

        arg = this%args(i)

        if (present(passed)) passed = arg%passed
        if (present(error))  error  = arg%error
    end subroutine arg_parser_get_arg

    subroutine arg_parser_get_int32(this, name, value, default, passed, error)
        !! Returns argument value as 4-byte integer.
        class(arg_parser_class), intent(inout)         :: this    !! Arg parser.
        character(*),            intent(in)            :: name    !! Argument name.
        integer,                 intent(inout)         :: value   !! Argument value.
        integer,                 intent(in),  optional :: default !! Default value.
        logical,                 intent(out), optional :: passed  !! Passed or not.
        integer,                 intent(out), optional :: error   !! Argument error.

        type(arg_type) :: arg

        call this%get_arg(name, arg, passed, error)

        if (dm_is_error(arg%error)) then
            if (present(default)) value = default
            return
        end if

        value = dm_atoi(arg%value)
    end subroutine arg_parser_get_int32

    subroutine arg_parser_get_logical(this, name, value, default, passed, error)
        !! Returns `.true.` if argument has been passed.
        class(arg_parser_class), intent(inout)         :: this    !! Arg parser.
        character(*),            intent(in)            :: name    !! Argument name.
        logical,                 intent(inout)         :: value   !! Argument value.
        logical,                 intent(in),  optional :: default !! Default value.
        logical,                 intent(out), optional :: passed  !! Passed or not.
        integer,                 intent(out), optional :: error   !! Argument error.

        type(arg_type) :: arg

        call this%get_arg(name, arg, passed, error)

        if (dm_is_error(arg%error)) then
            if (present(default)) value = default
            return
        end if

        value = .true.
    end subroutine arg_parser_get_logical

    subroutine arg_parser_get_real64(this, name, value, default, passed, error)
        !! Returns argument value as 8-byte real.
        class(arg_parser_class), intent(inout)         :: this    !! Arg parser.
        character(*),            intent(in)            :: name    !! Argument name.
        real(r8),                intent(inout)         :: value   !! Argument value.
        real(r8),                intent(in),  optional :: default !! Default value.
        logical,                 intent(out), optional :: passed  !! Passed or not.
        integer,                 intent(out), optional :: error   !! Argument error.

        type(arg_type) :: arg

        call this%get_arg(name, arg, passed, error)

        if (dm_is_error(arg%error)) then
            if (present(default)) value = default
            return
        end if

        value = dm_atof(arg%value)
    end subroutine arg_parser_get_real64

    subroutine arg_parser_get_string(this, name, value, default, passed, error)
        !! Returns argument value as character string.
        class(arg_parser_class), intent(inout)         :: this    !! Arg parser.
        character(*),            intent(in)            :: name    !! Argument name.
        character(*),            intent(inout)         :: value   !! Argument value.
        character(*),            intent(in),  optional :: default !! Default value.
        logical,                 intent(out), optional :: passed  !! Passed or not.
        integer,                 intent(out), optional :: error   !! Argument error.

        type(arg_type) :: arg

        call this%get_arg(name, arg, passed, error)

        if (dm_is_error(arg%error)) then
            if (present(default)) value = default
            return
        end if

        value = arg%value
    end subroutine arg_parser_get_string
end module dm_arg_parser
