! Author:  Philipp Engel
! Licence: ISC
module dm_arg
    !! Command-line argument type.
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
    integer, parameter, public :: ARG_TYPE_FILE     = 10 !! Path to file on file system.
    integer, parameter, public :: ARG_TYPE_DATABASE = 11 !! Path to database on file system.
    integer, parameter, public :: ARG_TYPE_LAST     = 11 !! Never use this.

    integer, parameter, public :: ARG_NAME_LEN  = 32            !! Maximum length of argument name.
    integer, parameter, public :: ARG_VALUE_LEN = FILE_PATH_LEN !! Maximum length of argument value.

    type, public :: arg_type
        !! Argument type.
        character(ARG_NAME_LEN)  :: name     = ' '              !! Identifier of the argument (without leading --).
        character                :: short    = ASCII_NUL        !! Short argument character.
        character(ARG_VALUE_LEN) :: value    = ' '              !! Default and passed value (if any).
        integer                  :: length   = 0                !! Value length.
        integer                  :: min_len  = 0                !! Minimum argument value length.
        integer                  :: max_len  = ARG_VALUE_LEN    !! Maximum argument value length.
        integer                  :: type     = ARG_TYPE_LOGICAL !! Value data type.
        logical                  :: required = .false.          !! Option is mandatory.
        logical                  :: exist    = .false.          !! File or database must exist.
        logical                  :: passed   = .false.          !! Option was passed.
        integer                  :: error    = E_NONE           !! Occured error.
    end type arg_type

    public :: dm_arg_has
    public :: dm_arg_help
    public :: dm_arg_parse
    public :: dm_arg_type_is_valid
    public :: dm_arg_validate
contains
    ! **************************************************************************
    ! PUBLIC PROCEDURES.
    ! **************************************************************************
    logical function dm_arg_has(name, short) result(has)
        !! Returns `.true.` if argument of given name is passed without value.
        character(*), intent(in)           :: name  !! Name of command-line argument.
        character,    intent(in), optional :: short !! Short name.

        integer        :: rc
        type(arg_type) :: args(1)

        has = .false.
        args(1) = arg_type(name=name, type=ARG_TYPE_LOGICAL)
        if (present(short)) args(1)%short = short
        rc = dm_arg_parse(args, size(args), ignore_unknown=.true., verbose=.true.)
        has = (args(1)%error == E_NONE)
    end function dm_arg_has

    pure elemental logical function dm_arg_type_is_valid(type) result(valid)
        !! Returns `.true.` if passed type is a valid argument type.
        integer, intent(in) :: type !! Argument type (`ARG_TYPE_*`).

        valid = (type >= ARG_TYPE_NONE .and. type <= ARG_TYPE_LAST)
    end function dm_arg_type_is_valid

    integer function dm_arg_parse(args, nargs, ignore_unknown, verbose) result(rc)
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
    end function dm_arg_parse

    integer function dm_arg_validate(arg) result(rc)
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
        if (.not. dm_arg_type_is_valid(arg%type)) return

        ! Required argument has not been passed.
        rc = E_ARG_INVALID
        if (arg%required .and. .not. arg%passed) return

        ! Exit early if argument has not been passed.
        rc = arg%error
        if (dm_is_error(rc)) return

        ! Validate the type.
        rc = E_ARG_TYPE
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
                if (.not. dm_file_is_valid(arg%value)) return
                if (arg%exist .and. .not. dm_file_exists(arg%value)) return
        end select

        rc = E_NONE
    end function dm_arg_validate

    subroutine dm_arg_help(args)
        !! Prints command-line arguments to standard output if `--help` or `-h`
        !! is passed.
        type(arg_type), intent(inout) :: args(:) !! Arguments array.

        integer :: i

        write (STDOUT, '("Available command-line options:", /)')

        do i = 1, size(args)
            write (STDOUT, '(4x, "-", a1, ", --", a, 1x)', advance='no') &
                args(i)%short, trim(args(i)%name)

            select case (args(i)%type)
                case (ARG_TYPE_INTEGER);  write (STDOUT, '("<integer>")')
                case (ARG_TYPE_REAL);     write (STDOUT, '("<real>")')
                case (ARG_TYPE_CHAR);     write (STDOUT, '("<char>")')
                case (ARG_TYPE_STRING);   write (STDOUT, '("<string>")')
                case (ARG_TYPE_ID);       write (STDOUT, '("<id>")')
                case (ARG_TYPE_UUID);     write (STDOUT, '("<uuid>")')
                case (ARG_TYPE_TIME);     write (STDOUT, '("<ISO 8601>")')
                case (ARG_TYPE_LEVEL);    write (STDOUT, '("<log level>")')
                case (ARG_TYPE_FILE);     write (STDOUT, '("<file path>")')
                case (ARG_TYPE_DATABASE); write (STDOUT, '("<database path>")')
                case default;             write (STDOUT, *)
            end select
        end do

        print *
    end subroutine dm_arg_help
end module dm_arg
