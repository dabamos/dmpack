! dmrecv.f90
!
! Author:  Philipp Engel
! Licence: ISC
program dmrecv
    !! Receives logs or observations from POSIX message queue and writes them to
    !! standard output or file, either in CSV, JSON, or Namelist format.
    !!
    !! The output can be piped to a graph tool like trend(1), to show a real-time
    !! plot:
    !!
    !! ```
    !! $ dmrecv --name dmrecv --type observ --format block --response tz0 | \
    !!   awk '{ print $2 | "trend - 60" }'
    !! ```
    !!
    !! Another process has to send observations to message queue "/dmrecv".
    !! Only responses of name "tz0" will be converted to ASCII block format and
    !! printed to standard output.
    use :: dmpack, dm_log => dm_logger_log
    implicit none (type, external)

    character(len=*), parameter :: APP_NAME  = 'dmrecv'
    integer,          parameter :: APP_MAJOR = 0
    integer,          parameter :: APP_MINOR = 9

    logical, parameter :: APP_MQ_BLOCKING = .true. !! Observation forwarding is blocking.

    type :: app_type
        !! Application settings.
        character(len=LOGGER_NAME_LEN)   :: name        = APP_NAME    !! Name of process and POSIX message queue.
        character(len=FILE_PATH_LEN)     :: config      = ' '         !! Path to configuration file.
        character(len=FILE_PATH_LEN)     :: output      = ' '         !! Path of output file (stdout if empty).
        character(len=FORMAT_NAME_LEN)   :: format_name = ' '         !! Format name.
        character(len=TYPE_NAME_LEN)     :: type_name   = ' '         !! Type name.
        character(len=RESPONSE_NAME_LEN) :: response    = ' '         !! Response name for block output of observations.
        integer                          :: format      = FORMAT_NONE !! Data output format.
        integer                          :: type        = TYPE_NONE   !! Data type.
        logical                          :: forward     = .false.     !! Observation forwarding.
        logical                          :: replace     = .false.     !! Replace output file.
        logical                          :: verbose     = .false.     !! Print debug messages to stderr.
    end type app_type

    integer           :: rc, type
    type(app_type)    :: app
    type(mqueue_type) :: mqueue

    ! Initialise DMPACK.
    call dm_init()

    ! Get command-line arguments, read options from configuration file.
    rc = read_args(app)
    if (dm_is_error(rc)) call dm_stop(1)

    init_block: block
        ! Initialise logger.
        call dm_logger_init(source=app%name, ipc=.false., verbose=app%verbose)

        ! Open log message queue for reading.
        rc = dm_mqueue_open(mqueue=mqueue, type=app%type, name=app%name, access=MQUEUE_RDONLY)

        if (dm_is_error(rc)) then
            call dm_log(LOG_ERROR, 'failed to open message queue /' // app%name, error=rc)
            exit init_block
        end if

        ! Run the IPC loop.
        call register_signal_handlers()
        call run(app, mqueue)
    end block init_block

    if (dm_is_error(rc)) call halt(1)
    call halt(0)
contains
    integer function forward_observ(observ) result(rc)
        !! Forwards given observation to next receiver.
        type(observ_type), intent(inout) :: observ !! Observation type.

        integer           :: next
        type(mqueue_type) :: mqueue

        next = max(0, observ%next) + 1

        ! Validate receiver.
        if (next > min(observ%nreceivers, OBSERV_MAX_NRECEIVERS)) then
            rc = E_NONE
            return
        end if

        if (.not. dm_id_valid(observ%receivers(next))) then
            call dm_log(LOG_ERROR, 'invalid receiver ' // trim(observ%receivers(next)) // &
                        ' in observation ' // observ%name, observ=observ, error=E_INVALID)
            rc = E_INVALID
            return
        end if

        mqueue_block: block
            ! Open message queue of receiver for writing.
            rc = dm_mqueue_open(mqueue   = mqueue, &
                                type     = TYPE_OBSERV, &
                                name     = observ%receivers(next), &
                                access   = MQUEUE_WRONLY, &
                                blocking = APP_MQ_BLOCKING)

            if (dm_is_error(rc)) then
                call dm_log(LOG_ERROR, 'failed to open message queue /' // observ%receivers(next), &
                            observ=observ, error=rc)
                exit mqueue_block
            end if

            ! Send observation to message queue.
            observ%next = next
            rc = dm_mqueue_write(mqueue, observ)

            if (dm_is_error(rc)) then
                call dm_log(LOG_ERROR, 'failed to send observation ' // trim(observ%name) // &
                            ' to message queue /' // observ%receivers(next), observ=observ, error=rc)
                exit mqueue_block
            end if
        end block mqueue_block

        ! Close message queue.
        rc = dm_mqueue_close(mqueue)

        if (dm_is_error(rc)) then
            call dm_log(LOG_WARNING, 'failed to close message queue /' // observ%receivers(next), &
                        observ=observ, error=rc)
        end if
    end function forward_observ

    integer function read_args(app) result(rc)
        !! Reads command-line arguments and settings from configuration file.
        type(app_type), intent(inout) :: app
        type(arg_type)                :: args(9)

        rc = E_NONE

        ! Required and optional command-line arguments.
        args = [ &
            arg_type('name',     short='n', type=ARG_TYPE_ID),   & ! -N, --name <string>
            arg_type('config',   short='c', type=ARG_TYPE_FILE), & ! -c, --config <path>
            arg_type('output',   short='o', type=ARG_TYPE_CHAR), & ! -o, --output <path>
            arg_type('format',   short='f', type=ARG_TYPE_CHAR), & ! -f, --format <string>
            arg_type('type',     short='t', type=ARG_TYPE_CHAR), & ! -t, --type <string>
            arg_type('response', short='R', type=ARG_TYPE_ID, max_len=RESPONSE_NAME_LEN), & ! -R, --response <string>
            arg_type('forward',  short='F', type=ARG_TYPE_BOOL), & ! -F, --forward
            arg_type('replace',  short='r', type=ARG_TYPE_BOOL), & ! -r, --replace
            arg_type('verbose',  short='V', type=ARG_TYPE_BOOL)  & ! -V, --verbose
        ]

        ! Read all command-line arguments.
        rc = dm_arg_read(args, APP_NAME, APP_MAJOR, APP_MINOR)
        if (dm_is_error(rc)) return

        rc = dm_arg_get(args(1), app%name)
        rc = dm_arg_get(args(2), app%config)

        ! Read configuration from file.
        rc = read_config(app)
        if (dm_is_error(rc)) return

        ! Overwrite settings.
        rc = dm_arg_get(args(3), app%output)
        rc = dm_arg_get(args(4), app%format_name)
        rc = dm_arg_get(args(5), app%type_name)
        rc = dm_arg_get(args(6), app%response)
        rc = dm_arg_get(args(7), app%forward)
        rc = dm_arg_get(args(8), app%replace)
        rc = dm_arg_get(args(9), app%verbose)

        app%format = dm_format_from_name(app%format_name)
        app%type   = dm_type_from_name(app%type_name)

        ! Validate settings.
        rc = E_INVALID

        if (.not. dm_id_valid(app%name)) then
            call dm_error_out(rc, 'invalid name')
            return
        end if

        if (app%forward .and. app%type /= TYPE_OBSERV) then
            call dm_error_out(rc, '--forward requires type observ')
            return
        end if

        if (app%replace .and. len_trim(app%output) == 0) then
            call dm_error_out(rc, '--replace requires output file')
            return
        end if

        if (app%format /= FORMAT_BLOCK .and. app%format /= FORMAT_CSV .and. &
            app%format /= FORMAT_JSON  .and. app%format /= FORMAT_NML) then
            call dm_error_out(rc, 'invalid format')
            return
        end if

        if (app%type /= TYPE_OBSERV .and. app%type /= TYPE_LOG) then
            call dm_error_out(rc, 'invalid type')
            return
        end if

        if (app%format == FORMAT_BLOCK) then
            if (app%type /= TYPE_OBSERV) then
                call dm_error_out(rc, 'block format requires type observ')
                return
            end if

            if (.not. dm_id_valid(app%response)) then
                call dm_error_out(rc, 'missing response name')
                return
            end if
        end if

        rc = E_NONE
    end function read_args

    integer function read_config(app) result(rc)
        !! Reads configuration from (Lua) file if path is not emty.
        type(app_type), intent(inout) :: app !! App type.
        type(config_type)             :: config

        rc = E_NONE
        if (len_trim(app%config) == 0) return

        rc = dm_config_open(config, app%config, app%name)

        if (dm_is_ok(rc)) then
            rc = dm_config_get(config, 'output',   app%output)
            rc = dm_config_get(config, 'format',   app%format_name)
            rc = dm_config_get(config, 'response', app%response)
            rc = dm_config_get(config, 'type',     app%type_name)
            rc = dm_config_get(config, 'forward',  app%forward)
            rc = dm_config_get(config, 'verbose',  app%verbose)
            rc = E_NONE
        end if

        call dm_config_close(config)
    end function read_config

    subroutine halt(stat)
        !! Cleans up and stops program.
        integer, intent(in) :: stat
        integer             :: rc

        rc = dm_mqueue_close(mqueue)
        rc = dm_mqueue_unlink(mqueue)
        call dm_stop(stat)
    end subroutine halt

    subroutine register_signal_handlers()
        !! Registers POSIX signal handlers.
        use, intrinsic :: iso_c_binding, only: c_funloc, c_funptr
        use :: unix
        type(c_funptr) :: ptr

        ptr = c_signal(SIGINT,  c_funloc(signal_handler))
        ptr = c_signal(SIGQUIT, c_funloc(signal_handler))
        ptr = c_signal(SIGABRT, c_funloc(signal_handler))
        ptr = c_signal(SIGKILL, c_funloc(signal_handler))
        ptr = c_signal(SIGTERM, c_funloc(signal_handler))
    end subroutine register_signal_handlers

    subroutine run(app, mqueue)
        !! Waits from incoming messages in the message queue.
        type(app_type),    intent(inout) :: app
        type(mqueue_type), intent(inout) :: mqueue

        integer           :: fu, i, j, rc, stat
        logical           :: is_file
        type(dp_type)     :: dp
        type(log_type)    :: log
        type(observ_type) :: observ

        fu = stdout
        is_file = .false.
        if (len_trim(app%output) > 0) is_file = .true.

        ipc_loop: do
            ! Read log or observation from POSIX message queue.
            if (app%type == TYPE_LOG) then
                rc = dm_mqueue_read(mqueue, log)
            else if (app%type == TYPE_OBSERV) then
                rc = dm_mqueue_read(mqueue, observ)
            end if

            ! Handle message queue error.
            if (dm_is_error(rc)) then
                call dm_log(LOG_ERROR, 'failed to read from message queue /' // app%name, error=rc)
                call dm_sleep(1)
                cycle ipc_loop
            end if

            ! Validate log or observation.
            if (app%type == TYPE_LOG) then
                if (.not. dm_log_valid(log)) then
                    call dm_log(LOG_ERROR, 'invalid log received', error=E_INVALID)
                    cycle ipc_loop
                end if
            else if (app%type == TYPE_OBSERV) then
                if (.not. dm_observ_valid(observ)) then
                    call dm_log(LOG_ERROR, 'invalid observation received', error=E_INVALID)
                    cycle ipc_loop
                end if
            end if

            ! Open output file.
            if (is_file) then
                if (app%type == TYPE_LOG) then
                    call dm_log(LOG_DEBUG, 'received log ' // trim(log%id))
                else if (app%type == TYPE_OBSERV) then
                    call dm_log(LOG_DEBUG, 'received observation ' // trim(observ%id))
                end if

                if (app%replace) then
                    ! Replace file.
                    open (action='write', file=trim(app%output), iostat=stat, &
                          newunit=fu, position='rewind', status='replace')
                else
                    ! Append data.
                    open (action='write', file=trim(app%output), iostat=stat, &
                          newunit=fu, position='append', status='unknown')
                end if

                if (stat /= 0) then
                    call dm_log(LOG_ERROR, 'failed to open file ' // app%output, error=E_IO)
                    exit ipc_loop
                end if
            end if

            ! Write serialised log or observation to file/stdout.
            if (app%type == TYPE_LOG) then
                select case (app%format)
                    case (FORMAT_CSV)
                        rc = dm_csv_write(log, unit=fu)
                    case (FORMAT_JSON)
                        rc = dm_json_write(log, unit=fu)
                    case (FORMAT_NML)
                        rc = dm_nml_write(log, unit=fu)
                end select
            else if (app%type == TYPE_OBSERV) then
                select case (app%format)
                    case (FORMAT_BLOCK)
                        ! Search for response of configured name and convert the
                        ! observation's response into a data point type.
                        if (dm_is_ok(dm_observ_index(observ, app%response, i, j))) then
                            dp = dp_type(x = observ%requests(i)%timestamp, &
                                         y = observ%requests(i)%responses(j)%value)
                            rc = dm_block_write(dp, unit=fu)
                        else
                            call dm_log(LOG_DEBUG, 'no response of name ' // app%response, error=E_NOT_FOUND)
                        end if
                    case (FORMAT_CSV)
                        rc = dm_csv_write(observ, unit=fu)
                    case (FORMAT_JSON)
                        rc = dm_json_write(observ, unit=fu)
                    case (FORMAT_NML)
                        rc = dm_nml_write(observ, unit=fu)
                end select
            end if

            ! Close file.
            if (is_file) then
                close (fu)

                if (app%type == TYPE_LOG) then
                    call dm_log(LOG_DEBUG, 'log ' // trim(log%id) // ' written to ' // app%output)
                else if (app%type == TYPE_OBSERV) then
                    call dm_log(LOG_DEBUG, 'observation ' // trim(observ%id) // ' written to ' // app%output)
                end if
            end if

            ! Handle write errors.
            if (dm_is_error(rc)) then
                if (app%type == TYPE_LOG) then
                    call dm_log(LOG_ERROR, 'failed to write log ' // log%id, error=rc)
                else if (app%type == TYPE_OBSERV) then
                    call dm_log(LOG_ERROR, 'failed to write observation ' // observ%id, error=rc)
                end if
            end if

            ! Forward observation to next receiver.
            if (app%forward .and. app%type == TYPE_OBSERV) then
                rc = forward_observ(observ)

                if (dm_is_error(rc)) then
                    call dm_log(LOG_ERROR, 'failed to forward observation ' // observ%id, error=rc)
                end if
            end if
        end do ipc_loop
    end subroutine run

    subroutine signal_handler(signum) bind(c)
        !! C-interoperable signal handler that closes database, removes message
        !! queue, and stops program.
        use, intrinsic :: iso_c_binding, only: c_int
        integer(kind=c_int), intent(in), value :: signum

        select case (signum)
            case default
                call dm_log(LOG_INFO, 'exit on signal ' // dm_itoa(signum))
                call halt(0)
        end select
    end subroutine signal_handler
end program dmrecv
