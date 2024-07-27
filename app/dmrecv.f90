! dmrecv.f90
!
! Author:  Philipp Engel
! Licence: ISC
program dmrecv
    !! Receives logs or observations from POSIX message queue and writes them
    !! to standard output or file, either in CSV, JSON Lines, or Namelist
    !! format.
    !!
    !! The output can be piped to a graph tool like _trend(1)_, to show a
    !! real-time plot:
    !!
    !! ```
    !! $ dmrecv --name dmrecv --type observ --format block --response tz0 \
    !!   | awk '{ print $2 | "trend - 60" }'
    !! ```
    !!
    !! Another DMPACK process has to send observations to message queue
    !! "/dmrecv".  Only responses of name `tz0` will be converted to ASCII
    !! block format and printed to standard output.
    use :: dmpack
    implicit none (type, external)

    character(len=*), parameter :: APP_NAME  = 'dmrecv'
    integer,          parameter :: APP_MAJOR = 0
    integer,          parameter :: APP_MINOR = 9
    integer,          parameter :: APP_PATCH = 3

    logical, parameter :: APP_MQ_BLOCKING = .true. !! Observation forwarding is blocking.

    type :: app_type
        !! Application settings.
        character(len=ID_LEN)            :: name        = APP_NAME    !! Name of process and POSIX message queue.
        character(len=FILE_PATH_LEN)     :: config      = ' '         !! Path to configuration file.
        character(len=LOGGER_NAME_LEN)   :: logger      = ' '         !! Name of logger (name implies IPC).
        character(len=NODE_ID_LEN)       :: node        = ' '         !! Node id (optional).
        character(len=FILE_PATH_LEN)     :: output      = ' '         !! Path of output file (stdout if empty or `-`).
        character(len=FORMAT_NAME_LEN)   :: format_name = ' '         !! Format name.
        character(len=TYPE_NAME_LEN)     :: type_name   = ' '         !! Type name.
        character(len=RESPONSE_NAME_LEN) :: response    = ' '         !! Response name for block output of observations.
        integer                          :: format      = FORMAT_NONE !! Data output format.
        integer                          :: type        = TYPE_NONE   !! Data type.
        logical                          :: debug       = .false.     !! Forward debug messages via IPC.
        logical                          :: file        = .false.     !! Output to file.
        logical                          :: forward     = .false.     !! Observation forwarding.
        logical                          :: replace     = .false.     !! Replace output file.
        logical                          :: verbose     = .false.     !! Print debug messages to stderr.
    end type app_type

    class(logger_class), pointer :: logger ! Logger object.

    integer           :: rc     ! Return code.
    type(app_type)    :: app    ! App settings.
    type(mqueue_type) :: mqueue ! POSIX message queue.

    ! Initialise DMPACK.
    call dm_init()

    ! Get command-line arguments and read options from configuration file.
    rc = read_args(app)
    if (dm_is_error(rc)) call dm_stop(STOP_FAILURE)

    ! Initialise logger.
    logger => dm_logger_get()
    call logger%configure(name    = app%logger, &
                          node_id = app%node, &
                          source  = app%name, &
                          debug   = app%debug, &
                          ipc     = (len_trim(app%logger) > 0), &
                          verbose = app%verbose)

    init_block: block
        ! Open log message queue for reading.
        rc = dm_mqueue_open(mqueue = mqueue, &
                            type   = app%type, &
                            name   = app%name, &
                            access = MQUEUE_RDONLY)

        if (dm_is_error(rc)) then
            call logger%error('failed to open mqueue /' // app%name, error=rc)
            exit init_block
        end if

        ! Run the IPC loop.
        call dm_signal_register(signal_handler)
        call run(app, mqueue)
    end block init_block

    call halt(rc)
contains
    integer function read_args(app) result(rc)
        !! Reads command-line arguments and settings from configuration file.
        type(app_type), intent(out) :: app

        type(arg_type) :: args(12)

        rc = E_NONE

        ! Required and optional command-line arguments.
        args = [ &
            arg_type('name',     short='n', type=ARG_TYPE_ID),      & ! -N, --name <string>
            arg_type('config',   short='c', type=ARG_TYPE_FILE),    & ! -c, --config <path>
            arg_type('logger',   short='l', type=ARG_TYPE_ID),      & ! -l, --logger <string>
            arg_type('node',     short='N', type=ARG_TYPE_ID),      & ! -N, --node <string>
            arg_type('output',   short='o', type=ARG_TYPE_STRING),  & ! -o, --output <path>
            arg_type('format',   short='f', type=ARG_TYPE_STRING),  & ! -f, --format <string>
            arg_type('type',     short='t', type=ARG_TYPE_STRING),  & ! -t, --type <string>
            arg_type('response', short='R', type=ARG_TYPE_ID, max_len=RESPONSE_NAME_LEN), & ! -R, --response <string>
            arg_type('debug',    short='D', type=ARG_TYPE_LOGICAL), & ! -D, --debug
            arg_type('forward',  short='F', type=ARG_TYPE_LOGICAL), & ! -F, --forward
            arg_type('replace',  short='r', type=ARG_TYPE_LOGICAL), & ! -r, --replace
            arg_type('verbose',  short='V', type=ARG_TYPE_LOGICAL)  & ! -V, --verbose
        ]

        ! Read all command-line arguments.
        rc = dm_arg_read(args, APP_NAME, APP_MAJOR, APP_MINOR, APP_PATCH, dm_lua_version(.true.))
        if (dm_is_error(rc)) return

        rc = dm_arg_get(args(1), app%name)
        rc = dm_arg_get(args(2), app%config)

        ! Read configuration from file.
        rc = read_config(app)
        if (dm_is_error(rc)) return

        ! Overwrite settings.
        rc = dm_arg_get(args( 3), app%logger)
        rc = dm_arg_get(args( 4), app%node)
        rc = dm_arg_get(args( 5), app%output)
        rc = dm_arg_get(args( 6), app%format_name)
        rc = dm_arg_get(args( 7), app%type_name)
        rc = dm_arg_get(args( 8), app%response)
        rc = dm_arg_get(args( 9), app%debug)
        rc = dm_arg_get(args(10), app%forward)
        rc = dm_arg_get(args(11), app%replace)
        rc = dm_arg_get(args(12), app%verbose)

        app%file   = (len_trim(app%output) > 0 .and. app%output /= '-')
        app%format = dm_format_from_name(app%format_name)
        app%type   = dm_type_from_name(app%type_name)

        ! Validate settings.
        rc = E_INVALID

        if (.not. dm_id_valid(app%name)) then
            call dm_error_out(rc, 'invalid name')
            return
        end if

        if (len_trim(app%node) > 0 .and. .not. dm_id_valid(app%node)) then
            call dm_error_out(rc, 'invalid node id')
            return
        end if

        if (len_trim(app%logger) > 0 .and. .not. dm_id_valid(app%logger)) then
            call dm_error_out(rc, 'invalid logger name')
            return
        end if

        if (app%forward .and. app%type /= TYPE_OBSERV) then
            call dm_error_out(rc, '--forward requires type observ')
            return
        end if

        if (app%replace .and. .not. app%file) then
            call dm_error_out(rc, '--replace requires output file')
            return
        end if

        select case (app%format)
            case (FORMAT_BLOCK, FORMAT_CSV, FORMAT_JSONL, FORMAT_NML)
                continue
            case default
                call dm_error_out(rc, 'invalid format')
                return
        end select

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
                call dm_error_out(rc, 'invalid or missing response name')
                return
            end if
        end if

        rc = E_NONE
    end function read_args

    integer function read_config(app) result(rc)
        !! Reads configuration from (Lua) file if path is not emty.
        type(app_type), intent(inout) :: app !! App type.

        type(config_type) :: config

        rc = E_NONE
        if (len_trim(app%config) == 0) return

        rc = dm_config_open(config, app%config, app%name)

        if (dm_is_ok(rc)) then
            rc = dm_config_get(config, 'logger',   app%logger)
            rc = dm_config_get(config, 'node',     app%node)
            rc = dm_config_get(config, 'output',   app%output)
            rc = dm_config_get(config, 'format',   app%format_name)
            rc = dm_config_get(config, 'response', app%response)
            rc = dm_config_get(config, 'type',     app%type_name)
            rc = dm_config_get(config, 'debug',    app%debug)
            rc = dm_config_get(config, 'forward',  app%forward)
            rc = dm_config_get(config, 'verbose',  app%verbose)
            rc = E_NONE
        end if

        call dm_config_close(config)
    end function read_config

    subroutine halt(error)
        !! Cleans up and stops program.
        integer, intent(in) :: error !! DMPACK error code.

        integer :: rc, stat

        stat = STOP_SUCCESS
        if (dm_is_error(error)) stat = STOP_FAILURE

        rc = dm_mqueue_close(mqueue)
        rc = dm_mqueue_unlink(mqueue)

        call dm_stop(stat)
    end subroutine halt

    subroutine run(app, mqueue)
        !! Waits from incoming messages in the message queue.
        type(app_type),    intent(inout) :: app
        type(mqueue_type), intent(inout) :: mqueue

        character(len=NML_LOG_LEN)    :: log_nml
        character(len=NML_OBSERV_LEN) :: observ_nml

        integer :: file_unit
        integer :: i, j, rc, stat

        type(dp_type)     :: dp
        type(log_type)    :: log
        type(observ_type) :: observ

        file_unit = stdout

        call logger%info('started ' // dm_version_to_string(APP_NAME, APP_MAJOR, APP_MINOR, APP_PATCH))

        ipc_loop: do
            ! Read observation or log from POSIX message queue (blocking).
            if (app%type == TYPE_OBSERV) then
                ! Observation.
                call logger%debug('waiting for observ on mqueue /' // app%name)
                rc = dm_mqueue_read(mqueue, observ)
            else if (app%type == TYPE_LOG) then
                ! Log.
                call logger%debug('waiting for log on mqueue /' // app%name)
                rc = dm_mqueue_read(mqueue, log)
            end if

            ! Handle message queue error.
            if (dm_is_error(rc)) then
                call logger%error('failed to read from mqueue /' // app%name, error=rc)
                call dm_sleep(5)
                cycle ipc_loop
            end if

            ! Validate observation or log.
            if (app%type == TYPE_OBSERV) then
                ! Observation.
                if (.not. dm_observ_valid(observ)) then
                    call logger%error('invalid observ received', error=E_INVALID)
                    cycle ipc_loop
                end if

                call logger%debug('received observ ' // trim(observ%id))
            else if (app%type == TYPE_LOG) then
                ! Log.
                if (.not. dm_log_valid(log)) then
                    call logger%error('invalid log received', error=E_INVALID)
                    cycle ipc_loop
                end if

                call logger%debug('received log ' // trim(log%id))
            end if

            ! Open output file.
            if (app%file) then
                if (app%replace) then
                    ! Replace file.
                    open (action='write', file=trim(app%output), iostat=stat, &
                          newunit=file_unit, position='rewind', status='replace')
                else
                    ! Append data.
                    open (action='write', file=trim(app%output), iostat=stat, &
                          newunit=file_unit, position='append', status='unknown')
                end if

                if (stat /= 0) then
                    call logger%error('failed to open file ' // app%output, error=E_IO)
                    exit ipc_loop
                end if
            end if

            ! Write serialised observation or log to file/stdout.
            if (app%type == TYPE_OBSERV) then
                select case (app%format)
                    case (FORMAT_BLOCK)
                        ! ASCII block format. Search for response of configured
                        ! name and convert the observation's response into a
                        ! data point type.
                        if (dm_is_ok(dm_observ_index(observ, app%response, i, j))) then
                            dp = dp_type(x = observ%requests(i)%timestamp, &
                                         y = observ%requests(i)%responses(j)%value)
                            rc = dm_block_write(dp, unit=file_unit)
                        else
                            call logger%debug('no response of name ' // app%response, error=E_NOT_FOUND)
                        end if
                    case (FORMAT_CSV)
                        ! CSV format.
                        rc = dm_csv_write(observ, unit=file_unit)
                    case (FORMAT_JSONL)
                        ! JSON Lines format.
                        rc = dm_json_write(observ, unit=file_unit)
                    case (FORMAT_NML)
                        ! Namelist format. Write Namelist to a string first, to
                        ! avoid newline characters.
                        rc = dm_nml_from(observ, observ_nml)

                        if (dm_is_ok(rc)) then
                            write (file_unit, '(a)', iostat=stat) trim(observ_nml)
                            if (stat /= 0) rc = E_WRITE
                        end if
                end select
            else if (app%type == TYPE_LOG) then
                select case (app%format)
                    case (FORMAT_CSV)
                        ! CSV format.
                        rc = dm_csv_write(log, unit=file_unit)
                    case (FORMAT_JSONL)
                        ! JSON Lines format.
                        rc = dm_json_write(log, unit=file_unit)
                    case (FORMAT_NML)
                        ! Namelist format. Write Namelist to a string first, to
                        ! avoid newline characters.
                        rc = dm_nml_from(log, log_nml)

                        if (dm_is_ok(rc)) then
                            write (file_unit, '(a)', iostat=stat) trim(log_nml)
                            if (stat /= 0) rc = E_WRITE
                        end if
                end select
            end if

            ! Handle write errors.
            if (dm_is_error(rc)) then
                if (app%type == TYPE_OBSERV) then
                    ! Observation.
                    call logger%error('failed to write observ ' // observ%id, error=rc)
                else if (app%type == TYPE_LOG) then
                    ! Log.
                    call logger%error('failed to write log ' // log%id, error=rc)
                end if
            end if

            ! Close file.
            if (app%file) then
                if (app%type == TYPE_OBSERV) then
                    ! Observation.
                    call logger%debug('observ ' // trim(observ%id) // ' written to ' // app%output)
                else if (app%type == TYPE_LOG) then
                    ! Log.
                    call logger%debug('log ' // trim(log%id) // ' written to ' // app%output)
                end if

                close (file_unit)
            end if

            ! Forward observation to next receiver.
            if (app%forward) rc = dm_mqueue_forward(observ, name=app%name, blocking=APP_MQ_BLOCKING)
        end do ipc_loop
    end subroutine run

    subroutine signal_handler(signum) bind(c)
        !! C-interoperable signal handler that closes database, removes message
        !! queue, and stops program.
        use, intrinsic :: iso_c_binding, only: c_int
        integer(kind=c_int), intent(in), value :: signum !! Signal number.

        select case (signum)
            case default
                call logger%info('exit on signal ' // dm_itoa(signum))
                call halt(E_NONE)
        end select
    end subroutine signal_handler
end program dmrecv
