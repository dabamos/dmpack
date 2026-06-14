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
    !! `/dmrecv`.  Only responses of name `tz0` will be converted to ASCII
    !! block format and printed to standard output.
    use :: dmpack
    implicit none (type, external)

    character(*), parameter :: APP_NAME  = 'dmrecv'
    integer,      parameter :: APP_MAJOR = 2
    integer,      parameter :: APP_MINOR = 0
    integer,      parameter :: APP_PATCH = 0

    logical, parameter :: APP_MQ_BLOCKING = .true. !! Observation forwarding is blocking.

    type :: app_type
        !! Application settings.
        character(ID_LEN)            :: name        = APP_NAME    !! Name of process and POSIX message queue.
        character(FILE_PATH_LEN)     :: config      = ' '         !! Path to configuration file.
        character(LOGGER_NAME_LEN)   :: logger      = ' '         !! Name of logger (name implies IPC).
        character(NODE_ID_LEN)       :: node_id     = ' '         !! Node id (optional).
        character(FILE_PATH_LEN)     :: output      = ' '         !! Path of output file (stdout if empty or `-`).
        character(FORMAT_NAME_LEN)   :: format_name = ' '         !! Format name.
        character(TYPE_NAME_LEN)     :: type_name   = ' '         !! Type name.
        character(RESPONSE_NAME_LEN) :: response    = ' '         !! Response name for block output of observations.
        integer                      :: format      = FORMAT_NONE !! Data output format.
        integer                      :: type        = TYPE_NONE   !! Data type.
        logical                      :: debug       = .false.     !! Forward debug messages via IPC.
        logical                      :: file        = .false.     !! Output to file.
        logical                      :: forward     = .false.     !! Observation forwarding.
        logical                      :: replace     = .false.     !! Replace output file.
        logical                      :: verbose     = .false.     !! Print debug messages to stderr.
    end type app_type

    class(logger_class), pointer :: logger ! Logger object.

    integer                 :: rc     ! Return code.
    type(app_type)          :: app    ! App settings.
    type(posix_mqueue_type) :: mqueue ! POSIX message queue.
    type(posix_signal_type) :: signal ! Self-pipe.

    ! Initialise DMPACK.
    call dm_init()

    ! Get command-line arguments and read options from configuration file.
    rc = read_args(app)
    if (dm_is_error(rc)) call dm_stop(STOP_FAILURE)

    ! Initialise logger.
    logger => dm_logger_get_default()
    call logger%configure(name    = app%logger,  & ! Name of logger process.
                          node_id = app%node_id, & ! Node id.
                          source  = app%name,    & ! Log source.
                          debug   = app%debug,   & ! Forward debug messages via IPC.
                          ipc     = .true.,      & ! Enable IPC (if logger is set).
                          verbose = app%verbose)   ! Print logs to standard error.
    call logger%status('started ' // APP_NAME)

    rc = init(mqueue, signal)
    if (dm_is_error(rc)) call shutdown(rc)

    rc = run(app, mqueue, signal)
    call shutdown(rc)
contains
    integer function init(mqueue, signal) result(rc)
        type(posix_mqueue_type), intent(out) :: mqueue !! Message queue.
        type(posix_signal_type), intent(out) :: signal !! Self-pipe.

        ! Open message queue for reading.
        rc = dm_posix_mqueue_open(mqueue, type=app%type, name=app%name, access=POSIX_MQUEUE_RDONLY)

        if (dm_is_error(rc)) then
            call logger%error('failed to open mqueue /' // app%name, error=rc)
            return
        end if

        call logger%debug('opened mqueue /' // app%name)

        ! Create self-pipe and register signal handler.
        signal_block: block
            rc = dm_posix_signal_create(signal)
            if (dm_is_error(rc)) exit signal_block

            rc = dm_posix_signal_register_all(signal_callback)
        end block signal_block

        if (dm_is_error(rc)) then
            call logger%error('failed to initialize signal handler', error=rc)
            return
        end if

        call logger%debug('initialized signal handler')
    end function init

    integer function open_file(path, replace, unit) result(rc)
        character(*), intent(in)  :: path
        logical,      intent(in)  :: replace
        integer,      intent(out) :: unit

        integer :: stat

        rc = E_IO

        if (replace) then
            ! Replace file.
            open (action='write', file=trim(path), iostat=stat, newunit=unit, position='rewind', status='replace')
        else
            ! Append data.
            open (action='write', file=trim(path), iostat=stat, newunit=unit, position='append', status='unknown')
        end if

        if (stat == 0) rc = E_NONE
    end function open_file

    integer function recv_log(app, mqueue) result(rc)
        type(app_type),          intent(inout) :: app    !! App type.
        type(posix_mqueue_type), intent(inout) :: mqueue !! Message queue type.

        character(NML_LOG_LEN) :: log_nml
        integer                :: unit, stat
        type(log_type)         :: log

        unit = stdout
        call logger%debug('waiting for log on mqueue /' // app%name)

        ! Read log from POSIX message queue (blocking).
        rc = dm_posix_mqueue_read(mqueue, log)

        if (rc == E_INTERRUPT) return

        if (dm_is_error(rc)) then
            call logger%error('failed to read log from mqueue /' // app%name, error=rc)
            return
        end if

        if (.not. dm_log_is_valid(log)) then
            call logger%warning('invalid log received', error=E_INVALID)
        else
            call logger%debug('received log ' // trim(log%id))
        end if

        ! Open output file.
        if (app%file) then
            rc = open_file(app%output, app%replace, unit)

            if (dm_is_error(rc)) then
                call logger%error('failed to open file ' // app%output, error=rc)
                return
            end if
        end if

        ! Write serialised log to file/stdout.
        select case (app%format)
            case (FORMAT_CSV)
                ! CSV format.
                rc = dm_csv_write(log, unit=unit)

            case (FORMAT_JSONL)
                ! JSON Lines format.
                rc = dm_json_write(log, unit=unit)

            case (FORMAT_NML)
                ! Namelist format. Write Namelist to a string first, to
                ! avoid newline characters.
                rc = dm_nml_from(log, log_nml)
                write (unit, '(a)', iostat=stat) trim(log_nml)
                if (stat /= 0) rc = E_WRITE
        end select

        ! Close output file.
        if (app%file) close (unit)

        if (dm_is_error(rc)) then
            call logger%error('failed to write log ' // log%id, error=rc)
            return
        end if

        if (app%file) call logger%debug('log ' // trim(log%id) // ' written to ' // app%output)
    end function recv_log

    integer function recv_observ(app, mqueue) result(rc)
        type(app_type),          intent(inout) :: app    !! App type.
        type(posix_mqueue_type), intent(inout) :: mqueue !! Message queue type.

        character(NML_OBSERV_LEN) :: observ_nml
        integer                   :: unit
        integer                   :: idx, stat
        type(dp_type)             :: dp
        type(observ_type)         :: observ

        unit = stdout
        call logger%debug('waiting for observation on mqueue /' // app%name)

        ! Read observation from POSIX message queue (blocking).
        rc = dm_posix_mqueue_read(mqueue, observ)

        if (rc == E_INTERRUPT) return

        if (dm_is_error(rc)) then
            call logger%error('failed to read observation from mqueue /' // app%name, error=rc)
            return
        end if

        if (.not. dm_observ_is_valid(observ)) then
            call logger%warning('invalid observation received', error=E_INVALID)
        else
            call logger%debug('received observation ' // trim(observ%id))
        end if

        ! Open output file.
        if (app%file) then
            rc = open_file(app%output, app%replace, unit)

            if (dm_is_error(rc)) then
                call logger%error('failed to open file ' // app%output, error=rc)
                return
            end if
        end if

        ! Write serialised observation to file/stdout.
        select case (app%format)
            case (FORMAT_BLOCK)
                ! ASCII block format. Search for response of configured name and convert the
                ! observation's response into a data point type.
                idx = dm_observ_find(observ, app%response)

                if (idx > 0) then
                    dp = dp_type(observ%timestamp, observ%responses(idx)%value)
                    rc = dm_block_write(dp, unit=unit)
                else
                    call logger%debug('no response of name ' // app%response, error=E_NOT_FOUND)
                end if

            case (FORMAT_CSV)
                ! CSV format.
                rc = dm_csv_write(observ, unit=unit)

            case (FORMAT_JSONL)
                ! JSON Lines format.
                rc = dm_json_write(observ, unit=unit)

            case (FORMAT_NML)
                ! Namelist format. Write Namelist to a string first, to avoid newline characters.
                rc = dm_nml_from(observ, observ_nml)
                write (unit, '(a)', iostat=stat) trim(observ_nml)
                if (stat /= 0) rc = E_WRITE
        end select

        ! Close output file.
        if (app%file) close (unit)

        if (dm_is_error(rc)) then
            call logger%error('failed to write observation ' // observ%id, error=rc)
            return
        end if

        if (app%file) call logger%debug('observation ' // trim(observ%id) // ' written to ' // app%output)

        ! Forward observation to next receiver.
        if (app%forward) rc = dm_posix_mqueue_forward(observ, name=app%name, blocking=APP_MQ_BLOCKING)
    end function recv_observ

    integer function run(app, mqueue, signal) result(rc)
        !! Event loop that receives logs or observations.
        type(app_type),          intent(inout) :: app    !! App type.
        type(posix_mqueue_type), intent(inout) :: mqueue !! Message queue type.
        type(posix_signal_type), intent(inout) :: signal !! Self-pipe.

        integer :: number

        main_loop: do
            ! Poll for signals.
            rc = dm_posix_signal_poll(signal, timeout=0)

            if (rc == E_INTERRUPT) cycle main_loop

            if (dm_is_error(rc)) then
                call logger%error('failed to poll signal pipe', error=rc)
                exit main_loop
            end if

            if (dm_posix_signal_should_stop(signal, number)) then
                call logger%debug('exit on signal ' // dm_posix_signal_name(number))
                exit main_loop
            end if

            ! Receive observation or log.
            select case (app%type)
                case (TYPE_OBSERV); rc = recv_observ(app, mqueue)
                case (TYPE_LOG);    rc = recv_log   (app, mqueue)
            end select

            ! Handle return code.
            select case (rc)
                case (E_INTERRUPT); cycle main_loop
                case (E_IO);        exit main_loop
                case (E_MQUEUE);    call dm_posix_sleep(5)
            end select
        end do main_loop
    end function run

    subroutine shutdown(error)
        !! Cleans up and stops program.
        integer, intent(in) :: error !! DMPACK error code.

        integer :: rc

        call dm_posix_signal_destroy(signal)

        call dm_posix_mqueue_close(mqueue, error=rc)
        if (dm_is_error(rc)) call logger%error('failed to close mqueue /' // app%name, error=rc)

        call dm_posix_mqueue_unlink(mqueue, error=rc)
        if (dm_is_error(rc)) call logger%error('failed to unlink mqueue /' // app%name, error=rc)

        call logger%status('stopped ' // APP_NAME, error=error)
        call dm_stop(merge(STOP_FAILURE, STOP_SUCCESS, dm_is_error(error)))
    end subroutine shutdown

    ! **************************************************************************
    ! COMMAND-LINE ARGUMENTS AND CONFIGURATION FILE
    ! **************************************************************************
    integer function read_args(app) result(rc)
        !! Reads command-line arguments and settings from configuration file.
        type(app_type), intent(out) :: app

        type(arg_parser_class) :: parser

        ! Required and optional command-line arguments.
        call parser%add('name',     short='n', type=ARG_TYPE_ID)                            ! -N, --name <string>
        call parser%add('config',   short='c', type=ARG_TYPE_FILE)                          ! -c, --config <path>
        call parser%add('logger',   short='l', type=ARG_TYPE_ID)                            ! -l, --logger <string>
        call parser%add('node',     short='N', type=ARG_TYPE_ID)                            ! -N, --node <string>
        call parser%add('output',   short='o', type=ARG_TYPE_FILE)                          ! -o, --output <path>
        call parser%add('format',   short='f', type=ARG_TYPE_STRING)                        ! -f, --format <string>
        call parser%add('type',     short='t', type=ARG_TYPE_STRING)                        ! -t, --type <string>
        call parser%add('response', short='R', type=ARG_TYPE_ID, max_len=RESPONSE_NAME_LEN) ! -R, --response <string>
        call parser%add('debug',    short='D', type=ARG_TYPE_LOGICAL)                       ! -D, --debug
        call parser%add('forward',  short='F', type=ARG_TYPE_LOGICAL)                       ! -F, --forward
        call parser%add('replace',  short='r', type=ARG_TYPE_LOGICAL)                       ! -r, --replace
        call parser%add('verbose',  short='V', type=ARG_TYPE_LOGICAL)                       ! -V, --verbose

        ! Read all command-line arguments.
        rc = parser%read(version_callback)
        if (dm_is_error(rc)) return

        call parser%get('name',   app%name)
        call parser%get('config', app%config)

        ! Read configuration from file.
        rc = read_config(app)
        if (dm_is_error(rc)) return

        ! Overwrite settings.
        call parser%get('logger',   app%logger)
        call parser%get('node',     app%node_id)
        call parser%get('output',   app%output)
        call parser%get('format',   app%format_name)
        call parser%get('type',     app%type_name)
        call parser%get('response', app%response)
        call parser%get('debug',    app%debug)
        call parser%get('forward',  app%forward)
        call parser%get('replace',  app%replace)
        call parser%get('verbose',  app%verbose)

        app%file   = (dm_string_has(app%output) .and. app%output /= '-')
        app%format = dm_format_from_name(app%format_name)
        app%type   = dm_type_from_name(app%type_name)

        rc = validate(app)
    end function read_args

    integer function read_config(app) result(rc)
        !! Reads configuration from (Lua) file if path is not emty.
        type(app_type), intent(inout) :: app !! App type.

        type(config_class) :: config

        rc = E_NONE
        if (.not. dm_string_has(app%config)) return

        rc = config%open(app%config, app%name)

        if (dm_is_ok(rc)) then
            call config%get('logger',   app%logger)
            call config%get('node',     app%node_id)
            call config%get('output',   app%output)
            call config%get('format',   app%format_name)
            call config%get('response', app%response)
            call config%get('type',     app%type_name)
            call config%get('debug',    app%debug)
            call config%get('forward',  app%forward)
            call config%get('verbose',  app%verbose)
        end if

        call config%close()
    end function read_config

    integer function validate(app) result(rc)
        !! Validates options and prints error messages.
        type(app_type), intent(in) :: app !! App type.

        rc = E_INVALID

        if (.not. dm_id_is_valid(app%name)) then
            call dm_error_out(rc, 'invalid name')
            return
        end if

        if (dm_string_has(app%node_id) .and. .not. dm_id_is_valid(app%node_id)) then
            call dm_error_out(rc, 'invalid node id')
            return
        end if

        if (dm_string_has(app%logger) .and. .not. dm_id_is_valid(app%logger)) then
            call dm_error_out(rc, 'invalid logger name')
            return
        end if

        if (app%forward .and. app%type /= TYPE_OBSERV) then
            call dm_error_out(rc, 'forward option requires type observation')
            return
        end if

        if (app%replace .and. .not. app%file) then
            call dm_error_out(rc, 'replace option requires output file')
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
                call dm_error_out(rc, 'block format requires type observation')
                return
            end if

            if (.not. dm_id_is_valid(app%response)) then
                call dm_error_out(rc, 'invalid response name')
                return
            end if
        end if

        rc = E_NONE
    end function validate

    ! **************************************************************************
    ! CALLBACKS
    ! **************************************************************************
    subroutine signal_callback(number) bind(c)
        integer(c_int), intent(in), value :: number

        call dm_posix_signal_write(signal, number)
    end subroutine signal_callback

    subroutine version_callback()
        call dm_version_out(APP_NAME, APP_MAJOR, APP_MINOR, APP_PATCH)
        print '(a)', dm_lua_version(.true.)
    end subroutine version_callback
end program dmrecv
