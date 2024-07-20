! dmsend.f90
!
! Author:  Philipp Engel
! Licence: ISC
program dmsend
    !! Reads logs or observations in CSV or Fortran 95 Namelist format from
    !! file or standard input, then sends all records to a POSIX message queue.
    use :: dmpack
    implicit none (type, external)

    character(len=*), parameter :: APP_NAME  = 'dmsend'
    integer,          parameter :: APP_MAJOR = 0
    integer,          parameter :: APP_MINOR = 9
    integer,          parameter :: APP_PATCH = 1

    logical, parameter :: APP_MQ_BLOCKING = .true. !! Observation forwarding is blocking.

    type :: app_type
        !! Application settings.
        character(len=ID_LEN)              :: name        = APP_NAME    !! Name of process and POSIX message queue.
        character(len=FILE_PATH_LEN)       :: config      = ' '         !! Path to configuration file.
        character(len=LOGGER_NAME_LEN)     :: logger      = ' '         !! Name of logger (name implies IPC).
        character(len=NODE_ID_LEN)         :: node        = ' '         !! Optional node id.
        character(len=FILE_PATH_LEN)       :: input       = ' '         !! Path to input file (stdin if empty or `-`).
        character(len=FORMAT_NAME_LEN)     :: format_name = ' '         !! Format name.
        character(len=TYPE_NAME_LEN)       :: type_name   = ' '         !! Type name.
        character(len=OBSERV_RECEIVER_LEN) :: receiver    = ' '         !! Name of receiver's message queue (without leading `/`).
        integer                            :: format      = FORMAT_NONE !! Input format.
        integer                            :: type        = TYPE_NONE   !! Data type.
        logical                            :: debug       = .false.     !! Forward debug messages via IPC.
        logical                            :: forward     = .false.     !! Enable observation forwarding.
        logical                            :: verbose     = .false.     !! Print debug messages to stderr.
    end type app_type

    class(logger_class), pointer :: logger ! Logger object.

    integer        :: rc  ! Return code.
    type(app_type) :: app ! App settings.

    ! Initialise DMPACK.
    call dm_init()

    ! Read command-line arguments and options from configuration file.
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

    ! Read and send data.
    rc = run(app)
    if (dm_is_error(rc)) call dm_stop(STOP_FAILURE)
contains
    integer function read_args(app) result(rc)
        !! Reads command-line arguments and settings from configuration file.
        type(app_type), intent(out) :: app
        type(arg_type)              :: args(11)

        rc = E_NONE

        ! Required and optional command-line arguments.
        args = [ &
            arg_type('name',     short='n', type=ARG_TYPE_ID),      & ! -n, --name <string>
            arg_type('config',   short='c', type=ARG_TYPE_FILE),    & ! -c, --config <path>
            arg_type('logger',   short='l', type=ARG_TYPE_ID),      & ! -l, --logger <string>
            arg_type('node',     short='N', type=ARG_TYPE_ID),      & ! -N, --node <string>
            arg_type('input',    short='i', type=ARG_TYPE_STRING),  & ! -i, --input <path>
            arg_type('format',   short='f', type=ARG_TYPE_STRING),  & ! -f, --format <string>
            arg_type('type',     short='t', type=ARG_TYPE_STRING),  & ! -t, --type <string>
            arg_type('receiver', short='r', type=ARG_TYPE_ID, max_len=OBSERV_RECEIVER_LEN), & ! -r, --receiver <string>
            arg_type('debug',    short='D', type=ARG_TYPE_LOGICAL), & ! -D, --debug
            arg_type('forward',  short='F', type=ARG_TYPE_LOGICAL), & ! -F, --forward
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
        rc = dm_arg_get(args( 5), app%input)
        rc = dm_arg_get(args( 6), app%format_name)
        rc = dm_arg_get(args( 7), app%type_name)
        rc = dm_arg_get(args( 8), app%receiver)
        rc = dm_arg_get(args( 9), app%debug)
        rc = dm_arg_get(args(10), app%forward)
        rc = dm_arg_get(args(11), app%verbose)

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

        if (app%type /= TYPE_OBSERV .and. app%type /= TYPE_LOG) then
            call dm_error_out(rc, 'invalid type')
            return
        end if

        if (app%format /= FORMAT_CSV .and. app%format /= FORMAT_NML) then
            call dm_error_out(rc, 'invalid format')
            return
        end if

        if (app%forward) then
            if (app%type /= TYPE_OBSERV) then
                call dm_error_out(rc, '--forward requires type observ')
                return
            end if
        else
            if (.not. dm_id_valid(app%receiver)) then
                call dm_error_out(rc, 'invalid receiver')
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
            rc = dm_config_get(config, 'logger',   app%logger)
            rc = dm_config_get(config, 'node',     app%node)
            rc = dm_config_get(config, 'input',    app%input)
            rc = dm_config_get(config, 'format',   app%format_name)
            rc = dm_config_get(config, 'receiver', app%receiver)
            rc = dm_config_get(config, 'type',     app%type_name)
            rc = dm_config_get(config, 'debug',    app%debug)
            rc = dm_config_get(config, 'forward',  app%forward)
            rc = dm_config_get(config, 'verbose',  app%verbose)
            rc = E_NONE
        end if

        call dm_config_close(config)
    end function read_config

    integer function run(app) result(rc)
        !! Reads logs or observations from file/standard input, and then sends
        !! all records to one or more POSIX messages queues.
        type(app_type), intent(inout) :: app !! App type.

        integer           :: file_unit, stat
        integer(kind=i8)  :: nrecords
        logical           :: is_file
        type(log_type)    :: log
        type(observ_type) :: observ
        type(mqueue_type) :: mqueue

        nrecords  = 0
        file_unit = stdin
        is_file   = .false.

        if (len_trim(app%input) > 0 .and. app%input /= '-') is_file = .true.
        call logger%info('started ' // dm_version_to_string(APP_NAME, APP_MAJOR, APP_MINOR, APP_PATCH))

        ! Open message queue of receiver for writing.
        if (.not. app%forward) then
            call logger%debug('opening mqueue /' // app%receiver)
            rc = dm_mqueue_open(mqueue   = mqueue, &
                                type     = app%type, &
                                name     = app%receiver, &
                                access   = MQUEUE_WRONLY, &
                                blocking = .true.)

            if (dm_is_error(rc)) then
                call logger%error('failed to open mqueue /' // app%receiver, error=rc)
                return
            end if
        end if

        read_block: block
            if (is_file) then
                ! Open input file.
                call logger%debug('opening input file ' // app%input)
                open (action='read', file=trim(app%input), iostat=stat, newunit=file_unit, status='old')

                if (stat /= 0) then
                    rc = E_IO
                    call logger%error('failed to open input file ' // app%input, error=rc)
                    exit read_block
                end if
            end if

            ! Read serialised log or observation from file/stdin.
            ipc_loop: do
                ! ******************************************************
                ! OBSERV TYPE.
                ! ******************************************************
                if (app%type == TYPE_OBSERV) then
                   ! Read observation in CSV or Namelist format.
                    select case (app%format)
                        case (FORMAT_CSV)
                            call logger%debug('reading observ in CSV format')
                            rc = dm_csv_read(observ, unit=file_unit)
                        case (FORMAT_NML)
                            call logger%debug('reading observ in NML format')
                            rc = dm_nml_read(observ, unit=file_unit)
                    end select

                    ! End of file reached.
                    if (rc == E_EOF) then
                        rc = E_NONE
                        call logger%debug('end of file reached')
                        exit ipc_loop
                    end if

                    if (dm_is_error(rc)) then
                        call logger%error('failed to read observ', error=rc)
                        exit ipc_loop
                    end if

                    ! Validate input.
                    if (.not. dm_observ_valid(observ)) then
                        call logger%error('invalid input observ ' // observ%id, error=E_INVALID)
                        cycle ipc_loop
                    end if

                    ! Forward observation to next receiver, or send it to message queue.
                    if (app%forward) then
                        rc = dm_mqueue_forward(observ, name=app%name, blocking=APP_MQ_BLOCKING)
                    else
                        rc = dm_mqueue_write(mqueue, observ)
                    end if
                ! ******************************************************
                ! LOG TYPE.
                ! ******************************************************
                else if (app%type == TYPE_LOG) then
                    ! Read log in CSV or Namelist format.
                    select case (app%format)
                        case (FORMAT_CSV)
                            call logger%debug('reading log in CSV format')
                            rc = dm_csv_read(log, unit=file_unit)
                        case (FORMAT_NML)
                            call logger%debug('reading log in NML format')
                            rc = dm_nml_read(log, unit=file_unit)
                    end select

                    ! End of file reached.
                    if (rc == E_EOF) then
                        rc = E_NONE
                        call logger%debug('end of file reached')
                        exit ipc_loop
                    end if

                    if (dm_is_error(rc)) then
                        call logger%error('failed to read log', error=rc)
                        exit ipc_loop
                    end if

                    ! Validate input.
                    if (.not. dm_log_valid(log)) then
                        call logger%error('invalid input log ' // log%id, error=E_INVALID)
                        cycle ipc_loop
                    end if

                    ! Send log to message queue.
                    rc = dm_mqueue_write(mqueue, log)
                end if

                ! Handle message queue error.
                if (dm_is_error(rc)) then
                    call logger%error('failed to write to mqueue', error=rc)
                    call dm_sleep(1)
                    cycle ipc_loop
                end if

                nrecords = max(0_i8, nrecords + 1)
            end do ipc_loop

            if (is_file) then
                call logger%debug('closing input file ' // app%input)
                close (file_unit)
            end if
        end block read_block

        ! Close message queue.
        if (.not. app%forward) then
            call logger%debug('closing mqueue /' // app%receiver)
            stat = dm_mqueue_close(mqueue)

            if (dm_is_error(stat)) then
                call logger%warning('failed to close mqueue /' // app%receiver, error=stat)
            end if

            rc = max(rc, stat)
        end if

        call logger%debug('finished transmission of ' // dm_itoa(nrecords) // ' records')
    end function run
end program dmsend
