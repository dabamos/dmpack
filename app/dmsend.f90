! dmsend.f90
!
! Author:  Philipp Engel
! Licence: ISC
program dmsend
    !! Reads logs or observations in CSV or Fortran 95 Namelist format from
    !! file or standard input, then sends all records to a POSIX message queue.
    use :: dmpack, dm_log => dm_logger_log
    implicit none (type, external)

    character(len=*), parameter :: APP_NAME  = 'dmsend'
    integer,          parameter :: APP_MAJOR = 0
    integer,          parameter :: APP_MINOR = 9
    integer,          parameter :: APP_PATCH = 0

    logical,   parameter :: APP_MQ_BLOCKING   = .true. !! Observation forwarding is blocking.

    type :: app_type
        !! Application settings.
        character(len=LOGGER_NAME_LEN)     :: name        = APP_NAME    !! Name of process and POSIX message queue.
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

    integer        :: rc
    type(app_type) :: app

    ! Initialise DMPACK.
    call dm_init()

    ! Read command-line arguments and options from configuration file.
    rc = read_args(app)
    if (dm_is_error(rc)) call dm_stop(1)

    ! Initialise logger.
    call dm_logger_init(name    = app%logger, &
                        node_id = app%node, &
                        source  = app%name, &
                        debug   = app%debug, &
                        ipc     = (len_trim(app%logger) > 0), &
                        verbose = app%verbose)

    ! Read and send data.
    rc = run(app)
    if (dm_is_error(rc)) call dm_stop(1)
contains
    integer function forward_observ(observ, name) result(rc)
        !! Forwards given observation to next receiver.
        type(observ_type), intent(inout)        :: observ !! Observation to forward.
        character(len=*),  intent(in), optional :: name   !! App name.

        integer           :: next
        type(mqueue_type) :: mqueue

        rc   = E_NONE
        next = observ%next

        do
            ! Increase the receiver index.
            next = max(0, next) + 1

            ! End of receiver list reached?
            if (next > observ%nreceivers) then
                call dm_log(LOG_DEBUG, 'no receivers left in observ ' // observ%name, observ=observ)
                return
            end if

            ! Invalid receiver name?
            if (.not. dm_id_valid(observ%receivers(next))) then
                rc = E_INVALID
                call dm_log(LOG_ERROR, 'invalid receiver ' // trim(observ%receivers(next)) // &
                            ' in observ ' // observ%name, observ=observ, error=rc)
                return
            end if

            ! Cycle to next + 1 if receiver name equals app name. We don't want
            ! to send the observation to this program instance.
            if (.not. present(name)) exit
            if (observ%receivers(next) /= name) exit
            call dm_log(LOG_DEBUG, 'skipping receiver ' // dm_itoa(next) // ' (' // &
                        trim(observ%receivers(next)) // ') of observ ' // observ%name)
        end do

        mqueue_block: block
            ! Open message queue of receiver for writing.
            rc = dm_mqueue_open(mqueue   = mqueue, &
                                type     = TYPE_OBSERV, &
                                name     = observ%receivers(next), &
                                access   = MQUEUE_WRONLY, &
                                blocking = APP_MQ_BLOCKING)

            if (dm_is_error(rc)) then
                call dm_log(LOG_ERROR, 'failed to open mqueue /' // observ%receivers(next), &
                            observ=observ, error=rc)
                exit mqueue_block
            end if

            ! Send observation to message queue.
            observ%next = next
            rc = dm_mqueue_write(mqueue, observ)

            if (dm_is_error(rc)) then
                call dm_log(LOG_ERROR, 'failed to send observ ' // trim(observ%name) // &
                            ' to mqueue /' // observ%receivers(next), observ=observ, error=rc)
                exit mqueue_block
            end if

            call dm_log(LOG_DEBUG, 'sent observ ' // trim(observ%name) // ' to mqueue /' // &
                        observ%receivers(next), observ=observ)
        end block mqueue_block

        ! Close message queue.
        rc = dm_mqueue_close(mqueue)

        if (dm_is_error(rc)) then
            call dm_log(LOG_WARNING, 'failed to close mqueue /' // observ%receivers(next), &
                        observ=observ, error=rc)
        end if
    end function forward_observ

    integer function read_args(app) result(rc)
        !! Reads command-line arguments and settings from configuration file.
        type(app_type), intent(inout) :: app
        type(arg_type)                :: args(11)

        rc = E_NONE

        ! Required and optional command-line arguments.
        args = [ &
            arg_type('name',     short='n', type=ARG_TYPE_ID),   & ! -n, --name <string>
            arg_type('config',   short='c', type=ARG_TYPE_FILE), & ! -c, --config <path>
            arg_type('logger',   short='l', type=ARG_TYPE_ID),   & ! -l, --logger <string>
            arg_type('node',     short='N', type=ARG_TYPE_ID),   & ! -N, --node <string>
            arg_type('input',    short='i', type=ARG_TYPE_CHAR), & ! -i, --input <path>
            arg_type('format',   short='f', type=ARG_TYPE_CHAR), & ! -f, --format <string>
            arg_type('type',     short='t', type=ARG_TYPE_CHAR), & ! -t, --type <string>
            arg_type('receiver', short='r', type=ARG_TYPE_ID, max_len=OBSERV_RECEIVER_LEN), & ! -r, --receiver <string>
            arg_type('debug',    short='D', type=ARG_TYPE_BOOL), & ! -D, --debug
            arg_type('forward',  short='F', type=ARG_TYPE_BOOL), & ! -F, --forward
            arg_type('verbose',  short='V', type=ARG_TYPE_BOOL)  & ! -V, --verbose
        ]

        ! Read all command-line arguments.
        rc = dm_arg_read(args, APP_NAME, APP_MAJOR, APP_MINOR, APP_PATCH)
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

        call dm_log(LOG_INFO, 'started ' // app%name)

        ! Open message queue of receiver for writing.
        if (.not. app%forward) then
            call dm_log(LOG_DEBUG, 'opening mqueue /' // app%receiver)
            rc = dm_mqueue_open(mqueue   = mqueue, &
                                type     = app%type, &
                                name     = app%receiver, &
                                access   = MQUEUE_WRONLY, &
                                blocking = .true.)

            if (dm_is_error(rc)) then
                call dm_log(LOG_ERROR, 'failed to open mqueue /' // app%receiver, error=rc)
                return
            end if
        end if

        read_block: block
            if (is_file) then
                ! Open input file.
                call dm_log(LOG_DEBUG, 'opening input file ' // app%input)
                open (action='read', file=trim(app%input), iostat=stat, newunit=file_unit, status='old')

                if (stat /= 0) then
                    rc = E_IO
                    call dm_log(LOG_ERROR, 'failed to open input file ' // app%input, error=rc)
                    exit read_block
                end if
            end if

            ! Read serialised log or observation from file/stdout.
            ipc_loop: do
                ! ******************************************************
                ! OBSERV TYPE.
                ! ******************************************************
                if (app%type == TYPE_OBSERV) then
                   ! Read observation in CSV or Namelist format.
                    select case (app%format)
                        case (FORMAT_CSV)
                            call dm_log(LOG_DEBUG, 'reading observ in CSV format')
                            rc = dm_csv_read(observ, unit=file_unit)
                        case (FORMAT_NML)
                            call dm_log(LOG_DEBUG, 'reading observ in NML format')
                            rc = dm_nml_read(observ, unit=file_unit)
                    end select

                    ! End of file reached.
                    if (rc == E_EOF) then
                        rc = E_NONE
                        call dm_log(LOG_DEBUG, 'end of file reached')
                        exit ipc_loop
                    end if

                    if (dm_is_error(rc)) then
                        call dm_log(LOG_ERROR, 'failed to read observ', error=rc)
                        exit ipc_loop
                    end if

                    ! Validate input.
                    if (.not. dm_observ_valid(observ)) then
                        call dm_log(LOG_ERROR, 'invalid input observ ' // observ%id, error=E_INVALID)
                        cycle ipc_loop
                    end if

                    ! Forward observation to next receiver, or send it to message queue.
                    if (app%forward) then
                        rc = forward_observ(observ, app%name)
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
                            call dm_log(LOG_DEBUG, 'reading log in CSV format')
                            rc = dm_csv_read(log, unit=file_unit)
                        case (FORMAT_NML)
                            call dm_log(LOG_DEBUG, 'reading log in NML format')
                            rc = dm_nml_read(log, unit=file_unit)
                    end select

                    ! End of file reached.
                    if (rc == E_EOF) then
                        rc = E_NONE
                        call dm_log(LOG_DEBUG, 'end of file reached')
                        exit ipc_loop
                    end if

                    if (dm_is_error(rc)) then
                        call dm_log(LOG_ERROR, 'failed to read log', error=rc)
                        exit ipc_loop
                    end if

                    ! Validate input.
                    if (.not. dm_log_valid(log)) then
                        call dm_log(LOG_ERROR, 'invalid input log ' // log%id, error=E_INVALID)
                        cycle ipc_loop
                    end if

                    ! Send log to message queue.
                    rc = dm_mqueue_write(mqueue, log)
                end if

                ! Handle message queue error.
                if (dm_is_error(rc)) then
                    call dm_log(LOG_ERROR, 'failed to write to mqueue', error=rc)
                    call dm_sleep(1)
                    cycle ipc_loop
                end if

                nrecords = max(0_i8, nrecords + 1)
            end do ipc_loop

            if (is_file) then
                call dm_log(LOG_DEBUG, 'closing input file ' // app%input)
                close (file_unit)
            end if
        end block read_block

        ! Close message queue.
        if (.not. app%forward) then
            call dm_log(LOG_DEBUG, 'closing mqueue /' // app%receiver)
            stat = dm_mqueue_close(mqueue)

            if (dm_is_error(stat)) then
                call dm_log(LOG_WARNING, 'failed to close mqueue /' // app%receiver, error=stat)
            end if

            rc = max(rc, stat)
        end if

        call dm_log(LOG_DEBUG, 'finished transmission of ' // dm_itoa(nrecords) // ' records')
    end function run
end program dmsend
