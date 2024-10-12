! dmfeed.f90
!
! Author:  Philipp Engel
! Licence: ISC
program dmfeed
    !! This program creates a web feed from log messages in Atom
    !! Syndication Format.
    use :: dmpack
    implicit none (type, external)

    character(len=*), parameter :: APP_NAME  = 'dmfeed'
    integer,          parameter :: APP_MAJOR = 0
    integer,          parameter :: APP_MINOR = 9
    integer,          parameter :: APP_PATCH = 4

    integer, parameter :: APP_MAX_ENTRIES = 500 !! Maximum number of feed entries.

    type :: app_type
        !! Application settings.
        character(len=ID_LEN)        :: name      = APP_NAME    !! Name of instance/configuration.
        character(len=FILE_PATH_LEN) :: config    = ' '         !! Path to config file.
        character(len=FILE_PATH_LEN) :: database  = ' '         !! Path to log database.
        character(len=FILE_PATH_LEN) :: output    = ' '         !! Output path of Atom file (stdout if empty).
        character(len=NODE_ID_LEN)   :: node      = ' '         !! Optional node id.
        integer                      :: entries   = 50          !! Max. number of entries in feed.
        integer                      :: min_level = LL_DEBUG    !! Minimum log level
        integer                      :: max_level = LL_CRITICAL !! Maximum log level.
        logical                      :: force     = .false.     !! Force writing of output file.
        type(atom_type)              :: atom                    !! Atom type.
    end type app_type

    integer        :: rc  ! Return code.
    type(app_type) :: app ! App settings.

    ! Initialise DMPACK.
    call dm_init()

    ! Read command-line arguments and configuration from file.
    rc = read_args(app)
    if (dm_is_error(rc)) call dm_stop(STOP_FAILURE)

    ! Write Atom XML feed to file or standard output.
    call create_feed(app, rc)
    if (dm_is_error(rc)) call dm_stop(STOP_FAILURE)
contains
    logical function is_stale_file(path, time) result(is_stale)
        !! Returns `.true.` if last modification time of file at `path` is
        !! before `time`.
        character(len=*),        intent(in) :: path !! Path to file.
        character(len=TIME_LEN), intent(in) :: time !! ISO 8601 time stamp of last record.

        integer(kind=i8)       :: epoch
        type(file_status_type) :: file_status

        is_stale = .true.

        ! File is stale if it does not exist yet.
        if (.not. dm_file_exists(path)) return

        ! Read file status to get last modification time.
        rc = dm_file_status(app%output, file_status)
        if (dm_is_error(rc)) return
        if (file_status%type /= FILE_TYPE_FILE) return

        ! Convert ISO 8601 to Unix epoch to normalise the GMT offset.
        rc = dm_time_to_unix(time, epoch)
        if (dm_is_error(rc)) return

        ! Last file modification time is older than given time stamp?
        if (file_status%m_time < epoch) return

        is_stale = .false.
    end function is_stale_file

    integer function read_args(app) result(rc)
        !! Reads command-line arguments and configuration from file (if
        !! `--config` is passed).
        type(app_type), intent(out) :: app !! App type.

        character(len=:), allocatable :: version
        type(arg_type)                :: args(16)

        args = [ &
            arg_type('name',     short='n', type=ARG_TYPE_ID),       & ! -n, --name <id>
            arg_type('config',   short='c', type=ARG_TYPE_FILE),     & ! -c, --config <path>
            arg_type('database', short='d', type=ARG_TYPE_DATABASE), & ! -d, --database <path>
            arg_type('output',   short='o', type=ARG_TYPE_STRING),   & ! -o, --output <path>
            arg_type('node',     short='N', type=ARG_TYPE_ID),       & ! -N, --node <id>
            arg_type('entries',  short='E', type=ARG_TYPE_INTEGER),  & ! -E, --entries <n>
            arg_type('minlevel', short='L', type=ARG_TYPE_LEVEL),    & ! -L, --minlevel <n>
            arg_type('maxlevel', short='K', type=ARG_TYPE_LEVEL),    & ! -K, --maxlevel <n>
            arg_type('force',    short='F', type=ARG_TYPE_LOGICAL),  & ! -F, --force
            arg_type('author',   short='A', type=ARG_TYPE_STRING),   & ! -A, --author <string>
            arg_type('email',    short='M', type=ARG_TYPE_STRING),   & ! -M, --email <string>
            arg_type('id',       short='I', type=ARG_TYPE_STRING),   & ! -I, --id <string>
            arg_type('title',    short='C', type=ARG_TYPE_STRING),   & ! -C, --title <string>
            arg_type('subtitle', short='G', type=ARG_TYPE_STRING),   & ! -G, --subtitle <string>
            arg_type('url',      short='U', type=ARG_TYPE_STRING),   & ! -U, --url <string>
            arg_type('xsl',      short='X', type=ARG_TYPE_STRING)    & ! -X, --xsl <string>
        ]

        ! Read all command-line arguments.
        version = dm_lua_version(.true.) // ' ' // dm_db_version(.true.)
        rc = dm_arg_read(args, APP_NAME, APP_MAJOR, APP_MINOR, APP_PATCH, version)
        if (dm_is_error(rc)) return

        call dm_arg_get(args(1), app%name)
        call dm_arg_get(args(2), app%config)

        ! Read configuration from file.
        rc = read_config(app)
        if (dm_is_error(rc)) return

        ! Get all other arguments.
        call dm_arg_get(args( 3), app%database)
        call dm_arg_get(args( 4), app%output)
        call dm_arg_get(args( 5), app%node)
        call dm_arg_get(args( 6), app%entries)
        call dm_arg_get(args( 7), app%min_level)
        call dm_arg_get(args( 8), app%max_level)
        call dm_arg_get(args( 9), app%force)
        call dm_arg_get(args(10), app%atom%author)
        call dm_arg_get(args(11), app%atom%email)
        call dm_arg_get(args(12), app%atom%id)
        call dm_arg_get(args(13), app%atom%title)
        call dm_arg_get(args(14), app%atom%subtitle)
        call dm_arg_get(args(15), app%atom%url)
        call dm_arg_get(args(16), app%atom%xsl)

        ! Validate passed options.
        rc = E_INVALID

        if (len_trim(app%node) > 0 .and. .not. dm_id_is_valid(app%node)) then
            call dm_error_out(rc, 'invalid node id')
            return
        end if

        if (len_trim(app%database) == 0) then
            call dm_error_out(rc, 'invalid or missing database')
            return
        end if

        if (.not. dm_file_exists(app%database)) then
            call dm_error_out(rc, 'database does not exist')
            return
        end if

        if (.not. dm_log_is_valid(app%min_level)) then
            call dm_error_out(rc, 'invalid minimum log level')
            return
        end if

        if (.not. dm_log_is_valid(app%max_level)) then
            call dm_error_out(rc, 'invalid maximum log level')
            return
        end if

        if (app%max_level < app%min_level) then
            call dm_error_out(rc, 'maximum level must be greater than minimum level')
            return
        end if

        if (app%entries < 1 .or. app%entries > APP_MAX_ENTRIES) then
            call dm_error_out(rc, 'invalid number of entries')
            return
        end if

        rc = E_NONE
    end function read_args

    integer function read_config(app) result(rc)
        !! Reads configuration from (Lua) file.
        type(app_type), intent(inout) :: app !! App type.
        type(config_type)             :: config

        rc = E_NONE
        if (len_trim(app%config) == 0) return

        rc = dm_config_open(config, app%config, app%name)

        if (dm_is_ok(rc)) then
            call dm_config_get(config, 'database', app%database)
            call dm_config_get(config, 'output',   app%output)
            call dm_config_get(config, 'node',     app%node)
            call dm_config_get(config, 'entries',  app%entries)
            call dm_config_get(config, 'minlevel', app%min_level)
            call dm_config_get(config, 'maxlevel', app%max_level)
            call dm_config_get(config, 'force',    app%force)
            call dm_config_get(config, 'author',   app%atom%author)
            call dm_config_get(config, 'email',    app%atom%email)
            call dm_config_get(config, 'id',       app%atom%id)
            call dm_config_get(config, 'title',    app%atom%title)
            call dm_config_get(config, 'subtitle', app%atom%subtitle)
            call dm_config_get(config, 'url',      app%atom%url)
            call dm_config_get(config, 'xsl',      app%atom%xsl)
        end if

        call dm_config_close(config)
    end function read_config

    subroutine create_feed(app, error)
        !! Creates Atom XML feed from logs in database.
        type(app_type), intent(inout)         :: app   !! App type.
        integer,        intent(out), optional :: error !! Error code.

        character(len=:), allocatable :: xml
        integer                       :: rc
        logical                       :: is_file
        type(db_type)                 :: db
        type(log_type), allocatable   :: logs(:)

        is_file = (len_trim(app%output) > 0 .and. app%output /= '-')

        feed_block: block
            ! Connect to database.
            rc = dm_db_open(db, app%database, timeout=DB_TIMEOUT_DEFAULT)

            if (dm_is_error(rc)) then
                call dm_error_out(rc, 'failed to open database')
                exit feed_block
            end if

            ! Get logs from database.
            if (len_trim(app%node) > 0) then
                rc = dm_db_select_logs(db        = db, &
                                       logs      = logs, &
                                       node_id   = app%node, &
                                       min_level = app%min_level, &
                                       max_level = app%max_level, &
                                       desc      = .true., &
                                       limit     = int(app%entries, kind=i8))
            else
                rc = dm_db_select_logs(db        = db, &
                                       logs      = logs, &
                                       min_level = app%min_level, &
                                       max_level = app%max_level, &
                                       desc      = .true., &
                                       limit     = int(app%entries, kind=i8))
            end if

            if (dm_is_error(rc) .and. rc /= E_DB_NO_ROWS) then
                call dm_error_out(rc, 'database error')
                exit feed_block
            end if

            ! Time stamp of last log record.
            if (size(logs) > 0) app%atom%updated = logs(1)%timestamp

            if (is_file .and. .not. app%force) then
                ! Write output file only if the time stamp of the last log
                ! record is greater than the file modification time.
                if (.not. is_stale_file(app%output, app%atom%updated)) then
                    ! Nothing to do here.
                    rc = E_NONE
                    exit feed_block
                end if
            end if

            ! Create Atom XML string.
            call dm_atom_from_logs(app%atom, logs, xml)

            if (is_file) then
                ! Write to file.
                call dm_file_write(app%output, xml, raw=.true., error=rc)
                if (dm_is_error(rc)) call dm_error_out(rc, 'failed to write to file ' // app%output)
            else
                ! Write to standard output.
                print '(a)', xml
            end if

            rc = E_NONE
        end block feed_block

        if (present(error)) error = rc
        rc = dm_db_close(db)
    end subroutine create_feed
end program dmfeed
