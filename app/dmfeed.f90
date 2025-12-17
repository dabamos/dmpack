! dmfeed.f90
!
! Author:  Philipp Engel
! Licence: ISC
program dmfeed
    !! This program creates a web feed from log messages in Atom
    !! Syndication Format.
    use :: dmpack
    implicit none (type, external)

    character(*), parameter :: APP_NAME  = 'dmfeed'
    integer,      parameter :: APP_MAJOR = 0
    integer,      parameter :: APP_MINOR = 9
    integer,      parameter :: APP_PATCH = 9

    integer, parameter :: APP_MAX_ENTRIES = 500 !! Maximum number of feed entries.

    type :: app_type
        !! Application settings.
        character(ID_LEN)        :: name      = APP_NAME    !! Name of instance/configuration.
        character(FILE_PATH_LEN) :: config    = ' '         !! Path to config file.
        character(FILE_PATH_LEN) :: database  = ' '         !! Path to log database.
        character(FILE_PATH_LEN) :: output    = ' '         !! Output path of Atom file (stdout if empty).
        character(NODE_ID_LEN)   :: node_id   = ' '         !! Optional node id.
        integer                  :: entries   = 50          !! Max. number of entries in feed.
        integer                  :: min_level = LL_DEBUG    !! Minimum log level
        integer                  :: max_level = LL_CRITICAL !! Maximum log level.
        logical                  :: force     = .false.     !! Force writing of output file.
        type(atom_type)          :: atom                    !! Atom type.
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
    logical function is_stale_file(path, time) result(is)
        !! Returns `.true.` if last modification time of file at `path` is
        !! before `time`.
        character(*),        intent(in) :: path !! Path to file.
        character(TIME_LEN), intent(in) :: time !! ISO 8601 time stamp of last record.

        integer(i8)            :: epoch
        type(file_status_type) :: file_status

        is = .true.

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

        is = .false.
    end function is_stale_file

    subroutine create_feed(app, error)
        !! Creates Atom XML feed from logs in database.
        type(app_type), intent(inout)         :: app   !! App type.
        integer,        intent(out), optional :: error !! Error code.

        integer                     :: rc
        logical                     :: is_file
        type(db_type)               :: db
        type(log_type), allocatable :: logs(:)
        character(:),   allocatable :: xml

        is_file = (dm_string_has(app%output) .and. app%output /= '-')

        feed_block: block
            ! Connect to database.
            rc = dm_db_open(db, app%database, timeout=DB_TIMEOUT_DEFAULT)

            if (dm_is_error(rc)) then
                call dm_error_out(rc, 'failed to open database')
                exit feed_block
            end if

            ! Get logs from database.
            rc = dm_db_select_logs(db        = db, &
                                   logs      = logs, &
                                   node_id   = app%node_id, &
                                   min_level = app%min_level, &
                                   max_level = app%max_level, &
                                   desc      = .true., &
                                   limit     = int(app%entries, i8))

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
        call dm_db_close(db)
    end subroutine create_feed

    ! **************************************************************************
    ! COMMAND-LINE ARGUMENTS AND CONFIGURATION FILE.
    ! **************************************************************************
    integer function read_args(app) result(rc)
        !! Reads command-line arguments and configuration from file (if
        !! `--config` is passed).
        type(app_type), intent(out) :: app !! App type.

        type(arg_class) :: arg

        call arg%create()
        call arg%add('name',     short='n', type=ARG_TYPE_ID)       ! -n, --name <id>
        call arg%add('config',   short='c', type=ARG_TYPE_FILE)     ! -c, --config <path>
        call arg%add('database', short='d', type=ARG_TYPE_DATABASE) ! -d, --database <path>
        call arg%add('output',   short='o', type=ARG_TYPE_STRING)   ! -o, --output <path>
        call arg%add('node',     short='N', type=ARG_TYPE_ID)       ! -N, --node <id>
        call arg%add('entries',  short='E', type=ARG_TYPE_INTEGER)  ! -E, --entries <n>
        call arg%add('minlevel', short='L', type=ARG_TYPE_LEVEL)    ! -L, --minlevel <n>
        call arg%add('maxlevel', short='K', type=ARG_TYPE_LEVEL)    ! -K, --maxlevel <n>
        call arg%add('force',    short='F', type=ARG_TYPE_LOGICAL)  ! -F, --force
        call arg%add('author',   short='A', type=ARG_TYPE_STRING)   ! -A, --author <string>
        call arg%add('email',    short='M', type=ARG_TYPE_STRING)   ! -M, --email <string>
        call arg%add('id',       short='I', type=ARG_TYPE_STRING)   ! -I, --id <string>
        call arg%add('title',    short='C', type=ARG_TYPE_STRING)   ! -C, --title <string>
        call arg%add('subtitle', short='G', type=ARG_TYPE_STRING)   ! -G, --subtitle <string>
        call arg%add('url',      short='U', type=ARG_TYPE_STRING)   ! -U, --url <string>
        call arg%add('xsl',      short='X', type=ARG_TYPE_STRING)   ! -X, --xsl <string>

        ! Read all command-line arguments.
        rc = arg%read(version_callback)
        if (dm_is_error(rc)) return

        call arg%get('name',   app%name)
        call arg%get('config', app%config)

        ! Read configuration from file.
        rc = read_config(app)
        if (dm_is_error(rc)) return

        ! Get all other arguments.
        call arg%get('database', app%database)
        call arg%get('output',   app%output)
        call arg%get('node',     app%node_id)
        call arg%get('entries',  app%entries)
        call arg%get('minlevel', app%min_level)
        call arg%get('maxlevel', app%max_level)
        call arg%get('force',    app%force)
        call arg%get('author',   app%atom%author)
        call arg%get('email',    app%atom%email)
        call arg%get('id',       app%atom%id)
        call arg%get('title',    app%atom%title)
        call arg%get('subtitle', app%atom%subtitle)
        call arg%get('url',      app%atom%url)
        call arg%get('xsl',      app%atom%xsl)
        call arg%destroy()

        ! Validate passed options.
        rc = validate(app)
    end function read_args

    integer function read_config(app) result(rc)
        !! Reads configuration from (Lua) file.
        type(app_type), intent(inout) :: app !! App type.

        type(config_class) :: config

        rc = E_NONE
        if (.not. dm_string_has(app%config)) return

        rc = config%open(app%config, app%name)

        if (dm_is_ok(rc)) then
            call config%get('database', app%database)
            call config%get('output',   app%output)
            call config%get('node',     app%node_id)
            call config%get('entries',  app%entries)
            call config%get('minlevel', app%min_level)
            call config%get('maxlevel', app%max_level)
            call config%get('force',    app%force)
            call config%get('author',   app%atom%author)
            call config%get('email',    app%atom%email)
            call config%get('id',       app%atom%id)
            call config%get('title',    app%atom%title)
            call config%get('subtitle', app%atom%subtitle)
            call config%get('url',      app%atom%url)
            call config%get('xsl',      app%atom%xsl)
        end if

        call config%close()
    end function read_config

    integer function validate(app) result(rc)
        !! Validates options and prints error messages.
        type(app_type), intent(inout) :: app !! App type.

        rc = E_INVALID

        if (dm_string_has(app%node_id) .and. .not. dm_id_is_valid(app%node_id)) then
            call dm_error_out(rc, 'invalid node id')
            return
        end if

        if (.not. dm_string_has(app%database)) then
            call dm_error_out(rc, 'invalid or missing database')
            return
        end if

        if (.not. dm_file_exists(app%database)) then
            call dm_error_out(rc, 'database does not exist')
            return
        end if

        if (.not. dm_log_level_is_valid(app%min_level)) then
            call dm_error_out(rc, 'invalid minimum log level')
            return
        end if

        if (.not. dm_log_level_is_valid(app%max_level)) then
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
    end function validate

    ! **************************************************************************
    ! CALLBACKS.
    ! **************************************************************************
    subroutine version_callback()
        call dm_version_out(APP_NAME, APP_MAJOR, APP_MINOR, APP_PATCH)
        print '(a, 1x, a)', dm_lua_version(.true.), dm_db_version(.true.)
    end subroutine version_callback
end program dmfeed
