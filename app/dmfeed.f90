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

    integer, parameter :: APP_MAX_NENTRIES = 500 !! Maximum number of feed entries.

    type :: app_type
        !! Application settings.
        character(len=APP_NAME_LEN)  :: name     = APP_NAME     !! Name of instance/configuration.
        character(len=FILE_PATH_LEN) :: config   = ' '          !! Path to config file.
        character(len=FILE_PATH_LEN) :: database = ' '          !! Path to log database.
        character(len=FILE_PATH_LEN) :: output   = ' '          !! Output path of Atom file (stdout if empty).
        character(len=NODE_ID_LEN)   :: node     = ' '          !! Node id.
        integer                      :: minlevel = LOG_DEBUG    !! Minimum log level
        integer                      :: maxlevel = LOG_CRITICAL !! Maximum log level.
        integer                      :: nentries = 50           !! Max. number of entries in feed.
        type(atom_type)              :: atom                    !! Atom type.
    end type app_type

    integer        :: rc  ! Return code.
    type(app_type) :: app ! App configuration.

    ! Initialise DMPACK.
    call dm_init()

    ! Read command-line arguments and configuration from file.
    rc = read_args(app)
    if (dm_is_error(rc)) call dm_stop(1)

    ! Write Atom XML feed to file or standard output.
    call create_feed(app, rc)
    if (dm_is_error(rc)) call dm_stop(1)
contains
    integer function read_args(app) result(rc)
        !! Reads command-line arguments and configuration
        !! from file (if `--config` is passed).
        type(app_type), intent(inout) :: app !! App type.
        type(arg_type)                :: args(15)

        rc = E_NONE

        args = [ &
            arg_type('name',     short='n', type=ARG_TYPE_ID),      & ! -n, --name <string>
            arg_type('config',   short='c', type=ARG_TYPE_FILE),    & ! -c, --config <path>
            arg_type('database', short='d', type=ARG_TYPE_DB),      & ! -d, --database <path>
            arg_type('output',   short='o', type=ARG_TYPE_CHAR),    & ! -o, --output <path>
            arg_type('node',     short='N', type=ARG_TYPE_ID),      & ! -N, --node <string>
            arg_type('minlevel', short='L', type=ARG_TYPE_INTEGER), & ! -L, --minlevel <n>
            arg_type('maxlevel', short='K', type=ARG_TYPE_INTEGER), & ! -K, --maxlevel <n>
            arg_type('nentries', short='E', type=ARG_TYPE_INTEGER), & ! -E, --nentries <n>
            arg_type('author',   short='A', type=ARG_TYPE_CHAR),    & ! -A, --author <string>
            arg_type('email',    short='M', type=ARG_TYPE_CHAR),    & ! -M, --email <string>
            arg_type('id',       short='I', type=ARG_TYPE_CHAR),    & ! -I, --id <string>
            arg_type('title',    short='C', type=ARG_TYPE_CHAR),    & ! -C, --title <string>
            arg_type('subtitle', short='G', type=ARG_TYPE_CHAR),    & ! -G, --subtitle <string>
            arg_type('url',      short='U', type=ARG_TYPE_CHAR),    & ! -U, --url <string>
            arg_type('xsl',      short='X', type=ARG_TYPE_CHAR)     & ! -X, --xsl <string>
        ]

        ! Read all command-line arguments.
        rc = dm_arg_read(args, APP_NAME, APP_MAJOR, APP_MINOR)
        if (dm_is_error(rc)) return

        rc = dm_arg_get(args(1), app%name)
        rc = dm_arg_get(args(2), app%config)

        ! Read configuration from file.
        rc = read_config(app)
        if (dm_is_error(rc)) return

        ! Get all other arguments.
        rc = dm_arg_get(args( 3), app%database)
        rc = dm_arg_get(args( 4), app%output)
        rc = dm_arg_get(args( 5), app%node)
        rc = dm_arg_get(args( 6), app%minlevel)
        rc = dm_arg_get(args( 7), app%maxlevel)
        rc = dm_arg_get(args( 8), app%nentries)
        rc = dm_arg_get(args( 9), app%atom%author)
        rc = dm_arg_get(args(10), app%atom%email)
        rc = dm_arg_get(args(11), app%atom%id)
        rc = dm_arg_get(args(12), app%atom%title)
        rc = dm_arg_get(args(13), app%atom%subtitle)
        rc = dm_arg_get(args(14), app%atom%url)
        rc = dm_arg_get(args(15), app%atom%xsl)

        ! Validate passed options.
        rc = E_INVALID

        if (len_trim(app%node) > 0 .and. .not. dm_id_valid(app%node)) then
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

        if (app%minlevel < LOG_DEBUG .or. app%minlevel > LOG_CRITICAL) then
            call dm_error_out(rc, 'invalid minimum log level')
            return
        end if

        if (app%maxlevel < LOG_DEBUG .or. app%maxlevel > LOG_CRITICAL) then
            call dm_error_out(rc, 'invalid maximum log level')
            return
        end if

        if (app%maxlevel < app%minlevel) then
            call dm_error_out(rc, 'maximum level must be greater than minimum level')
            return
        end if

        if (app%nentries < 1 .or. app%nentries > APP_MAX_NENTRIES) then
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
            rc = dm_config_get(config, 'database', app%database)
            rc = dm_config_get(config, 'output',   app%output)
            rc = dm_config_get(config, 'node',     app%node)
            rc = dm_config_get(config, 'minlevel', app%minlevel)
            rc = dm_config_get(config, 'maxlevel', app%maxlevel)
            rc = dm_config_get(config, 'nentries', app%nentries)
            rc = dm_config_get(config, 'author',   app%atom%author)
            rc = dm_config_get(config, 'email',    app%atom%email)
            rc = dm_config_get(config, 'id',       app%atom%id)
            rc = dm_config_get(config, 'title',    app%atom%title)
            rc = dm_config_get(config, 'subtitle', app%atom%subtitle)
            rc = dm_config_get(config, 'url',      app%atom%url)
            rc = dm_config_get(config, 'xsl',      app%atom%xsl)
            rc = E_NONE
        end if

        call dm_config_close(config)
    end function read_config

    subroutine create_feed(app, stat)
        !! Creates Atom XML feed from logs in database.
        type(app_type), intent(inout)         :: app  !! App type.
        integer,        intent(out), optional :: stat !! Status.

        character(len=:), allocatable :: xml
        integer                       :: rc
        type(db_type)                 :: db
        type(log_type), allocatable   :: logs(:)

        ! Connect to database.
        rc = dm_db_open(db, app%database, timeout=DB_TIMEOUT_DEFAULT)

        if (dm_is_error(rc)) then
            call dm_error_out(rc, 'failed to open database')
            return
        end if

        feed_block: block
            ! Get logs from database.
            if (len_trim(app%node) > 0) then
                rc = dm_db_select(db        = db, &
                                  logs      = logs, &
                                  node_id   = app%node, &
                                  min_level = app%minlevel, &
                                  max_level = app%maxlevel, &
                                  desc      = .true., &
                                  limit     = int(app%nentries, kind=i8))
            else
                rc = dm_db_select(db        = db, &
                                  logs      = logs, &
                                  min_level = app%minlevel, &
                                  max_level = app%maxlevel, &
                                  desc      = .true., &
                                  limit     = int(app%nentries, kind=i8))
            end if

            if (dm_is_error(rc) .and. rc /= E_DB_NO_ROWS) then
                call dm_error_out(rc, 'database error')
                exit feed_block
            end if

            ! Create Atom XML string.
            call dm_atom_from_logs(app%atom, logs, xml)

            ! Write to file.
            if (len_trim(app%output) > 0 .and. app%output /= '-') then
                call dm_file_write(app%output, xml, raw=.true., error=rc)
                if (dm_is_error(rc)) call dm_error_out(rc, 'failed to write to file ' // app%output)
                exit feed_block
            end if

            ! Write to standard output.
            print '(a)', xml
            rc = E_NONE
        end block feed_block

        if (present(stat)) stat = rc
        rc = dm_db_close(db)
    end subroutine create_feed
end program dmfeed
