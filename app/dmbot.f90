! dmbot.f90
!
! Author:  Philipp Engel
! Licence: ISC
program dmbot
    !! This program is an XMPP bot for remote control of sensor nodes.
    use :: dmpack
    implicit none (type, external)

    character(len=*), parameter :: APP_NAME  = 'dmbot'
    integer,          parameter :: APP_MAJOR = 0
    integer,          parameter :: APP_MINOR = 9
    integer,          parameter :: APP_PATCH = 6

    integer, parameter :: HOST_LEN     = 256 !! Max. length of XMPP host.
    integer, parameter :: JID_LEN      = 256 !! Max. length of XMPP jid.
    integer, parameter :: PASSWORD_LEN = 256 !! Max. length of XMPP password.

    type :: app_type
        !! Application settings.
        character(len=ID_LEN)          :: name      = APP_NAME    !! Name of instance/configuration.
        character(len=FILE_PATH_LEN)   :: config    = ' '         !! Path to config file.
        character(len=LOGGER_NAME_LEN) :: logger    = ' '         !! Name of logger.
        character(len=NODE_ID_LEN)     :: node      = ' '         !! Node id.
        character(len=HOST_LEN)        :: host      = ' '         !! IP or FQDN of XMPP server.
        integer                        :: port      = JABBER_PORT !! Port of XMPP server.
        logical                        :: tls       = .true.      !! TLS is mandatory.
        character(len=JID_LEN)         :: jid       = ' '         !! HTTP Basic Auth user name.
        character(len=PASSWORD_LEN)    :: password  = ' '         !! HTTP Basic Auth password.
        logical                        :: debug     = .false.     !! Force writing of output file.
        logical                        :: verbose   = .false.     !! Force writing of output file.
    end type app_type

    integer        :: rc  ! Return code.
    type(app_type) :: app ! App settings.

    ! Initialise DMPACK.
    call dm_init()

    ! Read command-line arguments and configuration from file.
    rc = read_args(app)
    if (dm_is_error(rc)) call dm_stop(STOP_FAILURE)
contains
    integer function read_args(app) result(rc)
        !! Reads command-line arguments and configuration from file (if
        !! `--config` is passed).
        type(app_type), intent(out) :: app !! App type.

        character(len=:), allocatable :: version
        type(arg_type)                :: args(11)

        args = [ &
            arg_type('name',     short='n', type=ARG_TYPE_ID),      & ! -n, --name <id>
            arg_type('config',   short='c', type=ARG_TYPE_FILE),    & ! -c, --config <path>
            arg_type('logger',   short='l', type=ARG_TYPE_ID),      & ! -l, --logger <string>
            arg_type('node',     short='N', type=ARG_TYPE_ID),      & ! -N, --node <id>
            arg_type('host',     short='H', type=ARG_TYPE_STRING),  & ! -H, --host <string>
            arg_type('port',     short='q', type=ARG_TYPE_INTEGER), & ! -q, --port <n>
            arg_type('tls',      short='E', type=ARG_TYPE_LOGICAL), & ! -E, --tls
            arg_type('jid',      short='J', type=ARG_TYPE_STRING),  & ! -J, --jid <string>
            arg_type('password', short='P', type=ARG_TYPE_STRING),  & ! -P, --password <string>
            arg_type('debug',    short='D', type=ARG_TYPE_LOGICAL), & ! -D, --debug
            arg_type('verbose',  short='V', type=ARG_TYPE_LOGICAL)  & ! -V, --verbose
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
        call dm_arg_get(args( 3), app%logger)
        call dm_arg_get(args( 4), app%node)
        call dm_arg_get(args( 5), app%host)
        call dm_arg_get(args( 6), app%port)
        call dm_arg_get(args( 7), app%tls)
        call dm_arg_get(args( 8), app%jid)
        call dm_arg_get(args( 9), app%password)
        call dm_arg_get(args(10), app%debug)
        call dm_arg_get(args(11), app%verbose)

        ! Validate passed options.
        rc = E_INVALID

        if (.not. dm_id_is_valid(app%node)) then
            call dm_error_out(rc, 'invalid node id')
            return
        end if

        if (len_trim(app%host) == 0) then
            call dm_error_out(rc, 'missing host')
            return
        end if

        if (app%port < 0) then
            call dm_error_out(rc, 'invalid port')
            return
        end if

        if (len_trim(app%jid) == 0) then
            call dm_error_out(rc, 'missing jabber id')
            return
        end if

        if (len_trim(app%password) == 0) then
            call dm_error_out(rc, 'missing password')
            return
        end if

        if (app%port == 0) app%port = JABBER_PORT

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
            call dm_config_get(config, 'logger',  app%logger)
            call dm_config_get(config, 'node',    app%node)
            call dm_config_get(config, 'host',    app%host)
            call dm_config_get(config, 'port',    app%port)
            call dm_config_get(config, 'tls',     app%tls)
            call dm_config_get(config, 'jid',     app%jid)
            call dm_config_get(config, 'password',app%password)
            call dm_config_get(config, 'debug',   app%debug)
            call dm_config_get(config, 'verbose', app%verbose)
        end if

        call dm_config_close(config)
    end function read_config
end program dmbot
