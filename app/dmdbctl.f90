! dmdbctl.f90
!
! Author:  Philipp Engel
! Licence: ISC
program dmdbctl
    !! Command-line interface to the DMPACK observation database.
    use :: dmpack
    implicit none (type, external)

    character(len=*), parameter :: APP_NAME  = 'dmdbctl'
    integer,          parameter :: APP_MAJOR = 0
    integer,          parameter :: APP_MINOR = 9
    integer,          parameter :: APP_PATCH = 4

    ! Database operations (CRUD).
    integer, parameter :: OP_NONE   = 0
    integer, parameter :: OP_CREATE = 1
    integer, parameter :: OP_READ   = 2
    integer, parameter :: OP_UPDATE = 3
    integer, parameter :: OP_DELETE = 4
    integer, parameter :: OP_LAST   = 4

    ! Affected data type attributes.
    integer, parameter :: ATTR_NONE  = 0
    integer, parameter :: ATTR_NAME  = 1
    integer, parameter :: ATTR_META  = 2
    integer, parameter :: ATTR_NODE  = 3
    integer, parameter :: ATTR_TYPE  = 4
    integer, parameter :: ATTR_SN    = 5
    integer, parameter :: ATTR_STATE = 6
    integer, parameter :: ATTR_X     = 7
    integer, parameter :: ATTR_Y     = 8
    integer, parameter :: ATTR_Z     = 9
    integer, parameter :: ATTR_LON   = 10
    integer, parameter :: ATTR_LAT   = 11
    integer, parameter :: ATTR_ALT   = 12
    integer, parameter :: ATTR_LAST  = 12

    type :: app_type
        !! Command-line arguments.
        character(len=FILE_PATH_LEN) :: database        = ' '       !! Path to SQLite database file.
        integer                      :: operation       = OP_NONE   !! Database operation (CRUD).
        integer                      :: type            = TYPE_NONE !! Entity type (node, sensor, target).
        logical                      :: mask(ATTR_LAST) = .false.   !! Attribute mask.
        logical                      :: verbose         = .false.   !! Print debug messages to stderr.
        type(node_type)              :: node                        !! Node type.
        type(sensor_type)            :: sensor                      !! Sensor type.
        type(target_type)            :: target                      !! Target type.
    end type app_type

    integer        :: rc  ! Return code.
    type(app_type) :: app ! App settings.

    ! Initialise DMPACK.
    call dm_init()

    ! Get command-line arguments.
    rc = read_args(app)
    if (dm_is_error(rc)) call dm_stop(STOP_FAILURE)

    ! Run database operation.
    rc = crud(app)
    if (dm_is_error(rc)) call dm_stop(STOP_FAILURE)
contains
    integer function crud(app) result(rc)
        !! Performs database operation.
        type(app_type), intent(inout) :: app !! App settings.
        type(db_type)                 :: db  !! Database type.

        ! Open database.
        rc = dm_db_open(db, path=app%database)

        if (dm_is_error(rc)) then
            call dm_error_out(rc, 'failed to open database ' // app%database)
            return
        end if

        ! Perform database operation.
        select case (app%operation)
            case (OP_CREATE); rc = db_create(db, app)
            case (OP_READ);   rc = db_read  (db, app)
            case (OP_UPDATE); rc = db_update(db, app)
            case (OP_DELETE); rc = db_delete(db, app)
            case default;     rc = E_INVALID
        end select

        ! Close database.
        if (dm_is_error(dm_db_close(db))) rc = E_DB
    end function crud

    integer function db_create(db, app) result(rc)
        !! Creates database record.
        type(db_type),  intent(inout) :: db  !! Database type.
        type(app_type), intent(inout) :: app !! App type.

        character(len=ID_LEN) :: id

        rc = E_NONE

        select case (app%type)
            case (TYPE_NODE)
                ! Create node.
                id = app%node%id

                if (dm_db_has_node(db, id)) then
                    rc = E_EXIST
                    call dm_error_out(rc, 'node ' // trim(id) // ' exists')
                    return
                end if

                rc = dm_db_insert(db, app%node)
                call dm_error_out(rc)

            case (TYPE_SENSOR)
                ! Create sensor.
                id = app%sensor%id

                if (dm_db_has_sensor(db, id)) then
                    rc = E_EXIST
                    call dm_error_out(rc, 'sensor ' // trim(id) // ' exists')
                    return
                end if

                if (.not. dm_db_has_node(db, app%sensor%node_id)) then
                    rc = E_INVALID
                    call dm_error_out(rc, 'node ' // trim(app%sensor%node_id) // ' not found')
                    return
                end if

                rc = dm_db_insert(db, app%sensor)

            case (TYPE_TARGET)
                ! Create target.
                id = app%target%id

                if (dm_db_has_target(db, id)) then
                    rc = E_EXIST
                    call dm_error_out(rc, 'target' // trim(id) // ' exists')
                    return
                end if

                rc = dm_db_insert(db, app%target)
        end select

        if (dm_is_ok(rc) .and. app%verbose) then
            print '(a, " added to database ", a)', trim(TYPE_NAMES(app%type)), trim(app%database)
            return
        end if

        call dm_error_out(rc, 'failed to add ' // trim(TYPE_NAMES(app%type)) // ' to database ' // app%database)
    end function db_create

    integer function db_delete(db, app) result(rc)
        !! Deletes database record.
        type(db_type),  intent(inout) :: db  !! Database type.
        type(app_type), intent(inout) :: app !! App type.

        character(len=ID_LEN) :: id

        rc = E_NONE

        select case (app%type)
            case (TYPE_NODE)
                ! Delete node.
                id = app%node%id

                if (.not. dm_db_has_node(db, id)) then
                    rc = E_NOT_FOUND
                    call dm_error_out(rc, 'node ' // trim(id) // ' not found')
                    return
                end if

                rc = dm_db_delete_node(db, id)

            case (TYPE_SENSOR)
                ! Delete sensor.
                id = app%sensor%id

                if (.not. dm_db_has_sensor(db, id)) then
                    rc = E_NOT_FOUND
                    call dm_error_out(rc, 'sensor ' // trim(id) // ' not found')
                    return
                end if

                rc = dm_db_delete_sensor(db, id)

            case (TYPE_TARGET)
                ! Delete target.
                id = app%target%id

                if (.not. dm_db_has_target(db, id)) then
                    rc = E_NOT_FOUND
                    call dm_error_out(rc, 'target' // trim(id) // ' not found')
                    return
                end if

                rc = dm_db_delete_target(db, id)
        end select

        if (dm_is_ok(rc) .and. app%verbose) then
            print '(a, " deleted from database ", a)', trim(TYPE_NAMES(app%type)), trim(app%database)
            return
        end if

        call dm_error_out(rc, 'failed to delete ' // trim(TYPE_NAMES(app%type)) // ' from database ' // app%database)
    end function db_delete

    integer function db_read(db, app) result(rc)
        !! Reads and outputs database record.
        type(db_type),  intent(inout) :: db  !! Database type.
        type(app_type), intent(inout) :: app !! App type.

        character(len=ID_LEN) :: id
        type(node_type)       :: node
        type(sensor_type)     :: sensor
        type(target_type)     :: target

        rc = E_INVALID

        db_select: select case (app%type)
            case (TYPE_NODE)
                ! Read node.
                id = app%node%id

                if (.not. dm_db_has_node(db, id)) then
                    rc = E_NOT_FOUND
                    call dm_error_out(rc, 'node ' // trim(id) // ' not found')
                    return
                end if

                rc = dm_db_select(db, node, node_id=id)
                if (dm_is_error(rc)) exit db_select

                ! Output node.
                call dm_node_out(node)

            case (TYPE_SENSOR)
                ! Read sensor.
                id = app%sensor%id

                if (.not. dm_db_has_sensor(db, id)) then
                    rc = E_NOT_FOUND
                    call dm_error_out(rc, 'sensor ' // trim(id) // ' not found')
                    return
                end if

                rc = dm_db_select(db, sensor, sensor_id=id)
                if (dm_is_error(rc)) exit db_select

                ! Output sensor.
                call dm_sensor_out(sensor)

            case (TYPE_TARGET)
                ! Read target.
                id = app%target%id

                if (.not. dm_db_has_target(db, id)) then
                    rc = E_NOT_FOUND
                    call dm_error_out(rc, 'target ' // trim(id) // ' not found')
                    return
                end if

                rc = dm_db_select(db, target, target_id=id)
                if (dm_is_error(rc)) exit db_select

                ! Output target.
                call dm_target_out(target)
        end select db_select

        if (dm_is_ok(rc)) return
        call dm_error_out(rc, 'failed to read ' // trim(TYPE_NAMES(app%type)) // ' from database ' // app%database)
    end function db_read

    integer function db_update(db, app) result(rc)
        !! Updates database record.
        type(db_type),  intent(inout) :: db  !! Database type.
        type(app_type), intent(inout) :: app !! App type.

        character(len=ID_LEN) :: id
        type(node_type)       :: old_node
        type(sensor_type)     :: old_sensor
        type(target_type)     :: old_target

        rc = E_NONE

        db_select: select case (app%type)
            case (TYPE_NODE)
                ! Update node.
                id = app%node%id

                if (.not. dm_db_has_node(db, id)) then
                    rc = E_NOT_FOUND
                    call dm_error_out(rc, 'node ' // trim(id) // ' not found')
                    return
                end if

                rc = dm_db_select(db, old_node, node_id=id)
                if (dm_is_error(rc)) exit db_select

                ! Overwrite if not passed.
                if (.not. app%mask(ATTR_NAME)) app%node%name = old_node%name
                if (.not. app%mask(ATTR_META)) app%node%meta = old_node%meta
                if (.not. app%mask(ATTR_X))    app%node%x    = old_node%x
                if (.not. app%mask(ATTR_Y))    app%node%y    = old_node%y
                if (.not. app%mask(ATTR_Z))    app%node%z    = old_node%z
                if (.not. app%mask(ATTR_LON))  app%node%lon  = old_node%lon
                if (.not. app%mask(ATTR_LAT))  app%node%lat  = old_node%lat
                if (.not. app%mask(ATTR_ALT))  app%node%alt  = old_node%alt

                rc = dm_db_update(db, app%node)

            case (TYPE_SENSOR)
                ! Update sensor.
                id = app%sensor%id

                if (.not. dm_db_has_sensor(db, id)) then
                    rc = E_NOT_FOUND
                    call dm_error_out(rc, 'sensor ' // trim(id) // ' not found')
                    return
                end if

                rc = dm_db_select(db, old_sensor, sensor_id=id)
                if (dm_is_error(rc)) exit db_select

                ! Overwrite if not passed.
                if (.not. app%mask(ATTR_TYPE)) app%sensor%type    = old_sensor%type
                if (.not. app%mask(ATTR_NODE)) app%sensor%node_id = old_sensor%node_id
                if (.not. app%mask(ATTR_NAME)) app%sensor%name    = old_sensor%name
                if (.not. app%mask(ATTR_SN))   app%sensor%sn      = old_sensor%sn
                if (.not. app%mask(ATTR_META)) app%sensor%meta    = old_sensor%meta
                if (.not. app%mask(ATTR_X))    app%sensor%x       = old_sensor%x
                if (.not. app%mask(ATTR_Y))    app%sensor%y       = old_sensor%y
                if (.not. app%mask(ATTR_Z))    app%sensor%z       = old_sensor%z
                if (.not. app%mask(ATTR_LON))  app%sensor%lon     = old_sensor%lon
                if (.not. app%mask(ATTR_LAT))  app%sensor%lat     = old_sensor%lat
                if (.not. app%mask(ATTR_ALT))  app%sensor%alt     = old_sensor%alt

                if (len_trim(app%sensor%node_id) == 0) then
                    rc = E_INVALID
                    call dm_error_out(rc, 'node id is missing')
                    return
                end if

                if (.not. dm_db_has_node(db, app%sensor%node_id)) then
                    rc = E_NOT_FOUND
                    call dm_error_out(rc, 'node ' // trim(app%sensor%node_id) // ' not found')
                    return
                end if

                rc = dm_db_update(db, app%sensor)

            case (TYPE_TARGET)
                ! Update target.
                id = app%target%id

                if (.not. dm_db_has_target(db, id)) then
                    rc = E_NOT_FOUND
                    call dm_error_out(rc, 'target ' // trim(id) // ' not found')
                    return
                end if

                rc = dm_db_select(db, old_target, target_id=id)
                if (dm_is_error(rc)) exit db_select

                ! Overwrite if not passed.
                if (.not. app%mask(ATTR_NAME))  app%target%name  = old_target%name
                if (.not. app%mask(ATTR_META))  app%target%meta  = old_target%meta
                if (.not. app%mask(ATTR_STATE)) app%target%state = old_target%state
                if (.not. app%mask(ATTR_X))     app%target%x     = old_target%x
                if (.not. app%mask(ATTR_Y))     app%target%y     = old_target%y
                if (.not. app%mask(ATTR_Z))     app%target%z     = old_target%z
                if (.not. app%mask(ATTR_LON))   app%target%lon   = old_target%lon
                if (.not. app%mask(ATTR_LAT))   app%target%lat   = old_target%lat
                if (.not. app%mask(ATTR_ALT))   app%target%alt   = old_target%alt

                rc = dm_db_update(db, app%target)
        end select db_select

        if (dm_is_ok(rc) .and. app%verbose) then
            print '(a, " updated in database ", a)', trim(TYPE_NAMES(app%type)), trim(app%database)
            return
        end if

        call dm_error_out(rc, 'failed to update ' // trim(TYPE_NAMES(app%type)) // ' in database ' // app%database)
    end function db_update

    integer function read_args(app) result(rc)
        !! Reads command-line arguments.
        type(app_type), intent(out) :: app !! App settings.

        character(len=SENSOR_TYPE_NAME_LEN) :: sensor ! Sensor type name.
        character(len=TYPE_NAME_LEN)        :: type   ! DMPACK derived type name.

        integer        :: i, n
        logical        :: mask(OP_LAST) ! CRUD operation mask.
        type(arg_type) :: args(19)

        ! Required and optional command-line arguments.
        args = [ &
            arg_type('create',    short='C', type=ARG_TYPE_STRING), &                               ! -C, --create <type>
            arg_type('read',      short='R', type=ARG_TYPE_STRING), &                               ! -R, --read <type>
            arg_type('update',    short='U', type=ARG_TYPE_STRING), &                               ! -U, --update <type>
            arg_type('delete',    short='D', type=ARG_TYPE_STRING), &                               ! -D, --delete <type>
            arg_type('database',  short='d', type=ARG_TYPE_DATABASE, required=.true.), &            ! -d, --database <path>
            arg_type('id',        short='I', type=ARG_TYPE_ID,       required=.true.), &            ! -I, --id <id>
            arg_type('name',      short='n', type=ARG_TYPE_STRING, max_len=NODE_NAME_LEN), &        ! -n, --name <string>
            arg_type('meta',      short='M', type=ARG_TYPE_STRING, max_len=NODE_META_LEN), &        ! -M, --meta <string>
            arg_type('node',      short='N', type=ARG_TYPE_ID), &                                   ! -N, --node <id>
            arg_type('sn',        short='Q', type=ARG_TYPE_STRING, max_len=SENSOR_SN_LEN), &        ! -Q, --sn <string>
            arg_type('type',      short='t', type=ARG_TYPE_STRING, max_len=SENSOR_TYPE_NAME_LEN), & ! -t, --type <type>
            arg_type('state',     short='S', type=ARG_TYPE_INTEGER), &                              ! -S, --state <state>
            arg_type('x',         short='X', type=ARG_TYPE_REAL), &                                 ! -X, --x <x>
            arg_type('y',         short='Y', type=ARG_TYPE_REAL), &                                 ! -Y, --y <y>
            arg_type('z',         short='Z', type=ARG_TYPE_REAL), &                                 ! -Z, --z <z>
            arg_type('lon',       short='G', type=ARG_TYPE_REAL), &                                 ! -G, --lon <lng>
            arg_type('lat',       short='L', type=ARG_TYPE_REAL), &                                 ! -L, --lat <lat>
            arg_type('alt',       short='A', type=ARG_TYPE_REAL), &                                 ! -A, --alt <alt>
            arg_type('verbose',   short='V', type=ARG_TYPE_LOGICAL) &                               ! -V, --verbose
        ]

        ! Read command-line arguments.
        rc = dm_arg_read(args, APP_NAME, APP_MAJOR, APP_MINOR, APP_PATCH, dm_db_version(.true.))
        if (dm_is_error(rc)) return

        ! CRUD operation.
        mask = [ (args(i)%passed, i = 1, OP_LAST) ]
        n = count(mask)

        rc = E_INVALID
        if (n == 0) then
            call dm_error_out(rc, 'database operation and type required')
            return
        else if (n > 1) then
            call dm_error_out(rc, 'single database operation and type required')
            return
        end if

        ! Get entity type (node, sensor, target).
        app%operation = sum(merge([1, 2, 3, 4], 0, mask))
        call dm_arg_get(args(app%operation), type)
        app%type = dm_type_from_name(type)

        ! Get remaining command-line arguments.
        call dm_arg_get(args(5), app%database)

        select case (app%type)
            case (TYPE_NODE)
                ! Get node attributes.
                call dm_arg_get(args( 6), app%node%id)
                call dm_arg_get(args( 7), app%node%name, passed=app%mask(ATTR_NAME))
                call dm_arg_get(args( 8), app%node%meta, passed=app%mask(ATTR_META))
                call dm_arg_get(args(13), app%node%x,    passed=app%mask(ATTR_X))
                call dm_arg_get(args(14), app%node%y,    passed=app%mask(ATTR_Y))
                call dm_arg_get(args(15), app%node%z,    passed=app%mask(ATTR_Z))
                call dm_arg_get(args(16), app%node%lon,  passed=app%mask(ATTR_LON))
                call dm_arg_get(args(17), app%node%lat,  passed=app%mask(ATTR_LAT))
                call dm_arg_get(args(18), app%node%alt,  passed=app%mask(ATTR_ALT))

            case (TYPE_SENSOR)
                ! Get sensor attributes.
                call dm_arg_get(args( 6), app%sensor%id)
                call dm_arg_get(args( 7), app%sensor%name,    passed=app%mask(ATTR_NAME))
                call dm_arg_get(args( 8), app%sensor%meta,    passed=app%mask(ATTR_META))
                call dm_arg_get(args( 9), app%sensor%node_id, passed=app%mask(ATTR_NODE))
                call dm_arg_get(args(10), app%sensor%sn,      passed=app%mask(ATTR_SN))
                call dm_arg_get(args(11), sensor,             passed=app%mask(ATTR_TYPE), default=SENSOR_TYPE_NAMES(SENSOR_TYPE_NONE))
                call dm_arg_get(args(13), app%sensor%x,       passed=app%mask(ATTR_X))
                call dm_arg_get(args(14), app%sensor%y,       passed=app%mask(ATTR_Y))
                call dm_arg_get(args(15), app%sensor%z,       passed=app%mask(ATTR_Z))
                call dm_arg_get(args(16), app%sensor%lon,     passed=app%mask(ATTR_LON))
                call dm_arg_get(args(17), app%sensor%lat,     passed=app%mask(ATTR_LAT))
                call dm_arg_get(args(18), app%sensor%alt,     passed=app%mask(ATTR_ALT))

                app%sensor%type = dm_sensor_type_from_name(sensor)

            case (TYPE_TARGET)
                ! Get target attributes.
                call dm_arg_get(args( 6), app%target%id)
                call dm_arg_get(args( 7), app%target%name,  passed=app%mask(ATTR_NAME))
                call dm_arg_get(args( 8), app%target%meta,  passed=app%mask(ATTR_META))
                call dm_arg_get(args(12), app%target%state, passed=app%mask(ATTR_STATE))
                call dm_arg_get(args(13), app%target%x,     passed=app%mask(ATTR_X))
                call dm_arg_get(args(14), app%target%y,     passed=app%mask(ATTR_Y))
                call dm_arg_get(args(15), app%target%z,     passed=app%mask(ATTR_Z))
                call dm_arg_get(args(16), app%target%lon,   passed=app%mask(ATTR_LON))
                call dm_arg_get(args(17), app%target%lat,   passed=app%mask(ATTR_LAT))
                call dm_arg_get(args(18), app%target%alt,   passed=app%mask(ATTR_ALT))

            case default
                rc = E_INVALID
                call dm_error_out(rc, 'invalid data type ' // trim(type) // ' (either node, sensor, or target)')
                return
        end select

        call dm_arg_get(args(19), app%verbose)

        ! Validate options.
        rc = E_INVALID

        select case (app%operation)
            case (OP_CREATE)
                ! Node, sensor, and target.
                if (.not. app%mask(ATTR_NAME)) then
                    call dm_error_out(rc, 'command-line option --name required')
                    return
                end if

                ! Sensor.
                if (app%type == TYPE_SENSOR) then
                    if (.not. dm_id_is_valid(app%sensor%node_id)) then
                        call dm_error_out(rc, 'command-line option --node is invalid or missing')
                        return
                    end if
                end if
            case (OP_UPDATE)
                ! Update operation.
                select case (app%type)
                    case (TYPE_NODE)
                        if (.not. app%mask(ATTR_NAME) .and. .not. app%mask(ATTR_META) .and. &
                            .not. app%mask(ATTR_X)    .and. .not. app%mask(ATTR_Y)    .and. &
                            .not. app%mask(ATTR_Z)    .and. .not. app%mask(ATTR_LON)  .and. &
                            .not. app%mask(ATTR_LAT)  .and. .not. app%mask(ATTR_ALT)) then
                            call dm_error_out(rc, 'command-line option --name, --meta, --x, --y, --z, ' // &
                                                  '--lon, --lat, or --alt required')
                            return
                        end if

                    case (TYPE_TARGET)
                        if (.not. app%mask(ATTR_NAME)  .and. .not. app%mask(ATTR_META) .and. &
                            .not. app%mask(ATTR_STATE) .and. .not. app%mask(ATTR_X)    .and. &
                            .not. app%mask(ATTR_Y)     .and. .not. app%mask(ATTR_Z)    .and. &
                            .not. app%mask(ATTR_LON)   .and. .not. app%mask(ATTR_LAT)  .and. &
                            .not. app%mask(ATTR_ALT)) then
                            call dm_error_out(rc, 'command-line option --name, --meta, --state, --x, --y, --z, ' // &
                                                  '--lon, --lat, or --alt required')
                            return
                        end if

                    case (TYPE_SENSOR)
                        if (app%mask(ATTR_STATE)) then
                            call dm_error_out(rc, 'command-line option --state is not allowed')
                            return
                        end if

                        if (.not. any(app%mask)) then
                            call dm_error_out(rc, 'command-line option --node, --type, --name, --sn, ' // &
                                                  '--meta, --x, --y, --z, --lon, --lat, or --alt required')
                            return
                        end if
                end select
        end select

        ! Validation passed.
        rc = E_NONE
    end function read_args
end program dmdbctl
