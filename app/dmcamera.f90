! dmcamera.f90
!
! Author:  Philipp Engel
! Licence: ISC
program dmcamera
    !! Program to capture images of a USB or IP camera. The images are written
    !! to file, and the image meta data to database if a database is
    !! configured.
    use :: dmpack
    implicit none (type, external)

    character(*), parameter :: APP_NAME  = 'dmcamera'
    integer,      parameter :: APP_MAJOR = 2
    integer,      parameter :: APP_MINOR = 0
    integer,      parameter :: APP_PATCH = 0

    ! Program parameters.
    integer, parameter :: APP_DB_NSTEPS  = 500                !! Number of steps before database is optimised.
    integer, parameter :: APP_DB_TIMEOUT = DB_TIMEOUT_DEFAULT !! SQLite 3 busy timeout in mseconds.

    type :: app_type
        !! Application settings.
        character(ID_LEN)          :: name        = APP_NAME           !! Name of database instance and POSIX semaphore.
        character(FILE_PATH_LEN)   :: config      = ' '                !! Path to configuration file.
        character(LOGGER_NAME_LEN) :: logger      = ' '                !! Name of logger (name implies IPC).
        character(NODE_ID_LEN)     :: node_id     = ' '                !! Node id.
        character(SENSOR_ID_LEN)   :: sensor_id   = ' '                !! Sensor id.
        character(TARGET_ID_LEN)   :: target_id   = ' '                !! Target id.
        character(FILE_PATH_LEN)   :: database    = ' '                !! Path to SQLite database file.
        character(FILE_PATH_LEN)   :: directory   = ' '                !! Path to camera image directory.
        character(FILE_PATH_LEN)   :: input       = ' '                !! Camera device path (`/dev/video0`, `rtsp://localhost/`).
        character(FILE_PATH_LEN)   :: device_name = ' '                !! Camera device name (`v4l2`, `rtsp`).
        character(MIME_LEN)        :: mime        = MIME_JPEG          !! Camera image format name (`image/jpeg`, `image/png`).
        character(GM_FONT_LEN)     :: font        = 'DejaVuSansMono'   !! Name of font for overlay text box.
        integer                    :: device      = CAMERA_DEVICE_NONE !! Camera device (`CAMERA_DEVICE_V4L2`, `CAMERA_DEVICE_RTSP`).
        integer                    :: font_size   = 12                 !! Font size of overlay.
        integer                    :: interval    = 0                  !! Snapshot interval [sec].
        integer                    :: width       = 0                  !! Camera image width (0: default).
        integer                    :: height      = 0                  !! Camera image height (0: default).
        logical                    :: debug       = .false.            !! Forward debug messages via IPC.
        logical                    :: ipc         = .false.            !! Use POSIX semaphore for process synchronisation.
        logical                    :: overlay     = .false.            !! Create text overlay on image.
        logical                    :: verbose     = .false.            !! Print debug messages to stderr.
    end type app_type

    class(logger_class), pointer :: logger ! Logger object.

    integer                    :: rc  ! Return code.
    type(app_type)             :: app ! App settings.
    type(db_type)              :: db  ! Database type.
    type(posix_sem_named_type) :: sem ! POSIX semaphore type.

    ! Initialise DMPACK.
    call dm_init()

    ! Get command-line arguments, read options from configuration file.
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
    call logger%info('started ' // APP_NAME)

    rc = init(app, db, sem)
    if (dm_is_error(rc)) call shutdown(rc)

    rc = run(app, db, sem)
    call shutdown(rc)
contains
    integer function capture(app, camera, image, path) result(rc)
        !! Writes camera image to file and returns image type and file path.
        type(app_type),            intent(inout) :: app    !! App settings.
        type(camera_type),         intent(inout) :: camera !! Camera type.
        type(image_type),          intent(out)   :: image  !! Image type.
        character(:), allocatable, intent(out)   :: path   !! Image file path.

        character(:), allocatable :: mime
        integer                   :: stat

        ! Initialise image type.
        image = image_type(id        = dm_uuid4(),    &
                           node_id   = app%node_id,   &
                           sensor_id = app%sensor_id, &
                           target_id = app%target_id, &
                           mime      = app%mime)

        ! Generate image path.
        path = dm_image_path(image, app%directory)

        if (len(path) == 0) then
            rc = E_INVALID
            call logger%error('invalid image path', error=rc)
            return
        end if

        if (dm_file_exists(path)) then
            rc = E_EXIST
            call logger%error('image file ' // trim(path) // ' already exists', error=rc)
            return
        end if

        ! Capture camera image.
        image%timestamp = dm_time_now()
        rc = dm_camera_capture(camera, path)

        if (dm_is_error(rc)) then
            call logger%error('failed to capture image from camera ' // app%input, error=rc)
            return
        end if

        if (.not. dm_file_exists(path)) then
            rc = E_NOT_FOUND
            call logger%error('failed to write image to file ' // path, error=rc)
            return
        end if

        call logger%debug('image written to file ' // path)

        if (dm_file_size(path) == 0) then
            rc = E_EMPTY
            call logger%error('image file ' // trim(path) // ' is empty', error=rc)
            return
        end if

        ! Query image parameters with GraphicsMagick.
        rc = dm_gm_get_mime(path, mime)

        if (dm_is_error(rc)) then
            call logger%error('failed to read MIME type of image file ' // path, error=rc)
            return
        end if

        if (mime /= image%mime) then
            call logger%warning('MIME type ' // trim(mime) // ' of image ' // image%id // ' does not match ' // image%mime, error=rc)
        else
            call logger%debug('MIME type of image is ' // mime)
        end if

        rc = dm_gm_get_dimensions(path, image%width, image%height)

        if (dm_is_error(rc)) then
            call logger%error('failed to read dimensions of image file ' // path, error=rc)
            return
        end if

        call logger%debug('image dimensions are ' // dm_itoa(image%width) // 'x' // dm_itoa(image%height))

        ! Add optional text box overlay.
        if (app%overlay) then
            stat = dm_gm_add_text_box(path, text=image%timestamp, text_box=gm_text_box_type(font=app%font, font_size=app%font_size))

            if (dm_is_error(stat)) then
                call logger%warning('failed to add text overlay to image file ' // path, error=stat)
                return
            end if

            call logger%debug('added text overlay to image with font ' // trim(app%font) // ':' // dm_itoa(app%font_size))
        end if

        ! Get file size of image after all GM manipulations, otherwise the size
        ! will differ from the actual size.
        image%size = dm_file_size(path)
        call logger%debug('image size is ' // dm_size_to_human(image%size))
    end function capture

    integer function init(app, db, sem) result(rc)
        !! Initialises program.
        type(app_type),             intent(inout) :: app !! App type.
        type(db_type),              intent(out)   :: db  !! Database type.
        type(posix_sem_named_type), intent(out)   :: sem !! POSIX semaphore type.

        rc = E_NONE

        ! Open SQLite database.
        if (dm_string_has(app%database)) then
            rc = dm_db_open(db, path=app%database, timeout=APP_DB_TIMEOUT)

            if (dm_is_error(rc)) then
                call logger%error('failed to open database ' // app%database, error=rc)
                return
            end if

            call logger%debug('opened database ' // app%database)

            if (.not. dm_db_table_has_images(db)) then
                rc = E_INVALID
                call logger%error('missing table in database ' // app%database, error=rc)
                return
            end if
        end if

        ! Create semaphore for IPC.
        if (app%ipc) then
            rc = dm_posix_sem_open(sem, name=app%name, value=0, create=.true.)

            if (dm_is_error(rc)) then
                call logger%error('failed to create semaphore /' // app%name, error=rc)
                return
            end if

            call logger%debug('opened semaphore /' // app%name)
        end if

        call dm_posix_signal_register(signal_callback)
    end function init

    integer function run(app, db, sem) result(rc)
        !! Captures camera image in configured interval.
        type(app_type),             intent(inout) :: app !! App settings.
        type(db_type),              intent(inout) :: db  !! Database type.
        type(posix_sem_named_type), intent(inout) :: sem !! Semaphore type.

        integer           :: steps
        type(camera_type) :: camera

        steps = 0
        if (.not. dm_db_is_connected(db)) call logger%debug('image meta data storage is disabled')

        ! Intialise camera type.
        camera = camera_type(app%input, app%device, app%width, app%height)
        call logger%debug('initialised ' // dm_to_upper(trim(CAMERA_DEVICE_NAMES(app%device))) // ' camera ' // trim(app%input) // &
                          ' of sensor ' // trim(app%sensor_id) // ' and target ' // app%target_id)

        main_loop: do
            io_block: block
                character(:), allocatable :: image_path
                integer                   :: stat, value
                type(image_type)          :: image

                ! Capture camera image.
                rc = capture(app, camera, image, image_path)

                if (dm_is_error(rc)) then
                    ! Remove image file on error.
                    call dm_file_delete(image_path, error=stat)
                    if (dm_is_ok(stat)) call logger%debug('deleted image file ' // image_path)
                    exit io_block
                end if

                ! Add image to database.
                db_loop: do
                    if (.not. dm_db_is_connected(db)) exit db_loop
                    rc = dm_db_insert(db, image)

                    ! Retry if database is busy.
                    if (rc == E_DB_BUSY) then
                        call logger%debug('database is busy', error=rc)
                        call dm_db_sleep(APP_DB_TIMEOUT)
                        cycle db_loop
                    end if

                    ! Handle database error.
                    if (dm_is_error(rc)) then
                        call logger%error('failed to add image ' // image%id // ' to database', error=rc)
                        exit db_loop
                    end if

                    call logger%debug('added image ' // image%id // ' to database')
                    exit db_loop
                end do db_loop

                if (dm_is_error(rc)) then
                    ! Remove image file on database error.
                    call dm_file_delete(image_path, error=stat)
                    if (dm_is_ok(stat)) call logger%debug('deleted image file ' // image_path)
                    exit io_block
                end if

                ! Post semaphore.
                if (app%ipc) then
                    rc = dm_posix_sem_value(sem, value)

                    if (dm_is_error(rc)) then
                        call logger%error('failed to get value of semaphore /' // app%name, error=rc)
                        exit io_block
                    end if

                    if (value /= 0) exit io_block

                    rc = dm_posix_sem_post(sem)

                    if (dm_is_error(rc)) then
                        call logger%error('failed to post to semaphore /' // app%name, error=rc)
                        exit io_block
                    end if
                end if

                ! Optimise database.
                if (dm_db_is_connected(db)) then
                    if (steps == 0) then
                        rc = dm_db_optimize(db)

                        if (dm_is_error(rc)) then
                            call logger%error('failed to optimize database', error=rc)
                        else
                            call logger%debug('optimized database')
                        end if
                    end if

                    ! Increase optimise step counter.
                    steps = modulo(steps + 1, APP_DB_NSTEPS)
                end if
            end block io_block

            if (app%interval == 0) exit main_loop
            call logger%debug('capturing next image in ' // dm_itoa(app%interval) // ' sec')
            call dm_sleep(app%interval)
        end do main_loop

        call logger%debug('finished camera image capturing')
    end function run

    subroutine shutdown(error)
        !! Cleans up and stops program.
        integer, intent(in) :: error !! DMPACK error code.

        integer :: rc, stat

        stat = merge(STOP_FAILURE, STOP_SUCCESS, dm_is_error(error))

        if (dm_db_is_connected(db)) then
            call dm_db_close(db, error=rc)
            if (dm_is_error(rc)) call logger%error('failed to close database', error=rc)
        end if

        if (app%ipc) then
            call dm_posix_sem_close(sem, error=rc)
            if (dm_is_error(rc)) call logger%error('failed to close semaphore /' // app%name, error=rc)

            call dm_posix_sem_unlink(sem, error=rc)
            if (dm_is_error(rc)) call logger%error('failed to unlink semaphore /' // app%name, error=rc)
        end if

        call logger%info('stopped ' // APP_NAME, error=error)
        call dm_stop(stat)
    end subroutine shutdown

    ! **************************************************************************
    ! COMMAND-LINE ARGUMENTS AND CONFIGURATION FILE.
    ! **************************************************************************
    integer function read_args(app) result(rc)
        !! Reads command-line arguments and settings from configuration file.
        type(app_type), intent(out) :: app !! App type.

        type(arg_parser_class) :: parser

        ! Required and optional command-line arguments.
        call parser%add('name',      short='n', type=ARG_TYPE_ID)       ! -n, --name <id>
        call parser%add('config',    short='c', type=ARG_TYPE_FILE)     ! -c, --config <path>
        call parser%add('logger',    short='l', type=ARG_TYPE_ID)       ! -l, --logger <id>
        call parser%add('node',      short='N', type=ARG_TYPE_ID)       ! -N, --node <id>
        call parser%add('sensor',    short='S', type=ARG_TYPE_ID)       ! -S, --sensor <id>
        call parser%add('target',    short='T', type=ARG_TYPE_ID)       ! -T, --target <id>
        call parser%add('database',  short='d', type=ARG_TYPE_DATABASE) ! -d, --database <path>
        call parser%add('directory', short='p', type=ARG_TYPE_FILE)     ! -p, --directory <path>
        call parser%add('input',     short='i', type=ARG_TYPE_STRING)   ! -i, --input <path>
        call parser%add('mime',      short='M', type=ARG_TYPE_STRING)   ! -M, --mime <id>
        call parser%add('font',      short='F', type=ARG_TYPE_STRING)   ! -F, --font <name>
        call parser%add('device',    short='C', type=ARG_TYPE_STRING)   ! -C, --device <name>
        call parser%add('fontsize',  short='Z', type=ARG_TYPE_INTEGER)  ! -Z, --fontsize <n>
        call parser%add('interval',  short='I', type=ARG_TYPE_INTEGER)  ! -I, --interval <sec>
        call parser%add('width',     short='W', type=ARG_TYPE_INTEGER)  ! -W, --width <n>
        call parser%add('height',    short='H', type=ARG_TYPE_INTEGER)  ! -H, --height <n>
        call parser%add('debug',     short='D', type=ARG_TYPE_LOGICAL)  ! -D, --debug
        call parser%add('ipc',       short='Q', type=ARG_TYPE_LOGICAL)  ! -Q, --ipc
        call parser%add('overlay',   short='O', type=ARG_TYPE_LOGICAL)  ! -O, --overlay
        call parser%add('verbose',   short='V', type=ARG_TYPE_LOGICAL)  ! -V, --verbose

        ! Read all command-line arguments.
        rc = parser%read(version_callback)
        if (dm_is_error(rc)) return

        call parser%get('name',   app%name)
        call parser%get('config', app%config)

        ! Read configuration from file.
        rc = read_config(app)
        if (dm_is_error(rc)) return

        ! Overwrite configuration.
        call parser%get('logger',    app%logger)
        call parser%get('node',      app%node_id)
        call parser%get('sensor',    app%sensor_id)
        call parser%get('target',    app%target_id)
        call parser%get('database',  app%database)
        call parser%get('directory', app%directory)
        call parser%get('input',     app%input)
        call parser%get('mime',      app%mime)
        call parser%get('font',      app%font)
        call parser%get('device',    app%device_name)
        call parser%get('fontsize',  app%font_size)
        call parser%get('interval',  app%interval)
        call parser%get('width',     app%width)
        call parser%get('height',    app%height)
        call parser%get('debug',     app%debug)
        call parser%get('ipc',       app%ipc)
        call parser%get('overlay',   app%overlay)
        call parser%get('verbose',   app%verbose)

        app%device = dm_camera_device_from_name(app%device_name)

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
            call config%get('logger',    app%logger)
            call config%get('node',      app%node_id)
            call config%get('sensor',    app%sensor_id)
            call config%get('target',    app%target_id)
            call config%get('database',  app%database)
            call config%get('directory', app%directory)
            call config%get('input',     app%input)
            call config%get('mime',      app%mime)
            call config%get('font',      app%font)
            call config%get('device',    app%device)
            call config%get('fontsize',  app%font_size)
            call config%get('interval',  app%interval)
            call config%get('width',     app%width)
            call config%get('height',    app%height)
            call config%get('debug',     app%debug)
            call config%get('ipc',       app%ipc)
            call config%get('overlay',   app%overlay)
            call config%get('verbose',   app%verbose)
        end if

        call config%close()
    end function read_config

    integer function validate(app) result(rc)
        !! Validates options and prints error messages.
        type(app_type), intent(inout) :: app !! App type.

        rc = E_INVALID

        if (.not. dm_id_is_valid(app%name)) then
            call dm_error_out(rc, 'invalid name')
            return
        end if

        if (dm_string_has(app%logger) .and. .not. dm_id_is_valid(app%logger)) then
            call dm_error_out(rc, 'invalid logger name')
            return
        end if

        if (.not. dm_id_is_valid(app%node_id)) then
            call dm_error_out(rc, 'invalid or missing node id')
            return
        end if

        if (.not. dm_id_is_valid(app%sensor_id)) then
            call dm_error_out(rc, 'invalid or missing sensor id')
            return
        end if

        if (.not. dm_id_is_valid(app%target_id)) then
            call dm_error_out(rc, 'invalid or missing target id')
            return
        end if

        if (dm_string_has(app%database) .and. .not. dm_file_exists(app%database)) then
            rc = E_NOT_FOUND
            call dm_error_out(rc, 'database ' // trim(app%database) // ' does not exist')
            return
        end if

        if (.not. dm_string_has(app%directory)) then
            call dm_error_out(rc, 'missing image directory')
            return
        end if

        if (.not. dm_file_exists(app%directory)) then
            rc = E_NOT_FOUND
            call dm_error_out(rc, 'image directory' // trim(app%directory) // ' does not exist')
            return
        end if

        if (.not. dm_file_is_directory(app%directory)) then
            call dm_error_out(rc, 'file ' // trim(app%directory) // ' is not a directory')
            return
        end if

        if (.not. dm_file_is_writeable(app%directory)) then
            rc = E_PERM
            call dm_error_out(rc, 'no write access to image directory' // trim(app%directory))
            return
        end if

        if (.not. dm_string_has(app%input)) then
            call dm_error_out(rc, 'missing input path')
            return
        end if

        if (app%mime /= MIME_JPEG .and. app%mime /= MIME_PNG) then
            call dm_error_out(rc, 'invalid MIME type, must be ' // MIME_JPEG // ' or ' // MIME_PNG)
            return
        end if

        if (.not. dm_camera_device_is_valid(app%device)) then
            call dm_error_out(rc, 'missing or invalid camera device')
            return
        end if

        if (app%device == CAMERA_DEVICE_V4L2 .and. .not. dm_file_exists(app%input)) then
            rc = E_NOT_FOUND
            call dm_error_out(rc, 'camera input device ' // trim(app%input) // ' not found')
            return
        end if

        if (app%device == CAMERA_DEVICE_RTSP .and. .not. dm_string_starts_with(app%input, 'rtsp://')) then
            call dm_error_out(rc, 'invalid camera input URL ' // app%input)
            return
        end if

        if (app%interval < 0) then
            call dm_error_out(rc, 'invalid interval')
            return
        end if

        if (app%width < 0) then
            call dm_error_out(rc, 'invalid width')
            return
        end if

        if (app%height < 0) then
            call dm_error_out(rc, 'invalid height')
            return
        end if

        if (app%overlay) then
            if (.not. dm_gm_font_is_valid(app%font)) then
                call dm_error_out(rc, 'invalid or missing font name')
                return
            end if

            if (app%font_size < 0) then
                call dm_error_out(rc, 'invalid font size')
                return
            end if
        end if

        rc = E_NONE
    end function validate

    ! **************************************************************************
    ! CALLBACKS.
    ! **************************************************************************
    subroutine signal_callback(signum) bind(c)
        !! C-interoperable signal handler that closes database, removes message
        !! queue, and stops program.
        integer(c_int), intent(in), value :: signum

        call logger%debug('exit on on signal ' // dm_posix_signal_name(signum))
        call shutdown(E_NONE)
    end subroutine signal_callback

    subroutine version_callback()
        call dm_version_out(APP_NAME, APP_MAJOR, APP_MINOR, APP_PATCH)
        print '(a)', dm_db_version(.true.)
    end subroutine version_callback
end program dmcamera
