! Author:  Philipp Engel
! Licence: ISC
module dm_geocom
    !! Object-oriented GeoCOM API for Fortran.
    !!
    !! The API provided by DMPACK does not follow the official Leica GeoCOM API
    !! for C/C++ and Visual Basic. Structured types and functions are simplified
    !! and given more memorable names. Function names do not contain any
    !! sub-system prefix.
    !!
    !! The following example opens the TTY `/dev/ttyUSB0` at 115,200 baud, and
    !! calls the null procedure of the instrument (`COM_NullProc`):
    !!
    !! ```fortran
    !! integer            :: rc     ! DMPACK return code.
    !! type(geocom_class) :: geocom ! GeoCOM object.
    !!
    !! call geocom%open('/dev/ttyUSB0', GEOCOM_COM_BAUD_115200, retries=1, verbose=.true. error=rc)
    !! if (dm_is_error(rc)) error stop
    !!
    !! call geocom%null()
    !! print '(i0, ": ", a)', geocom%code(), geocom%message()
    !!
    !! call geocom%close()
    !! ```
    use :: dm_error
    use :: dm_geocom_api
    use :: dm_geocom_error
    use :: dm_geocom_type
    use :: dm_kind
    use :: dm_request
    use :: dm_response
    use :: dm_tty
    use :: dm_util
    implicit none (type, external)
    private

    type, public :: geocom_class
        !! GeoCOM class for TTY access and GeoCOM API handling through the
        !! public methods. Objects of this class are not thread-safe.
        private
        integer            :: baud    = GEOCOM_COM_BAUD_19200 !! GeoCOM baud rate enumerator (`GEOCOM_COM_BAUD_RATE`).
        integer            :: grc     = GRC_OK                !! Last GeoCOM return code.
        integer            :: rc      = E_NONE                !! Last DMPACK return code.
        logical            :: verbose = .false.               !! Print error messages to stderr.
        type(request_type) :: request                         !! Last request sent to sensor.
        type(tty_type)     :: tty                             !! TTY type for serial connection.
    contains
        ! Public class methods.
        procedure, public :: baud_rate    => geocom_baud_rate
        procedure, public :: close        => geocom_close
        procedure, public :: code         => geocom_code
        procedure, public :: error        => geocom_error
        procedure, public :: last_request => geocom_last_request
        procedure, public :: message      => geocom_message
        procedure, public :: open         => geocom_open
        procedure, public :: path         => geocom_path
        procedure, public :: send         => geocom_send

        ! Public GeoCOM-specific methods.
        procedure, public :: abort_download     => geocom_abort_download
        procedure, public :: abort_list         => geocom_abort_list
        procedure, public :: beep_alarm         => geocom_beep_alarm
        procedure, public :: beep_normal        => geocom_beep_normal
        procedure, public :: beep_off           => geocom_beep_off
        procedure, public :: beep_on            => geocom_beep_on
        procedure, public :: change_face        => geocom_change_face
        procedure, public :: delete             => geocom_delete
        procedure, public :: do_measure         => geocom_do_measure
        procedure, public :: download           => geocom_download
        procedure, public :: fine_adjust        => geocom_fine_adjust
        procedure, public :: get_angle          => geocom_get_angle
        procedure, public :: get_angle_complete => geocom_get_angle_complete
        procedure, public :: null               => geocom_null
    end type geocom_class

    ! Private procedures.
    private :: geocom_close
    private :: geocom_code
    private :: geocom_error
    private :: geocom_baud_rate
    private :: geocom_path
    private :: geocom_last_request
    private :: geocom_message
    private :: geocom_open
    private :: geocom_send

    ! Private GeoCOM procedures.
    private :: geocom_abort_download
    private :: geocom_abort_list
    private :: geocom_beep_alarm
    private :: geocom_beep_normal
    private :: geocom_beep_off
    private :: geocom_beep_on
    private :: geocom_change_face
    private :: geocom_delete
    private :: geocom_do_measure
    private :: geocom_download
    private :: geocom_fine_adjust
    private :: geocom_get_angle
    private :: geocom_get_angle_complete
    private :: geocom_null
contains
    ! **************************************************************************
    ! PUBLIC METHODS.
    ! **************************************************************************
    subroutine geocom_baud_rate(this, baud_rate)
        !! Returns current baud rate enumerator (`GEOCOM_COM_BAUD_RATE`) of TTY
        !! in `baud_rate`.
        class(geocom_class), intent(inout) :: this      !! GeoCOM object.
        integer,             intent(out)   :: baud_rate !! Baud rate enumerator (`GEOCOM_COM_BAUD_RATE`).

        baud_rate = this%baud
    end subroutine geocom_baud_rate

    subroutine geocom_close(this)
        !! Closes TTY connection.
        class(geocom_class), intent(inout) :: this !! GeoCOM object.

        if (dm_tty_connected(this%tty)) call dm_tty_close(this%tty)
    end subroutine geocom_close

    integer function geocom_code(this) result(grc)
        !! Returns last GeoCOM return code.
        class(geocom_class), intent(inout) :: this !! GeoCOM object.

        grc = this%grc
    end function geocom_code

    integer function geocom_error(this) result(rc)
        !! Returns last DMPACK return code.
        class(geocom_class), intent(inout) :: this !! GeoCOM object.

        rc = this%rc
    end function geocom_error

    subroutine geocom_last_request(this, request)
        !! Returns the last request sent to the sensor in `request`. If no
        !! request has been sent, the derived type is uninitialised and the time
        !! stamp has the default value.
        class(geocom_class), intent(inout) :: this    !! GeoCOM object.
        type(request_type),  intent(out)   :: request !! Last request sent to sensor.

        request = this%request
    end subroutine geocom_last_request

    function geocom_message(this, grc) result(message)
        !! Returns message associated with given GeoCOM return code `grc` as
        !! allocatable string. If no return code is passed, the one in the
        !! GeoCOM object is used instead.
        class(geocom_class), intent(inout)        :: this    !! GeoCOM object.
        integer,             intent(in), optional :: grc     !! GeoCOM return code.
        character(len=:), allocatable             :: message !! Last return code message.

        if (present(grc)) then
            message = dm_geocom_error_message(grc)
        else
            message = dm_geocom_error_message(this%grc)
        end if
    end function geocom_message

    subroutine geocom_open(this, path, baud_rate, retries, verbose, error)
        !! Opens TTY connection to robotic total station.
        !!
        !! The argument `baud_rate` must be one of the following
        !! `GEOCOM_COM_BAUD_RATE` enumerators:
        !!
        !! * `GEOCOM_COM_BAUD_2400`   –   2400 baud.
        !! * `GEOCOM_COM_BAUD_4800`   –   4800 baud.
        !! * `GEOCOM_COM_BAUD_9600`   –   9600 baud.
        !! * `GEOCOM_COM_BAUD_19200`  –  19200 baud (default).
        !! * `GEOCOM_COM_BAUD_38400`  –  38400 baud.
        !! * `GEOCOM_COM_BAUD_57600`  –  57600 baud.
        !! * `GEOCOM_COM_BAUD_115200` – 115200 baud.
        !!
        !! Argument `retries` specifies the number of attempts to make to
        !! connect to the sensor. If `verbose` is `.true.`, error messages are
        !! printed to standard error.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_EXIST` if the TTY is already connected.
        !! * `E_INVALID` if baud rate is invalid.
        !! * `E_IO` if opening the TTY failed.
        !! * `E_NOT_FOUND` if TTY at path does no exist.
        !! * `E_SYSTEM` if setting the TTY attributes or flushing the buffers failed.
        use :: dm_file, only: dm_file_exists

        integer, parameter :: WAIT_TIME = 3 !! Retry wait time in [sec].

        class(geocom_class), intent(inout)         :: this      !! GeoCOM object.
        character(len=*),    intent(in)            :: path      !! Path of TTY (for example, `/dev/ttyUSB0`).
        integer,             intent(in)            :: baud_rate !! GeoCOM baud rate enumerator (`GEOCOM_COM_BAUD_RATE`).
        integer,             intent(in),  optional :: retries   !! Number of retries.
        logical,             intent(in),  optional :: verbose   !! Print errors to standard error.
        integer,             intent(out), optional :: error     !! DMPACK error code

        integer :: i, n, rc

        tty_block: block
            rc = E_EXIST
            if (dm_tty_connected(this%tty)) then
                if (this%verbose) call dm_error_out(rc, 'TTY already connected')
                exit tty_block
            end if

            ! Initialise TTY type.
            this%grc     = GRC_OK
            this%rc      = E_NONE
            this%request = request_type()
            this%verbose = .false.
            if (present(verbose)) this%verbose = verbose

            ! Validate and set baud rate.
            this%baud = dm_geocom_type_validated(GEOCOM_COM_BAUD_RATE, baud_rate, error=rc)

            if (dm_is_error(rc)) then
                if (this%verbose) call dm_error_out(rc, 'invalid baud rate')
                exit tty_block
            end if

            ! Verify TTY device exists.
            rc = E_NOT_FOUND
            if (.not. dm_file_exists(path)) then
                if (this%verbose) call dm_error_out(rc, 'TTY ' // trim(path) // ' not found')
                exit tty_block
            end if

            n = 0
            if (present(retries)) n = max(0, retries)

            ! Try to open TTY.
            do i = 0, n
                rc = dm_tty_open(tty       = this%tty, &
                                 path      = path, &
                                 baud_rate = this%baud, &
                                 byte_size = TTY_BYTE_SIZE8, &
                                 parity    = TTY_PARITY_NONE, &
                                 stop_bits = TTY_STOP_BITS1)

                ! Exit on success.
                if (dm_is_ok(rc)) exit
                if (this%verbose) call dm_error_out(rc, 'failed to open TTY ' // trim(path))

                ! Try again.
                if (i < n) call dm_sleep(WAIT_TIME)
            end do
        end block tty_block

        this%rc = rc
        if (present(error)) error = rc
    end subroutine geocom_open

    subroutine geocom_path(this, path)
        !! Returns TTY device path in allocatable character string `path`.
        class(geocom_class),           intent(inout) :: this !! GeoCOM object.
        character(len=:), allocatable, intent(out)   :: path !! TTY device path.

        path = trim(this%tty%path)
    end subroutine geocom_path

    subroutine geocom_send(this, request, delay, error)
        !! Sends request to configured TTY.
        use :: dm_regex, only: dm_regex_request
        use :: dm_time,  only: dm_time_now

        class(geocom_class), intent(inout)         :: this    !! GeoCOM object.
        type(request_type),  intent(inout)         :: request !! Request to send.
        integer,             intent(in),  optional :: delay   !! Request delay [msec].
        integer,             intent(out), optional :: error   !! DMPACK error code

        integer :: rc

        this%grc = GRC_UNDEFINED

        tty_block: block
            ! Verify that TTY is not connected yet.
            rc = E_IO
            if (.not. dm_tty_connected(this%tty)) then
                if (this%verbose) call dm_error_out(rc, 'TTY not connected')
                exit tty_block
            end if

            ! Set initial response errors.
            rc = dm_request_set_response_error(request, E_INCOMPLETE)

            if (dm_is_error(rc)) then
                if (this%verbose) call dm_error_out(rc, 'failed to initialize responses')
                exit tty_block
            end if

            ! Prepare request.
            request%timestamp = dm_time_now()

            ! Send request to sensor.
            rc = dm_tty_write(this%tty, request, flush=.true.)

            if (dm_is_error(rc)) then
                if (this%verbose) call dm_error_out(rc, 'failed to write to TTY')
                exit tty_block
            end if

            ! Read response from sensor.
            rc = dm_tty_read(this%tty, request)

            if (dm_is_error(rc)) then
                if (this%verbose) call dm_error_out(rc, 'failed to read from TTY')
                exit tty_block
            end if

            ! Parse raw response and extract response values.
            rc = dm_regex_request(request)

            if (dm_is_error(rc)) then
                if (this%verbose) call dm_error_out(rc, 'regular expression pattern does not match')
                exit tty_block
            end if

            ! Get GeoCOM return code from response.
            call dm_request_get(request, 'grc', this%grc)

            ! Wait additional delay.
            if (present(delay)) call dm_usleep(max(0, delay * 1000))
        end block tty_block

        this%rc      = rc
        this%request = request

        if (present(error)) error = rc
    end subroutine geocom_send

    ! **************************************************************************
    ! PUBLIC GEOCOM METHODS.
    ! **************************************************************************
    subroutine geocom_abort_download(this, delay)
        !! Sends *FTR_AbortDownload* request to sensor. Aborts or ends the file
        !! download command.
        class(geocom_class), intent(inout)        :: this  !! GeoCOM object.
        integer,             intent(in), optional :: delay !! Request delay [msec].

        type(request_type) :: request

        call dm_geocom_api_request_abort_download(request)
        call this%send(request, delay)
    end subroutine geocom_abort_download

    subroutine geocom_abort_list(this, delay)
        !! Sends *FTR_AbortList* request to sensor. Aborts or ends the file
        !! list command.
        class(geocom_class), intent(inout)        :: this  !! GeoCOM object.
        integer,             intent(in), optional :: delay !! Request delay [msec].

        type(request_type) :: request

        call dm_geocom_api_request_abort_list(request)
        call this%send(request, delay)
    end subroutine geocom_abort_list

    subroutine geocom_beep_alarm(this, delay)
        !! Sends *BMM_BeepAlarm* request to sensor. Outputs an alarm signal
        !! (triple beep).
        class(geocom_class), intent(inout)        :: this  !! GeoCOM object.
        integer,             intent(in), optional :: delay !! Request delay [msec].

        type(request_type) :: request

        call dm_geocom_api_request_beep_alarm(request)
        call this%send(request, delay)
    end subroutine geocom_beep_alarm

    subroutine geocom_beep_normal(this, delay)
        !! Sends *BMM_BeepNormal* request to sensor. Outputs an alarm signal
        !! (single beep).
        class(geocom_class), intent(inout)        :: this  !! GeoCOM object.
        integer,             intent(in), optional :: delay !! Request delay [msec].

        type(request_type) :: request

        call dm_geocom_api_request_beep_normal(request)
        call this%send(request, delay)
    end subroutine geocom_beep_normal

    subroutine geocom_beep_off(this, delay)
        !! Sends *IOS_BeepOff* request to sensor. Stops an active beep signal.
        class(geocom_class), intent(inout)        :: this  !! GeoCOM object.
        integer,             intent(in), optional :: delay !! Request delay [msec].

        type(request_type) :: request

        call dm_geocom_api_request_beep_off(request)
        call this%send(request, delay)
    end subroutine geocom_beep_off

    subroutine geocom_beep_on(this, intensity, delay)
        !! Sends *IOS_BeepOn* request to sensor. Outputs continuous beep signal
        !! of given intensity (between 0 and 100).  If no intensity is given,
        !! the default `GEOCOM_IOS_BEEP_STDINTENS` is used.
        class(geocom_class), intent(inout)        :: this      !! GeoCOM object.
        integer,             intent(in), optional :: intensity !! Intensity of beep, from 0 to 100.
        integer,             intent(in), optional :: delay     !! Request delay [msec].

        integer            :: intensity_
        type(request_type) :: request

        intensity_ = GEOCOM_IOS_BEEP_STDINTENS
        if (present(intensity)) intensity_ = max(0, min(100, intensity))

        call dm_geocom_api_request_beep_on(request, intensity_)
        call this%send(request, delay)
    end subroutine geocom_beep_on

    subroutine geocom_change_face(this, pos_mode, atr_mode, delay)
        !! Sends *AUT_ChangeFace* request to sensor. Turns the telescope to the
        !! other face.
        !!
        !! If `pos_mode` is `GEOCOM_AUT_NORMAL`, uses the current value of the
        !! compensator. For positioning distances > 25 gon, this mode might tend
        !! to inaccuracy. If set to `GEOCOM_AUT_PRECISE`, tries to measure the exact
        !! inclination of the target. Tends to long position time.
        !!
        !! If `atr_mode` is `GEOCOM_AUT_POSITION`, uses conventional position to
        !! other face. If set to `GEOCOM_AUT_TARGET`, tries to position into a
        !! target in the destination area. This mode requires activated ATR.
        class(geocom_class), intent(inout)        :: this     !! GeoCOM object.
        integer,             intent(in)           :: pos_mode !! Position mode (`GEOCOM_AUT_POSMODE`).
        integer,             intent(in)           :: atr_mode !! ATR mode (`GEOCOM_AUT_ATRMODE`).
        integer,             intent(in), optional :: delay    !! Request delay [msec].

        integer            :: atr_mode_, pos_mode_
        type(request_type) :: request

        pos_mode_ = dm_geocom_type_validated(GEOCOM_AUT_POSMODE, pos_mode)
        atr_mode_ = dm_geocom_type_validated(GEOCOM_AUT_ATRMODE, atr_mode)

        call dm_geocom_api_request_change_face(request, pos_mode_, atr_mode_)
        call this%send(request, delay)
    end subroutine geocom_change_face

    subroutine geocom_delete(this, device_type, file_type, day, month, year, file_name, nfiles, delay)
        !! Sends *FTR_Delete* request to sensor. Deletes one or more files.
        !!
        !! Wildcards may be used to delete multiple files. If the deletion date
        !! is valid, only files older than the date are deleted. The number of
        !! files deleted is returned in `nfiles`.
        class(geocom_class), intent(inout)         :: this        !! GeoCOM object.
        integer,             intent(in)            :: device_type !! Internal memory or memory card (`GEOCOM_FTR_DEVICETYPE`).
        integer,             intent(in)            :: file_type   !! Type of file (`GEOCOM_FTR_FILETYPE`).
        integer,             intent(in)            :: day         !! Day of month (`DD`).
        integer,             intent(in)            :: month       !! Month (`MM`).
        integer,             intent(in)            :: year        !! Year (`YY`).
        character(len=*),    intent(in)            :: file_name   !! Name of file to delete.
        integer,             intent(out), optional :: nfiles      !! Number of files deleted.
        integer,             intent(in),  optional :: delay       !! Request delay [msec].

        integer            :: device_type_, file_type_
        integer            :: day_, month_, year_
        type(request_type) :: request

        device_type_ = dm_geocom_type_validated(GEOCOM_FTR_DEVICETYPE, device_type)
        file_type_   = dm_geocom_type_validated(GEOCOM_FTR_FILETYPE,   file_type)

        day_   = max(0, min(255, day))
        month_ = max(0, min(255, month))
        year_  = max(0, min(255, year))

        if (present(nfiles)) nfiles = 0

        call dm_geocom_api_request_delete(request, device_type_, file_type_, day_, month_, year_, file_name)
        call this%send(request, delay)

        if (present(nfiles)) call dm_request_get(this%request, 'nfiles', nfiles)
    end subroutine geocom_delete

    subroutine geocom_do_measure(this, tmc_prog, inc_mode, delay)
        !! Sends *TMC_DoMeasure* request to sensor. The API procedure tries a
        !! distance measurement. This command does not return any values.
        !!
        !! The argument `tmc_prog` must be one of the following TMC measurement
        !! modes:
        !!
        !! * `GEOCOM_TMC_STOP`
        !! * `GEOCOM_TMC_DEF_DIST`
        !! * `GEOCOM_TMC_CLEAR`
        !! * `GEOCOM_TMC_SIGNAL`
        !! * `GEOCOM_TMC_DO_MEASURE`
        !! * `GEOCOM_TMC_RTRK_DIST`
        !! * `GEOCOM_TMC_RED_TRK_DIST`
        !! * `GEOCOM_TMC_FREQUENCY`
        !!
        !! The argument `inc_mode` must be one of the following inclination
        !! measurement modes:
        !!
        !! * `GEOCOM_TMC_MEA_INC`
        !! * `GEOCOM_TMC_AUTO_INC`
        !! * `GEOCOM_TMC_PLANE_INC`
        !!
        !! If a distance measurement is performed in measurement program
        !! `GEOCOM_TMC_DEF_DIST`, the distance sensor will work with the set
        !! EDM mode.
        !!
        !! This function sets measurement program `GEOCOM_TMC_DEF_DIST` and
        !! inclination mode `GEOCOM_TMC_MEA_INC` by default.
        class(geocom_class), intent(inout)        :: this     !! GeoCOM object.
        integer,             intent(in)           :: tmc_prog !! TMC measurement program (`GEOCOM_TMC_MEASURE_PRG`).
        integer,             intent(in), optional :: inc_mode !! Inclination measurement mode (`GEOCOM_TMC_INCLINE_PRG`).
        integer,             intent(in), optional :: delay    !! Request delay [msec].

        integer            :: inc_mode_, tmc_prog_
        type(request_type) :: request

        tmc_prog_ = dm_geocom_type_validated(GEOCOM_TMC_MEASURE_PRG, tmc_prog)
        inc_mode_ = GEOCOM_TMC_MEA_INC

        if (present(inc_mode)) inc_mode_ = dm_geocom_type_validated(GEOCOM_TMC_INCLINE_PRG, inc_mode)

        call dm_geocom_api_request_do_measure(request, tmc_prog_, inc_mode_)
        call this%send(request, delay)
    end subroutine geocom_do_measure

    subroutine geocom_download(this, block_number, block_value, block_length, delay)
        !! Sends *FTR_Download* request to sensor. Reads a single block of
        !! data. The *FTR_SetupDownload* command has to be called first.
        !!
        !! The block sequence starts with 1. The download process will be
        !! aborted if the block number is 0.
        !!
        !! The maximum block number is 65535. The file size is therefore
        !! limited to 28 MiB.
        !!
        !! On error, `block_value` is `NULL`, and `block_length` is 0.
        class(geocom_class), intent(inout)        :: this         !! GeoCOM object.
        integer,             intent(in)           :: block_number !! Block number, from 0 to 65535.
        character,           intent(out)          :: block_value  !! Block value [byte].
        integer,             intent(out)          :: block_length !! Block length.
        integer,             intent(in), optional :: delay        !! Request delay [msec].

        integer            :: block_number_
        type(request_type) :: request

        block_value   = achar(0)
        block_length  = 0
        block_number_ = max(0, min(65535, block_number))

        call dm_geocom_api_request_download(request, block_number_)
        call this%send(request, delay)

        call dm_request_get(this%request, 'blockval', block_value)
        call dm_request_get(this%request, 'blocklen', block_length)
    end subroutine geocom_download

    subroutine geocom_fine_adjust(this, search_hz, search_v, delay)
        !! Sends *AUT_FineAdjust* request to sensor to perform automatic target
        !! positioning.
        !!
        !! The procedure positions the telescope onto the target prosm and
        !! measures the ATR Hz and V deviations. If the target is not within
        !! the visible area of the ATR sensor (field of view), a target search
        !! will be executed. The target search range is limited by the
        !! parameter `search_v` in V direction, and by parameter `search_hz` in
        !! Hz direction. If no target was found, the instrument turns back to
        !! the initial start position.
        !!
        !! The Fine Adjust Lock-in towards a target is terminated by this
        !! procedure call. After positioning, the LOCK mode will be active. The
        !! timeout of the operation is set to 5 seconds, regardless of the
        !! general position timeout settings. The position tolerance depends on
        !! the previously selected find adjust mode.
        !!
        !! The tolerance settings have no influence to this operation. The
        !! tolerance settings and the ATR precision depend on the instrument
        !! class and the used EDM mode.
        class(geocom_class), intent(inout)        :: this      !! GeoCOM object.
        real(kind=r8),       intent(in)           :: search_hz !! Search range, Hz axis [rad].
        real(kind=r8),       intent(in)           :: search_v  !! Search range, V axis [rad].
        integer,             intent(in), optional :: delay     !! Request delay [msec].

        type(request_type) :: request

        call dm_geocom_api_request_fine_adjust(request, search_hz, search_v)
        call this%send(request, delay)
    end subroutine geocom_fine_adjust

    subroutine geocom_get_angle(this, hz, v, inc_mode, delay)
        !! Sends *TMC_GetAngle5* request to sensor. Starts an angle measurement
        !! and returns the results. This function sets inclination mode
        !! `GEOCOM_TMC_MEA_INC` by default.
        class(geocom_class), intent(inout)        :: this     !! GeoCOM object.
        real(kind=r8),       intent(out)          :: hz       !! Horizontal angle [rad].
        real(kind=r8),       intent(out)          :: v        !! Vertical angle [rad].
        integer,             intent(in), optional :: inc_mode !! Inclination measurement mode (`GEOCOM_TMC_INCLINE_PRG`).
        integer,             intent(in), optional :: delay    !! Request delay [msec].

        integer            :: inc_mode_
        type(request_type) :: request

        inc_mode_ = GEOCOM_TMC_MEA_INC
        if (present(inc_mode)) inc_mode_ = dm_geocom_type_validated(GEOCOM_TMC_INCLINE_PRG, inc_mode)

        call dm_geocom_api_request_get_angle(request, inc_mode)
        call this%send(request, delay)

        call dm_request_get(this%request, 'hz', hz)
        call dm_request_get(this%request, 'v',  v)
    end subroutine geocom_get_angle

    subroutine geocom_get_angle_complete(this, hz, v, angle_accuracy, angle_time, trans_inc, long_inc, &
                                         inc_accuracy, inc_time, face, inc_mode, delay)
        !! Sends *TMC_GetAngle1* request to sensor. Performs a complete angle
        !! measurement. The function starts an angle and, depending on the
        !! configuration, an inclination measurement, and returns the results.
        !! This function sets inclination mode `GEOCOM_TMC_MEA_INC` by default.
        class(geocom_class), intent(inout)         :: this           !! GeoCOM object.
        real(kind=r8),       intent(out)           :: hz             !! Horizontal angle [rad].
        real(kind=r8),       intent(out)           :: v              !! Vertical angle [rad].
        real(kind=r8),       intent(out), optional :: angle_accuracy !! Accuracy of angles [rad].
        integer(kind=i8),    intent(out), optional :: angle_time     !! Moment of measurement [ms].
        real(kind=r8),       intent(out), optional :: trans_inc      !! Transverse axis inclination [rad].
        real(kind=r8),       intent(out), optional :: long_inc       !! Longitude axis inclidation [rad].
        real(kind=r8),       intent(out), optional :: inc_accuracy   !! Inclination accuracy [rad].
        integer(kind=i8),    intent(out), optional :: inc_time       !! Moment of measurement [ms].
        integer,             intent(out), optional :: face           !! Face position of telescope (`GEOCOM_TMC_FACE`).
        integer,             intent(in),  optional :: inc_mode       !! Inclination measurement mode (`GEOCOM_TMC_INCLINE_PRG`).
        integer,             intent(in),  optional :: delay          !! Request delay [msec].

        integer            :: inc_mode_
        type(request_type) :: request

        hz = 0.0_r8
        v  = 0.0_r8

        if (present(angle_accuracy)) angle_accuracy = 0.0_r8
        if (present(angle_time))     angle_time     = 0_i8
        if (present(trans_inc))      trans_inc      = 0.0_r8
        if (present(long_inc))       long_inc       = 0.0_r8
        if (present(inc_accuracy))   inc_accuracy   = 0.0_r8
        if (present(inc_time))       inc_time       = 0_i8
        if (present(face))           face           = 0

        inc_mode_ = GEOCOM_TMC_MEA_INC
        if (present(inc_mode)) inc_mode_ = dm_geocom_type_validated(GEOCOM_TMC_INCLINE_PRG, inc_mode)

        call dm_geocom_api_request_get_angle_complete(request, inc_mode_)
        call this%send(request, delay)

        call dm_request_get(this%request, 'hz', hz)
        call dm_request_get(this%request, 'v',  v)

        if (present(angle_accuracy)) call dm_request_get(this%request, 'angacc',  angle_accuracy)
        if (present(angle_time))     call dm_request_get(this%request, 'angtime', angle_time)
        if (present(trans_inc))      call dm_request_get(this%request, 'xinc',    trans_inc)
        if (present(long_inc))       call dm_request_get(this%request, 'linc',    long_inc)
        if (present(inc_accuracy))   call dm_request_get(this%request, 'incacc',  inc_accuracy)
        if (present(inc_time))       call dm_request_get(this%request, 'inctime', inc_time)
        if (present(face))           call dm_request_get(this%request, 'face',    face)
    end subroutine geocom_get_angle_complete

    subroutine geocom_null(this, delay)
        !! Sends *COM_NullProc* request to sensor. API call for checking the
        !! communication.
        class(geocom_class), intent(inout)        :: this  !! GeoCOM object.
        integer,             intent(in), optional :: delay !! Request delay [msec].

        type(request_type) :: request

        call dm_geocom_api_request_null(request)
        call this%send(request, delay)
    end subroutine geocom_null
end module dm_geocom
