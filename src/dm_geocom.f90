! Author:  Philipp Engel
! Licence: ISC
module dm_geocom
    !! Unfinished object-oriented GeoCOM API for Fortran.
    !!
    !! The API provided by DMPACK does not follow the official Leica GeoCOM API
    !! for C/C++ and Visual Basic. Structured types and functions are simplified
    !! and given more memorable names. Function names do not contain a sub-system
    !! prefix.
    !!
    !! The following example opens the TTY `/dev/ttyUSB0` at 115,200 baud and
    !! sends a beep request to the instrument:
    !!
    !! ```fortran
    !! integer :: grc ! GeoCOM return code.
    !! integer :: rc  ! DMPACK return code.
    !!
    !! type(geocom_class) :: geocom ! GeoCOM object.
    !!
    !! call geocom%open('/dev/ttyUSB0', GEOCOM_COM_BAUD_115200, retries=1, error=rc)
    !!
    !! if (dm_is_error(rc)) then
    !!     dm_error_out(rc)
    !!     stop
    !! end if
    !!
    !! grc = geocom%beep_normal()
    !! print '(i0, ": ", a)', grc, geocom%message()
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
        !! public methods.
        private
        integer            :: rc      = E_NONE !! Last DMPACK return code.
        integer            :: grc     = GRC_OK !! Last GeoCOM return code.
        logical            :: verbose = .true. !! Print error messages to stderr.
        type(request_type) :: request          !! Last request.
        type(tty_type)     :: tty              !! TTY type for serial connection to sensor.
    contains
        ! Public class methods.
        procedure, public :: close   => geocom_close
        procedure, public :: code    => geocom_code
        procedure, public :: error   => geocom_error
        procedure, public :: message => geocom_message
        procedure, public :: open    => geocom_open
        procedure, public :: send    => geocom_send

        ! Public GeoCOM-specific methods.
        procedure, public :: beep_alarm  => geocom_beep_alarm
        procedure, public :: beep_normal => geocom_beep_normal
        procedure, public :: beep_off    => geocom_beep_off
        procedure, public :: beep_on     => geocom_beep_on
        procedure, public :: null        => geocom_null
    end type geocom_class

    ! Private procedures.
    private :: geocom_close
    private :: geocom_code
    private :: geocom_error
    private :: geocom_last_request
    private :: geocom_message
    private :: geocom_open
    private :: geocom_send

    ! Private GeoCOM procedures.
    private :: geocom_beep_alarm
    private :: geocom_beep_normal
    private :: geocom_beep_off
    private :: geocom_beep_on
    private :: geocom_null
contains
    ! **************************************************************************
    ! PUBLIC METHODS.
    ! **************************************************************************
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

    subroutine geocom_close(this)
        !! Closes TTY connection.
        class(geocom_class), intent(inout) :: this !! GeoCOM object.

        if (dm_tty_connected(this%tty)) call dm_tty_close(this%tty)
    end subroutine geocom_close

    subroutine geocom_last_request(this, request)
        !! Returns the last request sent to the sensor in `request`. If no
        !! request has been sent, the derived type is uninitialised and the time
        !! stamp has the default value.
        class(geocom_class), intent(inout) :: this    !! GeoCOM object.
        type(request_type),  intent(out)   :: request !! Last request sent to sensor.

        request = this%request
    end subroutine geocom_last_request

    subroutine geocom_open(this, path, baud_rate, retries, error)
        !! Opens TTY connection to robotic total station.
        !!
        !! The argument `baud_rate` must be one of the following:
        !!
        !! * `GEOCOM_COM_BAUD_2400`
        !! * `GEOCOM_COM_BAUD_4800`
        !! * `GEOCOM_COM_BAUD_9600`
        !! * `GEOCOM_COM_BAUD_19200`
        !! * `GEOCOM_COM_BAUD_38400`
        !! * `GEOCOM_COM_BAUD_57600`
        !! * `GEOCOM_COM_BAUD_115200`
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_EXIST` if the TTY is already connected.
        !! * `E_INVALID` if baud rate is invalid.
        !! * `E_NOT_FOUND` if path does no exist.
        use :: dm_file, only: dm_file_exists

        class(geocom_class), intent(inout)         :: this      !! GeoCOM object.
        character(len=*),    intent(in)            :: path      !! Path of TTY.
        integer,             intent(in)            :: baud_rate !! Baud rate value.
        integer,             intent(in),  optional :: retries   !! Number of retries
        integer,             intent(out), optional :: error     !! DMPACK error code

        integer :: baud, i, retries_, rc

        i = 0
        retries_ = 0
        if (present(retries)) retries_ = max(0, retries)

        tty_block: block
            rc = E_INVALID
            select case (baud_rate)
                case (GEOCOM_COM_BAUD_2400)
                    baud = TTY_B2400
                case (GEOCOM_COM_BAUD_4800)
                    baud = TTY_B4800
                case (GEOCOM_COM_BAUD_9600)
                    baud = TTY_B9600
                case (GEOCOM_COM_BAUD_19200)
                    baud = TTY_B19200
                case (GEOCOM_COM_BAUD_38400)
                    baud = TTY_B38400
                case (GEOCOM_COM_BAUD_57600)
                    baud = TTY_B57600
                case (GEOCOM_COM_BAUD_115200)
                    baud = TTY_B115200
                case default
                    exit tty_block
            end select

            rc = E_NOT_FOUND
            if (.not. dm_file_exists(path)) exit tty_block

            rc = E_EXIST
            if (dm_tty_connected(this%tty)) exit tty_block

            do
                if (i > retries_) exit

                ! Try to open TTY.
                rc = dm_tty_open(tty       = this%tty, &
                                 path      = path, &
                                 baud_rate = baud, &
                                 byte_size = TTY_BYTE_SIZE8, &
                                 parity    = TTY_PARITY_NONE, &
                                 stop_bits = TTY_STOP_BITS1)
                if (dm_is_ok(rc)) exit

                ! Re-try in 3 seconds.
                i = i + 1
                call dm_sleep(3)
            end do
        end block tty_block

        this%rc = rc
        if (present(error)) error = rc
    end subroutine geocom_open

    subroutine geocom_send(this, request, error)
        !! Sends request to configured TTY.
        use :: dm_regex
        use :: dm_time

        class(geocom_class), intent(inout)         :: this    !! GeoCOM object.
        type(request_type),  intent(inout)         :: request !! Request to send.
        integer,             intent(out), optional :: error   !! DMPACK error code

        integer :: grc, rc

        this%grc = GRC_UNDEFINED

        tty_block: block
            ! Prepare request.
            request%timestamp = dm_time_now()

            rc = E_IO
            if (.not. dm_tty_connected(this%tty)) exit tty_block

            ! Set initial response errors.
            rc = dm_request_set_response_error(request, E_INCOMPLETE)
            if (dm_is_error(rc)) exit tty_block

            ! Send request to sensor.
            rc = dm_tty_write(this%tty, request, flush=.true.)
            if (dm_is_error(rc)) exit tty_block

            ! Read response from sensor.
            rc = dm_tty_read(this%tty, request)
            if (dm_is_error(rc)) exit tty_block

            ! Parse raw response and extract response values.
            rc = dm_regex_request(request)
            if (dm_is_error(rc)) exit tty_block

            ! Get GeoCOM return code from response.
            call dm_request_get(request, 'grc', grc, status=rc)
            if (dm_is_ok(rc)) this%grc = grc
        end block tty_block

        this%rc      = rc
        this%request = request

        if (present(error)) error = rc
    end subroutine geocom_send

    ! **************************************************************************
    ! PUBLIC GEOCOM METHODS.
    ! **************************************************************************
    integer function geocom_beep_alarm(this) result(grc)
        class(geocom_class), intent(inout) :: this !! GeoCOM object.

        type(request_type) :: request

        call dm_geocom_api_request_beep_alarm(request)
        call this%send(request)
        grc = this%grc
    end function geocom_beep_alarm

    integer function geocom_beep_normal(this) result(grc)
        class(geocom_class), intent(inout) :: this !! GeoCOM object.

        type(request_type) :: request

        call dm_geocom_api_request_beep_normal(request)
        call this%send(request)
        grc = this%grc
    end function geocom_beep_normal

    integer function geocom_beep_off(this) result(grc)
        class(geocom_class), intent(inout) :: this !! GeoCOM object.

        type(request_type) :: request

        call dm_geocom_api_request_beep_off(request)
        call this%send(request)
        grc = this%grc
    end function geocom_beep_off

    integer function geocom_beep_on(this, intensity) result(grc)
        class(geocom_class), intent(inout)        :: this      !! GeoCOM object.
        integer,             intent(in), optional :: intensity !! Intensity of beep.

        integer            :: intensity_
        type(request_type) :: request

        intensity_ = GEOCOM_IOS_BEEP_STDINTENS
        if (present(intensity)) intensity_ = max(0, min(100, intensity))

        call dm_geocom_api_request_beep_on(request, intensity_)
        call this%send(request)
        grc = this%grc
    end function geocom_beep_on

    integer function geocom_null(this) result(grc)
        class(geocom_class), intent(inout) :: this !! GeoCOM object.

        type(request_type) :: request

        call dm_geocom_api_request_null(request)
        call this%send(request)
        grc = this%grc
    end function geocom_null
end module dm_geocom
