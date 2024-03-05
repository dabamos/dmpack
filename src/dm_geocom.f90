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
    !! References to the official API are made in the procedure descriptions.
    !!
    !! ```fortran
    !! integer            :: grc    ! GeoCOM return code.
    !! integer            :: rc     ! DMPACK return code.
    !! type(geocom_class) :: geocom ! GeoCOM object.
    !!
    !! call geocom%open('/dev/ttyUSB0', TTY_B115200, nretries=1, error=rc)
    !! if (dm_is_error(rc)) stop
    !!
    !! grc = geocom%beep_normal()
    !! print '(i0, ": ", a)', grc, geocom%message()
    !!
    !! call geocom%close()
    !! ```
    use :: dm_error
    use :: dm_file
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
        integer            :: grc     = GRC_OK !! Last GeoCOM return code.
        logical            :: verbose = .true. !! Print error messages to stderr.
        type(request_type) :: request          !! Last request.
        type(tty_type)     :: tty              !! TTY type for serial connection to sensor.
    contains
        ! Public class methods.
        procedure, public :: close       => geocom_close
        procedure, public :: code        => geocom_code
        procedure, public :: message     => geocom_message
        procedure, public :: open        => geocom_open
        procedure, public :: send        => geocom_send
        ! Public GeoCOM-specific methods.
        procedure, public :: beep_alarm  => geocom_beep_alarm
        procedure, public :: beep_normal => geocom_beep_normal
        procedure, public :: beep_off    => geocom_beep_off
        procedure, public :: beep_on     => geocom_beep_on
    end type geocom_class

    ! Private procedures.
    private :: geocom_close
    private :: geocom_code
    private :: geocom_message
    private :: geocom_open
    private :: geocom_send

    ! Private GeoCOM procedures.
    ! private :: geocom_abort_download()
    ! private :: geocom_abort_list()
    private :: geocom_beep_alarm
    private :: geocom_beep_normal
    private :: geocom_beep_off
    private :: geocom_beep_on
contains
    ! **************************************************************************
    ! PUBLIC METHODS.
    ! **************************************************************************
    integer function geocom_code(this) result(grc)
        !! Returns last GeoCOM return code.
        class(geocom_class), intent(inout) :: this !! GeoCOM object.

        grc = this%grc
    end function geocom_code

    function geocom_message(this, grc) result(str)
        !! Returns message associated with given GeoCOM return code `grc` as
        !! allocatable string. If no return code is passed, the one in the
        !! GeoCOM object is used instead.
        class(geocom_class), intent(inout)        :: this !! GeoCOM object.
        integer,             intent(in), optional :: grc  !! GeoCOM return code.
        character(len=:), allocatable             :: str  !! Last return code message.

        if (present(grc)) then
            str = dm_geocom_error_message(grc)
            return
        end if

        str = dm_geocom_error_message(this%grc)
    end function geocom_message

    subroutine geocom_close(this)
        !! Closes TTY connection.
        class(geocom_class), intent(inout) :: this !! GeoCOM object.

        if (dm_tty_connected(this%tty)) call dm_tty_close(this%tty)
    end subroutine geocom_close

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
        class(geocom_class), intent(inout)         :: this      !! GeoCOM object.
        character(len=*),    intent(in)            :: path      !! Path of TTY.
        integer,             intent(in)            :: baud_rate !! Baud rate value.
        integer,             intent(in),  optional :: retries   !! Number of retries
        integer,             intent(out), optional :: error     !! DMPACK error code

        integer :: baud, i, retries_, rc

        i = 0
        retries_ = 0
        if (present(retries)) retries_ = retries

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
                    continue
                case default
                    exit tty_block
            end select

            rc = E_NOT_FOUND
            if (.not. dm_file_exists(path)) exit tty_block

            rc = E_EXIST
            if (dm_tty_connected(this%tty)) exit tty_block

            do
                ! Try to open TTY.
                if (i > retries_) exit
                rc = dm_tty_open(this%tty, path, baud, TTY_BYTE_SIZE8, TTY_PARITY_NONE, TTY_STOP_BITS1)
                if (dm_is_ok(rc)) exit
                call dm_sleep(1)
            end do
        end block tty_block

        if (present(error)) error = rc
    end subroutine geocom_open

    subroutine geocom_send(this, request, error)
        !! Sends request to configured TTY.
        class(geocom_class), intent(inout)         :: this    !! GeoCOM object.
        type(request_type),  intent(inout)         :: request !! Request to send.
        integer,             intent(out), optional :: error   !! DMPACK error code

        integer :: grc, rc

        this%grc = GRC_UNDEFINED
        this%request = request

        tty_block: block
            rc = E_IO
            if (.not. dm_tty_connected(this%tty)) exit tty_block

            ! Send request to sensor.
            rc = dm_tty_write(this%tty, this%request, flush=.true.)
            if (dm_is_error(rc)) exit tty_block

            ! Get GeoCOM return code from response.
            call dm_request_get(request, 'grc', grc, error=rc)
            if (dm_is_ok(rc)) this%grc = grc
        end block tty_block

        if (present(error)) error = rc
    end subroutine geocom_send

    ! **************************************************************************
    ! PUBLIC GEOCOM METHODS.
    ! **************************************************************************
    integer function geocom_beep_alarm(this) result(grc)
        !! BMM_BeepAlarm
        !!
        !! Sends request to output an alarm signal (triple beep).
        class(geocom_class), intent(inout) :: this !! GeoCOM object.
        type(request_type)                 :: request

        call dm_geocom_api_request_beep_alarm(request)
        call this%send(request)
        grc = this%grc
    end function geocom_beep_alarm

    integer function geocom_beep_normal(this) result(grc)
        !! BMM_BeepNormal
        !!
        !! Sends request to output an alarm signal (single beep).
        class(geocom_class), intent(inout) :: this !! GeoCOM object.
        type(request_type)                 :: request

        call dm_geocom_api_request_beep_normal(request)
        call this%send(request)
        grc = this%grc
    end function geocom_beep_normal

    integer function geocom_beep_off(this) result(grc)
        !! IOS_BeepOff
        !!
        !! Sends request to stop an active beep signal.
        class(geocom_class), intent(inout) :: this !! GeoCOM object.
        type(request_type)                 :: request

        call dm_geocom_api_request_beep_off(request)
        call this%send(request)
        grc = this%grc
    end function geocom_beep_off

    integer function geocom_beep_on(this, intensity) result(grc)
        !! IOS_BeepOn
        !!
        !! Sends request to start a continuous beep signal of given intensity.
        !!
        !! The optional intensity must be between 0 and 100. If no intensity
        !! is passed, the default (`GEOCOM_IOS_BEEP_STDINTENS`) is used.
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
end module dm_geocom
