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
    !!
    !! ## API
    !!
    !! | GeoCOM API                  | DMPACK API                      |
    !! |-----------------------------|---------------------------------|
    !! | `AUT_ChangeFace`            | `change_face`                   |
    !! | `AUT_FineAdjust`            | `fine_adjust`                   |
    !! | `AUT_GetFineAdjustMode`     | `get_fine_adjust_mode`          |
    !! | `AUT_GetSearchArea`         | `get_search_area`               |
    !! | `AUT_MakePositioning`       | `set_position`                  |
    !! | `BAP_GetATRSetting`         | `get_atr_setting`               |
    !! | `BAP_GetMeasPrg`            | `get_measurement_program`       |
    !! | `BAP_GetPrismDef`           | `get_prism_definition`          |
    !! | `BAP_GetRedATRFov`          | `get_reduced_atr_fov`           |
    !! | `BAP_GetUserPrismDef`       | `get_user_prism_definition`     |
    !! | `BMM_BeepAlarm`             | `beep_alarm`                    |
    !! | `BMM_BeepNormal`            | `beep_normal`                   |
    !! | `COM_GetBinaryAvailable`    | `get_binary_mode`               |
    !! | `COM_GetDoublePrecision`    | `get_double_precision`          |
    !! | `COM_NullProc`              | `null`                          |
    !! | `COM_SwitchOffTPS`          | `switch_off`                    |
    !! | `COM_SwitchOffTPS`          | `switch_on`                     |
    !! | `CSV_CheckPower`            | `get_power`                     |
    !! | `CSV_GetDateTimeCentiSec`   | `get_date_time_centi`           |
    !! | `CSV_GetDateTime`           | `get_date_time`                 |
    !! | `CSV_GetDeviceConfig`       | `get_device_config`             |
    !! | `CSV_GetInstrumentName`     | `get_instrument_name`           |
    !! | `CSV_GetInstrumentNo`       | `get_instrument_number`         |
    !! | `CSV_GetIntTemp`            | `get_internal_temperature`      |
    !! | `CSV_GetReflectorlessClass` | `get_reflectorless_class`       |
    !! | `EDM_GetEglIntensity`       | `get_egl_intensity`             |
    !! | `FTR_AbortDownload`         | `abort_download`                |
    !! | `FTR_AbortList`             | `abort_list`                    |
    !! | `FTR_Delete`                | `delete`                        |
    !! | `FTR_Download`              | `download`                      |
    !! | `IMG_GetTccConfig`          | `get_image_config`              |
    !! | `IMG_TakeTccImage`          | `take_image`                    |
    !! | `IOS_BeepOff`               | `beep_off`                      |
    !! | `IOS_BeepOn`                | `beep_on`                       |
    !! | `MOT_ReadLockStatus`        | `get_lock_status`               |
    !! | `SUP_GetConfig`             | `get_config`                    |
    !! | `TMC_DoMeasure`             | `do_measure`                    |
    !! | `TMC_GeoPpm`                | `get_geometric_ppm`             |
    !! | `TMC_GetAngSwitch`          | `get_angular_correction_status` |
    !! | `TMC_GetAngle1`             | `get_angle_complete`            |
    !! | `TMC_GetAngle5`             | `get_angle`                     |
    !! | `TMC_GetAtmCorr`            | `get_atmospheric_correction`    |
    !! | `TMC_GetAtmPpm`             | `get_atmospheric_ppm`           |
    !! | `TMC_GetCoordinate`         | `get_coordinate`                |
    !! | `TMC_GetEdmMode`            | `get_edm_mode`                  |
    !! | `TMC_GetFace`               | `get_face`                      |
    !! | `TMC_GetFullMeas`           | `get_full_measurement`          |
    !! | `TMC_GetHeight`             | `get_height`                    |
    !! | `TMC_GetInclineSwitch`      | `get_incline_correction`        |
    !! | `TMC_GetPrismCorr`          | `get_prism_constant`            |
    !! | `TMC_GetPrismType`          | `get_prism_type`                |
    !! | `TMC_GetPrismType2`         | `get_prism_type_v2`             |
    !! | `TMC_GetQuickDist`          | `get_quick_distance`            |
    !! | `TMC_GetRefractiveMethod`   | `get_refraction_mode`           |
    !! | `TMC_GetSignal`             | `get_signal`                    |
    !! | `TMC_IfDataAzeCorrError`    | `get_atr_error`                 |
    !! | `TMC_IfDataIncCorrError`    | `get_inclination_error`         |
    !!
    use :: dm_error
    use :: dm_geocom_api
    use :: dm_geocom_error
    use :: dm_kind
    use :: dm_request
    use :: dm_response
    use :: dm_tty
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

    subroutine geocom_open(this, path, baud_rate, nretries, error)
        !! Opens TTY connection to robotic total station.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_INVALID` if baud rate is invalid.
        class(geocom_class), intent(inout)         :: this      !! GeoCOM object.
        character(len=*),    intent(in)            :: path      !! Path of TTY.
        integer,             intent(in)            :: baud_rate !! Baud rate enumerator.
        integer,             intent(in),  optional :: nretries  !! Number of retries
        integer,             intent(out), optional :: error     !! DMPACK error code

        integer :: nretries_, rc

        nretries_ = 0
        if (present(nretries)) nretries_ = nretries

        rc = E_INVALID
        if (.not. dm_tty_valid_baud_rate(baud_rate)) return

        rc = E_NONE

        !! ... TODO ...
    end subroutine geocom_open

    subroutine geocom_send(this, request, error)
        !! Sends request to configured TTY.
        class(geocom_class), intent(inout)         :: this    !! GeoCOM object.
        type(request_type),  intent(inout)         :: request !! Request to send.
        integer,             intent(out), optional :: error   !! DMPACK error code

        integer :: grc, rc

        !! Send request to sensor.
        !! ... TODO ...

        this%request = request

        !! Get GeoCOM return code from response.
        this%grc = GRC_UNDEFINED
        call dm_request_get(request, 'grc', grc, error=rc)
        if (dm_is_ok(rc)) this%grc = grc
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
        if (present(intensity)) intensity_ = max(0, min(intensity, GEOCOM_IOS_BEEP_STDINTENS))

        call dm_geocom_api_request_beep_on(request, intensity_)
        call this%send(request)
        grc = this%grc
    end function geocom_beep_on
end module dm_geocom
