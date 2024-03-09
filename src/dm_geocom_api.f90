! Author:  Philipp Engel
! Licence: ISC
module dm_geocom_api
    !! Procedures for GeoCOM protocol handling. The routines in this module
    !! only prepare requests and responses for GeoCOM API calls.
    !!
    !! The GeoCOM API is divided into the following sub-systems:
    !!
    !! * `AUT` – Automation
    !! * `BAP` – Basic Applications
    !! * `BMM` – Basic Man-Machine Interface
    !! * `COM` – Communication Settings
    !! * `CSV` – Central Services
    !! * `EDM` – Electronic Distance Measurement
    !! * `FTR` – File Transfer
    !! * `IMG` – Image Processing
    !! * `MOT` – Motorisation
    !! * `SUP` – Supervisor
    !! * `TMC` – Theodolite Measurement and Calculation
    !!
    !! ## API
    !!
    !! | GeoCOM API                  | DMPACK API                                            |
    !! |-----------------------------|-------------------------------------------------------|
    !! | `AUS_GetUserAtrState`       | `dm_geocom_api_request_get_user_atr_mode`             |
    !! | `AUS_GetUserLockState`      | `dm_geocom_api_request_get_user_lock_mode`            |
    !! | `AUS_SetUserAtrState`       | `dm_geocom_api_request_set_user_atr_mode`             |
    !! | `AUS_SetUserLockState`      | `dm_geocom_api_request_set_user_lock_mode`            |
    !! | `AUT_ChangeFace`            | `dm_geocom_api_request_change_face`                   |
    !! | `AUT_FineAdjust`            | `dm_geocom_api_request_fine_adjust`                   |
    !! | `AUT_GetFineAdjustMode`     | `dm_geocom_api_request_get_fine_adjust_mode`          |
    !! | `AUT_GetSearchArea`         | `dm_geocom_api_request_get_search_area`               |
    !! | `AUT_GetUserSpiral`         | `dm_geocom_api_request_get_user_spiral`               |
    !! | `AUT_LockIn`                | `dm_geocom_api_request_lock_in`                       |
    !! | `AUT_MakePositioning`       | `dm_geocom_api_request_set_position`                  |
    !! | `AUT_PS_EnableRange`        | `dm_geocom_api_request_ps_enable_range`               |
    !! | `AUT_PS_SearchNext`         | `dm_geocom_api_request_ps_search_next`                |
    !! | `AUT_PS_SearchWindow`       | `dm_geocom_api_request_ps_search_window`              |
    !! | `AUT_PS_SetRange`           | `dm_geocom_api_request_ps_set_range`                  |
    !! | `AUT_ReadTimeout`           | `dm_geocom_api_request_get_timeout`                   |
    !! | `AUT_ReadTol`               | `dm_geocom_api_request_get_tolerance`                 |
    !! | `AUT_Search`                | `dm_geocom_api_request_get_search`                    |
    !! | `AUT_SetFineAdjustMode`     | `dm_geocom_api_request_set_fine_adjust_mode`          |
    !! | `AUT_SetSearchArea`         | `dm_geocom_api_request_set_search_area`               |
    !! | `AUT_SetTimeout`            | `dm_geocom_api_request_set_positioning_timeout`       |
    !! | `AUT_SetTol`                | `dm_geocom_api_request_set_tolerance`                 |
    !! | `AUT_SetUserSpiral`         | `dm_geocom_api_request_set_user_spiral`               |
    !! | `BAP_GetATRSetting`         | `dm_geocom_api_request_get_atr_setting`               |
    !! | `BAP_GetMeasPrg`            | `dm_geocom_api_request_get_measurement_program`       |
    !! | `BAP_GetPrismDef`           | `dm_geocom_api_request_get_prism_definition`          |
    !! | `BAP_GetRedATRFov`          | `dm_geocom_api_request_get_reduced_atr_fov`           |
    !! | `BAP_GetTargetType`         | `dm_geocom_api_request_get_target_type`               |
    !! | `BAP_GetUserPrismDef`       | `dm_geocom_api_request_get_user_prism_definition`     |
    !! | `BAP_MeasDistanceAngle`     | `dm_geocom_api_request_measure_distance_angle`        |
    !! | `BAP_SearchTarget`          | `dm_geocom_api_request_search_target`                 |
    !! | `BAP_SetATRSetting`         | `dm_geocom_api_request_set_atr_mode`                  |
    !! | `BAP_SetAtmCorr`            | `dm_geocom_api_request_set_atmospheric_correction`    |
    !! | `BAP_SetAtmPpm`             | `dm_geocom_api_request_set_atmospheric_ppm`           |
    !! | `BAP_SetMeasPrg`            | `dm_geocom_api_request_set_measurement_program`       |
    !! | `BAP_SetPrismType2`         | `dm_geocom_api_request_set_prism_type_v2`             |
    !! | `BAP_SetPrismType`          | `dm_geocom_api_request_set_prism_type`                |
    !! | `BAP_SetRedATRFov`          | `dm_geocom_api_request_set_reduced_atr_fov`           |
    !! | `BAP_SetTargetType`         | `dm_geocom_api_request_set_target_type`               |
    !! | `BAP_SetUserPrismDef`       | `dm_geocom_api_request_set_user_prism_definition`     |
    !! | `BMM_BeepAlarm`             | `dm_geocom_api_request_beep_alarm`                    |
    !! | `BMM_BeepNormal`            | `dm_geocom_api_request_beep_normal`                   |
    !! | `COM_GetBinaryAvailable`    | `dm_geocom_api_request_get_binary_mode`               |
    !! | `COM_GetDoublePrecision`    | `dm_geocom_api_request_get_double_precision`          |
    !! | `COM_GetSWVersion`          | `dm_geocom_api_request_get_geocom_version`            |
    !! | `COM_NullProc`              | `dm_geocom_api_request_null`                          |
    !! | `COM_SetBinaryAvailable`    | `dm_geocom_api_request_set_binary_mode`               |
    !! | `COM_SetDoublePrecision`    | `dm_geocom_api_request_set_double_precision`          |
    !! | `COM_SwitchOffTPS`          | `dm_geocom_api_request_switch_off`                    |
    !! | `COM_SwitchOffTPS`          | `dm_geocom_api_request_switch_on`                     |
    !! | `CSV_CheckPower`            | `dm_geocom_api_request_get_power`                     |
    !! | `CSV_GetDateTimeCentiSec`   | `dm_geocom_api_request_get_date_time_centi`           |
    !! | `CSV_GetDateTime`           | `dm_geocom_api_request_get_date_time`                 |
    !! | `CSV_GetDeviceConfig`       | `dm_geocom_api_request_get_device_config`             |
    !! | `CSV_GetInstrumentName`     | `dm_geocom_api_request_get_instrument_name`           |
    !! | `CSV_GetInstrumentNo`       | `dm_geocom_api_request_get_instrument_number`         |
    !! | `CSV_GetIntTemp`            | `dm_geocom_api_request_get_internal_temperature`      |
    !! | `CSV_GetReflectorlessClass` | `dm_geocom_api_request_get_reflectorless_class`       |
    !! | `CSV_GetSWVersion`          | `dm_geocom_api_request_get_software_version`          |
    !! | `CSV_SetDateTime`           | `dm_geocom_api_request_set_date_time`                 |
    !! | `EDM_GetEglIntensity`       | `dm_geocom_api_request_get_egl_intensity`             |
    !! | `EDM_Laserpointer`          | `dm_geocom_api_request_set_laser_pointer`             |
    !! | `EDM_SetEglIntensity`       | `dm_geocom_api_request_set_egl_intensity`             |
    !! | `FTR_AbortDownload`         | `dm_geocom_api_request_abort_download`                |
    !! | `FTR_AbortList`             | `dm_geocom_api_request_abort_list`                    |
    !! | `FTR_Delete`                | `dm_geocom_api_request_delete`                        |
    !! | `FTR_Download`              | `dm_geocom_api_request_download`                      |
    !! | `FTR_List`                  | `dm_geocom_api_request_list`                          |
    !! | `FTR_SetupDownload`         | `dm_geocom_api_request_setup_download`                |
    !! | `FTR_SetupList`             | `dm_geocom_api_request_setup_list`                    |
    !! | `IMG_GetTccConfig`          | `dm_geocom_api_request_get_image_config`              |
    !! | `IMG_SetTccConfig`          | `dm_geocom_api_request_set_image_config`              |
    !! | `IMG_TakeTccImage`          | `dm_geocom_api_request_take_image`                    |
    !! | `IOS_BeepOff`               | `dm_geocom_api_request_beep_off`                      |
    !! | `IOS_BeepOn`                | `dm_geocom_api_request_beep_on`                       |
    !! | `MOT_ReadLockStatus`        | `dm_geocom_api_request_get_lock_status`               |
    !! | `MOT_SetVelocity`           | `dm_geocom_api_request_set_velocity`                  |
    !! | `MOT_StartController`       | `dm_geocom_api_request_start_controller`              |
    !! | `MOT_StopController`        | `dm_geocom_api_request_stop_controller`               |
    !! | `SUP_GetConfig`             | `dm_geocom_api_request_get_config`                    |
    !! | `SUP_SetConfig`             | `dm_geocom_api_request_set_config`                    |
    !! | `TMC_DoMeasure`             | `dm_geocom_api_request_do_measure`                    |
    !! | `TMC_GeoPpm`                | `dm_geocom_api_request_get_geometric_ppm`             |
    !! | `TMC_GetAngSwitch`          | `dm_geocom_api_request_get_angle_correction`          |
    !! | `TMC_GetAngle1`             | `dm_geocom_api_request_get_angle_complete`            |
    !! | `TMC_GetAngle5`             | `dm_geocom_api_request_get_angle`                     |
    !! | `TMC_GetAtmCorr`            | `dm_geocom_api_request_get_atmospheric_correction`    |
    !! | `TMC_GetAtmPpm`             | `dm_geocom_api_request_get_atmospheric_ppm`           |
    !! | `TMC_GetCoordinate`         | `dm_geocom_api_request_get_coordinate`                |
    !! | `TMC_GetEdmMode`            | `dm_geocom_api_request_get_edm_mode`                  |
    !! | `TMC_GetFace`               | `dm_geocom_api_request_get_face`                      |
    !! | `TMC_GetFullMeas`           | `dm_geocom_api_request_get_full_measurement`          |
    !! | `TMC_GetHeight`             | `dm_geocom_api_request_get_height`                    |
    !! | `TMC_GetInclineSwitch`      | `dm_geocom_api_request_get_inclination_correction`    |
    !! | `TMC_GetPrismCorr`          | `dm_geocom_api_request_get_prism_constant`            |
    !! | `TMC_GetPrismType2`         | `dm_geocom_api_request_get_prism_type_v2`             |
    !! | `TMC_GetPrismType`          | `dm_geocom_api_request_get_prism_type`                |
    !! | `TMC_GetQuickDist`          | `dm_geocom_api_request_get_quick_distance`            |
    !! | `TMC_GetRefractiveMethod`   | `dm_geocom_api_request_get_refraction_mode`           |
    !! | `TMC_GetSignal`             | `dm_geocom_api_request_get_signal`                    |
    !! | `TMC_GetSimpleCoord`        | `dm_geocom_api_request_get_simple_coordinates`        |
    !! | `TMC_GetSimpleMea`          | `dm_geocom_api_request_get_simple_measurement`        |
    !! | `TMC_GetSlopeDistCorr`      | `dm_geocom_api_request_get_slope_distance_correction` |
    !! | `TMC_GetStation`            | `dm_geocom_api_request_get_station`                   |
    !! | `TMC_IfDataAzeCorrError`    | `dm_geocom_api_request_get_atr_error`                 |
    !! | `TMC_IfDataIncCorrError`    | `dm_geocom_api_request_get_inclination_error`         |
    !! | `TMC_QuickDist`             | `dm_geocom_api_request_get_quick_distance`            |
    !! | `TMC_SetAngSwitch`          | `dm_geocom_api_request_set_angle_correction`          |
    !! | `TMC_SetEdmMode`            | `dm_geocom_api_request_set_edm_mode`                  |
    !! | `TMC_SetGeoPpm`             | `dm_geocom_api_request_set_geometric_ppm`             |
    !! | `TMC_SetHandDist`           | `dm_geocom_api_request_set_distance`                  |
    !! | `TMC_SetHeight`             | `dm_geocom_api_request_set_height`                    |
    !! | `TMC_SetInclineSwitch`      | `dm_geocom_api_request_set_inclination_correction`    |
    !! | `TMC_SetOrientation`        | `dm_geocom_api_request_set_orientation`               |
    !! | `TMC_SetPrismCorr`          | `dm_geocom_api_request_set_prism_constant`            |
    !! | `TMC_SetRefractiveMethod`   | `dm_geocom_api_request_set_refraction_mode`           |
    !! | `TMC_SetStation`            | `dm_geocom_api_request_set_station`                   |
    !!
    use :: dm_kind
    use :: dm_geocom_type
    use :: dm_request
    use :: dm_response
    use :: dm_util
    implicit none (type, external)
    private

    ! **************************************************************************
    ! PRIVATE GEOCOM API CONSTANTS.
    ! **************************************************************************
    character(len=*), parameter :: GEOCOM_DELIMITER = '\r\n' !! Default GeoCOM delimiter.

    character(len=*),    parameter :: GRC_PATTERN      = '(?<grc>\d+)' !! Default response pattern (GRC only).
    type(response_type), parameter :: GRC_RESPONSES(1) = [ response_type('grc', type=RESPONSE_TYPE_INT32) ] !! Default responses (GRC only).

    ! Public procedures.
    public :: dm_geocom_api_request
    public :: dm_geocom_api_request_abort_download
    public :: dm_geocom_api_request_abort_list
    public :: dm_geocom_api_request_beep_alarm
    public :: dm_geocom_api_request_beep_normal
    public :: dm_geocom_api_request_beep_off
    public :: dm_geocom_api_request_beep_on
    public :: dm_geocom_api_request_change_face
    public :: dm_geocom_api_request_delete
    public :: dm_geocom_api_request_do_measure
    public :: dm_geocom_api_request_download
    public :: dm_geocom_api_request_fine_adjust
    public :: dm_geocom_api_request_get_angle
    public :: dm_geocom_api_request_get_angle_complete
    public :: dm_geocom_api_request_get_angle_correction
    public :: dm_geocom_api_request_get_atmospheric_correction
    public :: dm_geocom_api_request_get_atmospheric_ppm
    public :: dm_geocom_api_request_get_atr_error
    public :: dm_geocom_api_request_get_atr_setting
    public :: dm_geocom_api_request_get_binary_mode
    public :: dm_geocom_api_request_get_config
    public :: dm_geocom_api_request_get_coordinate
    public :: dm_geocom_api_request_get_date_time
    public :: dm_geocom_api_request_get_date_time_centi
    public :: dm_geocom_api_request_get_device_config
    public :: dm_geocom_api_request_get_double_precision
    public :: dm_geocom_api_request_get_edm_mode
    public :: dm_geocom_api_request_get_egl_intensity
    public :: dm_geocom_api_request_get_face
    public :: dm_geocom_api_request_get_fine_adjust_mode
    public :: dm_geocom_api_request_get_full_measurement
    public :: dm_geocom_api_request_get_geocom_version
    public :: dm_geocom_api_request_get_geometric_ppm
    public :: dm_geocom_api_request_get_height
    public :: dm_geocom_api_request_get_image_config
    public :: dm_geocom_api_request_get_inclination_correction
    public :: dm_geocom_api_request_get_inclination_error
    public :: dm_geocom_api_request_get_instrument_name
    public :: dm_geocom_api_request_get_instrument_number
    public :: dm_geocom_api_request_get_internal_temperature
    public :: dm_geocom_api_request_get_lock_status
    public :: dm_geocom_api_request_get_measurement_program
    public :: dm_geocom_api_request_get_power
    public :: dm_geocom_api_request_get_prism_constant
    public :: dm_geocom_api_request_get_prism_definition
    public :: dm_geocom_api_request_get_prism_type
    public :: dm_geocom_api_request_get_prism_type_v2
    public :: dm_geocom_api_request_get_quick_distance
    public :: dm_geocom_api_request_get_reduced_atr_fov
    public :: dm_geocom_api_request_get_reflectorless_class
    public :: dm_geocom_api_request_get_refraction_mode
    public :: dm_geocom_api_request_get_search_area
    public :: dm_geocom_api_request_get_signal
    public :: dm_geocom_api_request_get_simple_coordinates
    public :: dm_geocom_api_request_get_simple_measurement
    public :: dm_geocom_api_request_get_slope_distance_correction
    public :: dm_geocom_api_request_get_software_version
    public :: dm_geocom_api_request_get_station
    public :: dm_geocom_api_request_get_target_type
    public :: dm_geocom_api_request_get_timeout
    public :: dm_geocom_api_request_get_tolerance
    public :: dm_geocom_api_request_get_user_atr_mode
    public :: dm_geocom_api_request_get_user_lock_mode
    public :: dm_geocom_api_request_get_user_prism_definition
    public :: dm_geocom_api_request_get_user_spiral
    public :: dm_geocom_api_request_list
    public :: dm_geocom_api_request_lock_in
    public :: dm_geocom_api_request_measure_distance_angle
    public :: dm_geocom_api_request_null
    public :: dm_geocom_api_request_ps_enable_range
    public :: dm_geocom_api_request_ps_search_next
    public :: dm_geocom_api_request_ps_search_window
    public :: dm_geocom_api_request_ps_set_range
    public :: dm_geocom_api_request_search
    public :: dm_geocom_api_request_search_target
    public :: dm_geocom_api_request_set_angle_correction
    public :: dm_geocom_api_request_set_atmospheric_correction
    public :: dm_geocom_api_request_set_atmospheric_ppm
    public :: dm_geocom_api_request_set_atr_mode
    public :: dm_geocom_api_request_set_binary_mode
    public :: dm_geocom_api_request_set_config
    public :: dm_geocom_api_request_set_date_time
    public :: dm_geocom_api_request_set_distance
    public :: dm_geocom_api_request_set_double_precision
    public :: dm_geocom_api_request_set_edm_mode
    public :: dm_geocom_api_request_set_egl_intensity
    public :: dm_geocom_api_request_set_fine_adjust_mode
    public :: dm_geocom_api_request_set_geometric_ppm
    public :: dm_geocom_api_request_set_height
    public :: dm_geocom_api_request_set_image_config
    public :: dm_geocom_api_request_set_inclination_correction
    public :: dm_geocom_api_request_set_laser_pointer
    public :: dm_geocom_api_request_set_measurement_program
    public :: dm_geocom_api_request_set_orientation
    public :: dm_geocom_api_request_set_position
    public :: dm_geocom_api_request_set_positioning_timeout
    public :: dm_geocom_api_request_set_prism_constant
    public :: dm_geocom_api_request_set_prism_type
    public :: dm_geocom_api_request_set_prism_type_v2
    public :: dm_geocom_api_request_set_reduced_atr_fov
    public :: dm_geocom_api_request_set_refraction_mode
    public :: dm_geocom_api_request_set_search_area
    public :: dm_geocom_api_request_set_station
    public :: dm_geocom_api_request_set_target_type
    public :: dm_geocom_api_request_set_tolerance
    public :: dm_geocom_api_request_set_user_atr_mode
    public :: dm_geocom_api_request_set_user_lock_mode
    public :: dm_geocom_api_request_set_user_prism_definition
    public :: dm_geocom_api_request_set_user_spiral
    public :: dm_geocom_api_request_set_velocity
    public :: dm_geocom_api_request_setup_download
    public :: dm_geocom_api_request_setup_list
    public :: dm_geocom_api_request_start_controller
    public :: dm_geocom_api_request_stop_controller
    public :: dm_geocom_api_request_switch_off
    public :: dm_geocom_api_request_switch_on
    public :: dm_geocom_api_request_take_image
contains
    ! **************************************************************************
    ! PUBLIC REQUEST PREPARATION PROCEDURES.
    ! **************************************************************************
    pure subroutine dm_geocom_api_request(request, name, code, arguments, pattern, responses, mode)
        !! Prepares a DMPACK request type by setting request command, response
        !! pattern, response delimiter, and response definition array.
        type(request_type),  intent(out)          :: request      !! Prepared request type.
        character(len=*),    intent(in)           :: name         !! Request name.
        integer,             intent(in)           :: code         !! GeoCOM request code.
        character(len=*),    intent(in), optional :: arguments    !! GeoCOM request arguments.
        character(len=*),    intent(in), optional :: pattern      !! Regular expression pattern that matches the raw response.
        type(response_type), intent(in), optional :: responses(:) !! Array of response types.
        integer,             intent(in), optional :: mode         !! Mode of returned observation request (`REQUEST_MODE_*`).

        integer :: n

        request%name = name
        request%delimiter = GEOCOM_DELIMITER

        if (present(arguments)) then
            write (request%request, '("%R1Q,", i0, ":", 2a)') code, trim(arguments), GEOCOM_DELIMITER
        else
            write (request%request, '("%R1Q,", i0, ":", a)') code, GEOCOM_DELIMITER
        end if

        if (present(pattern)) then
            write (request%pattern, '("%R1P,0,0:", 2a)') trim(pattern), GEOCOM_DELIMITER
        end if

        if (present(mode)) request%mode = mode

        if (.not. present(responses)) return

        n = min(size(responses), REQUEST_MAX_NRESPONSES)
        request%nresponses = n
        if (n == 0) return
        request%responses(1:n) = responses(1:n)
    end subroutine dm_geocom_api_request

    ! **************************************************************************
    ! PUBLIC GEOCOM REQUEST PREPARATION PROCEDURES.
    ! **************************************************************************
    pure subroutine dm_geocom_api_request_abort_download(request)
        !! Request of *FTR_AbortDownload* procedure. Creates request to abort
        !! or end the file download command.
        !!
        !! The instrument returns the following responses:
        !!
        !! * `grc` – GeoCOM return code.
        !!
        !! | Property       | Values                                           |
        !! |----------------|--------------------------------------------------|
        !! | Instruments    | TPS1200, TM30/TS30, TS16                         |
        !! | ASCII request  | `%R1Q,23305:`                                    |
        !! | ASCII response | `%R1P,0,0:<grc>`                                 |
        !!
        character(len=*), parameter :: REQUEST_NAME = 'abort_download'
        integer, parameter          :: REQUEST_CODE = 23305

        type(request_type), intent(out) :: request !! Prepared request.

        call dm_geocom_api_request(request, REQUEST_NAME, REQUEST_CODE, pattern=GRC_PATTERN, responses=GRC_RESPONSES)
    end subroutine dm_geocom_api_request_abort_download

    pure subroutine dm_geocom_api_request_abort_list(request)
        !! Request of *FTR_AbortList* procedure. Creates request to abort or
        !! end the file list command.
        !!
        !! The instrument returns the following responses:
        !!
        !! * `grc` – GeoCOM return code.
        !!
        !! | Property       | Values                                           |
        !! |----------------|--------------------------------------------------|
        !! | Instruments    | TPS1200, TM30/TS30, TS16                         |
        !! | ASCII request  | `%R1Q,23308:`                                    |
        !! | ASCII response | `%R1P,0,0:<grc>`                                 |
        !!
        character(len=*), parameter :: REQUEST_NAME = 'abort_list'
        integer,          parameter :: REQUEST_CODE = 23308

        type(request_type), intent(out) :: request !! Prepared request.

        call dm_geocom_api_request(request, REQUEST_NAME, REQUEST_CODE, pattern=GRC_PATTERN, responses=GRC_RESPONSES)
    end subroutine dm_geocom_api_request_abort_list

    pure subroutine dm_geocom_api_request_beep_alarm(request)
        !! Request of *BMM_BeepAlarm* procedure. Creates request to output an
        !! alarm signal (triple beep).
        !!
        !! The instrument returns the following responses:
        !!
        !! * `grc` – GeoCOM return code.
        !!
        !! | Property       | Values                                           |
        !! |----------------|--------------------------------------------------|
        !! | Instruments    | TPS1100, TPS1200, TM30/TS30, TS16                |
        !! | ASCII request  | `%R1Q,11004:`                                    |
        !! | ASCII response | `%R1P,0,0:<grc>`                                 |
        !!
        character(len=*), parameter :: REQUEST_NAME = 'beep_alarm'
        integer,          parameter :: REQUEST_CODE = 11004

        type(request_type), intent(out) :: request !! Prepared request.

        call dm_geocom_api_request(request, REQUEST_NAME, REQUEST_CODE, pattern=GRC_PATTERN, responses=GRC_RESPONSES)
    end subroutine dm_geocom_api_request_beep_alarm

    pure subroutine dm_geocom_api_request_beep_normal(request)
        !! Request of *BMM_BeepNormal* procedure. Creates request to output an
        !! alarm signal (single beep).
        !!
        !! The instrument returns the following responses:
        !!
        !! * `grc` – GeoCOM return code.
        !!
        !! | Property       | Values                                           |
        !! |----------------|--------------------------------------------------|
        !! | Instruments    | TPS1100, TPS1200, TM30/TS30, TS16                |
        !! | ASCII request  | `%R1Q,11003:`                                    |
        !! | ASCII response | `%R1P,0,0:<grc>`                                 |
        !!
        character(len=*), parameter :: REQUEST_NAME = 'beep_normal'
        integer,          parameter :: REQUEST_CODE = 11003

        type(request_type), intent(out) :: request !! Prepared request.

        call dm_geocom_api_request(request, REQUEST_NAME, REQUEST_CODE, pattern=GRC_PATTERN, responses=GRC_RESPONSES)
    end subroutine dm_geocom_api_request_beep_normal

    pure subroutine dm_geocom_api_request_beep_off(request)
        !! Request of *IOS_BeepOff* procedure. Creates request to stop an
        !! active beep signal.
        !!
        !! The instrument returns the following responses:
        !!
        !! * `grc` – GeoCOM return code.
        !!
        !! | Property       | Values                                           |
        !! |----------------|--------------------------------------------------|
        !! | Instruments    | TPS1100, TPS1200, TM30/TS30, TS16                |
        !! | ASCII request  | `%R1Q,20000:`                                    |
        !! | ASCII response | `%R1P,0,0:<grc>`                                 |
        !!
        character(len=*), parameter :: REQUEST_NAME = 'beep_off'
        integer,          parameter :: REQUEST_CODE = 20000

        type(request_type), intent(out) :: request !! Prepared request.

        call dm_geocom_api_request(request, REQUEST_NAME, REQUEST_CODE, pattern=GRC_PATTERN, responses=GRC_RESPONSES)
    end subroutine dm_geocom_api_request_beep_off

    pure subroutine dm_geocom_api_request_beep_on(request, intensity)
        !! Request of *IOS_BeepOn* procedure. Creates request for continuous
        !! beep signal of given intensity.
        !!
        !! The instrument returns the following responses:
        !!
        !! * `grc` – GeoCOM return code.
        !!
        !! | Property       | Values                                           |
        !! |----------------|--------------------------------------------------|
        !! | Instruments    | TPS1100, TPS1200, TM30/TS30, TS16                |
        !! | ASCII request  | `%R1Q,20001:<intensity>`                         |
        !! | ASCII response | `%R1P,0,0:<grc>`                                 |
        !!
        character(len=*), parameter :: REQUEST_NAME = 'beep_on'
        integer,          parameter :: REQUEST_CODE = 20001

        type(request_type), intent(out) :: request   !! Prepared request.
        integer,            intent(in)  :: intensity !! Intensity of beep, from 0 to 100.

        character(len=80) :: args

        write (args, '(i0)') intensity
        call dm_geocom_api_request(request, REQUEST_NAME, REQUEST_CODE, args, GRC_PATTERN, GRC_RESPONSES)
    end subroutine dm_geocom_api_request_beep_on

    pure subroutine dm_geocom_api_request_change_face(request, pos_mode, atr_mode)
        !! Request of *AUT_ChangeFace* procedure. Creates request for turning
        !! the telescope to the other face.
        !!
        !! If `pos_mode` is `GEOCOM_AUT_NORMAL`, uses the current value of the
        !! compensator. For positioning distances > 25 gon, this mode might tend
        !! to inaccuracy. If set to `GEOCOM_AUT_PRECISE`, tries to measure the exact
        !! inclination of the target. Tends to long position time.
        !!
        !! If `atr_mode` is `GEOCOM_AUT_POSITION`, uses conventional position to
        !! other face. If set to `GEOCOM_AUT_TARGET`, tries to position into a
        !! target in the destination area. This mode requires activated ATR.
        !!
        !! The instrument returns the following responses:
        !!
        !! * `grc` – GeoCOM return code.
        !!
        !! | Property       | Values                                           |
        !! |----------------|--------------------------------------------------|
        !! | Instruments    | TPS1100, TPS1200, TM30/TS30, TS16                |
        !! | ASCII request  | `%R1Q,9028:<pos_mode>,<atr_mode>,0`              |
        !! | ASCII response | `%R1P,0,0:<grc>`                                 |
        !!
        character(len=*), parameter :: REQUEST_NAME = 'change_face'
        integer,          parameter :: REQUEST_CODE = 9028

        type(request_type), intent(out) :: request  !! Prepared request.
        integer,            intent(in)  :: pos_mode !! Position mode (`GEOCOM_AUT_POSMODE`).
        integer,            intent(in)  :: atr_mode !! ATR mode (`GEOCOM_AUT_ATRMODE`).

        character(len=80) :: args

        write (args, '(i0, ",", i0, ",0")') pos_mode, atr_mode
        call dm_geocom_api_request(request, REQUEST_NAME, REQUEST_CODE, args, GRC_PATTERN, GRC_RESPONSES)
    end subroutine dm_geocom_api_request_change_face

    pure subroutine dm_geocom_api_request_delete(request, device_type, file_type, day, month, year, file_name)
        !! Request of *FTR_Delete* procedure. Creates request for deleting one
        !! or more files.
        !!
        !! Wildcards may be used to delete multiple files. If the deletion date
        !! is valid, only files older than the date are deleted.
        !!
        !! The instrument returns the following responses:
        !!
        !! * `grc`    – GeoCOM return code.
        !! * `nfiles` – Number of files.
        !!
        !! | Property       | Values                                                                  |
        !! |----------------|-------------------------------------------------------------------------|
        !! | Instruments    | TPS1200, TM30/TS30, TS16                                                |
        !! | ASCII request  | `%R1Q,23309:<device_type>,<file_type>,<day>,<month>,<year>,<file_name>` |
        !! | ASCII response | `%R1P,0,0:<grc>,<nfiles>`                                               |
        !!
        character(len=*), parameter :: REQUEST_NAME    = 'delete'
        character(len=*), parameter :: REQUEST_PATTERN = GRC_PATTERN // ',(?<nfiles>\d+)'
        integer,          parameter :: REQUEST_CODE    = 23309

        type(request_type),     intent(out) :: request     !! Prepared request.
        integer,                intent(in)  :: device_type !! Internal memory or memory card (`GEOCOM_FTR_DEVICETYPE`).
        integer,                intent(in)  :: file_type   !! Type of file (`GEOCOM_FTR_FILETYPE`).
        integer,                intent(in)  :: day         !! Day (`DD`).
        integer,                intent(in)  :: month       !! Month (`MM`).
        integer,                intent(in)  :: year        !! Year (`YY`).
        character(len=*),       intent(in)  :: file_name   !! Name of file to delete.

        character(len=80)   :: args
        type(response_type) :: responses(2)

        write (args, '(2(i0, ","), 3(z2.2, ","), a)') device_type, file_type, day, month, year, file_name

        responses = [ &
            response_type('grc',    type=RESPONSE_TYPE_INT32), &
            response_type('nfiles', type=RESPONSE_TYPE_INT32)  &
        ]

        call dm_geocom_api_request(request, REQUEST_NAME, REQUEST_CODE, args, REQUEST_PATTERN, responses)
    end subroutine dm_geocom_api_request_delete

    pure subroutine dm_geocom_api_request_do_measure(request, tmc_prog, inc_mode)
        !! Request of *TMC_DoMeasure* procedure. Creates request for trying a
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
        !! The instrument returns the following responses:
        !!
        !! * `grc` – GeoCOM return code.
        !!
        !! | Property       | Values                                           |
        !! |----------------|--------------------------------------------------|
        !! | Instruments    | TPS1100, TPS1200, TM30/TS30, TS16                |
        !! | ASCII request  | `%R1Q,2008:<tmc_prog>,<inc_mode>`                |
        !! | ASCII response | `%R1P,0,0:<grc>`                                 |
        !!
        character(len=*), parameter :: REQUEST_NAME = 'do_measure'
        integer,          parameter :: REQUEST_CODE = 2008

        type(request_type), intent(out) :: request  !! Prepared request.
        integer,            intent(in)  :: tmc_prog !! TMC measurement program (`GEOCOM_TMC_MEASURE_PRG`).
        integer,            intent(in)  :: inc_mode !! Inclination measurement mode (`GEOCOM_TMC_INCLINE_PRG`).

        character(len=80) :: args

        write (args, '(i0, ",", i0)') tmc_prog, inc_mode
        call dm_geocom_api_request(request, REQUEST_NAME, REQUEST_CODE, args, GRC_PATTERN, GRC_RESPONSES)
    end subroutine dm_geocom_api_request_do_measure

    pure subroutine dm_geocom_api_request_download(request, block_number)
        !! Request of *FTR_Download* procedure. Creates request to get a
        !! single block of data. The *FTR_SetupDownload* command has to be
        !! called first.
        !!
        !! The block sequence starts with 1. The download process will be
        !! aborted if the block number is set to 0.
        !!
        !! The maximum block number is 65535. The file size is therefore
        !! limited to 28 MiB.
        !!
        !! The instrument returns the following responses:
        !!
        !! * `grc`      – GeoCOM return code.
        !! * `blockval` – Block value [byte].
        !! * `blocklen` – Block length.
        !!
        !! | Property       | Values                                           |
        !! |----------------|--------------------------------------------------|
        !! | Instruments    | TPS1200, TM30/TS30, TS16                         |
        !! | ASCII request  | `%R1Q,23304:<blocknum>`                          |
        !! | ASCII response | `%R1P,0,0:<grc>,<blockval>,<blocklen>`           |
        !!
        character(len=*), parameter :: REQUEST_NAME    = 'download'
        character(len=*), parameter :: REQUEST_PATTERN = GRC_PATTERN // ",'(?<blockval>[0-9a-f]+)',(?<blocklen>\d+)"
        integer,          parameter :: REQUEST_CODE    = 23304
        integer,          parameter :: MODE            = REQUEST_MODE_GEOCOM_FILE

        type(request_type), intent(out) :: request      !! Prepared request.
        integer,            intent(in)  :: block_number !! Block number.

        character(len=80)   :: args
        type(response_type) :: responses(3)

        write (args, '(i0)') block_number

        responses = [ &
            response_type('grc',      type=RESPONSE_TYPE_INT32), &
            response_type('blockval', type=RESPONSE_TYPE_BYTE), &
            response_type('blocklen', type=RESPONSE_TYPE_INT32)  &
        ]

        call dm_geocom_api_request(request, REQUEST_NAME, REQUEST_CODE, args, REQUEST_PATTERN, responses, mode=MODE)
    end subroutine dm_geocom_api_request_download

    pure subroutine dm_geocom_api_request_fine_adjust(request, search_hz, search_v)
        !! Request of *AUT_FineAdjust* procedure. Creates request for
        !! automatic target positioning.
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
        !!
        !! The instrument returns the following responses:
        !!
        !! * `grc` – GeoCOM return code.
        !!
        !! | Property       | Values                                           |
        !! |----------------|--------------------------------------------------|
        !! | Instruments    | TPS1100, TPS1200, TM30/TS30, TS16                |
        !! | ASCII request  | `%R1Q,9037:<search_hz>,<search_v>`               |
        !! | ASCII response | `%R1P,0,0:<grc>`                                 |
        !!
        character(len=*), parameter :: REQUEST_NAME = 'fine_adjust'
        integer,          parameter :: REQUEST_CODE = 9027

        type(request_type), intent(out) :: request   !! Prepared request.
        real(kind=r8),      intent(in)  :: search_hz !! Search range, Hz axis [rad].
        real(kind=r8),      intent(in)  :: search_v  !! Search range, V axis [rad].

        character(len=80) :: args

        write (args, '(2(f0.12, ","), "0")') search_hz, search_v
        call dm_geocom_api_request(request, REQUEST_NAME, REQUEST_CODE, args, GRC_PATTERN, GRC_RESPONSES)
    end subroutine dm_geocom_api_request_fine_adjust

    pure subroutine dm_geocom_api_request_get_angle(request, inc_mode)
        !! Request of *TMC_GetAngle5* procedure. Creates request for returning
        !! a simple angle measurement.
        !!
        !! The function starts an angle measurement and returns the results.
        !!
        !! The instrument returns the following responses:
        !!
        !! * `grc` – GeoCOM return code.
        !! * `hz`  – Horizontal angle [rad].
        !! * `v`   – Vertical angle [rad].
        !!
        !! | Property       | Values                                           |
        !! |----------------|--------------------------------------------------|
        !! | Instruments    | TPS1100, TPS1200, TM30/TS30, TS16                |
        !! | ASCII request  | `%R1Q,2107:<inc_mode>`                           |
        !! | ASCII response | `%R1P,0,0:<grc>,<hz>,<v>`                        |
        !!
        character(len=*), parameter :: REQUEST_NAME    = 'get_angle'
        character(len=*), parameter :: REQUEST_PATTERN = GRC_PATTERN // ',(?<hz>[\d\.]+),(?<v>[\d\.]+)'
        integer,          parameter :: REQUEST_CODE    = 2107

        type(request_type), intent(out) :: request  !! Prepared request.
        integer,            intent(in)  :: inc_mode !! Inclination measurement mode (`GEOCOM_TMC_INCLINE_PRG`).

        character(len=80)   :: args
        type(response_type) :: responses(3)

        write (args, '(i0)') inc_mode

        responses = [ &
            response_type('grc', unit=' ',   type=RESPONSE_TYPE_INT32),  & ! GeoCOM return code.
            response_type('hz',  unit='rad', type=RESPONSE_TYPE_REAL64), & ! Horizontal angle [rad].
            response_type('v',   unit='rad', type=RESPONSE_TYPE_REAL64)  & ! Vertical angle [rad].
        ]

        call dm_geocom_api_request(request, REQUEST_NAME, REQUEST_CODE, args, REQUEST_PATTERN, responses)
    end subroutine dm_geocom_api_request_get_angle

    pure subroutine dm_geocom_api_request_get_angle_complete(request, inc_mode)
        !! Request of *TMC_GetAngle1* procedure. Creates request for returning
        !! a complete angle measurement.
        !!
        !! The function starts an angle and, depending on the configuration, an
        !! inclination measurement, and returns the results.
        !!
        !! The instrument returns the following responses:
        !!
        !! * `grc`     – GeoCOM return code.
        !! * `hz`      – Horizontal angle [rad].
        !! * `v`       – Vertical angle [rad].
        !! * `angacc`  – Accuracy of angles [rad].
        !! * `angtime` – Moment of measurement [msec].
        !! * `xinc`    – Transverse axis inclination [rad].
        !! * `linc`    – Longitude axis inclidation [rad].
        !! * `incacc`  – Inclination accuracy [rad].
        !! * `inctime` – Moment of measurement [msec].
        !! * `face`    – Face position of telescope.
        !!
        !! | Property       | Values                                                                               |
        !! |----------------|--------------------------------------------------------------------------------------|
        !! | Instruments    | TPS1100, TPS1200, TM30/TS30, TS16                                                    |
        !! | ASCII request  | `%R1Q,2003:<inc_mode>`                                                               |
        !! | ASCII response | `%R1P,0,0:<grc>,<hz>,<v>,<angacc>,<angtime>,<xinc>,<linc>,<incacc>,<inctime>,<face>` |
        !!
        character(len=*), parameter :: REQUEST_NAME    = 'get_angle_complete'
        character(len=*), parameter :: REQUEST_PATTERN = &
            GRC_PATTERN // ',(?<hz>[-\d\.]+),(?<v>[-\d\.]+),(?<angacc>[-\d\.]+),(?<angtime>\d+),' // &
            '(?<xinc>[-\d\.]+),(?<linc>[-\d\.]+),(?<incacc>[-\d\.]+),(?<inctime>\d+),(?<face>\d+)'
        integer,          parameter :: REQUEST_CODE    = 2003

        type(request_type), intent(out) :: request  !! Prepared request.
        integer,            intent(in)  :: inc_mode !! Inclination measurement mode (`GEOCOM_TMC_INCLINE_PRG`).

        character(len=80)   :: args
        type(response_type) :: responses(10)

        write (args, '(i0)') inc_mode

        responses = [ &
            response_type('grc',     unit=' ',   type=RESPONSE_TYPE_INT32),  & ! GeoCOM return code.
            response_type('hz',      unit='rad', type=RESPONSE_TYPE_REAL64), & ! Horizontal angle [rad].
            response_type('v',       unit='rad', type=RESPONSE_TYPE_REAL64), & ! Vertical angle [rad].
            response_type('angacc',  unit='rad', type=RESPONSE_TYPE_REAL64), & ! Accuracy of angles [rad].
            response_type('angtime', unit='ms',  type=RESPONSE_TYPE_INT64),  & ! Moment of measurement [msec].
            response_type('xinc',    unit='rad', type=RESPONSE_TYPE_REAL64), & ! Transverse axis inclination [rad].
            response_type('linc',    unit='rad', type=RESPONSE_TYPE_REAL64), & ! Longitude axis inclidation [rad].
            response_type('incacc',  unit='rad', type=RESPONSE_TYPE_REAL64), & ! Inclination accuracy [rad].
            response_type('inctime', unit='ms',  type=RESPONSE_TYPE_INT64),  & ! Moment of measurement [msec].
            response_type('face',    unit=' ',   type=RESPONSE_TYPE_INT32)   & ! Face position of telescope.
        ]

        call dm_geocom_api_request(request, REQUEST_NAME, REQUEST_CODE, args, REQUEST_PATTERN, responses)
    end subroutine dm_geocom_api_request_get_angle_complete

    pure subroutine dm_geocom_api_request_get_angle_correction(request)
        !! Request of *TMC_GetAngSwitch* procedure. Creates request for
        !! getting the angular correction status.
        !!
        !! The instrument returns the following responses:
        !!
        !! * `grc`    – GeoCOM return code.
        !! * `inccor` – Inclination correction on/off [bool].
        !! * `stdcor` – Standing axis correction on/off [bool].
        !! * `colcor` – Collimation error correction on/off [bool].
        !! * `tilcor` – Tilting axis correction on/off [bool].
        !!
        !! | Property       | Values                                               |
        !! |----------------|------------------------------------------------------|
        !! | Instruments    | TPS1100, TPS1200, TM30/TS30, TS16                    |
        !! | ASCII request  | `%R1Q,2014:`                                         |
        !! | ASCII response | `%R1P,0,0:<grc>,<inccor>,<stdcor>,<colcor>,<tilcor>` |
        !!
        character(len=*), parameter :: REQUEST_NAME    = 'get_angle_correction'
        character(len=*), parameter :: REQUEST_PATTERN = &
            GRC_PATTERN // ',(?<inccor>\d+),(?<stdcor>\d+),(?<colcor>\d+),(?<tilcor>\d+)'
        integer,          parameter :: REQUEST_CODE    = 2014

        type(request_type), intent(out) :: request !! Prepared request.
        type(response_type)             :: responses(5)

        responses = [ &
            response_type('grc',    type=RESPONSE_TYPE_INT32),   & ! GeoCOM return code.
            response_type('inccor', type=RESPONSE_TYPE_LOGICAL), & ! Inclination correction on/off [bool].
            response_type('stdcor', type=RESPONSE_TYPE_LOGICAL), & ! Standing axis correction on/off [bool].
            response_type('colcor', type=RESPONSE_TYPE_LOGICAL), & ! Collimation error correction on/off [bool].
            response_type('tilcor', type=RESPONSE_TYPE_LOGICAL)  & ! Tilting axis correction on/off [bool].
        ]

        call dm_geocom_api_request(request, REQUEST_NAME, REQUEST_CODE, pattern=REQUEST_PATTERN, responses=responses)
    end subroutine dm_geocom_api_request_get_angle_correction

    pure subroutine dm_geocom_api_request_get_atmospheric_correction(request)
        !! Request of *TMC_GetAtmCorr* procedure. Creates request for getting
        !! the atmospheric correction parameters.
        !!
        !! The instrument returns the following responses:
        !!
        !! * `grc`      – GeoCOM return code.
        !! * `lambda`   – Wave length of the EDM transmitter [m].
        !! * `pressure` – Atmospheric pressure [mbar].
        !! * `drytemp`  – Dry temperature [°C].
        !! * `wettemp`  – Wet temperature [°C].
        !!
        !! | Property       | Values                                                   |
        !! |----------------|----------------------------------------------------------|
        !! | Instruments    | TPS1100, TPS1200, TM30/TS30, TS16                        |
        !! | ASCII request  | `%R1Q,2029:`                                             |
        !! | ASCII response | `%R1P,0,0:<grc>,<lambda>,<pressure>,<drytemp>,<wettemp>` |
        !!
        character(len=*), parameter :: REQUEST_NAME    = 'get_atmospheric_correction'
        character(len=*), parameter :: REQUEST_PATTERN = &
            GRC_PATTERN // ',(?<lambda>[-\d\.]+),(?<pressure>[-\d\.]+),(?<drytemp>[-\d\.]+),(?<wettemp>[-\d\.]+)'
        integer,          parameter :: REQUEST_CODE    = 2029

        type(request_type), intent(out) :: request !! Prepared request.
        type(response_type)             :: responses(5)

        responses = [ &
            response_type('grc',      unit=' ',    type=RESPONSE_TYPE_INT32),  & ! GeoCOM return code.
            response_type('lambda',   unit='m',    type=RESPONSE_TYPE_REAL64), & ! Wave length of the EDM transmitter [m].
            response_type('pressure', unit='mbar', type=RESPONSE_TYPE_REAL64), & ! Atmospheric pressure [mbar].
            response_type('drytemp',  unit='degC', type=RESPONSE_TYPE_REAL64), & ! Dry temperature [°C].
            response_type('wettemp',  unit='degC', type=RESPONSE_TYPE_REAL64)  & ! Wet temperature [°C].
        ]

        call dm_geocom_api_request(request, REQUEST_NAME, REQUEST_CODE, pattern=REQUEST_PATTERN, responses=responses)
    end subroutine dm_geocom_api_request_get_atmospheric_correction

    pure subroutine dm_geocom_api_request_get_atmospheric_ppm(request)
        !! Request of *TMC_GetAtmPpm* procedure. Creates request for getting
        !! the atmospheric ppm correction factor.
        !!
        !! The instrument returns the following responses:
        !!
        !! * `grc`    – GeoCOM return code.
        !! * `atmppm` – Atmospheric ppm correction factor [ppm].
        !!
        !! | Property       | Values                                           |
        !! |----------------|--------------------------------------------------|
        !! | Instruments    | TPS1200, TM30/TS30, TS16                         |
        !! | ASCII request  | `%R1Q,2151:`                                     |
        !! | ASCII response | `%R1P,0,0:<grc>,<atmppm>`                        |
        !!
        character(len=*), parameter :: REQUEST_NAME    = 'get_atmospheric_ppm'
        character(len=*), parameter :: REQUEST_PATTERN = GRC_PATTERN // ',(?<atmppm>[-\d\.]+)'
        integer,          parameter :: REQUEST_CODE    = 2151

        type(request_type), intent(out) :: request !! Prepared request.
        type(response_type)             :: responses(2)

        responses = [ &
            response_type('grc',    unit=' ',   type=RESPONSE_TYPE_INT32), &
            response_type('atmppm', unit='ppm', type=RESPONSE_TYPE_REAL64) &
        ]

        call dm_geocom_api_request(request, REQUEST_NAME, REQUEST_CODE, pattern=REQUEST_PATTERN, responses=responses)
    end subroutine dm_geocom_api_request_get_atmospheric_ppm

    pure subroutine dm_geocom_api_request_get_atr_error(request)
        !! Request of *TMC_IfDataAzeCorrError* procedure. Creates request for
        !! getting the ATR error status.
        !!
        !! The instrument returns the following responses:
        !!
        !! * `grc`    – GeoCOM return code.
        !! * `atrerr` – ATR correction error occured [bool].
        !!
        !! | Property       | Values                                           |
        !! |----------------|--------------------------------------------------|
        !! | Instruments    | TPS1100, TPS1200, TM30/TS30, TS16                |
        !! | ASCII request  | `%R1Q,2114:`                                     |
        !! | ASCII response | `%R1P,0,0:<grc>,<atrerr>`                        |
        !!
        character(len=*), parameter :: REQUEST_NAME    = 'get_atr_error'
        character(len=*), parameter :: REQUEST_PATTERN = GRC_PATTERN // ',(?<atrerr>\d+)'
        integer,          parameter :: REQUEST_CODE    = 2114

        type(request_type), intent(out) :: request !! Prepared request.
        type(response_type)             :: responses(2)

        responses = [ &
            response_type('grc',    type=RESPONSE_TYPE_INT32),  &
            response_type('atrerr', type=RESPONSE_TYPE_LOGICAL) &
        ]

        call dm_geocom_api_request(request, REQUEST_NAME, REQUEST_CODE, pattern=REQUEST_PATTERN, responses=responses)
    end subroutine dm_geocom_api_request_get_atr_error

    pure subroutine dm_geocom_api_request_get_atr_setting(request)
        !! Request of *BAP_GetATRSetting* procedure. Creates request for
        !! getting the current ATR Low-Vis mode.
        !!
        !! The instrument returns the following responses:
        !!
        !! * `grc`    – GeoCOM return code.
        !! * `atrset` – ATR setting (`GEOCOM_BAP_ATRSETTING`).
        !!
        !! | Property       | Values                                           |
        !! |----------------|--------------------------------------------------|
        !! | Instruments    | TPS1200, TM30/TS30, TS16                         |
        !! | ASCII request  | `%R1Q,17034:`                                    |
        !! | ASCII response | `%R1P,0,0:<grc>,<atrset>`                        |
        !!
        character(len=*), parameter :: REQUEST_NAME    = 'get_atr_setting'
        character(len=*), parameter :: REQUEST_PATTERN = GRC_PATTERN // ',(?<atrset>\d+)'
        integer,          parameter :: REQUEST_CODE    = 17034

        type(request_type), intent(out) :: request !! Prepared request.
        type(response_type)             :: responses(2)

        responses = [ &
            response_type('grc',    type=RESPONSE_TYPE_INT32), &
            response_type('atrset', type=RESPONSE_TYPE_INT32)  &
        ]

        call dm_geocom_api_request(request, REQUEST_NAME, REQUEST_CODE, pattern=REQUEST_PATTERN, responses=responses)
    end subroutine dm_geocom_api_request_get_atr_setting

    pure subroutine dm_geocom_api_request_get_binary_mode(request)
        !! Request of *COM_GetBinaryAvailable* procedure. Creates request for
        !! getting the binary attribute of the server.
        !!
        !! The instrument returns the following responses:
        !!
        !! * `grc`     – GeoCOM return code.
        !! * `binmode` – Binary operation is enabled [bool].
        !!
        !! | Property       | Values                                           |
        !! |----------------|--------------------------------------------------|
        !! | Instruments    | TPS1100, TPS1200, TM30/TS30, TS16                |
        !! | ASCII request  | `%R1Q,113:`                                      |
        !! | ASCII response | `%R1P,0,0:<grc>,<binmode>`                       |
        !!
        character(len=*), parameter :: REQUEST_NAME    = 'get_binary_mode'
        character(len=*), parameter :: REQUEST_PATTERN = GRC_PATTERN // ',(?<binmode>\d+)'
        integer,          parameter :: REQUEST_CODE    = 113

        type(request_type), intent(out) :: request !! Prepared request.
        type(response_type)             :: responses(2)

        responses = [ &
            response_type('grc',     type=RESPONSE_TYPE_INT32),  &
            response_type('binmode', type=RESPONSE_TYPE_LOGICAL) &
        ]

        call dm_geocom_api_request(request, REQUEST_NAME, REQUEST_CODE, pattern=REQUEST_PATTERN, responses=responses)
    end subroutine dm_geocom_api_request_get_binary_mode

    pure subroutine dm_geocom_api_request_get_config(request)
        !! Request of *SUP_GetConfig* procedure. Creates request for getting
        !! the power management configuration status. The power timeout
        !! specifies the time after which the device switches into the mode
        !! indicated by `autopwr`.
        !!
        !! The instrument returns the following responses:
        !!
        !! * `grc`     – GeoCOM return code.
        !! * `autopwr` – Currently activated shut-down mode (`GEOCOM_SUP_AUTO_POWER`).
        !! * `pwrtime` – Power timeout [msec].
        !!
        !! | Property       | Values                                           |
        !! |----------------|--------------------------------------------------|
        !! | Instruments    | TPS1100, TPS1200, TM30/TS30, TS16                |
        !! | ASCII request  | `%R1Q,14001:`                                    |
        !! | ASCII response | `%R1P,0,0:<grc>,0,<autopwr>,<pwrtime>`           |
        !!
        character(len=*), parameter :: REQUEST_NAME    = 'get_config'
        character(len=*), parameter :: REQUEST_PATTERN = GRC_PATTERN // ',(?<autopwr>\d+),(?<pwrtime>\d+)'
        integer,          parameter :: REQUEST_CODE    = 14001

        type(request_type), intent(out) :: request !! Prepared request.
        type(response_type)             :: responses(3)

        responses = [ &
            response_type('grc',     unit=' ',  type=RESPONSE_TYPE_INT32), &
            response_type('autopwr', unit=' ',  type=RESPONSE_TYPE_INT32), &
            response_type('pwrtime', unit='ms', type=RESPONSE_TYPE_INT32)  &
        ]

        call dm_geocom_api_request(request, REQUEST_NAME, REQUEST_CODE, pattern=REQUEST_PATTERN, responses=responses)
    end subroutine dm_geocom_api_request_get_config

    pure subroutine dm_geocom_api_request_get_coordinate(request, wait_time, inc_mode)
        !! Request of *TMC_GetCoordinate* procedure. Creates request for
        !! getting the coordinates of a measured point.
        !!
        !! This function conducts an angle and, in dependence of the selected
        !! `inc_mode`, an inclination measurement, and then calculates the
        !! coordinates of the measured point with the last distance.
        !!
        !! The argument `wait_time` specifies the delay to wait for the
        !! distance measurement to finish. Single and tracking measurements are
        !! supported. The quality of the result is returned in the GeoCOM
        !! return code.
        !!
        !! The instrument returns the following responses:
        !!
        !! * `grc`     – GeoCOM return code.
        !! * `east`    – E coordinate [m].
        !! * `north`   – N coordinate [m]
        !! * `height`  – H coordinate [m].
        !! * `ctime`   – Timestamp of distance measurement [msec].
        !! * `eastc`   – E coordinate (continuously) [m].
        !! * `northc`  – N coordinate (continuously) [m].
        !! * `heightc` – H coordinate (continuously) [m].
        !! * `ctimec`  – Timestamp of continuous measurement [msec].
        !!
        !! | Property       | Values                                                                               |
        !! |----------------|--------------------------------------------------------------------------------------|
        !! | Instruments    | TPS1100, TPS1200, TM30/TS30, TS16                                                    |
        !! | ASCII request  | `%R1Q,2082:<wait_time>,<inc_mode>`                                                   |
        !! | ASCII response | `%R1P,0,0:<grc>,<east>,<north>,<height>,<ctime>,<eastc>,<northc>,<heightc>,<ctimec>` |
        !!
        character(len=*), parameter :: REQUEST_NAME    = 'get_coordinate'
        character(len=*), parameter :: REQUEST_PATTERN = &
            GRC_PATTERN // ',(?<east>[-\d\.]+),(?<north>[-\d\.]+),(?<height>[-\d\.]+),(?<ctime>\d+),' // &
            '(?<eastc>[-\d\.]+),(?<northc>[-\d\.]+),(?<heightc>[-\d\.]+),(?<ctimec>\d+)'
        integer,          parameter :: REQUEST_CODE    = 2082

        type(request_type), intent(out) :: request   !! Prepared request.
        integer,            intent(in)  :: wait_time !! Delay to wait for the distance measurement to finish [msec].
        integer,            intent(in)  :: inc_mode  !! Inclination measurement mode (`GEOCOM_TMC_INCLINE_PRG`).

        character(len=80)   :: args
        type(response_type) :: responses(9)

        write (args, '(i0, ",", i0)') wait_time, inc_mode

        responses = [ &
            response_type('grc',     unit=' ',  type=RESPONSE_TYPE_INT32),  & ! GeoCOM return code.
            response_type('east',    unit='m',  type=RESPONSE_TYPE_REAL64), & ! E coordinate [m].
            response_type('north',   unit='m',  type=RESPONSE_TYPE_REAL64), & ! N coordinate [m]
            response_type('height',  unit='m',  type=RESPONSE_TYPE_REAL64), & ! H coordinate [m].
            response_type('ctime',   unit='ms', type=RESPONSE_TYPE_INT64),  & ! Timestamp of distance measurement [msec].
            response_type('eastc',   unit='m',  type=RESPONSE_TYPE_REAL64), & ! E coordinate (continuously) [m].
            response_type('northc',  unit='m',  type=RESPONSE_TYPE_REAL64), & ! N coordinate (continuously) [m].
            response_type('heightc', unit='m',  type=RESPONSE_TYPE_REAL64), & ! H coordinate (continuously) [m].
            response_type('ctimec',  unit='ms', type=RESPONSE_TYPE_INT64)   & ! Timestamp of continuous measurement [m].
        ]

        call dm_geocom_api_request(request, REQUEST_NAME, REQUEST_CODE, args, REQUEST_PATTERN, responses)
    end subroutine dm_geocom_api_request_get_coordinate

    pure subroutine dm_geocom_api_request_get_date_time(request)
        !! Request of *CSV_GetDateTime* procedure. Creates request for getting
        !! the current date and time of the instrument. A possible response may
        !! look like `%R1P,0,0:0,1996,'07','19','10','13','2f'`.
        !!
        !! The instrument returns the following responses:
        !!
        !! * `grc`    – GeoCOM return code.
        !! * `year`   – Year.
        !! * `month`  – Month [byte].
        !! * `day`    – Day of month [byte].
        !! * `hour`   – Hours [byte].
        !! * `minute` – Minutes [byte].
        !! * `second` – Seconds [byte].
        !!
        !! | Property       | Values                                                                   |
        !! |----------------|--------------------------------------------------------------------------|
        !! | Instruments    | TPS1100, TPS1200, TM30/TS30, TS16                                        |
        !! | ASCII request  | `%R1Q,5008:`                                                             |
        !! | ASCII response | `%R1P,0,0:<grc>,<year>,'<month>','<day>','<hour>','<minute>','<second>'` |
        !!
        character(len=*), parameter :: REQUEST_NAME    = 'get_date_time'
        character(len=*), parameter :: REQUEST_PATTERN = &
            GRC_PATTERN // ",(?<year>\d+),'(?<month>[0-9a-f]+)','(?<day>[0-9a-f]+)'," // &
            "'(?<hour>[0-9a-f]+)','(?<minute>[0-9a-f]+)','(?<second>[0-9a-f]+)'"
        integer,          parameter :: REQUEST_CODE    = 5008

        type(request_type), intent(out) :: request !! Prepared request.
        type(response_type)             :: responses(7)

        responses = [ &
            response_type('grc',    type=RESPONSE_TYPE_INT32), &
            response_type('year',   type=RESPONSE_TYPE_INT32), &
            response_type('month',  type=RESPONSE_TYPE_BYTE),  &
            response_type('day',    type=RESPONSE_TYPE_BYTE),  &
            response_type('hour',   type=RESPONSE_TYPE_BYTE),  &
            response_type('minute', type=RESPONSE_TYPE_BYTE),  &
            response_type('second', type=RESPONSE_TYPE_BYTE)   &
        ]

        call dm_geocom_api_request(request, REQUEST_NAME, REQUEST_CODE, pattern=REQUEST_PATTERN, responses=responses)
    end subroutine dm_geocom_api_request_get_date_time

    pure subroutine dm_geocom_api_request_get_date_time_centi(request)
        !! Request of *CSV_GetDateTimeCentiSec* procedure. Creates request for
        !! getting the current date and time of the instrument, including
        !! centiseconds.
        !!
        !! The instrument returns the following responses:
        !!
        !! * `grc`     – GeoCOM return code.
        !! * `year`    – Year.
        !! * `month`   – Month.
        !! * `day`     – Day.
        !! * `hour`    – Hours.
        !! * `minute`  – Minutes.
        !! * `second`  – Seconds.
        !! * `csecond` – Centiseconds.
        !!
        !! | Property       | Values                                                                   |
        !! |----------------|--------------------------------------------------------------------------|
        !! | Instruments    | TPS1200, TM30/TS30, TS16                                                 |
        !! | ASCII request  | `%R1Q,5117:`                                                             |
        !! | ASCII response | `%R1P,0,0:<grc>,<year>,<month>,<day>,<hour>,<minute>,<second>,<csecond>` |
        !!
        character(len=*), parameter :: REQUEST_NAME    = 'get_date_time_centi'
        character(len=*), parameter :: REQUEST_PATTERN = &
            GRC_PATTERN // ',(?<year>\d+),(?<month>\d+),(?<day>\d+),(?<hour>\d+),' // &
            '(?<minute>\d+),(?<second>\d+),(?<csecond>\d+)'
        integer,          parameter :: REQUEST_CODE    = 5117

        type(request_type), intent(out) :: request !! Prepared request.
        type(response_type)             :: responses(8)

        responses = [ &
            response_type('grc',     type=RESPONSE_TYPE_INT32), &
            response_type('year',    type=RESPONSE_TYPE_INT32), &
            response_type('month',   type=RESPONSE_TYPE_INT32), &
            response_type('day',     type=RESPONSE_TYPE_INT32), &
            response_type('hour',    type=RESPONSE_TYPE_INT32), &
            response_type('minute',  type=RESPONSE_TYPE_INT32), &
            response_type('second',  type=RESPONSE_TYPE_INT32), &
            response_type('csecond', type=RESPONSE_TYPE_INT32)  &
        ]

        call dm_geocom_api_request(request, REQUEST_NAME, REQUEST_CODE, pattern=REQUEST_PATTERN, responses=responses)
    end subroutine dm_geocom_api_request_get_date_time_centi

    pure subroutine dm_geocom_api_request_get_device_config(request)
        !! Request of *CSV_GetDeviceConfig* procedure. Creates request for
        !! getting the instrument configuration.
        !!
        !! The instrument returns the following responses:
        !!
        !! * `grc`      – GeoCOM return code.
        !! * `devclass` – Device precision class (`GEOCOM_TPS_DEVICE_CLASS`).
        !! * `devtype`  – Device configuration type (`GEOCOM_TPS_DEVICE_TYPE`).
        !!
        !! | Property       | Values                                           |
        !! |----------------|--------------------------------------------------|
        !! | Instruments    | TPS1100, TPS1200, TM30/TS30, TS16                |
        !! | ASCII request  | `%R1Q,5035:`                                     |
        !! | ASCII response | `%R1P,0,0:<grc>,<devclass>,<devtype>`            |
        !!
        character(len=*), parameter :: REQUEST_NAME    = 'get_device_config'
        character(len=*), parameter :: REQUEST_PATTERN = GRC_PATTERN // ',(?<devclass>\d+),(?<devtype>\d+)'
        integer,          parameter :: REQUEST_CODE    = 5035

        type(request_type), intent(out) :: request !! Prepared request.
        type(response_type)             :: responses(3)

        responses = [ &
            response_type('grc',      type=RESPONSE_TYPE_INT32), &
            response_type('devclass', type=RESPONSE_TYPE_INT32), &
            response_type('devtype',  type=RESPONSE_TYPE_INT32)  &
        ]

        call dm_geocom_api_request(request, REQUEST_NAME, REQUEST_CODE, pattern=REQUEST_PATTERN, responses=responses)
    end subroutine dm_geocom_api_request_get_device_config

    pure subroutine dm_geocom_api_request_get_double_precision(request)
        !! Request of *COM_GetDoublePrecision* procedure. Creates request for
        !! getting the double precision setting – the number of digits to the
        !! right of the decimal point – when double floating-point values are
        !! transmitted.
        !!
        !! The instrument returns the following responses:
        !!
        !! * `grc`     – GeoCOM return code.
        !! * `ndigits` – Number of digits to the right of the decimal point.
        !!
        !! | Property       | Values                                           |
        !! |----------------|--------------------------------------------------|
        !! | Instruments    | TPS1100, TPS1200, TM30/TS30, TS16                |
        !! | ASCII request  | `%R1Q,108:`                                      |
        !! | ASCII response | `%R1P,0,0:<grc>,<ndigits>`                       |
        !!
        character(len=*), parameter :: REQUEST_NAME    = 'get_double_precision'
        character(len=*), parameter :: REQUEST_PATTERN = GRC_PATTERN // ',(?<ndigits>\d+)'
        integer,          parameter :: REQUEST_CODE    = 108

        type(request_type), intent(out) :: request !! Prepared request.
        type(response_type)             :: responses(2)

        responses = [ &
            response_type('grc',     type=RESPONSE_TYPE_INT32), &
            response_type('ndigits', type=RESPONSE_TYPE_INT32)  &
        ]

        call dm_geocom_api_request(request, REQUEST_NAME, REQUEST_CODE, pattern=REQUEST_PATTERN, responses=responses)
    end subroutine dm_geocom_api_request_get_double_precision

    pure subroutine dm_geocom_api_request_get_edm_mode(request)
        !! Request of *TMC_GetEdmMode* procedure. Creates request for getting
        !! the EDM measurement mode.
        !!
        !! The instrument returns the following responses:
        !!
        !! * `grc`     – GeoCOM return code.
        !! * `edmmode` – EDM mode (`GEOCOM_EDM_MODE`).
        !!
        !! | Property       | Values                                           |
        !! |----------------|--------------------------------------------------|
        !! | Instruments    | TPS1100, TPS1200, TM30/TS30, TS16                |
        !! | ASCII request  | `%R1Q,2021:`                                     |
        !! | ASCII response | `%R1P,0,0:<grc>,<edmmode>`                       |
        !!
        character(len=*), parameter :: REQUEST_NAME    = 'get_edm_mode'
        character(len=*), parameter :: REQUEST_PATTERN = GRC_PATTERN // ',(?<edmmode>\d+)'
        integer,          parameter :: REQUEST_CODE    = 2021

        type(request_type), intent(out) :: request !! Prepared request.
        type(response_type)             :: responses(2)

        responses = [ &
            response_type('grc',     type=RESPONSE_TYPE_INT32), &
            response_type('edmmode', type=RESPONSE_TYPE_INT32)  &
        ]

        call dm_geocom_api_request(request, REQUEST_NAME, REQUEST_CODE, pattern=REQUEST_PATTERN, responses=responses)
    end subroutine dm_geocom_api_request_get_edm_mode

    pure subroutine dm_geocom_api_request_get_egl_intensity(request)
        !! Request of *EDM_GetEglIntensity* procedure. Creates request for
        !! getting the value of the intensity of the electronic guide light
        !! (EGL).
        !!
        !! The instrument returns the following responses:
        !!
        !! * `grc`    – GeoCOM return code.
        !! * `eglint` – EDM EGL intensity (`GEOCOM_EDM_EGLINTENSITY_TYPE`).
        !!
        !! | Property       | Values                                           |
        !! |----------------|--------------------------------------------------|
        !! | Instruments    | TPS1100, TPS1200, TM30/TS30, TS16                |
        !! | ASCII request  | `%R1Q,1058:`                                     |
        !! | ASCII response | `%R1P,0,0:<grc>,<eglint>`                        |
        !!
        character(len=*), parameter :: REQUEST_NAME    = 'get_egl_intensity'
        character(len=*), parameter :: REQUEST_PATTERN = GRC_PATTERN // ',(?<eglint>\d+)'
        integer,          parameter :: REQUEST_CODE    = 1058

        type(request_type), intent(out) :: request !! Prepared request.
        type(response_type)             :: responses(2)

        responses = [ &
            response_type('grc',    type=RESPONSE_TYPE_INT32), &
            response_type('eglint', type=RESPONSE_TYPE_INT32)  &
        ]

        call dm_geocom_api_request(request, REQUEST_NAME, REQUEST_CODE, pattern=REQUEST_PATTERN, responses=responses)
    end subroutine dm_geocom_api_request_get_egl_intensity

    pure subroutine dm_geocom_api_request_get_face(request)
        !! Request of *TMC_GetFace* procedure. Creates request for getting the
        !! face of the current telescope position.
        !!
        !! The instrument returns the following responses:
        !!
        !! * `grc`  – GeoCOM return code.
        !! * `face` – Telescope face (`GEOCOM_TMC_FACE`).
        !!
        !! | Property       | Values                                           |
        !! |----------------|--------------------------------------------------|
        !! | Instruments    | TPS1100, TPS1200, TM30/TS30, TS16                |
        !! | ASCII request  | `%R1Q,2026:`                                     |
        !! | ASCII response | `%R1P,0,0:<grc>,<face>`                          |
        !!
        character(len=*), parameter :: REQUEST_NAME    = 'get_face'
        character(len=*), parameter :: REQUEST_PATTERN = GRC_PATTERN // ',(?<face>\d+)'
        integer,          parameter :: REQUEST_CODE    = 2026

        type(request_type), intent(out) :: request !! Prepared request.
        type(response_type)             :: responses(2)

        responses = [ &
            response_type('grc',  type=RESPONSE_TYPE_INT32), &
            response_type('face', type=RESPONSE_TYPE_INT32)  &
        ]

        call dm_geocom_api_request(request, REQUEST_NAME, REQUEST_CODE, pattern=REQUEST_PATTERN, responses=responses)
    end subroutine dm_geocom_api_request_get_face

    pure subroutine dm_geocom_api_request_get_fine_adjust_mode(request)
        !! Request of *AUT_GetFineAdjustMode* procedure. Creates request for
        !! getting the fine adjustment positioning mode.
        !!
        !! The instrument returns the following responses:
        !!
        !! * `grc`     – GeoCOM return code.
        !! * `adjmode` – Fine adjustment positioning mode (`GEOCOM_AUT_ADJMODE`).
        !!
        !! | Property       | Values                                           |
        !! |----------------|--------------------------------------------------|
        !! | Instruments    | TPS1100, TPS1200, TM30/TS30, TS16                |
        !! | ASCII request  | `%R1Q,9030:`                                     |
        !! | ASCII response | `%R1P,0,0:<grc>,<adjmode>`                       |
        !!
        character(len=*), parameter :: REQUEST_NAME    = 'get_fine_adjust_mode'
        character(len=*), parameter :: REQUEST_PATTERN = GRC_PATTERN // ',(?<adjmode>\d+)'
        integer,          parameter :: REQUEST_CODE    = 9030

        type(request_type), intent(out) :: request !! Prepared request.
        type(response_type)             :: responses(2)

        responses = [ &
            response_type('grc',     type=RESPONSE_TYPE_INT32), &
            response_type('adjmode', type=RESPONSE_TYPE_INT32)  &
        ]

        call dm_geocom_api_request(request, REQUEST_NAME, REQUEST_CODE, pattern=REQUEST_PATTERN, responses=responses)
    end subroutine dm_geocom_api_request_get_fine_adjust_mode

    pure subroutine dm_geocom_api_request_get_full_measurement(request, wait_time, inc_mode)
        !! Request of *TMC_GetFullMeas* procedure. Creates request to query
        !! angle, inclination, and distance measurement values.
        !!
        !! The GeoCOM function returns angle, inclination, and distance
        !! measurement data, including accuracy and measurement time. This
        !! command does not issue a new distance measurement. A distance
        !! measurement has to be started in advance. If the distance is valid,
        !! the function ignores `wait_time` and returns the results
        !! immediately. If no valid distance is available, and the measurement
        !! unit is not activated, the angle measurement result is retuned after
        !! the waiting time.
        !!
        !! The instrument returns the following responses:
        !!
        !! * `grc`      – GeoCOM return code.
        !! * `hz`       – Horizontal angle [rad].
        !! * `v`        – Vertical angle [rad].
        !! * `angacc`   – Accuracy of angles [rad].
        !! * `xinc`     – Transverse axis inclination [rad].
        !! * `linc`     – Longitude axis inclidation [rad].
        !! * `incacc`   – Inclination accuracy [rad].
        !! * `sdist`    – Slope distance [m].
        !! * `disttime` – Time of distance measurement [msec].
        !!
        !! | Property       | Values                                                                       |
        !! |----------------|------------------------------------------------------------------------------|
        !! | Instruments    | TPS1200, TM30/TS30, TS16                                                     |
        !! | ASCII request  | `%R1Q,2167:<wait_time>,<inc_mode>`                                           |
        !! | ASCII response | `%R1P,0,0:<grc>,<hz>,<v>,<angacc>,<xinc>,<linc>,<incacc>,<sdist>,<disttime>` |
        !!
        character(len=*), parameter :: REQUEST_NAME    = 'get_full_measurement'
        character(len=*), parameter :: REQUEST_PATTERN = &
            '(?<grc>\d+),(?<hz>[-\d\.]+),(?<v>[-\d\.]+),(?<angacc>[-\d\.]+),(?<xinc>[-\d\.]+),' // &
            '(?<linc>[-\d\.]+),(?<incacc>[-\d\.]+),(?<sdist>[-\d\.]+),(?<disttime>[-\d\.]+)'
        integer,          parameter :: REQUEST_CODE    = 2167

        type(request_type), intent(out) :: request   !! Prepared request.
        integer,            intent(in)  :: wait_time !! Delay to wait for the distance measurement to finish [msec].
        integer,            intent(in)  :: inc_mode  !! Inclination measurement mode (`GEOCOM_TMC_INCLINE_PRG`).

        character(len=80)   :: args
        type(response_type) :: responses(9)

        write (args, '(i0, ",", i0)') wait_time, inc_mode

        responses = [ &
            response_type('grc',      unit=' ',   type=RESPONSE_TYPE_INT32),  & ! GeoCOM return code.
            response_type('hz',       unit='rad', type=RESPONSE_TYPE_REAL64), & ! Horizontal angle [rad].
            response_type('v',        unit='rad', type=RESPONSE_TYPE_REAL64), & ! Vertical angle [rad].
            response_type('angacc',   unit='rad', type=RESPONSE_TYPE_REAL64), & ! Accuracy of angles [rad].
            response_type('xinc',     unit='rad', type=RESPONSE_TYPE_REAL64), & ! Cross inclination [rad].
            response_type('linc',     unit='rad', type=RESPONSE_TYPE_REAL64), & ! Length inclination [rad].
            response_type('incacc',   unit='rad', type=RESPONSE_TYPE_REAL64), & ! Inclination accuracy [rad].
            response_type('sdist',    unit='m',   type=RESPONSE_TYPE_REAL64), & ! Distance measurement [m].
            response_type('disttime', unit='ms',  type=RESPONSE_TYPE_REAL64)  & ! Time of distance measurement [msec].
        ]

        call dm_geocom_api_request(request, REQUEST_NAME, REQUEST_CODE, args, REQUEST_PATTERN, responses)
    end subroutine dm_geocom_api_request_get_full_measurement

    pure subroutine dm_geocom_api_request_get_geocom_version(request)
        !! Request of *COM_GetSWVersion* procedure. Creates request for getting
        !! the GeoCOM server software version.
        !!
        !! The instrument returns the following responses:
        !!
        !! * `grc`   – GeoCOM return code.
        !! * `gcrel` – GeoCOM software release.
        !! * `gcver` – GeoCOM software version.
        !! * `gcsub` – GeoCOM software sub-version.
        !!
        !! | Property       | Values                                           |
        !! |----------------|--------------------------------------------------|
        !! | Instruments    | TPS1100, TPS1200, TM30/TS30, TS16                |
        !! | ASCII request  | `%R1Q,110:`                                      |
        !! | ASCII response | `%R1P,0,0:<grc>,<gcrel>,<gcver>,<gcsub>`         |
        !!
        character(len=*), parameter :: REQUEST_NAME    = 'get_geocom_version'
        character(len=*), parameter :: REQUEST_PATTERN = GRC_PATTERN // ',(?<gcrel>\d+),(?<gcver>\d+),(?<gcsub>\d+)'
        integer,          parameter :: REQUEST_CODE    = 110

        type(request_type), intent(out) :: request !! Prepared request.
        type(response_type)             :: responses(4)

        responses = [ &
            response_type('grc',   type=RESPONSE_TYPE_INT32), &
            response_type('gcrel', type=RESPONSE_TYPE_INT32), &
            response_type('gcver', type=RESPONSE_TYPE_INT32), &
            response_type('gcsub', type=RESPONSE_TYPE_INT32)  &
        ]

        call dm_geocom_api_request(request, REQUEST_NAME, REQUEST_CODE, pattern=REQUEST_PATTERN, responses=responses)
    end subroutine dm_geocom_api_request_get_geocom_version

    pure subroutine dm_geocom_api_request_get_geometric_ppm(request)
        !! Request of *TMC_GeoPpm* procedure. Creates request for getting the
        !! geometric ppm correction factor.
        !!
        !! The instrument returns the following responses:
        !!
        !! * `grc`      – GeoCOM return code.
        !! * `geomauto` – Geometric ppm calculation on/off [bool].
        !! * `scalefcm` – Scale factor on central meridian.
        !! * `offsetcm` – Offset from central meridian [m].
        !! * `hredppm`  – Height above reference ppm value [ppm].
        !! * `indippm`  – Individual ppm value [ppm].
        !!
        !! | Property       | Values                                                                |
        !! |----------------|-----------------------------------------------------------------------|
        !! | Instruments    | TPS1200, TM30/TS30, TS16                                              |
        !! | ASCII request  | `%R1Q,2154:`                                                          |
        !! | ASCII response | `%R1P,0,0:<grc>,<geomauto>,<scalefcm>,<offsetcm>,<hredppm>,<indippm>` |
        !!
        character(len=*), parameter :: REQUEST_NAME    = 'get_geometric_ppm'
        character(len=*), parameter :: REQUEST_PATTERN = &
             '(?<grc>\d+),(?<geomauto>\d+),(?<scalefcm>[-\d\.]+),(?<offsetcm>[-\d\.]+),' // &
             '(?<hredppm>[-\d\.]+),(?<indippm>[-\d\.]+)'
        integer,          parameter :: REQUEST_CODE    = 2154

        type(request_type), intent(out) :: request !! Prepared request.
        type(response_type)             :: responses(6)

        responses = [ &
            response_type('grc',      unit=' ',   type=RESPONSE_TYPE_INT32),   & ! GeoCOM return code.
            response_type('geomauto', unit=' ',   type=RESPONSE_TYPE_LOGICAL), & ! State of geometric ppm calculation [bool].
            response_type('scalefcm', unit=' ',   type=RESPONSE_TYPE_REAL64),  & ! Scale factor on central meridian.
            response_type('offsetcm', unit='m',   type=RESPONSE_TYPE_REAL64),  & ! Offset from central meridian [m].
            response_type('hredppm',  unit='ppm', type=RESPONSE_TYPE_REAL64),  & ! Height above reference ppm value [ppm].
            response_type('indippm',  unit='ppm', type=RESPONSE_TYPE_REAL64)   & ! Individual ppm value [ppm].
        ]

        call dm_geocom_api_request(request, REQUEST_NAME, REQUEST_CODE, pattern=REQUEST_PATTERN, responses=responses)
    end subroutine dm_geocom_api_request_get_geometric_ppm

    pure subroutine dm_geocom_api_request_get_height(request)
        !! Request of *TMC_GetHeight* procedure. Creates request for getting
        !! the current reflector height.
        !!
        !! The instrument returns the following responses:
        !!
        !! * `grc`     – GeoCOM return code.
        !! * `rheight` – Reflector height [m].
        !!
        !! | Property       | Values                                           |
        !! |----------------|--------------------------------------------------|
        !! | Instruments    | TPS1100, TPS1200, TM30/TS30, TS16                |
        !! | ASCII request  | `%R1Q,2011:`                                     |
        !! | ASCII response | `%R1P,0,0:<grc>,<height>`                        |
        !!
        character(len=*), parameter :: REQUEST_NAME    = 'get_height'
        character(len=*), parameter :: REQUEST_PATTERN = GRC_PATTERN // ',(?<rheight>[-\d\.]+)'
        integer,          parameter :: REQUEST_CODE    = 2011

        type(request_type), intent(out) :: request !! Prepared request.
        type(response_type)             :: responses(2)

        responses = [ &
            response_type('grc',     unit=' ', type=RESPONSE_TYPE_INT32), &
            response_type('rheight', unit='m', type=RESPONSE_TYPE_REAL64) &
        ]

        call dm_geocom_api_request(request, REQUEST_NAME, REQUEST_CODE, pattern=REQUEST_PATTERN, responses=responses)
    end subroutine dm_geocom_api_request_get_height

    pure subroutine dm_geocom_api_request_get_image_config(request, mem_type)
        !! Request of *IMG_GetTccConfig* procedure. Creates request to read
        !! the current image configuration. The response `subfunc` is a binary
        !! combination of the following settings:
        !!
        !! * `1` – Test image.
        !! * `2` – Automatic exposure time selection.
        !! * `4` – Two-times sub-sampling.
        !! * `8` – Four-times sub-sampling.
        !!
        !! The instrument returns the following responses:
        !!
        !! * `grc`      – GeoCOM return code.
        !! * `imageno`  – Actual image number.
        !! * `quality`  – JPEG compression quality factor (0 to 100).
        !! * `subfunc`  – Binary combination of sub-function number.
        !! * `fnprefix` – File name prefix.
        !!
        !! | Property       | Values                                                    |
        !! |----------------|-----------------------------------------------------------|
        !! | Instruments    | TM30/TS30                                                 |
        !! | ASCII request  | `%R1Q,23400:<mem_type>`                                   |
        !! | ASCII response | `%R1P,0,0:<grc>,<imageno>,<quality>,<subfunc>,<fnprefix>` |
        !!
        character(len=*), parameter :: REQUEST_NAME    = 'get_image_config'
        character(len=*), parameter :: REQUEST_PATTERN = &
            '(?<grc>\d+),(?<imageno>\d+),(?<quality>\d+),(?<subfunc>\d+),"(?<fnprefix>.+)"$'
        integer,          parameter :: REQUEST_CODE    = 23400

        type(request_type), intent(out) :: request  !! Prepared request.
        integer,            intent(in)  :: mem_type !! Memory device type (`GEOCOM_IMG_MEM_TYPE`).

        character(len=80)   :: args
        type(response_type) :: responses(5)

        write (args, '(i0)') mem_type

        responses = [ &
            response_type('grc',      type=RESPONSE_TYPE_INT32), &
            response_type('imageno',  type=RESPONSE_TYPE_INT32), &
            response_type('quality',  type=RESPONSE_TYPE_INT32), &
            response_type('subfunc',  type=RESPONSE_TYPE_INT32), &
            response_type('fnprefix', type=RESPONSE_TYPE_STRING) &
        ]

        call dm_geocom_api_request(request, REQUEST_NAME, REQUEST_CODE, args, REQUEST_PATTERN, responses)
    end subroutine dm_geocom_api_request_get_image_config

    pure subroutine dm_geocom_api_request_get_inclination_correction(request)
        !! Request of *TMC_GetInclineSwitch* procedure. Creates request for
        !! getting the dual-axis compensator status.
        !!
        !! The instrument returns the following responses:
        !!
        !! * `grc`    – GeoCOM return code.
        !! * `inccor` – Compensator is enabled [bool].
        !!
        !! | Property       | Values                                           |
        !! |----------------|--------------------------------------------------|
        !! | Instruments    | TPS1100, TPS1200, TM30/TS30, TS16                |
        !! | ASCII request  | `%R1Q,2007:`                                     |
        !! | ASCII response | `%R1P,0,0:<grc>,<inccor>`                        |
        !!
        character(len=*), parameter :: REQUEST_NAME    = 'get_inclination_correction'
        character(len=*), parameter :: REQUEST_PATTERN = GRC_PATTERN // ',(?<inccor>\d+)'
        integer,          parameter :: REQUEST_CODE    = 2007

        type(request_type), intent(out) :: request  !! Prepared request.
        type(response_type)             :: responses(2)

        responses = [ &
            response_type('grc',   type=RESPONSE_TYPE_INT32),  &
            response_type('inccor',type=RESPONSE_TYPE_LOGICAL) &
        ]

        call dm_geocom_api_request(request, REQUEST_NAME, REQUEST_CODE, pattern=REQUEST_PATTERN, responses=responses)
    end subroutine dm_geocom_api_request_get_inclination_correction

    pure subroutine dm_geocom_api_request_get_inclination_error(request)
        !! Request of *TMC_IfDataIncCorrError* procedure. Creates request for
        !! getting the inclination error status.
        !!
        !! The instrument returns the following responses:
        !!
        !! * `grc`    – GeoCOM return code.
        !! * `incerr` – Last measurement not incline-corrected [bool].
        !!
        !! | Property       | Values                                           |
        !! |----------------|--------------------------------------------------|
        !! | Instruments    | TPS1100, TPS1200, TM30/TS30, TS16                |
        !! | ASCII request  | `%R1Q,2115:`                                     |
        !! | ASCII response | `%R1P,0,0:<grc>,<incerr>`                        |
        !!
        character(len=*), parameter :: REQUEST_NAME    = 'get_inclination_error'
        character(len=*), parameter :: REQUEST_PATTERN = GRC_PATTERN // ',(?<incerr>\d+)'
        integer,          parameter :: REQUEST_CODE    = 2115

        type(request_type), intent(out) :: request !! Prepared request.
        type(response_type)             :: responses(2)

        responses = [ &
            response_type('grc',    type=RESPONSE_TYPE_INT32),  &
            response_type('incerr', type=RESPONSE_TYPE_LOGICAL) &
        ]

        call dm_geocom_api_request(request, REQUEST_NAME, REQUEST_CODE, pattern=REQUEST_PATTERN, responses=responses)
    end subroutine dm_geocom_api_request_get_inclination_error

    pure subroutine dm_geocom_api_request_get_instrument_name(request)
        !! Request of *CSV_GetInstrumentName* procedure. Creates request for
        !! getting the Leica-specific instrument name.
        !!
        !! The instrument returns the following responses:
        !!
        !! * `grc`  – GeoCOM return code.
        !! * `name` – Instrument name.
        !!
        !! | Property       | Values                                           |
        !! |----------------|--------------------------------------------------|
        !! | Instruments    | Nivel210, TPS1100, TPS1200, TM30/TS30, TS16      |
        !! | ASCII request  | `%R1Q,5004:`                                     |
        !! | ASCII response | `%R1P,0,0:<grc>,<name>`                          |
        !!
        character(len=*), parameter :: REQUEST_NAME    = 'get_instrument_name'
        character(len=*), parameter :: REQUEST_PATTERN = GRC_PATTERN // ',"(?<name>.+)"$'
        integer,          parameter :: REQUEST_CODE    = 5004

        type(request_type), intent(out) :: request !! Prepared request.
        type(response_type)             :: responses(2)

        responses = [ &
            response_type('grc',  type=RESPONSE_TYPE_INT32), &
            response_type('name', type=RESPONSE_TYPE_STRING) &
        ]

        call dm_geocom_api_request(request, REQUEST_NAME, REQUEST_CODE, pattern=REQUEST_PATTERN, responses=responses)
    end subroutine dm_geocom_api_request_get_instrument_name

    pure subroutine dm_geocom_api_request_get_instrument_number(request)
        !! Request of *CSV_GetInstrumentNo* procedure. Creates request for
        !! getting the factory defined instrument number.
        !!
        !! The instrument returns the following responses:
        !!
        !! * `grc`      – GeoCOM return code.
        !! * `serialno` – Serial number of the instrument (integer).
        !!
        !! | Property       | Values                                           |
        !! |----------------|--------------------------------------------------|
        !! | Instruments    | TPS1100, TPS1200, TM30/TS30, TS16                |
        !! | ASCII request  | `%R1Q,5003:`                                     |
        !! | ASCII response | `%R1P,0,0:<grc>,<serialno>`                      |
        !!
        character(len=*), parameter :: REQUEST_NAME    = 'get_instrument_number'
        character(len=*), parameter :: REQUEST_PATTERN = GRC_PATTERN // ',(?<serialno>\d+)'
        integer,          parameter :: REQUEST_CODE    = 5003

        type(request_type), intent(out) :: request !! Prepared request.
        type(response_type)             :: responses(2)

        responses = [ &
            response_type('grc',      type=RESPONSE_TYPE_INT32), &
            response_type('serialno', type=RESPONSE_TYPE_INT32)  &
        ]

        call dm_geocom_api_request(request, REQUEST_NAME, REQUEST_CODE, pattern=REQUEST_PATTERN, responses=responses)
    end subroutine dm_geocom_api_request_get_instrument_number

    pure subroutine dm_geocom_api_request_get_internal_temperature(request)
        !! Request of *CSV_GetIntTemp* procedure. Creates request for getting
        !! the internal temperature of the instrument, measured on the
        !! mainboard side.
        !!
        !! The instrument returns the following responses:
        !!
        !! * `grc`  – GeoCOM return code.
        !! * `temp` – Instrument temperature [°C].
        !!
        !! | Property       | Values                                           |
        !! |----------------|--------------------------------------------------|
        !! | Instruments    | TPS1100, TPS1200, TM30/TS30, TS16                |
        !! | ASCII request  | `%R1Q,5011:`                                     |
        !! | ASCII response | `%R1P,0,0:<grc>,<temp>`                          |
        !!
        character(len=*), parameter :: REQUEST_NAME    = 'get_internal_temperature'
        character(len=*), parameter :: REQUEST_PATTERN = GRC_PATTERN // ',(?<temp>\d+)'
        integer,          parameter :: REQUEST_CODE    = 5011

        type(request_type), intent(out) :: request !! Prepared request.
        type(response_type)             :: responses(2)

        responses = [ &
            response_type('grc',  type=RESPONSE_TYPE_INT32), &
            response_type('temp', type=RESPONSE_TYPE_INT32)  &
        ]

        call dm_geocom_api_request(request, REQUEST_NAME, REQUEST_CODE, pattern=REQUEST_PATTERN, responses=responses)
    end subroutine dm_geocom_api_request_get_internal_temperature

    pure subroutine dm_geocom_api_request_get_lock_status(request)
        !! Request of *MOT_ReadLockStatus* procedure. Creates request for
        !! returning the condition of the Lock-In control.
        !!
        !! The instrument returns the following responses:
        !!
        !! * `grc`      – GeoCOM return code.
        !! * `lockstat` – Lock status (`GEOCOM_MOT_LOCK_STATUS`).
        !!
        !! | Property       | Values                                           |
        !! |----------------|--------------------------------------------------|
        !! | Instruments    | TPS1100, TPS1200, TM30/TS30, TS16                |
        !! | ASCII request  | `%R1Q,6021:`                                     |
        !! | ASCII response | `%R1P,0,0:<grc>,<lockstat>`                      |
        !!
        character(len=*), parameter :: REQUEST_NAME    = 'get_lock_status'
        character(len=*), parameter :: REQUEST_PATTERN = GRC_PATTERN // ',(?<lockstat>\d+)'
        integer,          parameter :: REQUEST_CODE    = 6021

        type(request_type), intent(out) :: request !! Prepared request.
        type(response_type)             :: responses(2)

        responses = [ &
            response_type('grc',      type=RESPONSE_TYPE_INT32), &
            response_type('lockstat', type=RESPONSE_TYPE_INT32)  &
        ]

        call dm_geocom_api_request(request, REQUEST_NAME, REQUEST_CODE, pattern=REQUEST_PATTERN, responses=responses)
    end subroutine dm_geocom_api_request_get_lock_status

    pure subroutine dm_geocom_api_request_get_measurement_program(request)
        !! Request of *BAP_GetMeasPrg* procedure. Creates request for getting
        !! the distance measurement program of the instrument.
        !!
        !! The instrument returns the following responses:
        !!
        !! * `grc`     – GeoCOM return code.
        !! * `measprg` – Measurement program (`GEOCOM_BAP_USER_MEASPRG`).
        !!
        !! | Property       | Values                                           |
        !! |----------------|--------------------------------------------------|
        !! | Instruments    | TPS1100, TPS1200, TM30/TS30, TS16                |
        !! | ASCII request  | `%R1Q,17018:`                                    |
        !! | ASCII response | `%R1P,0,0:<grc>,<measprg>`                       |
        !!
        character(len=*), parameter :: REQUEST_NAME    = 'get_measurement_program'
        integer,          parameter :: REQUEST_CODE    = 17018
        character(len=*), parameter :: REQUEST_PATTERN = GRC_PATTERN // ',(?<measprg>\d+)'

        type(request_type), intent(out) :: request !! Prepared request.
        type(response_type)             :: responses(2)

        responses = [ &
            response_type('grc',     type=RESPONSE_TYPE_INT32), &
            response_type('measprg', type=RESPONSE_TYPE_INT32)  &
        ]

        call dm_geocom_api_request(request, REQUEST_NAME, REQUEST_CODE, pattern=REQUEST_PATTERN, responses=responses)
    end subroutine dm_geocom_api_request_get_measurement_program

    pure subroutine dm_geocom_api_request_get_power(request)
        !! Request of *CSV_CheckPower* procedure. Creates request for checking
        !! the available power.
        !!
        !! The instrument returns the following responses:
        !!
        !! * `grc`      – GeoCOM return code.
        !! * `battlife` – Battery capacity [%].
        !! * `pwrsrc`   – Power source (`GEOCOM_CSV_POWER_PATH`).
        !! * `pwrsug`   – Not supported (`GEOCOM_CSV_POWER_PATH`).
        !!
        !! | Property       | Values                                           |
        !! |----------------|--------------------------------------------------|
        !! | Instruments    | TPS1200, TM30/TS30, TS16                         |
        !! | ASCII request  | `%R1Q,5039:`                                     |
        !! | ASCII response | `%R1P,0,0:<grc>,<battlife>,<pwrsrc>, <pwrsug>`   |
        !!
        character(len=*), parameter :: REQUEST_NAME    = 'get_power'
        character(len=*), parameter :: REQUEST_PATTERN = GRC_PATTERN // ',(?<battlife>\d+),(?<pwrsrc>\d+),(?<pwrsug>\d+)'
        integer,          parameter :: REQUEST_CODE    = 5039

        type(request_type), intent(out) :: request !! Prepared request.
        type(response_type)             :: responses(4)

        responses = [ &
            response_type('grc',      unit=' ', type=RESPONSE_TYPE_INT32), &
            response_type('battlife', unit='%', type=RESPONSE_TYPE_INT32), &
            response_type('pwrsrc',   unit=' ', type=RESPONSE_TYPE_INT32), &
            response_type('pwrsug',   unit=' ', type=RESPONSE_TYPE_INT32)  &
        ]

        call dm_geocom_api_request(request, REQUEST_NAME, REQUEST_CODE, pattern=REQUEST_PATTERN, responses=responses)
    end subroutine dm_geocom_api_request_get_power

    pure subroutine dm_geocom_api_request_get_prism_constant(request)
        !! Request of *TMC_GetPrismCorr* procedure. Creates request for
        !! getting the prism constant.
        !!
        !! The instrument returns the following responses:
        !!
        !! * `grc`     – GeoCOM return code.
        !! * `reflcor` – Prism correction constant [m].
        !!
        !! | Property       | Values                                           |
        !! |----------------|--------------------------------------------------|
        !! | Instruments    | TPS1100, TPS1200, TM30/TS30, TS16                |
        !! | ASCII request  | `%R1Q,2023:`                                     |
        !! | ASCII response | `%R1P,0,0:<grc>,<reflcor>`                       |
        !!
        character(len=*), parameter :: REQUEST_NAME    = 'get_prism_constant'
        character(len=*), parameter :: REQUEST_PATTERN = GRC_PATTERN // ',(?<reflcor>[-\d\.]+)'
        integer,          parameter :: REQUEST_CODE    = 2023

        type(request_type), intent(out) :: request !! Prepared request.
        type(response_type)             :: responses(2)

        responses = [ &
            response_type('grc',     unit=' ', type=RESPONSE_TYPE_INT32), &
            response_type('reflcor', unit='m', type=RESPONSE_TYPE_REAL64) &
        ]

        call dm_geocom_api_request(request, REQUEST_NAME, REQUEST_CODE, pattern=REQUEST_PATTERN, responses=responses)
    end subroutine dm_geocom_api_request_get_prism_constant

    pure subroutine dm_geocom_api_request_get_prism_definition(request, prism_type)
        !! Request of *BAP_GetPrismDef* procedure. Creates request for getting
        !! the default prism definition.
        !!
        !! The instrument returns the following responses:
        !!
        !! * `grc`      – GeoCOM return code.
        !! * `prsmname` – Prism name [string].
        !! * `prsmcor`  – Prism correction constant [m].
        !! * `prsmtype` – Prism type (`GEOCOM_BAP_PRISMTYPE`).
        !!
        !! | Property       | Values                                           |
        !! |----------------|--------------------------------------------------|
        !! | Instruments    | TPS1100, TPS1200, TM30/TS30, TS16                |
        !! | ASCII request  | `%R1Q,17023:<prism_type>`                        |
        !! | ASCII response | `%R1P,0,0:<grc>,<prsmname>,<prsmcor>,<prsmtype>` |
        !!
        character(len=*), parameter :: REQUEST_NAME    = 'get_prism_definition'
        character(len=*), parameter :: REQUEST_PATTERN = &
            '(?<grc>\d+),"(?<prsmname>.+)",(?<prsmcor>[-\d\.]+),(?<prsmtype>\d+)'
        integer,          parameter :: REQUEST_CODE    = 17023

        type(request_type), intent(out) :: request    !! Prepared request.
        integer,            intent(in)  :: prism_type !! Prism type (`GEOCOM_BAP_PRISMTYPE`).

        character(len=80)   :: args
        type(response_type) :: responses(4)

        write (args, '(i0)') prism_type

        responses = [ &
            response_type('grc',      unit=' ', type=RESPONSE_TYPE_INT32),  &
            response_type('prsmname', unit=' ', type=RESPONSE_TYPE_STRING), &
            response_type('prsmcor',  unit='m', type=RESPONSE_TYPE_REAL64), &
            response_type('prsmtype', unit=' ', type=RESPONSE_TYPE_INT32)   &
        ]

        call dm_geocom_api_request(request, REQUEST_NAME, REQUEST_CODE, args, REQUEST_PATTERN, responses)
    end subroutine dm_geocom_api_request_get_prism_definition

    pure subroutine dm_geocom_api_request_get_prism_type(request)
        !! Request of *TMC_GetPrismType* procedure. Creates request for
        !! getting the default prism type.
        !!
        !! The instrument returns the following responses:
        !!
        !! * `grc`      – GeoCOM return code.
        !! * `prsmtype` – Prism type (`GEOCOM_BAP_PRISMTYPE`).
        !!
        !! | Property       | Values                                           |
        !! |----------------|--------------------------------------------------|
        !! | Instruments    | TPS1200, TM30/TS30, TS16                         |
        !! | ASCII request  | `%R1Q,17009:`                                    |
        !! | ASCII response | `%R1P,0,0:<grc>,<pristype>`                      |
        !!
        character(len=*), parameter :: REQUEST_NAME    = 'get_prism_type'
        character(len=*), parameter :: REQUEST_PATTERN = GRC_PATTERN // ',(?<prsmtype>\d+)'
        integer,          parameter :: REQUEST_CODE    = 17009

        type(request_type), intent(out) :: request !! Prepared request.
        type(response_type)             :: responses(2)

        responses = [ &
            response_type('grc',      type=RESPONSE_TYPE_INT32), &
            response_type('prsmtype', type=RESPONSE_TYPE_INT32)  &
        ]

        call dm_geocom_api_request(request, REQUEST_NAME, REQUEST_CODE, pattern=REQUEST_PATTERN, responses=responses)
    end subroutine dm_geocom_api_request_get_prism_type

    pure subroutine dm_geocom_api_request_get_prism_type_v2(request)
        !! Request of *TMC_GetPrismType2* procedure. Creates request for
        !! getting the default or user prism type.
        !!
        !! The instrument returns the following responses:
        !!
        !! * `grc`      – GeoCOM return code.
        !! * `prsmtype` – Prism type (`GEOCOM_BAP_PRISMTYPE`).
        !!
        !! | Property       | Values                                           |
        !! |----------------|--------------------------------------------------|
        !! | Instruments    | TPS1200, TM30/TS30, TS16                         |
        !! | ASCII request  | `%R1Q,17031:`                                    |
        !! | ASCII response | `%R1P,0,0:<grc>,<prsmtype>`                      |
        !!
        character(len=*), parameter :: REQUEST_NAME    = 'get_prism_type_v2'
        character(len=*), parameter :: REQUEST_PATTERN = GRC_PATTERN // ',(?<prsmtype>\d+)'
        integer,          parameter :: REQUEST_CODE    = 17031

        type(request_type), intent(out) :: request !! Prepared request.
        type(response_type)             :: responses(2)

        responses = [ &
            response_type('grc',      type=RESPONSE_TYPE_INT32), &
            response_type('prsmtype', type=RESPONSE_TYPE_INT32)  &
        ]

        call dm_geocom_api_request(request, REQUEST_NAME, REQUEST_CODE, pattern=REQUEST_PATTERN, responses=responses)
    end subroutine dm_geocom_api_request_get_prism_type_v2

    pure subroutine dm_geocom_api_request_get_quick_distance(request)
        !! Request of *TMC_QuickDist* procedure. Creates request for returning
        !! the slope distance and both angles.
        !!
        !! The function starts an EDM tracking measurement, and waits until a
        !! distance has been measured. Then, it returns the angles and the
        !! slope distance, but no coordinates. If no distance could be
        !! measured, only angles and an error code are returned. A measurement
        !! may be aborted by calling *TMC_DoMeasure*.
        !!
        !! The instrument returns the following responses:
        !!
        !! * `grc`   – GeoCOM return code.
        !! * `hz`    – Horizontal angle [rad].
        !! * `v`     – Vertical angle [rad].
        !! * `sdist` – Slope distance [m].
        !!
        !! | Property       | Values                                           |
        !! |----------------|--------------------------------------------------|
        !! | Instruments    | TPS1100, TPS1200, TM30/TS30, TS16                |
        !! | ASCII request  | `%R1Q,2117:`                                     |
        !! | ASCII response | `%R1P,0,0:<grc>,<hz>,<v>,<sdist>`                |
        !!
        character(len=*), parameter :: REQUEST_NAME    = 'get_quick_distance'
        character(len=*), parameter :: REQUEST_PATTERN = &
            '(?<grc>\d+),(?<hz>[-\d\.]+),(?<v>[-\d\.]+),(?<sdist>[-\d\.]+)'
        integer,          parameter :: REQUEST_CODE    = 2117

        type(request_type), intent(out) :: request !! Prepared request.
        type(response_type)             :: responses(4)

        responses = [ &
            response_type('grc',   unit=' ',   type=RESPONSE_TYPE_INT32),  &
            response_type('hz',    unit='rad', type=RESPONSE_TYPE_REAL64), &
            response_type('v',     unit='rad', type=RESPONSE_TYPE_REAL64), &
            response_type('sdist', unit='m',   type=RESPONSE_TYPE_REAL64)  &
        ]

        call dm_geocom_api_request(request, REQUEST_NAME, REQUEST_CODE, pattern=REQUEST_PATTERN, responses=responses)
    end subroutine dm_geocom_api_request_get_quick_distance

    pure subroutine dm_geocom_api_request_get_reduced_atr_fov(request)
        !! Request of *BAP_GetRedATRFov* procedure. Creates request for
        !! getting the reduced ATR field of view.
        !!
        !! The instrument returns the following responses:
        !!
        !! * `grc`    – GeoCOM return code.
        !! * `atrfov` – ATR uses reduced field of view (about 1/9) [bool].
        !!
        !! | Property       | Values                                           |
        !! |----------------|--------------------------------------------------|
        !! | Instruments    | TPS1200, TM30/TS30, TS16                         |
        !! | ASCII request  | `%R1Q,17036:`                                    |
        !! | ASCII response | `%R1P,0,0:<grc>,<atrfov>`                        |
        !!
        character(len=*), parameter :: REQUEST_NAME    = 'get_reduced_atr_fov'
        character(len=*), parameter :: REQUEST_PATTERN = GRC_PATTERN // ',(?<atrfov>\d+)'
        integer,          parameter :: REQUEST_CODE    = 17036

        type(request_type), intent(out) :: request !! Prepared request.
        type(response_type)             :: responses(2)

        responses = [ &
            response_type('grc',    type=RESPONSE_TYPE_INT32),  &
            response_type('atrfov', type=RESPONSE_TYPE_LOGICAL) &
        ]

        call dm_geocom_api_request(request, REQUEST_NAME, REQUEST_CODE, pattern=REQUEST_PATTERN, responses=responses)
    end subroutine dm_geocom_api_request_get_reduced_atr_fov

    pure subroutine dm_geocom_api_request_get_reflectorless_class(request)
        !! Request of *CSV_GetReflectorlessClass* procedure. Creates request
        !! for getting the RL type.
        !!
        !! The function returns the class of the reflectorless and long-range
        !! distance measurement of the instrument.
        !!
        !! The instrument returns the following responses:
        !!
        !! * `grc`     – GeoCOM return code.
        !! * `rlclass` – Reflectorless class (`GEOCOM_TPS_REFLESS_CLASS`).
        !!
        !! | Property       | Values                                           |
        !! |----------------|--------------------------------------------------|
        !! | Instruments    | TPS1200, TM30/TS30, TS16                         |
        !! | ASCII request  | `%R1Q,5100:`                                     |
        !! | ASCII response | `%R1P,0,0:<grc>,<rlclass>`                       |
        !!
        character(len=*), parameter :: REQUEST_NAME    = 'get_reflectorless_class'
        character(len=*), parameter :: REQUEST_PATTERN = GRC_PATTERN // ',(?<rlclass>\d+)'
        integer,          parameter :: REQUEST_CODE    = 5100

        type(request_type), intent(out) :: request !! Prepared request.
        type(response_type)             :: responses(2)

        responses = [ &
            response_type('grc',     type=RESPONSE_TYPE_INT32), &
            response_type('rlclass', type=RESPONSE_TYPE_INT32)  &
        ]

        call dm_geocom_api_request(request, REQUEST_NAME, REQUEST_CODE, pattern=REQUEST_PATTERN, responses=responses)
    end subroutine dm_geocom_api_request_get_reflectorless_class

    pure subroutine dm_geocom_api_request_get_refraction_mode(request)
        !! Request of *TMC_GetRefractiveMethod* procedure. Creates request for
        !! getting the refraction model.
        !!
        !! The function is used to get the current refraction model. Changing
        !! the method is not indicated on the interface of the instrument.
        !!
        !! The instrument returns the following responses:
        !!
        !! * `grc`      – GeoCOM return code.
        !! * `refrmode` – Refraction mode (`1` for world, `2` for Australia).
        !!
        !! | Property       | Values                                           |
        !! |----------------|--------------------------------------------------|
        !! | Instruments    | TPS1100, TPS1200, TM30/TS30, TS16                |
        !! | ASCII request  | `%R1Q,2091:`                                     |
        !! | ASCII response | `%R1P,0,0:<grc>,<refrmode>`                      |
        !!
        character(len=*), parameter :: REQUEST_NAME    = 'get_refraction_mode'
        character(len=*), parameter :: REQUEST_PATTERN = GRC_PATTERN // ',(?<refrmode>\d+)'
        integer,          parameter :: REQUEST_CODE    = 2091

        type(request_type), intent(out) :: request !! Prepared request.
        type(response_type)             :: responses(2)

        responses = [ &
            response_type('grc',      type=RESPONSE_TYPE_INT32), &
            response_type('refrmode', type=RESPONSE_TYPE_INT32)  &
        ]

        call dm_geocom_api_request(request, REQUEST_NAME, REQUEST_CODE, pattern=REQUEST_PATTERN, responses=responses)
    end subroutine dm_geocom_api_request_get_refraction_mode

    pure subroutine dm_geocom_api_request_get_search_area(request)
        !! Request of *AUT_GetSearchArea* procedure. Creates request for
        !! getting the dimensions of the PowerSearch window.
        !!
        !! This command is valid for all instruments, but has only effects for
        !! instruments equipped with PowerSearch.
        !!
        !! The instrument returns the following responses:
        !!
        !! * `grc`      – GeoCOM return code.
        !! * `centerhz` – Hz angle of search area (center) [rad].
        !! * `centerv`  – V angle of search area (center) [rad].
        !! * `rangehz`  – Width of search area [rad].
        !! * `rangev`   – Max. height of search area [rad].
        !! * `userarea` – User-defined search area is active [bool].
        !!
        !! | Property       | Values                                                              |
        !! |----------------|---------------------------------------------------------------------|
        !! | Instruments    | TPS1100, TPS1200, TM30/TS30, TS16                                   |
        !! | ASCII request  | `%R1Q,9042:`                                                        |
        !! | ASCII response | `%R1P,0,0:<grc>,<centerhz>,<centerv>,<rangehz>,<rangev>,<userarea>` |
        !!
        character(len=*), parameter :: REQUEST_NAME    = 'get_search_area'
        character(len=*), parameter :: REQUEST_PATTERN = &
            '(?<grc>\d+),(?<centerhz>[-\d\.]+),(?<centerv>[-\d\.]+),(?<rangehz>[-\d\.]+),' // &
            '(?<rangev>[-\d\.]+),(?<userarea>\d+)'
        integer,          parameter :: REQUEST_CODE    = 9042

        type(request_type), intent(out) :: request !! Prepared request.
        type(response_type)             :: responses(6)

        responses = [ &
            response_type('grc',      unit=' ',   type=RESPONSE_TYPE_INT32),  &
            response_type('centerhz', unit='rad', type=RESPONSE_TYPE_REAL64), &
            response_type('centerv' , unit='rad', type=RESPONSE_TYPE_REAL64), &
            response_type('rangehz',  unit='rad', type=RESPONSE_TYPE_REAL64), &
            response_type('rangev',   unit='rad', type=RESPONSE_TYPE_REAL64), &
            response_type('userarea', unit=' ',   type=RESPONSE_TYPE_LOGICAL) &
        ]

        call dm_geocom_api_request(request, REQUEST_NAME, REQUEST_CODE, pattern=REQUEST_PATTERN, responses=responses)
    end subroutine dm_geocom_api_request_get_search_area

    pure subroutine dm_geocom_api_request_get_signal(request)
        !! Request of *TMC_GetSignal* procedure. Creates request for getting
        !! the EDM signal intensity.
        !!
        !! The function returns the intensity of the EDM signal. The function
        !! can only perform a measurement if the signal measurement program is
        !! activated. Start the signal measurement program with *TMC_DoMeasure* in
        !! program `GEOCOM_TMC_SIGNAL`. After the measurement, the EDM must be
        !! switched off with program `GEOCOM_TMC_CLEAR`. While measuring, there
        !! is no angle data available.
        !!
        !! The instrument returns the following responses:
        !!
        !! * `grc`     – GeoCOM return code.
        !! * `sigint`  – Signal intensity of EDM [%].
        !! * `sigtime` – Timestamp [msec].
        !!
        !! | Property       | Values                                           |
        !! |----------------|--------------------------------------------------|
        !! | Instruments    | TPS1100, TPS1200, TM30/TS30, TS16                |
        !! | ASCII request  | `%R1Q,2022:`                                     |
        !! | ASCII response | `%R1P,0,0:<grc>,<sigint>,<sigtime>`              |
        !!
        character(len=*), parameter :: REQUEST_NAME    = 'get_signal'
        character(len=*), parameter :: REQUEST_PATTERN = GRC_PATTERN // ',(?<sigint>[-\d\.]+),(?<sigtime>\d+)'
        integer,          parameter :: REQUEST_CODE    = 2022

        type(request_type), intent(out) :: request !! Prepared request.
        type(response_type)             :: responses(3)

        responses = [ &
            response_type('grc',     unit=' ',  type=RESPONSE_TYPE_INT32),  &
            response_type('sigint',  unit='%',  type=RESPONSE_TYPE_REAL64), &
            response_type('sigtime', unit='ms', type=RESPONSE_TYPE_LOGICAL) &
        ]

        call dm_geocom_api_request(request, REQUEST_NAME, REQUEST_CODE, pattern=REQUEST_PATTERN, responses=responses)
    end subroutine dm_geocom_api_request_get_signal

    pure subroutine dm_geocom_api_request_get_simple_coordinates(request, wait_time, inc_mode)
        !! Request of *TMC_GetSimpleCoord* procedure. Creates request for
        !! returning cartesian coordinates.
        !!
        !! The API function returns the cartesian coordinates if a valid
        !! distance is set. The argument `wait_time` sets the maximum time to
        !! wait for a valid distance. Without a valid distance, the coordinates
        !! are set to 0.0, and an error is returned. The coordinate calculation
        !! requires inclination results. The argument `inc_mode` sets the
        !! inclination measurement mode (`GEOCOM_TMC_INCLINE_PRG`).
        !!
        !! The instrument returns the following responses:
        !!
        !! * `grc`    – GeoCOM return code.
        !! * `east`   – Easting [m].
        !! * `north`  – Northing [m].
        !! * `height` – Orthometric height [m].
        !!
        !! | Property       | Values                                           |
        !! |----------------|--------------------------------------------------|
        !! | Instruments    | TPS1100, TPS1200, TM30/TS30, TS16                |
        !! | ASCII request  | `%R1Q,2116:<wait_time>,<inc_mode>`               |
        !! | ASCII response | `%R1P,0,0:<grc>,<east>,<north>,<height>`         |
        !!
        character(len=*), parameter :: REQUEST_NAME    = 'get_simple_coordinates'
        character(len=*), parameter :: REQUEST_PATTERN = &
            '(?<grc>\d+),(?<east>[-\d\.]+),(?<north>[-\d\.]+),(?<height>[-\d\.]+)'
        integer,          parameter :: REQUEST_CODE    = 2116

        type(request_type), intent(out) :: request   !! Prepared request.
        integer,            intent(in)  :: wait_time !! Delay to wait for the distance measurement to finish [msec].
        integer,            intent(in)  :: inc_mode  !! Inclination measurement mode (`GEOCOM_TMC_INCLINE_PRG`).

        character(len=80)   :: args
        type(response_type) :: responses(4)

        write (args, '(i0, ",", i0)') wait_time, inc_mode

        responses = [ &
            response_type('grc',    unit=' ', type=RESPONSE_TYPE_INT32),  &
            response_type('east',   unit='m', type=RESPONSE_TYPE_REAL64), &
            response_type('north',  unit='m', type=RESPONSE_TYPE_REAL64), &
            response_type('height', unit='m', type=RESPONSE_TYPE_REAL64)  &
        ]

        call dm_geocom_api_request(request, REQUEST_NAME, REQUEST_CODE, pattern=REQUEST_PATTERN, responses=responses)
    end subroutine dm_geocom_api_request_get_simple_coordinates

    pure subroutine dm_geocom_api_request_get_simple_measurement(request, wait_time, inc_mode)
        !! Request of *TMC_GetSimpleMea* procedure. Creates request for
        !! returning the values of the angle and distance measurement.
        !!
        !! The API function returns the angles and distance measurement data.
        !! The argument `wait_time` sets the maximum time to wait for a valid
        !! distance. If a distance is available, the wait time is ignored.
        !!
        !! The instrument returns the following responses:
        !!
        !! * `grc`   – GeoCOM return code.
        !! * `hz`    – Horizontal angle [rad].
        !! * `v`     – Vertical angle [rad].
        !! * `sdist` – Slope distance [m].
        !!
        !! | Property       | Values                                           |
        !! |----------------|--------------------------------------------------|
        !! | Instruments    | TPS1100, TPS1200, TM30/TS30, TS16                |
        !! | ASCII request  | `%R1Q,2108:<wait_time>,<inc_mode>`               |
        !! | ASCII response | `%R1P,0,0:<grc>,<hz>,<v>,<sdist>`                |
        !!
        character(len=*), parameter :: REQUEST_NAME    = 'get_simple_measurement'
        character(len=*), parameter :: REQUEST_PATTERN = GRC_PATTERN // ',(?<hz>[-\d\.]+),(?<v>[-\d\.]+),(?<sdist>[-\d\.]+)'
        integer,          parameter :: REQUEST_CODE    = 2108

        type(request_type), intent(out) :: request   !! Prepared request.
        integer,            intent(in)  :: wait_time !! Delay to wait for the distance measurement to finish [msec].
        integer,            intent(in)  :: inc_mode  !! Inclination measurement mode (`GEOCOM_TMC_INCLINE_PRG`).

        character(len=80)   :: args
        type(response_type) :: responses(4)

        write (args, '(i0, ",", i0)') wait_time, inc_mode

        responses = [ &
            response_type('grc',   unit=' ',   type=RESPONSE_TYPE_INT32),  &
            response_type('hz',    unit='rad', type=RESPONSE_TYPE_REAL64), &
            response_type('v',     unit='rad', type=RESPONSE_TYPE_REAL64), &
            response_type('sdist', unit='m',   type=RESPONSE_TYPE_REAL64)  &
        ]

        call dm_geocom_api_request(request, REQUEST_NAME, REQUEST_CODE, pattern=REQUEST_PATTERN, responses=responses)
    end subroutine dm_geocom_api_request_get_simple_measurement

    pure subroutine dm_geocom_api_request_get_slope_distance_correction(request)
        !! Request of *TMC_GetSlopeDistCorr* procedure. Creates request for
        !! getting the total ppm and prism correction.
        !!
        !! The function returns the total ppm value (atmospheric ppm +
        !! geometric ppm) plus the current prism constant.
        !!
        !! The instrument returns the following responses:
        !!
        !! * `grc`     – GeoCOM return code.
        !! * `distppm` – Total ppm correction factor [ppm].
        !! * `reflcor` – Correction factor if the reflector [m].
        !!
        !! | Property       | Values                                           |
        !! |----------------|--------------------------------------------------|
        !! | Instruments    | TPS1100, TPS1200, TM30/TS30, TS16                |
        !! | ASCII request  | `%R1Q,2126:`                                     |
        !! | ASCII response | `%R1P,0,0:<grc>,<distppm>,<reflcor>`             |
        !!
        character(len=*), parameter :: REQUEST_NAME    = 'get_slope_distance_correction'
        character(len=*), parameter :: REQUEST_PATTERN = GRC_PATTERN // ',(?<distppm>[-\d\.]+),(?<reflcor>[-\d\.]+)'
        integer,          parameter :: REQUEST_CODE    = 2126

        type(request_type), intent(out) :: request !! Prepared request.
        type(response_type)             :: responses(3)

        responses = [ &
            response_type('grc',     unit=' ',   type=RESPONSE_TYPE_INT32),  &
            response_type('distppm', unit='ppm', type=RESPONSE_TYPE_REAL64), &
            response_type('reflcor', unit='m',   type=RESPONSE_TYPE_REAL64)  &
        ]

        call dm_geocom_api_request(request, REQUEST_NAME, REQUEST_CODE, pattern=REQUEST_PATTERN, responses=responses)
    end subroutine dm_geocom_api_request_get_slope_distance_correction

    pure subroutine dm_geocom_api_request_get_software_version(request)
        !! Request of *CSV_GetSWVersion* procedure. Creates request for getting
        !! the system software version of the instrument.
        !!
        !! The instrument returns the following responses:
        !!
        !! * `grc`   – GeoCOM return code.
        !! * `swrel` – Software release.
        !! * `swver` – Software version.
        !! * `swsub` – Software sub-version.
        !!
        !! | Property       | Values                                           |
        !! |----------------|--------------------------------------------------|
        !! | Instruments    | TPS1100, TPS1200, TM30/TS30, TS16                |
        !! | ASCII request  | `%R1Q,5034:`                                     |
        !! | ASCII response | `%R1P,0,0:<grc>,<swrel>,<swver>,<swsub>`         |
        !!
        character(len=*), parameter :: REQUEST_NAME    = 'get_software_version'
        character(len=*), parameter :: REQUEST_PATTERN = GRC_PATTERN // ',(?<swrel>\d+),(?<swver>\d+),(?<swsub>\d+)'
        integer,          parameter :: REQUEST_CODE    = 5034

        type(request_type), intent(out) :: request !! Prepared request.
        type(response_type)             :: responses(4)

        responses = [ &
            response_type('grc',   type=RESPONSE_TYPE_INT32), &
            response_type('swrel', type=RESPONSE_TYPE_INT32), &
            response_type('swver', type=RESPONSE_TYPE_INT32), &
            response_type('swsub', type=RESPONSE_TYPE_INT32)  &
        ]

        call dm_geocom_api_request(request, REQUEST_NAME, REQUEST_CODE, pattern=REQUEST_PATTERN, responses=responses)
    end subroutine dm_geocom_api_request_get_software_version

    pure subroutine dm_geocom_api_request_get_station(request)
        !! Request of *TMC_GetStation* procedure. Creates request for getting
        !! the station coordinates of the instrument.
        !!
        !! The instrument returns the following responses:
        !!
        !! * `grc`     – GeoCOM return code.
        !! * `east0`   – Station easting coordinate [m].
        !! * `north0`  – Station northing coordinate [m].
        !! * `height0` – Station height coordinate [m].
        !! * `heighti` – Instrument height [m].
        !!
        !! | Property       | Values                                           |
        !! |----------------|--------------------------------------------------|
        !! | Instruments    | TPS1100, TPS1200, TM30/TS30, TS16                |
        !! | ASCII request  | `%R1Q,2009:`                                     |
        !! | ASCII response | `%R1P,0,0:<east0>,<north0>,<height0>,<heighti>`  |
        !!
        character(len=*), parameter :: REQUEST_NAME    = 'get_station'
        character(len=*), parameter :: REQUEST_PATTERN = &
            '(?<grc>\d+),(?<east0>[-\d\.]+),(?<north0>[-\d\.]+),(?<height0>[-\d\.]+),(?<heighti>[-\d\.]+)'
        integer,          parameter :: REQUEST_CODE    = 2009

        type(request_type), intent(out) :: request !! Prepared request.
        type(response_type)             :: responses(5)

        responses = [ &
            response_type('grc',     unit=' ', type=RESPONSE_TYPE_INT32),  &
            response_type('east0',   unit='m', type=RESPONSE_TYPE_REAL64), &
            response_type('north0',  unit='m', type=RESPONSE_TYPE_REAL64), &
            response_type('height0', unit='m', type=RESPONSE_TYPE_REAL64), &
            response_type('heighti', unit='m', type=RESPONSE_TYPE_REAL64)  &
        ]

        call dm_geocom_api_request(request, REQUEST_NAME, REQUEST_CODE, pattern=REQUEST_PATTERN, responses=responses)
    end subroutine dm_geocom_api_request_get_station

    pure subroutine dm_geocom_api_request_get_target_type(request)
        !! Request of *BAP_GetTargetType* procedure. Creates request for
        !! getting the EDM type.
        !!
        !! The function returns the current EDM type (`GEOCOM_BAP_TARGET_TYPE`)
        !! for distance measurements: reflector (IR) or reflectorless (RL).
        !!
        !! The instrument returns the following responses:
        !!
        !! * `grc`     – GeoCOM return code.
        !! * `tartype` – Target type (`GEOCOM_BAP_TARGET_TYPE`).
        !!
        !! | Property       | Values                                           |
        !! |----------------|--------------------------------------------------|
        !! | Instruments    | TPS1100, TPS1200, TM30/TS30, TS16                |
        !! | ASCII request  | `%R1Q,17022:`                                    |
        !! | ASCII response | `%R1P,0,0:<tartype>`                             |
        !!
        character(len=*), parameter :: REQUEST_NAME    = 'get_target_type'
        character(len=*), parameter :: REQUEST_PATTERN = GRC_PATTERN // ',(?<tartype>\d+)'
        integer,          parameter :: REQUEST_CODE    = 17022

        type(request_type), intent(out) :: request !! Prepared request.
        type(response_type)             :: responses(2)

        responses = [ &
            response_type('grc',     type=RESPONSE_TYPE_INT32), &
            response_type('tartype', type=RESPONSE_TYPE_INT32)  &
        ]

        call dm_geocom_api_request(request, REQUEST_NAME, REQUEST_CODE, pattern=REQUEST_PATTERN, responses=responses)
    end subroutine dm_geocom_api_request_get_target_type

    pure subroutine dm_geocom_api_request_get_timeout(request)
        !! Request of *AUT_ReadTimeout* procedure. Creates request for getting
        !! the timeout for positioning.
        !!
        !! The function returns the maximum time to perform positioning.
        !!
        !! The instrument returns the following responses:
        !!
        !! * `grc`    – GeoCOM return code.
        !! * `timehz` – Positioning timeout in Hz [sec].
        !! * `timev`  – Positioning timeout in V [sec].
        !!
        !! | Property       | Values                                           |
        !! |----------------|--------------------------------------------------|
        !! | Instruments    | TPS1100, TPS1200, TM30/TS30, TS16                |
        !! | ASCII request  | `%R1Q,9012:`                                     |
        !! | ASCII response | `%R1P,0,0:<timehz>,<timev>`                      |
        !!
        character(len=*), parameter :: REQUEST_NAME    = 'get_timeout'
        character(len=*), parameter :: REQUEST_PATTERN = GRC_PATTERN // ',(?<timehz>[-\d\.]+),(?<timev>[-\d\.]+)'
        integer,          parameter :: REQUEST_CODE    = 9012

        type(request_type), intent(out) :: request !! Prepared request.
        type(response_type)             :: responses(3)

        responses = [ &
            response_type('grc',    unit=' ', type=RESPONSE_TYPE_INT32),  &
            response_type('timehz', unit='s', type=RESPONSE_TYPE_REAL64), &
            response_type('timev',  unit='s', type=RESPONSE_TYPE_REAL64)  &
        ]

        call dm_geocom_api_request(request, REQUEST_NAME, REQUEST_CODE, pattern=REQUEST_PATTERN, responses=responses)
    end subroutine dm_geocom_api_request_get_timeout

    pure subroutine dm_geocom_api_request_get_tolerance(request)
        !! Request of *AUT_ReadTol* procedure. Creates request for getting the
        !! positioning tolerances.
        !!
        !! The function returns the positioning tolerances of the Hz and V
        !! instrument axis.
        !!
        !! The instrument returns the following responses:
        !!
        !! * `grc`   – GeoCOM return code.
        !! * `tolhz` – Positioning tolerance in Hz [rad].
        !! * `tolv`  – Positioning tolerance in V [rad].
        !!
        !! | Property       | Values                                           |
        !! |----------------|--------------------------------------------------|
        !! | Instruments    | TPS1100, TPS1200, TM30/TS30, TS16                |
        !! | ASCII request  | `%R1Q,9008:`                                     |
        !! | ASCII response | `%R1P,0,0:<tolhz>,<tolv>`                        |
        !!
        character(len=*), parameter :: REQUEST_NAME    = 'get_tolerance'
        character(len=*), parameter :: REQUEST_PATTERN = GRC_PATTERN // ',(?<tolhz>[-\d\.]+),(?<tolv>[-\d\.]+)'
        integer,          parameter :: REQUEST_CODE    = 9008

        type(request_type), intent(out) :: request !! Prepared request.
        type(response_type)             :: responses(3)

        responses = [ &
            response_type('grc',   unit=' ',   type=RESPONSE_TYPE_INT32),  &
            response_type('tolhz', unit='rad', type=RESPONSE_TYPE_REAL64), &
            response_type('tolv',  unit='rad', type=RESPONSE_TYPE_REAL64)  &
        ]

        call dm_geocom_api_request(request, REQUEST_NAME, REQUEST_CODE, pattern=REQUEST_PATTERN, responses=responses)
    end subroutine dm_geocom_api_request_get_tolerance

    pure subroutine dm_geocom_api_request_get_user_atr_mode(request)
        !! Request of *AUS_GetUserAtrState* procedure. Creates request for
        !! getting the status of the ATR mode.
        !!
        !! The instrument returns the following responses:
        !!
        !! * `grc` – GeoCOM return code.
        !! * `atr` – ATR mode is enabled [bool].
        !!
        !! | Property       | Values                                           |
        !! |----------------|--------------------------------------------------|
        !! | Instruments    | TPS1100, TPS1200, TM30/TS30, TS16                |
        !! | ASCII request  | `%R1Q,18006:`                                    |
        !! | ASCII response | `%R1P,0,0:<atr>`                                 |
        !!
        character(len=*), parameter :: REQUEST_NAME    = 'get_user_atr_mode'
        character(len=*), parameter :: REQUEST_PATTERN = GRC_PATTERN // ',(?<atr>\d+)'
        integer,          parameter :: REQUEST_CODE    = 18006

        type(request_type), intent(out) :: request !! Prepared request.
        type(response_type)             :: responses(2)

        responses = [ &
            response_type('grc', type=RESPONSE_TYPE_INT32),  &
            response_type('atr', type=RESPONSE_TYPE_LOGICAL) &
        ]

        call dm_geocom_api_request(request, REQUEST_NAME, REQUEST_CODE, pattern=REQUEST_PATTERN, responses=responses)
    end subroutine dm_geocom_api_request_get_user_atr_mode

    pure subroutine dm_geocom_api_request_get_user_lock_mode(request)
        !! Request of *AUS_GetUserLockState* procedure. Creates request for
        !! getting the status of the LOCK mode.
        !!
        !! The instrument returns the following responses:
        !!
        !! * `grc`  – GeoCOM return code.
        !! * `lock` – LOCK mode is enabled [bool].
        !!
        !! | Property       | Values                                           |
        !! |----------------|--------------------------------------------------|
        !! | Instruments    | TPS1100, TPS1200, TM30/TS30, TS16                |
        !! | ASCII request  | `%R1Q,18008:`                                    |
        !! | ASCII response | `%R1P,0,0:<lock>`                                |
        !!
        character(len=*), parameter :: REQUEST_NAME    = 'get_user_lock_mode'
        character(len=*), parameter :: REQUEST_PATTERN = GRC_PATTERN // ',(?<lock>\d+)'
        integer,          parameter :: REQUEST_CODE    = 18006

        type(request_type), intent(out) :: request !! Prepared request.
        type(response_type)             :: responses(2)

        responses = [ &
            response_type('grc',  type=RESPONSE_TYPE_INT32),  &
            response_type('lock', type=RESPONSE_TYPE_LOGICAL) &
        ]

        call dm_geocom_api_request(request, REQUEST_NAME, REQUEST_CODE, pattern=REQUEST_PATTERN, responses=responses)
    end subroutine dm_geocom_api_request_get_user_lock_mode

    pure subroutine dm_geocom_api_request_get_user_prism_definition(request, name)
        !! Request of *BAP_GetUserPrismDef* procedure. Creates request for
        !! getting the user prism definition.
        !!
        !! The instrument returns the following responses:
        !!
        !! * `grc`      – GeoCOM return code.
        !! * `prsmcor`  – Prism correction constant [m].
        !! * `prsmtype` – Prism type (`GEOCOM_BAP_PRISMTYPE`).
        !! * `prsmuser` – Name of creator [string].
        !!
        !! | Property       | Values                                           |
        !! |----------------|--------------------------------------------------|
        !! | Instruments    | TPS1200, TM30/TS30, TS16                         |
        !! | ASCII request  | `%R1Q,17033:<name>`                              |
        !! | ASCII response | `%R1P,0,0:<grc>,<prsmcor>,<prsmtype>,<prsmuser>` |
        !!
        character(len=*), parameter :: REQUEST_NAME    = 'get_user_prism_definition'
        character(len=*), parameter :: REQUEST_PATTERN = &
            GRC_PATTERN // ',(?<prsmcor>[-\d\.]+),(?<prsmtype>\d+),"(?<prsmuser>.+)"$'
        integer,          parameter :: REQUEST_CODE    = 17033

        type(request_type), intent(out) :: request !! Prepared request.
        character(len=*),   intent(in)  :: name    !! Prism name.

        type(response_type) :: responses(4)

        responses = [ &
            response_type('grc',      unit=' ', type=RESPONSE_TYPE_INT32),  &
            response_type('prsmcor',  unit='m', type=RESPONSE_TYPE_REAL64), &
            response_type('prsmtype', unit=' ', type=RESPONSE_TYPE_INT32),  &
            response_type('prsmuser', unit=' ', type=RESPONSE_TYPE_STRING)  &
        ]

        call dm_geocom_api_request(request, REQUEST_NAME, REQUEST_CODE, name, REQUEST_PATTERN, responses)
    end subroutine dm_geocom_api_request_get_user_prism_definition

    pure subroutine dm_geocom_api_request_get_user_spiral(request)
        !! Request of *AUT_GetUserSpiral* procedure. Creates request for
        !! getting the user-defined search spiral.
        !!
        !! The function returns the current dimensions of the searching spiral.
        !! Requires at least a TCA instrument.
        !!
        !! The instrument returns the following responses:
        !!
        !! * `grc`     – GeoCOM return code.
        !! * `rangehz` – Horizontal angle [rad]
        !! * `rangev`  – Vertical angle [rad].
        !!
        !! | Property       | Values                                           |
        !! |----------------|--------------------------------------------------|
        !! | Instruments    | TPS1100, TPS1200, TM30/TS30, TS16                |
        !! | ASCII request  | `%R1Q,9040:`                                     |
        !! | ASCII response | `%R1P,0,0:<grc>,<rangehz>,<rangev>`              |
        !!
        character(len=*), parameter :: REQUEST_NAME    = 'get_user_spiral'
        character(len=*), parameter :: REQUEST_PATTERN = GRC_PATTERN // ',(?<rangehz>[-\d\.]+),(?<rangev>[-\d\.]+)'
        integer,          parameter :: REQUEST_CODE    = 9040

        type(request_type), intent(out) :: request !! Prepared request.

        type(response_type) :: responses(3)

        responses = [ &
            response_type('grc',     unit=' ',   type=RESPONSE_TYPE_INT32), &
            response_type('rangehz', unit='rad', type=RESPONSE_TYPE_INT64), &
            response_type('rangev',  unit='rad', type=RESPONSE_TYPE_INT64)  &
        ]

        call dm_geocom_api_request(request, REQUEST_NAME, REQUEST_CODE, pattern=REQUEST_PATTERN, responses=responses)
    end subroutine dm_geocom_api_request_get_user_spiral

    pure subroutine dm_geocom_api_request_list(request, next)
        !! Request of *FTR_List* procedure. Creates request for listing file
        !! information.
        !!
        !! The instrument returns the following responses:
        !!
        !! * `grc`     – GeoCOM return code.
        !! * `last`    – File is last entry [bool].
        !! * `fname`   – File name, max. 80 characters long [string].
        !! * `fsize`   – File size [bytes].
        !! * `fhour`   – UTC modification hour [byte].
        !! * `fminute` – UTC modification minute [byte].
        !! * `fsecond` – UTC modification second [byte].
        !! * `fday`    – UTC modification day [byte].
        !! * `fmonth`  – UTC modification month [byte].
        !! * `fyear`   – UTC modification year [byte].
        !!
        !! | Property       | Values                                                                                                 |
        !! |----------------|--------------------------------------------------------------------------------------------------------|
        !! | Instruments    | TPS1200, TM30/TS30, TS16                                                                               |
        !! | ASCII request  | `%R1Q,23307:<next>`                                                                                    |
        !! | ASCII response | `%R1P,0,0:<grc>,<last>,<fname>,<fsize>,<fhour>,<fminute>,<fsecond>,<fcsecond>,<fday>,<fmonth>,<fyear>` |
        !!
        character(len=*), parameter :: REQUEST_NAME = 'list'
        integer,          parameter :: REQUEST_CODE = 23307
        character(len=*), parameter :: REQUEST_PATTERN = &
            "(?<grc>\d+),(?<last>\d+),""(?<fname>.+)"",(?<fsize>\d+),'(?<fhour>[0-9a-f]+)'," // &
            "'(?<fminute>[0-9a-f]+)','(?<fsecond>[0-9a-f]+)','(?<fcsecond>[0-9a-f]+)'," // &
            "'(?<fday>[0-9a-f]+)','(?<fmonth>[0-9a-f]+)','(?<fyear>[0-9a-f]+)'"

        type(request_type), intent(out) :: request !! Prepared request.
        logical,            intent(in)  :: next    !! First or next entry.

        character(len=80)   :: args
        type(response_type) :: responses(11)

        write (args, '(i1)') dm_btoi(next)

        responses = [ &
            response_type('grc',      unit=' ',     type=RESPONSE_TYPE_INT32),   &
            response_type('last',     unit=' ',     type=RESPONSE_TYPE_LOGICAL), &
            response_type('fname',    unit=' ',     type=RESPONSE_TYPE_STRING),  &
            response_type('fsize',    unit='bytes', type=RESPONSE_TYPE_INT64),   &
            response_type('fhour',    unit='utc',   type=RESPONSE_TYPE_BYTE),    &
            response_type('fminute',  unit='utc',   type=RESPONSE_TYPE_BYTE),    &
            response_type('fsecond',  unit='utc',   type=RESPONSE_TYPE_BYTE),    &
            response_type('fcsecond', unit='utc',   type=RESPONSE_TYPE_BYTE),    &
            response_type('fday',     unit='utc',   type=RESPONSE_TYPE_BYTE),    &
            response_type('fmonth',   unit='utc',   type=RESPONSE_TYPE_BYTE),    &
            response_type('fyear',    unit='utc',   type=RESPONSE_TYPE_BYTE)     &
        ]

        call dm_geocom_api_request(request, REQUEST_NAME, REQUEST_CODE, args, REQUEST_PATTERN, responses)
    end subroutine dm_geocom_api_request_list

    pure subroutine dm_geocom_api_request_lock_in(request)
        !! Request of *AUT_LockIn* procedure. Creates request for starting the
        !! target tracking.
        !!
        !! The API function will start the target tracking if the LOCK mode is
        !! activated (*AUS_SetUserLockState*). The *AUT_FineAdjust* call must
        !! have finished successfully before executing this function.
        !!
        !! The instrument returns the following responses:
        !!
        !! * `grc` – GeoCOM return code.
        !!
        !! | Property       | Values                                           |
        !! |----------------|--------------------------------------------------|
        !! | Instruments    | TPS1200, TM30/TS30, TS16                         |
        !! | ASCII request  | `%R1Q,9013:`                                     |
        !! | ASCII response | `%R1P,0,0:<grc>`                                 |
        !!
        character(len=*), parameter :: REQUEST_NAME = 'lock_in'
        integer,          parameter :: REQUEST_CODE = 9013

        type(request_type), intent(out) :: request !! Prepared request.

        call dm_geocom_api_request(request, REQUEST_NAME, REQUEST_CODE, pattern=GRC_PATTERN, responses=GRC_RESPONSES)
    end subroutine dm_geocom_api_request_lock_in

    pure subroutine dm_geocom_api_request_measure_distance_angle(request, dist_mode)
        !! Request of *BAP_MeasDistanceAngle* procedure. Creates request for
        !! measuring Hz, V angles and a single distance.
        !!
        !! The API function measures angles and a single distance depending on
        !! the distance measurement mode `dist_mode`. It is not suited for
        !! continuous measurements (LOCK mode and TRK mode), and uses the
        !! current automation settings.
        !!
        !! The instrument returns the following responses:
        !!
        !! * `grc`      – GeoCOM return code.
        !! * `hz`       – Horizontal angle [rad].
        !! * `v`        – Vertical angle [rad].
        !! * `sdist`    – Slope distance [m].
        !! * `distmode` – Distance measurement mode (`GEOCOM_BAP_MEASURE_PRG`).
        !!
        !! | Property       | Values                                           |
        !! |----------------|--------------------------------------------------|
        !! | Instruments    | TPS1100, TPS1200, TM30/TS30, TS16                |
        !! | ASCII request  | `%R1Q,17017:<dist_mode>`                         |
        !! | ASCII response | `%R1P,0,0:<grc>,<hz>,<v>,<sdist>,<distmode>`     |
        !!
        character(len=*), parameter :: REQUEST_NAME    = 'measure_distance_angle'
        character(len=*), parameter :: REQUEST_PATTERN = &
            GRC_PATTERN // ',(?<hz>[-\d\.]+),(?<v>[-\d\.]+),(?<sdist>[-\d\.]+),(?<distmode>\d+)'
        integer,          parameter :: REQUEST_CODE    = 17017

        type(request_type), intent(out) :: request   !! Prepared request.
        integer,            intent(in)  :: dist_mode !! Distance measurement mode (`GEOCOM_BAP_MEASURE_PRG`).

        character(len=80)   :: args
        type(response_type) :: responses(5)

        write (args, '(i0)') dist_mode

        responses = [ &
            response_type('grc',      unit=' ',   type=RESPONSE_TYPE_INT32),  &
            response_type('hz',       unit='rad', type=RESPONSE_TYPE_REAL64), &
            response_type('v',        unit='rad', type=RESPONSE_TYPE_REAL64), &
            response_type('sdist',    unit='m',   type=RESPONSE_TYPE_REAL64), &
            response_type('distmode', unit=' ',   type=RESPONSE_TYPE_INT32)   &
        ]

        call dm_geocom_api_request(request, REQUEST_NAME, REQUEST_CODE, pattern=REQUEST_PATTERN, responses=responses)
    end subroutine dm_geocom_api_request_measure_distance_angle

    pure subroutine dm_geocom_api_request_null(request)
        !! Request of *COM_NullProc* procedure. Creates request for checking
        !! the communication.
        !!
        !! The instrument returns the following responses:
        !!
        !! * `grc` – GeoCOM return code.
        !!
        !! | Property       | Values                                           |
        !! |----------------|--------------------------------------------------|
        !! | Instruments    | TPS1100, TPS1200, TM30/TS30, TS16                |
        !! | ASCII request  | `%R1Q,0:`                                        |
        !! | ASCII response | `%R1P,0,0:<grc>`                                 |
        !!
        character(len=*), parameter :: REQUEST_NAME = 'null'
        integer,          parameter :: REQUEST_CODE = 0

        type(request_type), intent(out) :: request !! Prepared request.

        call dm_geocom_api_request(request, REQUEST_NAME, REQUEST_CODE, pattern=GRC_PATTERN, responses=GRC_RESPONSES)
    end subroutine dm_geocom_api_request_null

    pure subroutine dm_geocom_api_request_ps_enable_range(request, enabled)
        !! Request of *AUT_PS_EnableRange* procedure. Creates request for
        !! enabling the PowerSearch window and range.
        !!
        !! The function enabled or disables the predefined PowerSearch window
        !! including the PowerSearch range limits set by API call
        !! *AUT_PS_SetRange* (requires GeoCOM robotic licence). If `enabled` is
        !! `.false.`, the default range is set to ≤ 400 m.
        !!
        !! The instrument returns the following responses:
        !!
        !! * `grc` – GeoCOM return code.
        !!
        !! | Property       | Values                                           |
        !! |----------------|--------------------------------------------------|
        !! | Instruments    | TPS1200, TM30/TS30, TS16                         |
        !! | ASCII request  | `%R1Q,9048:<enabled>`                            |
        !! | ASCII response | `%R1P,0,0:<grc>`                                 |
        !!
        character(len=*), parameter :: REQUEST_NAME = 'ps_enable_range'
        integer,          parameter :: REQUEST_CODE = 9048

        type(request_type), intent(out) :: request !! Prepared request.
        logical,            intent(in)  :: enabled !! Enable PowerSearch.

        character(len=80) :: args

        write (args, '(i1)') dm_btoi(enabled)
        call dm_geocom_api_request(request, REQUEST_NAME, REQUEST_CODE, args, GRC_PATTERN, GRC_RESPONSES)
    end subroutine dm_geocom_api_request_ps_enable_range

    pure subroutine dm_geocom_api_request_ps_search_next(request, direction, swing)
        !! Request of *AUT_PS_SearchNext* procedure. Creates request for
        !! searching for the next target
        !!
        !! The function executes the 360° default PowerSearch and searches for
        !! the next targets. A previously defined PowerSearch window
        !! (*AUT_SetSearchArea*) is not taken into account. Use API call
        !! *AUT_PS_SearchWindow* first.
        !!
        !! The instrument returns the following responses:
        !!
        !! * `grc` – GeoCOM return code.
        !!
        !! | Property       | Values                                           |
        !! |----------------|--------------------------------------------------|
        !! | Instruments    | TPS1200, TM30/TS30, TS16                         |
        !! | ASCII request  | `%R1Q,9051:<direction>,<swing>`                  |
        !! | ASCII response | `%R1P,0,0:<grc>`                                 |
        !!
        character(len=*), parameter :: REQUEST_NAME = 'ps_search_next'
        integer,          parameter :: REQUEST_CODE = 9051

        type(request_type), intent(out) :: request   !! Prepared request.
        integer,            intent(in)  :: direction !! Searching direction (`1` for clockwise, `-1` for counter-clockwise).
        logical,            intent(in)  :: swing     !! Searching starts –10 gon to the given direction.

        character(len=80) :: args

        write (args, '(i0, ",", i1)') direction, dm_btoi(swing)
        call dm_geocom_api_request(request, REQUEST_NAME, REQUEST_CODE, args, GRC_PATTERN, GRC_RESPONSES)
    end subroutine dm_geocom_api_request_ps_search_next

    pure subroutine dm_geocom_api_request_ps_search_window(request)
        !! Request of *AUT_PS_SearchWindow* procedure. Creates request for
        !! starting PowerSearch.
        !!
        !! The function starts PowerSearch in the window defined by API calls
        !! *AUT_SetSearchArea* and *AUT_PS_SetRange* (requires GeoCOM robotic
        !! licence).
        !!
        !! The instrument returns the following responses:
        !!
        !! * `grc` – GeoCOM return code.
        !!
        !! | Property       | Values                                           |
        !! |----------------|--------------------------------------------------|
        !! | Instruments    | TPS1200, TM30/TS30, TS16                         |
        !! | ASCII request  | `%R1Q,9052:`                                     |
        !! | ASCII response | `%R1P,0,0:<grc>`                                 |
        !!
        character(len=*), parameter :: REQUEST_NAME = 'ps_search_window'
        integer,          parameter :: REQUEST_CODE = 9052

        type(request_type), intent(out) :: request !! Prepared request.

        call dm_geocom_api_request(request, REQUEST_NAME, REQUEST_CODE, pattern=GRC_PATTERN, responses=GRC_RESPONSES)
    end subroutine dm_geocom_api_request_ps_search_window

    pure subroutine dm_geocom_api_request_ps_set_range(request, min_dist, max_dist)
        !! Request of *AUT_PS_SetRange* procedure. Creates request for setting
        !! the PowerSearch range.
        !!
        !! The instrument returns the following responses:
        !!
        !! * `grc` – GeoCOM return code.
        !!
        !! | Property       | Values                                           |
        !! |----------------|--------------------------------------------------|
        !! | Instruments    | TPS1200, TM30/TS30, TS16                         |
        !! | ASCII request  | `%R1Q,9047:<min_dist>,<max_dist>`                |
        !! | ASCII response | `%R1P,0,0:<grc>`                                 |
        !!
        character(len=*), parameter :: REQUEST_NAME = 'ps_set_range'
        integer,          parameter :: REQUEST_CODE = 9047

        type(request_type), intent(out) :: request  !! Prepared request.
        integer,            intent(in)  :: min_dist !! Min. distance to prism (≥ 0) [m].
        integer,            intent(in)  :: max_dist !! Max. distance to prism (≤ 400, ≥ `min_dist` + 10) [m].

        character(len=80) :: args

        write (args, '(i0, ",", i0)') min_dist, max_dist
        call dm_geocom_api_request(request, REQUEST_NAME, REQUEST_CODE, args, GRC_PATTERN, GRC_RESPONSES)
    end subroutine dm_geocom_api_request_ps_set_range

    pure subroutine dm_geocom_api_request_search(request, search_hz, search_v)
        !! Request of *AUT_Search* procedure. Creates request for performing an
        !! automatic target search.
        !!
        !! The function performs an automatic target search within the given
        !! search area (requires GeoCOM robotic licence). The search is
        !! terminated once the prism appears in the field of view of the ATR
        !! sensor. If no prism is found in the specified area, the instrument
        !! turns back into the initial position. For an exact positioning onto
        !! the prism centre, use the fine-adjust API call afterwards
        !! (*AUT_FineAdjust*).
        !!
        !! If the search range of the API function *AUT_FineAdjust* is
        !! expanded, target search and fine positioning are done in one step.
        !!
        !! The instrument returns the following responses:
        !!
        !! * `grc` – GeoCOM return code.
        !!
        !! | Property       | Values                                           |
        !! |----------------|--------------------------------------------------|
        !! | Instruments    | TPS1100, TPS1200, TM30/TS30, TS16                |
        !! | ASCII request  | `%R1Q,9029:<search_hz>,<search_v>,0`             |
        !! | ASCII response | `%R1P,0,0:<grc>`                                 |
        !!
        character(len=*), parameter :: REQUEST_NAME = 'search'
        integer,          parameter :: REQUEST_CODE = 9029

        type(request_type), intent(out) :: request   !! Prepared request.
        real(kind=r8),      intent(in)  :: search_hz !! Horizontal search region [rad].
        real(kind=r8),      intent(in)  :: search_v  !! Vertical search region [rad].

        character(len=80) :: args

        write (args, '(f0.12, ",", f0.12, ",0")') search_hz, search_v
        call dm_geocom_api_request(request, REQUEST_NAME, REQUEST_CODE, args, GRC_PATTERN, GRC_RESPONSES)
    end subroutine dm_geocom_api_request_search

    pure subroutine dm_geocom_api_request_search_target(request)
        !! Request of *BAP_SearchTarget* procedure. Creates request for
        !! searching a target.
        !!
        !! The function searches for a target in the ATR search window.
        !!
        !! The instrument returns the following responses:
        !!
        !! * `grc` – GeoCOM return code.
        !!
        !! | Property       | Values                                           |
        !! |----------------|--------------------------------------------------|
        !! | Instruments    | TPS1100, TPS1200, TM30/TS30, TS16                |
        !! | ASCII request  | `%R1Q,17020:0`                                   |
        !! | ASCII response | `%R1P,0,0:<grc>`                                 |
        !!
        character(len=*), parameter :: REQUEST_NAME = 'search_target'
        integer,          parameter :: REQUEST_CODE = 17020

        type(request_type), intent(out) :: request !! Prepared request.

        call dm_geocom_api_request(request, REQUEST_NAME, REQUEST_CODE, pattern=GRC_PATTERN, responses=GRC_RESPONSES)
    end subroutine dm_geocom_api_request_search_target

    pure subroutine dm_geocom_api_request_set_angle_correction(request, incline, stand_axis, collimation, tilt_axis)
        !! Request of *TMC_SetAngSwitch* procedure. Creates request for
        !! turning angle corrections on or off.
        !!
        !! The instrument returns the following responses:
        !!
        !! * `grc` – GeoCOM return code.
        !!
        !! | Property       | Values                                                       |
        !! |----------------|--------------------------------------------------------------|
        !! | Instruments    | TPS1100, TPS1200, TM30/TS30, TS16                            |
        !! | ASCII request  | `%R1Q,2016:<incline>,<stand_axis>,<collimation>,<tilt_axis>` |
        !! | ASCII response | `%R1P,0,0:<grc>`                                             |
        !!
        character(len=*), parameter :: REQUEST_NAME = 'set_angle_correction'
        integer,          parameter :: REQUEST_CODE = 2016

        type(request_type), intent(out) :: request     !! Prepared request.
        logical,            intent(in)  :: incline     !! Enable inclination correction.
        logical,            intent(in)  :: stand_axis  !! Enable standard axis correction.
        logical,            intent(in)  :: collimation !! Enable collimation correction.
        logical,            intent(in)  :: tilt_axis   !! Enable tilt axis correction.

        character(len=80) :: args

        write (args, '(3(i1, ","), i1)') dm_btoi(incline), dm_btoi(stand_axis), dm_btoi(collimation), dm_btoi(tilt_axis)
        call dm_geocom_api_request(request, REQUEST_NAME, REQUEST_CODE, args, GRC_PATTERN, GRC_RESPONSES)
    end subroutine dm_geocom_api_request_set_angle_correction

    pure subroutine dm_geocom_api_request_set_atmospheric_correction(request, lambda, pressure, dry_temp, wet_temp)
        !! Request of *BAP_SetAtmCorr* procedure. Creates request for setting
        !! the atmospheric correction parameters.
        !!
        !! The argument `lambda` should be queried with API call *TMC_GetAtmCorr*.
        !!
        !! The instrument returns the following responses:
        !!
        !! * `grc` – GeoCOM return code.
        !!
        !! | Property       | Values                                                |
        !! |----------------|-------------------------------------------------------|
        !! | Instruments    | TPS1100, TPS1200, TM30/TS30, TS16                     |
        !! | ASCII request  | `%R1Q,2028:<lambda>,<pressure>,<dry_temp>,<wet_temp>` |
        !! | ASCII response | `%R1P,0,0:<grc>`                                      |
        !!
        character(len=*), parameter :: REQUEST_NAME = 'set_atmospheric_correction'
        integer,          parameter :: REQUEST_CODE = 2028

        type(request_type), intent(out) :: request  !! Prepared request.
        real(kind=r8),      intent(in)  :: lambda   !! Wave-length of EDM transmitter [m].
        real(kind=r8),      intent(in)  :: pressure !! Atmospheric pressure [mbar].
        real(kind=r8),      intent(in)  :: dry_temp !! Dry temperature [°C].
        real(kind=r8),      intent(in)  :: wet_temp !! Wet temperature [°C].

        character(len=80) :: args

        write (args, '(4(f0.12, ","), f0.12)') lambda, pressure, dry_temp, wet_temp
        call dm_geocom_api_request(request, REQUEST_NAME, REQUEST_CODE, args, GRC_PATTERN, GRC_RESPONSES)
    end subroutine dm_geocom_api_request_set_atmospheric_correction

    pure subroutine dm_geocom_api_request_set_atmospheric_ppm(request, atm_ppm)
        !! Request of *BAP_SetAtmPpm* procedure. Creates request for setting
        !! the atmospheric ppm correction factor.
        !!
        !! The instrument returns the following responses:
        !!
        !! * `grc` – GeoCOM return code.
        !!
        !! | Property       | Values                                           |
        !! |----------------|--------------------------------------------------|
        !! | Instruments    | TPS1200, TM30/TS30, TS16                         |
        !! | ASCII request  | `%R1Q,2148:<atm_ppm>`                            |
        !! | ASCII response | `%R1P,0,0:<grc>`                                 |
        !!
        character(len=*), parameter :: REQUEST_NAME = 'set_atmospheric_ppm'
        integer,          parameter :: REQUEST_CODE = 2148

        type(request_type), intent(out) :: request !! Prepared request.
        real(kind=r8),      intent(in)  :: atm_ppm !! Atmospheric ppm correction factor [ppm].

        character(len=80) :: args

        write (args, '(f0.12)') atm_ppm
        call dm_geocom_api_request(request, REQUEST_NAME, REQUEST_CODE, args, GRC_PATTERN, GRC_RESPONSES)
    end subroutine dm_geocom_api_request_set_atmospheric_ppm

    pure subroutine dm_geocom_api_request_set_atr_mode(request, atr_mode)
        !! Request of *BAP_SetATRSetting* procedure. Creates request for
        !! setting the ATR low-vis mode.
        !!
        !! The argument `atr_mode` must be one of the following:
        !!
        !! * `GEOCOM_BAP_ATRSET_NORMAL`     – No special flags or modes.
        !! * `GEOCOM_BAP_ATRSET_LOWVIS_ON`  – ATR low-vis mode on.
        !! * `GEOCOM_BAP_ATRSET_LOWVIS_AON` – ATR low-vis mode always on.
        !! * `GEOCOM_BAP_ATRSET_SRANGE_ON`  – ATR high-reflectivity mode on.
        !! * `GEOCOM_BAP_ATRSET_SRANGE_AON` – ATR high-reflectivity mode always on.
        !!
        !! The instrument returns the following responses:
        !!
        !! * `grc` – GeoCOM return code.
        !!
        !! | Property       | Values                                           |
        !! |----------------|--------------------------------------------------|
        !! | Instruments    | TPS1200, TM30/TS30, TS16                         |
        !! | ASCII request  | `%R1Q,17035:<atr_mode>`                          |
        !! | ASCII response | `%R1P,0,0:<grc>`                                 |
        !!
        character(len=*), parameter :: REQUEST_NAME = 'set_atr_mode'
        integer,          parameter :: REQUEST_CODE = 17035

        type(request_type), intent(out) :: request  !! Prepared request.
        integer,            intent(in)  :: atr_mode !! ATR low-vis mode (`GEOCOM_BAP_ATRSETTING`).

        character(len=80) :: args

        write (args, '(i0)') atr_mode
        call dm_geocom_api_request(request, REQUEST_NAME, REQUEST_CODE, args, GRC_PATTERN, GRC_RESPONSES)
    end subroutine dm_geocom_api_request_set_atr_mode

    pure subroutine dm_geocom_api_request_set_binary_mode(request, enabled)
        !! Request of *COM_SetBinaryAvailable* procedure. Creates request for
        !! setting the binary attribute of the server.
        !!
        !! The function sets the ability of the GeoCOM server to handle binary
        !! communication (not supported by DMPACK).
        !!
        !! The instrument returns the following responses:
        !!
        !! * `grc` – GeoCOM return code.
        !!
        !! | Property       | Values                                           |
        !! |----------------|--------------------------------------------------|
        !! | Instruments    | TPS1100, TPS1200, TM30/TS30, TS16                |
        !! | ASCII request  | `%R1Q,114:<enabled>`                             |
        !! | ASCII response | `%R1P,0,0:<grc>`                                 |
        !!
        character(len=*), parameter :: REQUEST_NAME = 'set_binary_mode'
        integer,          parameter :: REQUEST_CODE = 114

        type(request_type), intent(out) :: request !! Prepared request.
        logical,            intent(in)  :: enabled !! Enable binary communication.

        character(len=80) :: args

        write (args, '(i1)') dm_btoi(enabled)
        call dm_geocom_api_request(request, REQUEST_NAME, REQUEST_CODE, args, GRC_PATTERN, GRC_RESPONSES)
    end subroutine dm_geocom_api_request_set_binary_mode

    pure subroutine dm_geocom_api_request_set_config(request, auto_power, timeout)
        !! Request of *SUP_SetConfig* procedure. Creates request for setting
        !! the power management configuration.
        !!
        !! The argument `timeout` sets the duration after which the instrument
        !! switches into the mode `auto_power` (`GEOCOM_SUP_AUTO_POWER`) when
        !! no user activity occured (key press, GeoCOM communication). The
        !! value must be between 60,000 m/s (1 min) and 6,000,000 m/s (100 min).
        !!
        !! The instrument returns the following responses:
        !!
        !! * `grc` – GeoCOM return code.
        !!
        !! | Property       | Values                                           |
        !! |----------------|--------------------------------------------------|
        !! | Instruments    | TPS1100, TPS1200, TM30/TS30, TS16                |
        !! | ASCII request  | `%R1Q,14002:<reserved>,<auto_power>,<timeout>`   |
        !! | ASCII response | `%R1P,0,0:<grc>`                                 |
        !!
        character(len=*), parameter :: REQUEST_NAME = 'set_config'
        integer,          parameter :: REQUEST_CODE = 14002

        type(request_type), intent(out) :: request    !! Prepared request.
        integer,            intent(in)  :: auto_power !! Power-off mode (`GEOCOM_SUP_AUTO_POWER`).
        integer,            intent(in)  :: timeout    !! Timeout [msec].

        character(len=80) :: args

        write (args, '("0", 2(",", i0))') auto_power, timeout
        call dm_geocom_api_request(request, REQUEST_NAME, REQUEST_CODE, args, GRC_PATTERN, GRC_RESPONSES)
    end subroutine dm_geocom_api_request_set_config

    pure subroutine dm_geocom_api_request_set_date_time(request, year, month, day, hour, minute, second)
        !! Request of *CSV_SetDateTime* procedure. Creates request for
        !! setting the date and time of the instrument.
        !!
        !! The instrument returns the following responses:
        !!
        !! * `grc` – GeoCOM return code.
        !!
        !! | Property       | Values                                                     |
        !! |----------------|------------------------------------------------------------|
        !! | Instruments    | TPS1100, TPS1200, TM30/TS30, TS16                          |
        !! | ASCII request  | `%R1Q,5007:<year>,<month>,<day>,<hour>,<minute>,<second>`  |
        !! | ASCII response | `%R1P,0,0:<grc>`                                           |
        !!
        character(len=*), parameter :: REQUEST_NAME = 'set_date_time'
        integer,          parameter :: REQUEST_CODE = 5007

        type(request_type), intent(out) :: request !! Prepared request.
        integer,            intent(in)  :: year    !! Year (`YYYY`).
        integer,            intent(in)  :: month   !! Month (`MM`).
        integer,            intent(in)  :: day     !! Day of month (`DD`).
        integer,            intent(in)  :: hour    !! Hour (`hh`).
        integer,            intent(in)  :: minute  !! Minute (`mm`).
        integer,            intent(in)  :: second  !! Second (`ss`).

        character(len=80) :: args

        write (args, '(i0, 5(",", z2.2))') year, month, day, hour, minute, second
        call dm_geocom_api_request(request, REQUEST_NAME, REQUEST_CODE, args, GRC_PATTERN, GRC_RESPONSES)
    end subroutine dm_geocom_api_request_set_date_time

    pure subroutine dm_geocom_api_request_set_distance(request, slope_dist, height_offset, inc_mode)
        !! Request of *TMC_SetHandDist* procedure. Creates request for
        !! setting the slope distance and height offset.
        !!
        !! The function is used to set the manually measured slope distance and
        !! height offset for a following measurement. Additionally, an
        !! inclination and an angle measurement are started to determine the
        !! coordinates of the target. The vertical angle is corrected to π/2 or
        !! 3π/2, depending on the face of the instrument. The previously
        !! measured distance is cleared.
        !!
        !! The argument `inc_mode` must be one of the following:
        !!
        !! * `GEOCOM_TMC_MEA_INC`   – Use sensor (a priori sigma).
        !! * `GEOCOM_TMC_AUTO_INC`  – Automatic mode (sensor/plane).
        !! * `GEOCOM_TMC_PLANE_INC` – Use plane (a priori sigma).
        !!
        !! The instrument returns the following responses:
        !!
        !! * `grc` – GeoCOM return code.
        !!
        !! | Property       | Values                                               |
        !! |----------------|------------------------------------------------------|
        !! | Instruments    | TPS1100, TPS1200, TM30/TS30, TS16                    |
        !! | ASCII request  | `%R1Q,2019:<slope_dist>,<height_offset>,<inc_mode>`  |
        !! | ASCII response | `%R1P,0,0:<grc>`                                     |
        !!
        character(len=*), parameter :: REQUEST_NAME = 'set_distance'
        integer,          parameter :: REQUEST_CODE = 2019

        type(request_type), intent(out) :: request       !! Prepared request.
        real(kind=r8),      intent(in)  :: slope_dist    !! Slope distance [m].
        real(kind=r8),      intent(in)  :: height_offset !! Height offset [m].
        integer,            intent(in)  :: inc_mode      !! Inclination measurement mode (`GEOCOM_TMC_INCLINE_PRG`).

        character(len=80) :: args

        write (args, '(2(f0.12, ","), i0)') slope_dist, height_offset, inc_mode
        call dm_geocom_api_request(request, REQUEST_NAME, REQUEST_CODE, args, GRC_PATTERN, GRC_RESPONSES)
    end subroutine dm_geocom_api_request_set_distance

    pure subroutine dm_geocom_api_request_set_double_precision(request, ndigits)
        !! Request of *COM_SetDoublePrecision* procedure. Creates request for
        !! setting the double precision.
        !!
        !! The function sets the precision – the number of digits right of the
        !! decimal – when double floating-point values are transmitted. The
        !! default precision is 15 digits. The setting is only valid for the
        !! ASCII transmission mode. Trailing zeroes will not be sent by the
        !! instrument. For example, if `ndigits` is set to 3 and the exact
        !! value is 1.99975, the resulting value will be 2.0.
        !!
        !! The instrument returns the following responses:
        !!
        !! * `grc` – GeoCOM return code.
        !!
        !! | Property       | Values                                           |
        !! |----------------|--------------------------------------------------|
        !! | Instruments    | TPS1100, TPS1200, TM30/TS30, TS16                |
        !! | ASCII request  | `%R1Q,107:<ndigits>`                             |
        !! | ASCII response | `%R1P,0,0:<grc>`                                 |
        !!
        character(len=*), parameter :: REQUEST_NAME = 'set_double_precision'
        integer,          parameter :: REQUEST_CODE = 107

        type(request_type), intent(out) :: request !! Prepared request.
        integer,            intent(in)  :: ndigits !! Number of digits right to the comma.

        character(len=80) :: args

        write (args, '(i0)') ndigits
        call dm_geocom_api_request(request, REQUEST_NAME, REQUEST_CODE, args, GRC_PATTERN, GRC_RESPONSES)
    end subroutine dm_geocom_api_request_set_double_precision

    pure subroutine dm_geocom_api_request_set_edm_mode(request, edm_mode)
        !! Request of *TMC_SetEdmMode* procedure. Creates request for setting
        !! the EDM measurement mode.
        !!
        !! The mode set by this API function is used by
        !! *TMC_DoMeasure(TMC_DEF_DIST)*.
        !!
        !! The instrument returns the following responses:
        !!
        !! * `grc` – GeoCOM return code.
        !!
        !! | Property       | Values                                           |
        !! |----------------|--------------------------------------------------|
        !! | Instruments    | TPS1100, TPS1200, TM30/TS30, TS16                |
        !! | ASCII request  | `%R1Q,2020:<edm_mode>`                           |
        !! | ASCII response | `%R1P,0,0:<grc>`                                 |
        !!
        character(len=*), parameter :: REQUEST_NAME = 'set_edm_mode'
        integer,          parameter :: REQUEST_CODE = 2020

        type(request_type), intent(out) :: request  !! Prepared request.
        integer,            intent(in)  :: edm_mode !! EDM measurement mode (`GEOCOM_EDM_MODE`).

        character(len=80) :: args

        write (args, '(i0)') edm_mode
        call dm_geocom_api_request(request, REQUEST_NAME, REQUEST_CODE, args, GRC_PATTERN, GRC_RESPONSES)
    end subroutine dm_geocom_api_request_set_edm_mode

    pure subroutine dm_geocom_api_request_set_egl_intensity(request, intensity)
        !! Request of *EDM_SetEglIntensity* procedure. Creates request for
        !! setting the intensity of the electronic guide light.
        !!
        !! The argument `intensity` must be one of the following:
        !!
        !! * `GEOCOM_EDM_EGLINTEN_OFF`
        !! * `GEOCOM_EDM_EGLINTEN_LOW`
        !! * `GEOCOM_EDM_EGLINTEN_MID`
        !! * `GEOCOM_EDM_EGLINTEN_HIGH`
        !!
        !! The instrument returns the following responses:
        !!
        !! * `grc` – GeoCOM return code.
        !!
        !! | Property       | Values                                           |
        !! |----------------|--------------------------------------------------|
        !! | Instruments    | TPS1100, TPS1200, TM30/TS30, TS16                |
        !! | ASCII request  | `%R1Q,1059:<intensity>`                          |
        !! | ASCII response | `%R1P,0,0:<grc>`                                 |
        !!
        character(len=*), parameter :: REQUEST_NAME = 'set_egl_intensity'
        integer,          parameter :: REQUEST_CODE = 1059

        type(request_type), intent(out) :: request   !! Prepared request.
        integer,            intent(in)  :: intensity !! EGL intensity (`GEOCOM_EDM_EGLINTENSITY_TYPE`).

        character(len=80) :: args

        write (args, '(i0)') intensity
        call dm_geocom_api_request(request, REQUEST_NAME, REQUEST_CODE, args, GRC_PATTERN, GRC_RESPONSES)
    end subroutine dm_geocom_api_request_set_egl_intensity

    pure subroutine dm_geocom_api_request_set_fine_adjust_mode(request, adj_mode)
        !! Request of *AUT_SetFineAdjustMode* procedure. Creates request for
        !! setting the fine adjust positioning mode.
        !!
        !! The function sets the positioning tolerances relating to angle
        !! accuracy or point accuracy for the fine adjust (requires GeoCOM
        !! robotic licence). If a target is near or held by hand, it is
        !! recommended to set the adjust mode to `GEOCOM_AUT_POINT_MODE`.
        !!
        !! The argument `adj_mode` has to be either `GEOCOM_AUT_NORM_MODE` or
        !! `GEOCOM_AUT_POINT_MODE`.
        !!
        !! The instrument returns the following responses:
        !!
        !! * `grc` – GeoCOM return code.
        !!
        !! | Property       | Values                                           |
        !! |----------------|--------------------------------------------------|
        !! | Instruments    | TPS1100, TPS1200, TM30/TS30, TS16                |
        !! | ASCII request  | `%R1Q,9031:<adj_mode>`                           |
        !! | ASCII response | `%R1P,0,0:<grc>`                                 |
        !!
        character(len=*), parameter :: REQUEST_NAME = 'set_fine_adjust_mode'
        integer,          parameter :: REQUEST_CODE = 9031

        type(request_type), intent(out) :: request  !! Prepared request.
        integer,            intent(in)  :: adj_mode !! Fine adjust positioning mode (`GEOCOM_AUT_ADJMODE`).

        character(len=80) :: args

        write (args, '(i0)') adj_mode
        call dm_geocom_api_request(request, REQUEST_NAME, REQUEST_CODE, args, GRC_PATTERN, GRC_RESPONSES)
    end subroutine dm_geocom_api_request_set_fine_adjust_mode

    pure subroutine dm_geocom_api_request_set_geometric_ppm(request, enabled, scale_factor, offset, height_ppm, individual_ppm)
        !! Request of *TMC_SetGeoPpm* procedure. Creates request for setting the
        !! geometric ppm correction factor.
        !!
        !! The instrument returns the following responses:
        !!
        !! * `grc` – GeoCOM return code.
        !!
        !! | Property       | Values                                                                      |
        !! |----------------|-----------------------------------------------------------------------------|
        !! | Instruments    | TPS1200, TM30/TS30, TS16                                                    |
        !! | ASCII request  | `%R1Q,2153:<enabled>,<scale_factor>,<offset>,<height_ppm>,<individual_ppm>` |
        !! | ASCII response | `%R1P,0,0:<grc>`                                                            |
        !!
        character(len=*), parameter :: REQUEST_NAME = 'set_geometric_ppm'
        integer,          parameter :: REQUEST_CODE = 2153

        type(request_type), intent(out) :: request        !! Prepared request.
        logical,            intent(in)  :: enabled        !! Enable geometric ppm calculation.
        real(kind=r8),      intent(in)  :: scale_factor   !! Scale factor on central meridian.
        real(kind=r8),      intent(in)  :: offset         !! Offset from central meridian [m].
        real(kind=r8),      intent(in)  :: height_ppm     !! Ppm value due to height above reference.
        real(kind=r8),      intent(in)  :: individual_ppm !! Individual ppm value.

        character(len=80) :: args

        write (args, '(i1, 4(",", f0.12))') dm_btoi(enabled), scale_factor, offset, height_ppm, individual_ppm
        call dm_geocom_api_request(request, REQUEST_NAME, REQUEST_CODE, args, GRC_PATTERN, GRC_RESPONSES)
    end subroutine dm_geocom_api_request_set_geometric_ppm

    pure subroutine dm_geocom_api_request_set_height(request, height)
        !! Request of *TMC_SetHeight* procedure. Creates request for setting a
        !! new reflector height.
        !!
        !! The instrument returns the following responses:
        !!
        !! * `grc` – GeoCOM return code.
        !!
        !! | Property       | Values                                           |
        !! |----------------|--------------------------------------------------|
        !! | Instruments    | TPS1100, TPS1200, TM30/TS30, TS16                |
        !! | ASCII request  | `%R1Q,2012:<height>`                             |
        !! | ASCII response | `%R1P,0,0:<grc>`                                 |
        !!
        character(len=*), parameter :: REQUEST_NAME = 'set_height'
        integer,          parameter :: REQUEST_CODE = 2012

        type(request_type), intent(out) :: request !! Prepared request.
        real(kind=r8),      intent(in)  :: height  !! Reflector height [m].

        character(len=80) :: args

        write (args, '(f0.12)') height
        call dm_geocom_api_request(request, REQUEST_NAME, REQUEST_CODE, args, GRC_PATTERN, GRC_RESPONSES)
    end subroutine dm_geocom_api_request_set_height

    pure subroutine dm_geocom_api_request_set_image_config(request, mem_type, image_number, quality, sub_function, prefix)
        !! Request of *IMG_SetTccConfig* procedure. Creates request for setting
        !! the image configuration.
        !!
        !! The argument `sub_function` may be a binary combination of the
        !! following settings:
        !!
        !! * `1` – Test image.
        !! * `2` – Automatic exposure-time selection.
        !! * `3` – Two-times sub-sampling.
        !! * `4` – Four-times sub-sampling.
        !!
        !! The instrument returns the following responses:
        !!
        !! * `grc` – GeoCOM return code.
        !!
        !! | Property       | Values                                                                   |
        !! |----------------|--------------------------------------------------------------------------|
        !! | Instruments    | TPS1200, TM30/TS30, TS16                                                 |
        !! | ASCII request  | `%R1Q,23401:<mem_type>,<image_number>,<quality>,<sub_function>,<prefix>` |
        !! | ASCII response | `%R1P,0,0:<grc>`                                                         |
        !!
        character(len=*), parameter :: REQUEST_NAME = 'set_image_config'
        integer,          parameter :: REQUEST_CODE = 23401

        type(request_type), intent(out) :: request      !! Prepared request.
        integer,            intent(in)  :: mem_type     !! Memory device type (`GEOCOM_IMG_MEM_TYPE`).
        integer,            intent(in)  :: image_number !! Actual image number.
        integer,            intent(in)  :: quality      !! JPEG compression factor (0 – 100).
        integer,            intent(in)  :: sub_function !! Additional sub-functions to call.
        character(len=*),   intent(in)  :: prefix       !! File name prefix.

        character(len=80) :: args

        write (args, '(4(i0, ","), """", a, """")') mem_type, image_number, quality, sub_function, trim(prefix)
        call dm_geocom_api_request(request, REQUEST_NAME, REQUEST_CODE, args, GRC_PATTERN, GRC_RESPONSES)
    end subroutine dm_geocom_api_request_set_image_config

    pure subroutine dm_geocom_api_request_set_inclination_correction(request, enabled)
        !! Request of *TMC_SetInclineSwitch* procedure. Creates request for
        !! turning the dual-axis compensator on or off.
        !!
        !! The instrument returns the following responses:
        !!
        !! * `grc` – GeoCOM return code.
        !!
        !! | Property       | Values                                           |
        !! |----------------|--------------------------------------------------|
        !! | Instruments    | TPS1100, TPS1200, TM30/TS30, TS16                |
        !! | ASCII request  | `%R1Q,2006:<enabled>`                            |
        !! | ASCII response | `%R1P,0,0:<grc>`                                 |
        !!
        character(len=*), parameter :: REQUEST_NAME = 'set_inclination_correction'
        integer,          parameter :: REQUEST_CODE = 2006

        type(request_type), intent(out) :: request !! Prepared request.
        logical,            intent(in)  :: enabled !! Enable dual-axis compensator.

        character(len=80) :: args

        write (args, '(i1)') dm_btoi(enabled)
        call dm_geocom_api_request(request, REQUEST_NAME, REQUEST_CODE, args, GRC_PATTERN, GRC_RESPONSES)
    end subroutine dm_geocom_api_request_set_inclination_correction

    pure subroutine dm_geocom_api_request_set_laser_pointer(request, enabled)
        !! Request of *EDM_Laserpointer* procedure. Creates request for turning
        !! the laser pointer on or off.
        !!
        !! The function is only available on models which support reflectorless
        !! distance measurement.
        !!
        !! The instrument returns the following responses:
        !!
        !! * `grc` – GeoCOM return code.
        !!
        !! | Property       | Values                                           |
        !! |----------------|--------------------------------------------------|
        !! | Instruments    | TPS1100, TPS1200, TM30/TS30, TS16                |
        !! | ASCII request  | `%R1Q,1004:<enabled>`                            |
        !! | ASCII response | `%R1P,0,0:<grc>`                                 |
        !!
        character(len=*), parameter :: REQUEST_NAME = 'set_laser_pointer'
        integer,          parameter :: REQUEST_CODE = 1004

        type(request_type), intent(out) :: request !! Prepared request.
        logical,            intent(in)  :: enabled !! Enable laser pointer.

        character(len=80) :: args

        write (args, '(i1)') dm_btoi(enabled)
        call dm_geocom_api_request(request, REQUEST_NAME, REQUEST_CODE, args, GRC_PATTERN, GRC_RESPONSES)
    end subroutine dm_geocom_api_request_set_laser_pointer

    pure subroutine dm_geocom_api_request_set_measurement_program(request, bap_prog)
        !! Request of *BAP_SetMeasPrg* procedure. Creates request for setting
        !! the distance measurement program.
        !!
        !! The function sets the distance measurement program, for example, for
        !! API call *BAP_MeasDistanceAngle*. The RL EDM type programs are not
        !! available on all instruments. Changing the measurement program may
        !! change the EDM type as well (IR, RL).
        !!
        !! The instrument returns the following responses:
        !!
        !! * `grc` – GeoCOM return code.
        !!
        !! | Property       | Values                                           |
        !! |----------------|--------------------------------------------------|
        !! | Instruments    | TPS1100, TPS1200, TM30/TS30, TS16                |
        !! | ASCII request  | `%R1Q,17019:<bap_prog>`                          |
        !! | ASCII response | `%R1P,0,0:<grc>`                                 |
        !!
        character(len=*), parameter :: REQUEST_NAME = 'set_measurement_program'
        integer,          parameter :: REQUEST_CODE = 17019

        type(request_type), intent(out) :: request  !! Prepared request.
        integer,            intent(in)  :: bap_prog !! Measurement program (`GEOCOM_BAP_USER_MEASPRG`).

        character(len=80) :: args

        write (args, '(i0)') bap_prog
        call dm_geocom_api_request(request, REQUEST_NAME, REQUEST_CODE, args, GRC_PATTERN, GRC_RESPONSES)
    end subroutine dm_geocom_api_request_set_measurement_program

    pure subroutine dm_geocom_api_request_set_orientation(request, hz)
        !! Request of *TMC_SetOrientation* procedure. Creates request for
        !! orientating the instrument in horizontal direction.
        !!
        !! The API function is a combination of an angle measurement to get the
        !! horizontal offset and setting the angle offset afterwards, in order
        !! to orientate to a target. Before the new orientation can be set, an
        !! existing distance must be cleared by calling API function
        !! *TMC_DoMeasure* with command `GEOCOM_TMC_CLEAR`.
        !!
        !! The instrument returns the following responses:
        !!
        !! * `grc` – GeoCOM return code.
        !!
        !! | Property       | Values                                           |
        !! |----------------|--------------------------------------------------|
        !! | Instruments    | TPS1100, TPS1200, TM30/TS30, TS16                |
        !! | ASCII request  | `%R1Q,2113:<hz>`                                 |
        !! | ASCII response | `%R1P,0,0:<grc>`                                 |
        !!
        character(len=*), parameter :: REQUEST_NAME = 'set_orientation'
        integer,          parameter :: REQUEST_CODE = 2113

        type(request_type), intent(out) :: request !! Prepared request.
        real(kind=r8),      intent(in)  :: hz      !! Horizontal orientation [rad].

        character(len=80) :: args

        write (args, '(f0.12)') hz
        call dm_geocom_api_request(request, REQUEST_NAME, REQUEST_CODE, args, GRC_PATTERN, GRC_RESPONSES)
    end subroutine dm_geocom_api_request_set_orientation

    pure subroutine dm_geocom_api_request_set_position(request, hz, v, pos_mode, atr_mode)
        !! Request of *AUT_MakePositioning* procedure. Creates request for
        !! turning the telescope to a specified position.
        !!
        !! If `pos_mode` is `GEOCOM_AUT_NORMAL`, uses the current value of the
        !! compensator. For positioning distances > 25 gon, this mode might tend
        !! to inaccuracy. If set to `GEOCOM_AUT_PRECISE`, tries to measure the
        !! exact inclination of the target. Tends to long position time.
        !!
        !! If `atr_mode` is `GEOCOM_AUT_POSITION`, uses conventional position to
        !! other face. If set to `GEOCOM_AUT_TARGET`, tries to position into a
        !! target in the destination area. This mode requires activated ATR.
        !!
        !! The instrument returns the following responses:
        !!
        !! * `grc` – GeoCOM return code.
        !!
        !! | Property       | Values                                           |
        !! |----------------|--------------------------------------------------|
        !! | Instruments    | TPS1100, TPS1200, TM30/TS30, TS16                |
        !! | ASCII request  | `%R1Q,9027:<hz>,<v>,<pos_mode>,<atr_mode>,0`     |
        !! | ASCII response | `%R1P,0,0:<grc>`                                 |
        !!
        character(len=*), parameter :: REQUEST_NAME = 'set_position'
        integer,          parameter :: REQUEST_CODE = 9027

        type(request_type), intent(out) :: request  !! Prepared request.
        real(kind=r8),      intent(in)  :: hz       !! Horizontal angle [rad].
        real(kind=r8),      intent(in)  :: v        !! Vertical angle [rad].
        integer,            intent(in)  :: pos_mode !! Position mode (`GEOCOM_AUT_POSMODE`).
        integer,            intent(in)  :: atr_mode !! ATR mode (`GEOCOM_AUT_ATRMODE`).

        character(len=80) :: args

        write (args, '(2(f0.12, ","), 2(i0, ","), "0")') hz, v, pos_mode, atr_mode
        call dm_geocom_api_request(request, REQUEST_NAME, REQUEST_CODE, args, GRC_PATTERN, GRC_RESPONSES)
    end subroutine dm_geocom_api_request_set_position

    pure subroutine dm_geocom_api_request_set_positioning_timeout(request, time_hz, time_v)
        !! Request of *AUT_SetTimeout* procedure. Creates request for setting
        !! the timeout for positioning.
        !!
        !! This function sets the maximum time to perform a positioning. The
        !! timeout is reset on 7 seconds after each power on. Valid value for
        !! `time_hz` and `time_v` are between 7.0 [sec] and 60.0 [sec].
        !!
        !! The instrument returns the following responses:
        !!
        !! * `grc` – GeoCOM return code.
        !!
        !! | Property       | Values                                           |
        !! |----------------|--------------------------------------------------|
        !! | Instruments    | TPS1100, TPS1200, TM30/TS30, TS16                |
        !! | ASCII request  | `%R1Q,9011:<time_hz>,<time_v>`                   |
        !! | ASCII response | `%R1P,0,0:<grc>`                                 |
        !!
        character(len=*), parameter :: REQUEST_NAME = 'set_positioning_timeout'
        integer,          parameter :: REQUEST_CODE = 9011

        type(request_type), intent(out) :: request !! Prepared request.
        real(kind=r8),      intent(in)  :: time_hz !! Timeout in Hz direction [s].
        real(kind=r8),      intent(in)  :: time_v  !! Timeout in V direction [s].

        character(len=80) :: args

        write (args, '(f0.12, ",", f0.12)') time_hz, time_v
        call dm_geocom_api_request(request, REQUEST_NAME, REQUEST_CODE, args, GRC_PATTERN, GRC_RESPONSES)
    end subroutine dm_geocom_api_request_set_positioning_timeout

    pure subroutine dm_geocom_api_request_set_prism_constant(request, prism_const)
        !! Request of *TMC_SetPrismCorr* procedure. Creates request for
        !! setting the prism constant. The API function *BAP_SetPrismType*
        !! overwrites this setting.
        !!
        !! The instrument returns the following responses:
        !!
        !! * `grc` – GeoCOM return code.
        !!
        !! | Property       | Values                                           |
        !! |----------------|--------------------------------------------------|
        !! | Instruments    | TPS1100, TPS1200, TM30/TS30, TS16                |
        !! | ASCII request  | `%R1Q,2024:<prism_const>`                        |
        !! | ASCII response | `%R1P,0,0:<grc>`                                 |
        !!
        character(len=*), parameter :: REQUEST_NAME = 'set_prism_constant'
        integer,          parameter :: REQUEST_CODE = 2024

        type(request_type), intent(out) :: request     !! Prepared request.
        real(kind=r8),      intent(in)  :: prism_const !! Prism constant [mm].

        character(len=80) :: args

        write (args, '(f0.12)') prism_const
        call dm_geocom_api_request(request, REQUEST_NAME, REQUEST_CODE, args, GRC_PATTERN, GRC_RESPONSES)
    end subroutine dm_geocom_api_request_set_prism_constant

    pure subroutine dm_geocom_api_request_set_prism_type(request, prism_type)
        !! Request of *BAP_SetPrismType* procedure. Creates request for
        !! setting the default prism type.
        !!
        !! This function sets the prism type for measurement with a reflector
        !! (`GEOCOM_BAP_PRISMTYPE`). It overwrites the prism constant set by
        !! API call *TMC_SetPrimCorr*.
        !!
        !! The instrument returns the following responses:
        !!
        !! * `grc` – GeoCOM return code.
        !!
        !! | Property       | Values                                           |
        !! |----------------|--------------------------------------------------|
        !! | Instruments    | TPS1100, TPS1200, TM30/TS30, TS16                |
        !! | ASCII request  | `%R1Q,17008:<prism_type>`                        |
        !! | ASCII response | `%R1P,0,0:<grc>`                                 |
        !!
        character(len=*), parameter :: REQUEST_NAME = 'set_prism_type'
        integer,          parameter :: REQUEST_CODE = 17008

        type(request_type), intent(out) :: request    !! Prepared request.
        integer,            intent(in)  :: prism_type !! Prism type (`GEOCOM_BAP_PRISMTYPE`).

        character(len=80) :: args

        write (args, '(i0)') prism_type
        call dm_geocom_api_request(request, REQUEST_NAME, REQUEST_CODE, args, GRC_PATTERN, GRC_RESPONSES)
    end subroutine dm_geocom_api_request_set_prism_type

    pure subroutine dm_geocom_api_request_set_prism_type_v2(request, prism_type, prism_name)
        !! Request of *BAP_SetPrismType2* procedure. Creates request for
        !! setting the default or user prism type.
        !!
        !! This function sets the default or the user prism type for
        !! measurements with a reflector. It overwrites the prism constant set
        !! by *TMC_SetPrismCorr*. The user defined prism must have been added
        !! with API call *BAP_SetUserPrismDef* beforehand.
        !!
        !! The instrument returns the following responses:
        !!
        !! * `grc` – GeoCOM return code.
        !!
        !! | Property       | Values                                           |
        !! |----------------|--------------------------------------------------|
        !! | Instruments    | TPS1200, TM30/TS30, TS16                         |
        !! | ASCII request  | `%R1Q,17030:<prism_type>,<prism_name>`           |
        !! | ASCII response | `%R1P,0,0:<grc>`                                 |
        !!
        character(len=*), parameter :: REQUEST_NAME = 'set_prism_type_v2'
        integer,          parameter :: REQUEST_CODE = 17030

        type(request_type), intent(out) :: request    !! Prepared request.
        integer,            intent(in)  :: prism_type !! Prism type (`GEOCOM_BAP_PRISMTYPE`).
        character(len=*),   intent(in)  :: prism_name !! Prism name (required if prism type is `GEOCOM_BAP_PRISM_USER`).

        character(len=80) :: args

        write (args, '(i0, ",", a)') prism_type, prism_name
        call dm_geocom_api_request(request, REQUEST_NAME, REQUEST_CODE, args, GRC_PATTERN, GRC_RESPONSES)
    end subroutine dm_geocom_api_request_set_prism_type_v2

    pure subroutine dm_geocom_api_request_set_reduced_atr_fov(request, enabled)
        !! Request of *BAP_SetRedATRFov* procedure. Creates request for
        !! setting the reduced ATR field of view.
        !!
        !! If `enabled` is `.true.`, ATR uses reduced field of view (about
        !! 1/9), full field of view otherwise.
        !!
        !! The instrument returns the following responses:
        !!
        !! * `grc` – GeoCOM return code.
        !!
        !! | Property       | Values                                           |
        !! |----------------|--------------------------------------------------|
        !! | Instruments    | TPS1200, TM30/TS30, TS16                         |
        !! | ASCII request  | `%R1Q,17008:<reduced>`                           |
        !! | ASCII response | `%R1P,0,0:<grc>`                                 |
        !!
        character(len=*), parameter :: REQUEST_NAME = 'set_reduced_atr_fov'
        integer,          parameter :: REQUEST_CODE = 17008

        type(request_type), intent(out) :: request !! Prepared request.
        logical,            intent(in)  :: enabled !! Use reduced field of view.

        character(len=80) :: args

        write (args, '(i1)') dm_btoi(enabled)
        call dm_geocom_api_request(request, REQUEST_NAME, REQUEST_CODE, args, GRC_PATTERN, GRC_RESPONSES)
    end subroutine dm_geocom_api_request_set_reduced_atr_fov

    pure subroutine dm_geocom_api_request_set_refraction_mode(request, mode)
        !! Request of *TMC_SetRefractiveMethod* procedure. Creates request for
        !! setting the refraction model.
        !!
        !! Mode `1` means method 1 for the rest of the world, mode `2` means
        !! method for Australia.
        !!
        !! The instrument returns the following responses:
        !!
        !! * `grc` – GeoCOM return code.
        !!
        !! | Property       | Values                                           |
        !! |----------------|--------------------------------------------------|
        !! | Instruments    | TPS1100, TPS1200, TM30/TS30, TS16                |
        !! | ASCII request  | `%R1Q,2090:<mode>`                               |
        !! | ASCII response | `%R1P,0,0:<grc>`                                 |
        !!
        character(len=*), parameter :: REQUEST_NAME = 'set_refraction_mode'
        integer,          parameter :: REQUEST_CODE = 2090

        type(request_type), intent(out) :: request !! Prepared request.
        integer,            intent(in)  :: mode    !! Refraction data method (1 or 2).

        character(len=80) :: args

        write (args, '(i0)') mode
        call dm_geocom_api_request(request, REQUEST_NAME, REQUEST_CODE, args, GRC_PATTERN, GRC_RESPONSES)
    end subroutine dm_geocom_api_request_set_refraction_mode

    pure subroutine dm_geocom_api_request_set_search_area(request, center_hz, center_v, range_hz, range_v, enabled)
        !! Request of *AUT_SetSearchArea* procedure. Creates request for
        !! setting the PowerSearch window.
        !!
        !! The function sets the position and dimensions of the PowerSearch
        !! window, and activates it. The API call is valid for all instruments,
        !! but has effects only for those equipped with PowerSearch (requires
        !! GeoCOM robotic licence).
        !!
        !! The instrument returns the following responses:
        !!
        !! * `grc` – GeoCOM return code.
        !!
        !! | Property       | Values                                                            |
        !! |----------------|-------------------------------------------------------------------|
        !! | Instruments    | TPS1100, TPS1200, TM30/TS30, TS16                                 |
        !! | ASCII request  | `%R1Q,9043:<center_hz>,<center_v>,<range_hz>,<range_v>,<enabled>` |
        !! | ASCII response | `%R1P,0,0:<grc>`                                                  |
        !!
        character(len=*), parameter :: REQUEST_NAME = 'set_search_area'
        integer,          parameter :: REQUEST_CODE = 9043

        type(request_type), intent(out) :: request   !! Prepared request.
        real(kind=r8),      intent(in)  :: center_hz !! Search area center Hz angle [rad].
        real(kind=r8),      intent(in)  :: center_v  !! Search area center V angle [rad].
        real(kind=r8),      intent(in)  :: range_hz  !! Search area range Hz angle [rad].
        real(kind=r8),      intent(in)  :: range_v   !! Search area range V angle [rad].
        logical,            intent(in)  :: enabled   !! Enable search area.

        character(len=80) :: args

        write (args, '(4(f0.12, ","), i1)') center_hz, center_v, range_hz, range_v, dm_btoi(enabled)
        call dm_geocom_api_request(request, REQUEST_NAME, REQUEST_CODE, args, GRC_PATTERN, GRC_RESPONSES)
    end subroutine dm_geocom_api_request_set_search_area

    pure subroutine dm_geocom_api_request_set_station(request, easting, northing, height, instr_height)
        !! Request of *TMC_SetStation* procedure. Creates request for setting
        !! the station coordinates of the instrument.
        !!
        !! The instrument returns the following responses:
        !!
        !! * `grc` – GeoCOM return code.
        !!
        !! | Property       | Values                                                   |
        !! |----------------|----------------------------------------------------------|
        !! | Instruments    | TPS1100, TPS1200, TM30/TS30, TS16                        |
        !! | ASCII request  | `%R1Q,2010:<easting>,<northing>,<height>,<instr_height>` |
        !! | ASCII response | `%R1P,0,0:<grc>`                                         |
        !!
        character(len=*), parameter :: REQUEST_NAME = 'set_station'
        integer,          parameter :: REQUEST_CODE = 2010

        type(request_type), intent(out) :: request      !! Prepared request.
        real(kind=r8),      intent(in)  :: easting      !! E coordinate [m].
        real(kind=r8),      intent(in)  :: northing     !! N coordinate [m].
        real(kind=r8),      intent(in)  :: height       !! H coordinate [m].
        real(kind=r8),      intent(in)  :: instr_height !! Instrument height [m].

        character(len=80) :: args

        write (args, '(3(f0.12, ","), f0.12)') easting, northing, height, instr_height
        call dm_geocom_api_request(request, REQUEST_NAME, REQUEST_CODE, args, GRC_PATTERN, GRC_RESPONSES)
    end subroutine dm_geocom_api_request_set_station

    pure subroutine dm_geocom_api_request_set_target_type(request, target_type)
        !! Request of *BAP_SetTargetType* procedure. Creates request for
        !! setting the EDM type.
        !!
        !! The function sets the current EDM type (`GEOCOM_BAP_TARGET_TYPE`)
        !! for distance measurements: reflector (IR) or reflectorless (RL). For
        !! each EDM type, the EDM mode used last is remembered and actived if
        !! the EDM type is changed. If EDM type IR is selected, the automation
        !! mode used last is activated automatically. The API function
        !! *BAP_SetMeasPrg* can also change the target type. The EDM type RL is
        !! not available on all instruments.
        !!
        !! The instrument returns the following responses:
        !!
        !! * `grc` – GeoCOM return code.
        !!
        !! | Property       | Values                                           |
        !! |----------------|--------------------------------------------------|
        !! | Instruments    | TPS1100, TPS1200, TM30/TS30, TS16                |
        !! | ASCII request  | `%R1Q,17021:<target_type>`                       |
        !! | ASCII response | `%R1P,0,0:<grc>`                                 |
        !!
        character(len=*), parameter :: REQUEST_NAME = 'set_target_type'
        integer,          parameter :: REQUEST_CODE = 17021

        type(request_type), intent(out) :: request     !! Prepared request.
        integer,            intent(in)  :: target_type !! Target type (`GEOCOM_BAP_TARGET_TYPE`).

        character(len=80) :: args

        write (args, '(i0)') target_type
        call dm_geocom_api_request(request, REQUEST_NAME, REQUEST_CODE, args, GRC_PATTERN, GRC_RESPONSES)
    end subroutine dm_geocom_api_request_set_target_type

    pure subroutine dm_geocom_api_request_set_tolerance(request, hz, v)
        !! Request of *AUT_SetTol* procedure. Creates request for setting
        !! the positioning tolerances.
        !!
        !! This function sets the position tolerances of the Hz and V
        !! instrument axes (GeoCOM robotic licence required). The tolerances
        !! must be in the range of 1 [cc] (1.57079E-06 [rad]) to 100 [cc]
        !! (1.57079E-04 [rad]).
        !!
        !! The maximum resolution of the angle measurement system depends on
        !! the instrument accuracy class. If smaller positioning tolerances are
        !! required, the positioning time can increase drastically.
        !!
        !! The instrument returns the following responses:
        !!
        !! * `grc` – GeoCOM return code.
        !!
        !! | Property       | Values                                           |
        !! |----------------|--------------------------------------------------|
        !! | Instruments    | TPS1100, TPS1200, TM30/TS30, TS16                |
        !! | ASCII request  | `%R1Q,9007:<hz>,<v>`                             |
        !! | ASCII response | `%R1P,0,0:<grc>`                                 |
        !!
        character(len=*), parameter :: REQUEST_NAME = 'set_tolerance'
        integer,          parameter :: REQUEST_CODE = 9007

        type(request_type), intent(out) :: request !! Prepared request.
        real(kind=r8),      intent(in)  :: hz      !! Positioning tolerance in Hz direction [rad].
        real(kind=r8),      intent(in)  :: v       !! Positioning tolerance in V direction [rad].

        character(len=80) :: args

        write (args, '(f0.12, ",", f0.12)') hz, v
        call dm_geocom_api_request(request, REQUEST_NAME, REQUEST_CODE, args, GRC_PATTERN, GRC_RESPONSES)
    end subroutine dm_geocom_api_request_set_tolerance

    pure subroutine dm_geocom_api_request_set_user_atr_mode(request, enabled)
        !! Request of *AUS_SetUserAtrState* procedure. Creates request for
        !! setting the status of the ATR state.
        !!
        !! The function activates or deactivates the ATR mode (requires GeoCOM
        !! robotic licence). If `enabled` is `.true.`, ATR mode is activated,
        !! and if LOCK mode is enabled while the API call is made, LOCK mode
        !! will change to ATR mode. If `enabled` is `.false.`, ATR mode is
        !! deactivated, and if LOCK mode is enabled then it stays enabled.
        !!
        !! The instrument returns the following responses:
        !!
        !! * `grc` – GeoCOM return code.
        !!
        !! | Property       | Values                                           |
        !! |----------------|--------------------------------------------------|
        !! | Instruments    | TPS1100, TPS1200, TM30/TS30, TS16                |
        !! | ASCII request  | `%R1Q,18005:<enabled>`                           |
        !! | ASCII response | `%R1P,0,0:<grc>`                                 |
        !!
        character(len=*), parameter :: REQUEST_NAME = 'set_user_atr_mode'
        integer,          parameter :: REQUEST_CODE = 18005

        type(request_type), intent(out) :: request !! Prepared request.
        logical,            intent(in)  :: enabled !! Enable ATR mode.

        character(len=80) :: args

        write (args, '(i1)') dm_btoi(enabled)
        call dm_geocom_api_request(request, REQUEST_NAME, REQUEST_CODE, args, GRC_PATTERN, GRC_RESPONSES)
    end subroutine dm_geocom_api_request_set_user_atr_mode

    pure subroutine dm_geocom_api_request_set_user_lock_mode(request, enabled)
        !! Request of *AUS_SetUserLockState* procedure. Creates request for
        !! setting the status of the LOCK state.
        !!
        !! The function activated or deactivates the LOCK mode (GeoCOM robotic
        !! licence required). If `enabled` is `.true.`, LOCK mode is activated.
        !! In order to lock and follow a moving target, call API function
        !! *AUT_LockIn*. If `enabled` is `.false.`, LOCK mode is deactivated.
        !! Tracking of a moving target will be aborted, and the manual drive
        !! wheel is activated.
        !!
        !! The instrument returns the following responses:
        !!
        !! * `grc` – GeoCOM return code.
        !!
        !! | Property       | Values                                           |
        !! |----------------|--------------------------------------------------|
        !! | Instruments    | TPS1100, TPS1200, TM30/TS30, TS16                |
        !! | ASCII request  | `%R1Q,18007:<enabled>`                           |
        !! | ASCII response | `%R1P,0,0:<grc>`                                 |
        !!
        character(len=*), parameter :: REQUEST_NAME = 'set_user_lock_mode'
        integer,          parameter :: REQUEST_CODE = 18007

        type(request_type), intent(out) :: request !! Prepared request.
        logical,            intent(in)  :: enabled !! Enable LOCK mode.

        character(len=80) :: args

        write (args, '(i1)') dm_btoi(enabled)
        call dm_geocom_api_request(request, REQUEST_NAME, REQUEST_CODE, args, GRC_PATTERN, GRC_RESPONSES)
    end subroutine dm_geocom_api_request_set_user_lock_mode

    pure subroutine dm_geocom_api_request_set_user_prism_definition(request, prism_name, prism_const, refl_type, creator)
        !! Request of *BAP_SetUserPrismDef* procedure. Creates request for
        !! setting a user prism definition.
        !!
        !! The instrument returns the following responses:
        !!
        !! * `grc` – GeoCOM return code.
        !!
        !! | Property       | Values                                                         |
        !! |----------------|----------------------------------------------------------------|
        !! | Instruments    | TPS1200, TM30/TS30, TS16                                       |
        !! | ASCII request  | `%R1Q,17032:<prism_name>,<prism_const>,<refl_type>,<creator>`  |
        !! | ASCII response | `%R1P,0,0:<grc>`                                               |
        !!
        character(len=*), parameter :: REQUEST_NAME = 'set_user_prism_definition'
        integer,          parameter :: REQUEST_CODE = 17032

        type(request_type), intent(out) :: request     !! Prepared request.
        character(len=*),   intent(in)  :: prism_name  !! Prism name.
        real(kind=r8),      intent(in)  :: prism_const !! Prism correction constant [mm].
        integer,            intent(in)  :: refl_type   !! Reflector type (`GEOCOM_BAP_REFLTYPE`).
        character(len=*),   intent(in)  :: creator     !! Name of creator.

        character(len=80) :: args

        write (args, '("""", a, """,", f0.12, ",", i0, ", """, a, """")') &
            trim(prism_name), prism_const, refl_type, trim(creator)
        call dm_geocom_api_request(request, REQUEST_NAME, REQUEST_CODE, args, GRC_PATTERN, GRC_RESPONSES)
    end subroutine dm_geocom_api_request_set_user_prism_definition

    pure subroutine dm_geocom_api_request_set_user_spiral(request, hz, v)
        !! Request of *AUT_SetUserSpiral* procedure. Creates request for
        !! setting the ATR search window.
        !!
        !! The function sets the dimensions of the ATR search window (GeoCOM
        !! robotic licence required).
        !!
        !! The instrument returns the following responses:
        !!
        !! * `grc` – GeoCOM return code.
        !!
        !! | Property       | Values                                           |
        !! |----------------|--------------------------------------------------|
        !! | Instruments    | TPS1100, TPS1200, TM30/TS30, TS16                |
        !! | ASCII request  | `%R1Q,9041:<hz>,<v>`                             |
        !! | ASCII response | `%R1P,0,0:<grc>`                                 |
        !!
        character(len=*), parameter :: REQUEST_NAME = 'set_user_spiral'
        integer,          parameter :: REQUEST_CODE = 9041

        type(request_type), intent(out) :: request !! Prepared request.
        real(kind=r8),      intent(in)  :: hz      !! ATR search window in Hz direction [rad].
        real(kind=r8),      intent(in)  :: v       !! ATR search window in V direction [rad].

        character(len=80) :: args

        write (args, '(f0.12, ",", f0.12)') hz, v
        call dm_geocom_api_request(request, REQUEST_NAME, REQUEST_CODE, args, GRC_PATTERN, GRC_RESPONSES)
    end subroutine dm_geocom_api_request_set_user_spiral

    pure subroutine dm_geocom_api_request_set_velocity(request, omega_hz, omega_v)
        !! Request of *MOT_SetVelocity* procedure. Creates request for
        !! driving the instrument with constant speed.
        !!
        !! The function is used to set up the velocity of the motorisation
        !! (GeoCOM robotic licence required). The API function
        !! *MOT_StartController* must have been called with argument
        !! `GEOCOM_MOT_OCONST` before.
        !!
        !! The velocity in horizontal and vertical direction are in [rad/s].
        !! The maximum velocity is ±3.14 rad/s for TM30/TS30, and ±0.79 rad/s
        !! for TPS1100/TPS1200.
        !!
        !! The instrument returns the following responses:
        !!
        !! * `grc` – GeoCOM return code.
        !!
        !! | Property       | Values                                           |
        !! |----------------|--------------------------------------------------|
        !! | Instruments    | TPS1100, TPS1200, TM30/TS30, TS16                |
        !! | ASCII request  | `%R1Q,6004:<omega_hz>,<omega_v>`                 |
        !! | ASCII response | `%R1P,0,0:<grc>`                                 |
        !!
        character(len=*), parameter :: REQUEST_NAME = 'set_velocity'
        integer,          parameter :: REQUEST_CODE = 6004

        type(request_type), intent(out) :: request  !! Prepared request.
        real(kind=r8),      intent(in)  :: omega_hz !! Velocity in Hz direction [rad/s].
        real(kind=r8),      intent(in)  :: omega_v  !! Velocity in V direction [rad/s].

        character(len=80) :: args

        write (args, '(f0.12, ",", f0.12)') omega_hz, omega_v
        call dm_geocom_api_request(request, REQUEST_NAME, REQUEST_CODE, args, GRC_PATTERN, GRC_RESPONSES)
    end subroutine dm_geocom_api_request_set_velocity

    pure subroutine dm_geocom_api_request_setup_download(request, device_type, file_type, file_name, block_size)
        !! Request of *FTR_SetupDownload* procedure. Creates request for
        !! setting up a file download.
        !!
        !! The function has to be called before *FTR_Download*. If the file
        !! type is `GEOCOM_FTR_FILE_UNKNOWN`, an additional file path is
        !! required.
        !!
        !! The argument `device_type` must be one of the following:
        !!
        !! * `GEOCOM_FTR_DEVICE_INTERNAL` – Internal memory (path `/ata1a/`).
        !! * `GEOCOM_FTR_DEVICE_PCPARD`   – CF Card (path `/ata0a/`).
        !!
        !! The argument `file_type` is usually `GEOCOM_FTR_FILE_IMAGES`. The
        !! maximum value for `block_size` is `GEOCOM_FTR_MAX_BLOCKSIZE`.
        !!
        !! The instrument returns the following responses:
        !!
        !! * `grc`     – GeoCOM return code.
        !! * `nblocks` – Number of blocks required to upload the file.
        !!
        !! | Property       | Values                                                          |
        !! |----------------|-----------------------------------------------------------------|
        !! | Instruments    | TM30/TS30                                                       |
        !! | ASCII request  | `%R1Q,23303:<device_type>,<file_type>,<file_name>,<block_size>` |
        !! | ASCII response | `%R1P,0,0:<grc>,<nblocks>`                                      |
        !!
        character(len=*), parameter :: REQUEST_NAME    = 'setup_download'
        character(len=*), parameter :: REQUEST_PATTERN = GRC_PATTERN // ',(?<nblocks>\d+)'
        integer,          parameter :: REQUEST_CODE    = 23303

        type(request_type), intent(out) :: request     !! Prepared request.
        integer,            intent(in)  :: device_type !! Device type (`GEOCOM_FTR_DEVICETYPE`).
        integer,            intent(in)  :: file_type   !! File type (`GEOCOM_FTR_FILETYPE`).
        character(len=*),   intent(in)  :: file_name   !! File name with extension.
        integer,            intent(in)  :: block_size  !! Block size.

        character(len=80)   :: args
        type(response_type) :: responses(2)

        responses = [ &
            response_type('grc',     type=RESPONSE_TYPE_INT32), &
            response_type('nblocks', type=RESPONSE_TYPE_INT32)  &
        ]

        write (args, '(2(i0, ","), """", a, """,", i0)') device_type, file_type, trim(file_name), block_size
        call dm_geocom_api_request(request, REQUEST_NAME, REQUEST_CODE, args, REQUEST_PATTERN, responses)
    end subroutine dm_geocom_api_request_setup_download

    pure subroutine dm_geocom_api_request_setup_list(request, device_type, file_type, search_path)
        !! Request of *FTR_SetupList* procedure. Creates request for
        !! setting up file listing.
        !!
        !! The function sets up the device type, file type, and search path. It
        !! has to be called before *FTR_List*.
        !!
        !! The instrument returns the following responses:
        !!
        !! * `grc` – GeoCOM return code.
        !!
        !! | Property       | Values                                               |
        !! |----------------|------------------------------------------------------|
        !! | Instruments    | TM30/TS30                                            |
        !! | ASCII request  | `%R1Q,23306:<device_type>,<file_type>,<search_path>` |
        !! | ASCII response | `%R1P,0,0:<grc>`                                     |
        !!
        character(len=*), parameter :: REQUEST_NAME = 'setup_list'
        integer,          parameter :: REQUEST_CODE = 23306

        type(request_type), intent(out) :: request     !! Prepared request.
        integer,            intent(in)  :: device_type !! Device type (`GEOCOM_FTR_DEVICETYPE`).
        integer,            intent(in)  :: file_type   !! File type (`GEOCOM_FTR_FILETYPE`).
        character(len=*),   intent(in)  :: search_path !! Optional search path, required for file type `GEOCOM_FTR_FILE_UNKNOWN`.

        character(len=80) :: args

        write (args, '(2(i0, ","), a)') device_type, file_type, search_path
        call dm_geocom_api_request(request, REQUEST_NAME, REQUEST_CODE, args, GRC_PATTERN, GRC_RESPONSES)
    end subroutine dm_geocom_api_request_setup_list

    pure subroutine dm_geocom_api_request_start_controller(request, start_mode)
        !! Request of *MOT_StartController* procedure. Creates request for
        !! starting the motor controller.
        !!
        !! If this function is used in combination with API call
        !! *MOT_SetVelocity*, the controller mode has to be `GEOCOM_MOT_OCONST`.
        !!
        !! The argument `start_mode` must be one of the following:
        !!
        !! * `GEOCOM_MOT_POSIT`   –  Relative positioning.
        !! * `GEOCOM_MOT_OCONST`  –  Constant speed.
        !! * `GEOCOM_MOT_MANUPOS` –  Manual positioning (default setting).
        !! * `GEOCOM_MOT_LOCK`    –  "Lock-in" controller.
        !! * `GEOCOM_MOT_BREAK`   –  "Brake" controller.
        !! * `GEOCOM_MOT_TERM`    –  Terminates the controller task.
        !!
        !! The instrument returns the following responses:
        !!
        !! * `grc` – GeoCOM return code.
        !!
        !! | Property       | Values                                           |
        !! |----------------|--------------------------------------------------|
        !! | Instruments    | TPS1100, TPS1200, TM30/TS30, TS16                |
        !! | ASCII request  | `%R1Q,6001:<start_mode>`                         |
        !! | ASCII response | `%R1P,0,0:<grc>`                                 |
        !!
        character(len=*), parameter :: REQUEST_NAME = 'start_controller'
        integer,          parameter :: REQUEST_CODE = 6001

        type(request_type), intent(out) :: request    !! Prepared request.
        integer,            intent(in)  :: start_mode !! Controller start mode (`GEOCOM_MOT_MODE`).

        character(len=80) :: args

        write (args, '(i0)') start_mode
        call dm_geocom_api_request(request, REQUEST_NAME, REQUEST_CODE, args, GRC_PATTERN, GRC_RESPONSES)
    end subroutine dm_geocom_api_request_start_controller

    pure subroutine dm_geocom_api_request_stop_controller(request, stop_mode)
        !! Request of *MOT_StartController* procedure. Creates request for
        !! stopping the motor controller.
        !!
        !! This function stops the movement and the motor controller program.
        !!
        !! The argument `stop_mode` must be one of the following:
        !!
        !! * `GEOCOM_MOT_NORMAL`   – Slow down with current acceleration.
        !! * `GEOCOM_MOT_SHUTDOWN` – Slow down by switching off power supply.
        !!
        !! The instrument returns the following responses:
        !!
        !! * `grc` – GeoCOM return code.
        !!
        !! | Property       | Values                                           |
        !! |----------------|--------------------------------------------------|
        !! | Instruments    | TPS1100, TPS1200, TM30/TS30, TS16                |
        !! | ASCII request  | `%R1Q,6002:<stop_mode>`                          |
        !! | ASCII response | `%R1P,0,0:<grc>`                                 |
        !!
        character(len=*), parameter :: REQUEST_NAME = 'stop_controller'
        integer,          parameter :: REQUEST_CODE = 6002

        type(request_type), intent(out) :: request   !! Prepared request.
        integer,            intent(in)  :: stop_mode !! Controller stop mode (`GEOCOM_MOT_STOPMODE`).

        character(len=80) :: args

        write (args, '(i0)') stop_mode
        call dm_geocom_api_request(request, REQUEST_NAME, REQUEST_CODE, args, GRC_PATTERN, GRC_RESPONSES)
    end subroutine dm_geocom_api_request_stop_controller

    pure subroutine dm_geocom_api_request_switch_off(request, stop_mode)
        !! Request of *COM_SwitchOffTPS* procedure. Creates request for
        !! turning the instrument off.
        !!
        !! The argument `stop_mode` has to be one of the following:
        !!
        !! * `GEOCOM_COM_TPS_STOP_SHUT_DOWN` – Power down instrument.
        !! * `GEOCOM_COM_TPS_STOP_SLEEP`     – Sleep mode (not supported by TPS1200).
        !!
        !! The instrument returns the following responses:
        !!
        !! * `grc` – GeoCOM return code.
        !!
        !! | Property       | Values                                           |
        !! |----------------|--------------------------------------------------|
        !! | Instruments    | TPS1100, TPS1200, TM30/TS30, TS16                |
        !! | ASCII request  | `%R1Q,112:<mode>`                                |
        !! | ASCII response | `%R1P,0,0:<grc>`                                 |
        !!
        character(len=*), parameter :: REQUEST_NAME = 'switch_off'
        integer,          parameter :: REQUEST_CODE = 112

        type(request_type), intent(out) :: request   !! Prepared request.
        integer,            intent(in)  :: stop_mode !! Switch-off mode (`GEOCOM_COM_TPS_STOP_MODE`).

        character(len=80) :: args

        write (args, '(i0)') stop_mode
        call dm_geocom_api_request(request, REQUEST_NAME, REQUEST_CODE, args, GRC_PATTERN, GRC_RESPONSES)
    end subroutine dm_geocom_api_request_switch_off

    pure subroutine dm_geocom_api_request_switch_on(request, start_mode)
        !! Request of *COM_SwitchOnTPS* procedure. Creates request for turning
        !! the instrument on.
        !!
        !! The argument `start_mode` has to be one of the following:
        !!
        !! * `GEOCOM_COM_TPS_STARTUP_LOCAL`  – Not supported by TPS1200.
        !! * `GEOCOM_COM_TPS_STARTUP_REMOTE` – Online mode (RPC is enabled).
        !!
        !! The instrument may return the following responses:
        !!
        !! * `grc` – GeoCOM return code.
        !!
        !! The response is not captured by the returned request.
        !!
        !! | Property       | Values                                           |
        !! |----------------|--------------------------------------------------|
        !! | Instruments    | TPS1100, TPS1200, TM30/TS30, TS16                |
        !! | ASCII request  | `%R1Q,111:<start_mode>`                          |
        !! | ASCII response | `%R1P,0,0:5` (if switched on)                    |
        !!
        character(len=*), parameter :: REQUEST_NAME = 'switch_on'
        integer,          parameter :: REQUEST_CODE = 111

        type(request_type), intent(out) :: request    !! Prepared request.
        integer,            intent(in)  :: start_mode !! Switch-on mode (`GEOCOM_COM_TPS_STARTUP_MODE`).

        character(len=80) :: args

        write (args, '(i0)') start_mode
        call dm_geocom_api_request(request, REQUEST_NAME, REQUEST_CODE, args)
    end subroutine dm_geocom_api_request_switch_on

    pure subroutine dm_geocom_api_request_take_image(request, mem_type)
        !! Request of *IMG_TakeTccImage* procedure. Creates request for
        !! capturing a telescope image.
        !!
        !! The memory type `mem_type` has to be one of the following:
        !!
        !! * `GEOCOM_IMG_INTERNAL_MEMORY` – Internal memory module.
        !! * `GEOCOM_IMG_PC_CARD`         – External PC card.
        !!
        !! The instrument returns the following responses:
        !!
        !! * `grc`     – GeoCOM return code.
        !! * `imageno` – Number of the currently captured image.
        !!
        !! | Property       | Values                                           |
        !! |----------------|--------------------------------------------------|
        !! | Instruments    | TM30/TS30                                        |
        !! | ASCII request  | `%R1Q,23402:<mem_type>`                          |
        !! | ASCII response | `%R1P,0,0:<grc>,<imageno>`                       |
        !!
        character(len=*), parameter :: REQUEST_NAME    = 'take_image'
        character(len=*), parameter :: REQUEST_PATTERN = GRC_PATTERN // ',(?<imageno>\d+)'
        integer,          parameter :: REQUEST_CODE    = 23402

        type(request_type), intent(out) :: request  !! Prepared request.
        integer,            intent(in)  :: mem_type !! Memory type (`GEOCOM_IMG_MEM_TYPE`).

        character(len=80)   :: args
        type(response_type) :: responses(2)

        responses = [ &
            response_type('grc',     type=RESPONSE_TYPE_INT32), &
            response_type('imageno', type=RESPONSE_TYPE_INT64)  &
        ]

        write (args, '(i0)') mem_type
        call dm_geocom_api_request(request, REQUEST_NAME, REQUEST_CODE, args, REQUEST_PATTERN, responses)
    end subroutine dm_geocom_api_request_take_image
end module dm_geocom_api
