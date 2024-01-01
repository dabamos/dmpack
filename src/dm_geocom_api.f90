! Author:  Philipp Engel
! Licence: ISC
module dm_geocom_api
    !! Parameters, types, and procedures for GeoCOM protocol handling.
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
    !! All GeoCOM parameters start with prefix `GEOCOM_`.
    use :: dm_geocom_error
    use :: dm_kind
    use :: dm_request
    use :: dm_response
    implicit none (type, external)
    private

    ! **************************************************************************
    ! PRIVATE GEOCOM API CONSTANTS.
    ! **************************************************************************
    character(len=*), parameter :: GEOCOM_DELIMITER   = '\r\n'        !! Default GeoCOM delimiter.
    character(len=*), parameter :: GEOCOM_GRC_PATTERN = '(?<grc>\d+)' !! Default GeoCOM response pattern.

    type(response_type), parameter :: GEOCOM_GRC_RESPONSES(1) = [ response_type('grc', type=RESPONSE_TYPE_INT32) ] !! Default GeoCOM responses (GRC only).

    ! **************************************************************************
    ! AUT - AUTOMATION.
    ! **************************************************************************
    ! AUT_POSMODE: Position precision.
    integer, parameter, public :: GEOCOM_AUT_NORMAL        = 0  !! Fast positioning mode.
    integer, parameter, public :: GEOCOM_AUT_PRECISE       = 1  !! Exact positioning mode.
    integer, parameter, public :: GEOCOM_AUT_FAST          = 2  !! For TM30/TS30.

    ! AUT_ADJMODE: Fine-adjust position mode.
    integer, parameter, public :: GEOCOM_AUT_NORM_MODE     = 0  !! Angle tolerance.
    integer, parameter, public :: GEOCOM_AUT_POINT_MODE    = 1  !! Point tolerance.
    integer, parameter, public :: GEOCOM_AUT_DEFINE_MODE   = 2  !! System independent positioning tolerance.

    ! AUT_ATRMODE: Automatic target recognition mode.
    integer, parameter, public :: GEOCOM_AUT_POSITION      = 0  !! Positioning to Hz and V angle.
    integer, parameter, public :: GEOCOM_AUT_TARGET        = 1  !! Positioning to a target in the env. of the Hz V angle.

    integer, parameter, public :: GEOCOM_AUT_CLOCKWISE     = 1  !! Direction close-wise.
    integer, parameter, public :: GEOCOM_AUT_ANTICLOCKWISE = -1 !! Direction counter clock-wise.

    ! **************************************************************************
    ! BAP - BASIC APPLICATIONS.
    ! **************************************************************************
    ! BAP_MEASURE_PRG: Measurement modes.
    integer, parameter, public :: GEOCOM_BAP_NO_MEAS    = 0 !! No measurements, take last one.
    integer, parameter, public :: GEOCOM_BAP_NO_DIST    = 1 !! No dist. measurement, angles only.
    integer, parameter, public :: GEOCOM_BAP_DEF_DIST   = 2 !! Default distance measurements.
    integer, parameter, public :: GEOCOM_BAP_CLEAR_DIST = 5 !! Clear distances.
    integer, parameter, public :: GEOCOM_BAP_STOP_TRK   = 6 !! Stop tracking.

    ! BAP_USER_MEASPRG: Distance measurement programs.
    integer, parameter, public :: GEOCOM_BAP_SINGLE_REF_STANDARD  = 0  !! IR standard.
    integer, parameter, public :: GEOCOM_BAP_SINGLE_REF_FAST      = 1  !! IR fast.
    integer, parameter, public :: GEOCOM_BAP_SINGLE_REF_VISIBLE   = 2  !! LO standard.
    integer, parameter, public :: GEOCOM_BAP_SINGLE_RLESS_VISIBLE = 3  !! RL standard.
    integer, parameter, public :: GEOCOM_BAP_CONT_REF_STANDARD    = 4  !! IR tracking.
    integer, parameter, public :: GEOCOM_BAP_CONT_REF_FAST        = 5  !! Not supported by TPS1200.
    integer, parameter, public :: GEOCOM_BAP_CONT_RLESS_VISIBLE   = 6  !! RL fast tracking.
    integer, parameter, public :: GEOCOM_BAP_AVG_REF_STANDARD     = 7  !! IR average.
    integer, parameter, public :: GEOCOM_BAP_AVG_REF_VISIBLE      = 8  !! LO average.
    integer, parameter, public :: GEOCOM_BAP_AVG_RLESS_VISIBLE    = 9  !! RL average.
    integer, parameter, public :: GEOCOM_BAP_CONT_REF_SYNCHRO     = 10 !! IR synchro tracking.
    integer, parameter, public :: GEOCOM_BAP_SINGLE_REF_PRECISE   = 11 !! IR precise (TM30/TS30).

    ! BAP_PRISMTYPE: Prism type definition.
    integer, parameter, public :: GEOCOM_BAP_PRISM_ROUND        = 0  !! Leica Circular Prism.
    integer, parameter, public :: GEOCOM_BAP_PRISM_MINI         = 1  !! Leica Mini Prism.
    integer, parameter, public :: GEOCOM_BAP_PRISM_TAPE         = 2  !! Leica Reflector Tape.
    integer, parameter, public :: GEOCOM_BAP_PRISM_360          = 3  !! Leica 360° Prism.
    integer, parameter, public :: GEOCOM_BAP_PRISM_USER1        = 4  !! not supported by TPS1200.
    integer, parameter, public :: GEOCOM_BAP_PRISM_USER2        = 5  !! not supported by TPS1200.
    integer, parameter, public :: GEOCOM_BAP_PRISM_USER3        = 6  !! not supported by TPS1200.
    integer, parameter, public :: GEOCOM_BAP_PRISM_360_MINI     = 7  !! Leica Mini 360° Prism
    integer, parameter, public :: GEOCOM_BAP_PRISM_MINI_ZERO    = 8  !! Leica Mini Zero Prism.
    integer, parameter, public :: GEOCOM_BAP_PRISM_USER         = 9  !! User Defined Prism.
    integer, parameter, public :: GEOCOM_BAP_PRISM_NDS_TAPE     = 10 !! Leica HDS Target.
    integer, parameter, public :: GEOCOM_BAP_PRISM_GRZ121_ROUND = 11 !! GRZ121 360º Prism for Machine Guidance.
    integer, parameter, public :: GEOCOM_BAP_PRISM_MA_MPR122    = 12 !! MPR122 360º Prism for Machine Guidance.

    ! BAP_REFLTYPE: Reflector type definition.
    integer, parameter, public :: GEOCOM_BAP_REFL_UNDEF = 0 !! Reflector not defined.
    integer, parameter, public :: GEOCOM_BAP_REFL_PRISM = 1 !! Reflector prism.
    integer, parameter, public :: GEOCOM_BAP_REFL_TAPE  = 2 !! Reflector tape.

    ! BAP_TARGET_TYPE: Target type definition.
    integer, parameter, public :: GEOCOM_BAP_REFL_USE  = 0 !! With reflector.
    integer, parameter, public :: GEOCOM_BAP_REFL_LESS = 1 !! Without reflector.

    ! BAP_ATRSETTING: ATR Low-Vis mode definition.
    integer, parameter, public :: GEOCOM_BAP_ATRSET_NORMAL     = 0 !! ATR is using no special flags or modes.
    integer, parameter, public :: GEOCOM_BAP_ATRSET_LOWVIS_ON  = 1 !! ATR low vis mode on.
    integer, parameter, public :: GEOCOM_BAP_ATRSET_LOWVIS_AON = 2 !! ATR low vis mode always on.
    integer, parameter, public :: GEOCOM_BAP_ATRSET_SRANGE_ON  = 3 !! ATR high reflectivity mode on.
    integer, parameter, public :: GEOCOM_BAP_ATRSET_SRANGE_AON = 4 !! ATR high reflectivity mode always on.

    ! BAP_PRISMDEF: Prism definition.
    integer, parameter, public :: GEOCOM_BAP_PRISMNAME_LEN = 16 !! Prism name string length.

    ! **************************************************************************
    ! BMM - BASIC MAN-MACHINE INTERFACE.
    ! **************************************************************************
    integer, parameter, public :: GEOCOM_IOS_BEEP_STDINTENS = 100 !! Standard intensity of beep expressed as percentage.

    ! **************************************************************************
    ! COM - COMMUNICATION SETTINGS.
    ! **************************************************************************
    ! COM_FORMAT: Transmission data format.
    integer, parameter, public :: GEOCOM_COM_ASCII  = 0 !! ASCII protocol.
    integer, parameter, public :: GEOCOM_COM_BINARY = 1 !! Binary protocol.

    ! COM_BAUD_RATE: Baud rate.
    integer, parameter, public :: GEOCOM_COM_BAUD_38400  = 0
    integer, parameter, public :: GEOCOM_COM_BAUD_19200  = 1 !! Default baud rate.
    integer, parameter, public :: GEOCOM_COM_BAUD_9600   = 2
    integer, parameter, public :: GEOCOM_COM_BAUD_4800   = 3
    integer, parameter, public :: GEOCOM_COM_BAUD_2400   = 4
    integer, parameter, public :: GEOCOM_COM_BAUD_115200 = 5
    integer, parameter, public :: GEOCOM_COM_BAUD_57600  = 6

    ! COM_TPS_STOP_MODE: Stop mode.
    integer, parameter, public :: GEOCOM_COM_STOP_SHUT_DOWN = 0 !! Power down instrument.
    integer, parameter, public :: GEOCOM_COM_STOP_SLEEP     = 1 !! Not supported by TPS1200.

    ! COM_TPS_STARTUP_MODE: Start mode.
    integer, parameter, public :: GEOCOM_COM_STARTUP_LOCAL  = 0 !! Not supported by TPS1200.
    integer, parameter, public :: GEOCOM_COM_STARTUP_REMOTE = 1 !! RPC is enabled (online mode).

    ! **************************************************************************
    ! CSV - CENTRAL SERVICES.
    ! **************************************************************************
    ! CSV_POWER_PATH: Power sources.
    integer, parameter, public :: GEOCOM_CSV_EXTERNAL_POWER = 1 !! Power source is external.
    integer, parameter, public :: GEOCOM_CSV_INTERNAL_POWER = 2 !! Power source is the internal battery.

    ! TPS_DEVICE_CLASS: TPS device precision class.
    integer, parameter, public :: GEOCOM_TPS_CLASS_1100 = 0   !! TPS1000 family member, 1 mgon, 3 ".
    integer, parameter, public :: GEOCOM_TPS_CLASS_1700 = 1   !! TPS1000 family member, 0.5 mgon, 1.5 ".
    integer, parameter, public :: GEOCOM_TPS_CLASS_1800 = 2   !! TPS1000 family member, 0.3 mgon, 1 ".
    integer, parameter, public :: GEOCOM_TPS_CLASS_5000 = 3   !! TPS2000 family member.
    integer, parameter, public :: GEOCOM_TPS_CLASS_6000 = 4   !! TPS2000 family member.
    integer, parameter, public :: GEOCOM_TPS_CLASS_1500 = 5   !! TPS1000 family member.
    integer, parameter, public :: GEOCOM_TPS_CLASS_2003 = 6   !! TPS2000 family member.
    integer, parameter, public :: GEOCOM_TPS_CLASS_5005 = 7   !! TPS5000 family member.
    integer, parameter, public :: GEOCOM_TPS_CLASS_5100 = 8   !! TPS5000 family member.
    integer, parameter, public :: GEOCOM_TPS_CLASS_1102 = 100 !! TPS1100 family member, 2 ".
    integer, parameter, public :: GEOCOM_TPS_CLASS_1103 = 101 !! TPS1100 family member, 3 ".
    integer, parameter, public :: GEOCOM_TPS_CLASS_1105 = 102 !! TPS1100 family member, 5 ".
    integer, parameter, public :: GEOCOM_TPS_CLASS_1101 = 103 !! TPS1100 family member, 1 ".
    integer, parameter, public :: GEOCOM_TPS_CLASS_1202 = 200 !! TPS1200 family member, 2 ".
    integer, parameter, public :: GEOCOM_TPS_CLASS_1203 = 201 !! TPS1200 family member, 3 ".
    integer, parameter, public :: GEOCOM_TPS_CLASS_1205 = 202 !! TPS1200 family member, 5 ".
    integer, parameter, public :: GEOCOM_TPS_CLASS_1201 = 203 !! TPS1200 family member, 1 ".
    integer, parameter, public :: GEOCOM_TPS_CLASS_TX30 = 300 !! TS30,TM30 family member, 0.5 ".
    integer, parameter, public :: GEOCOM_TPS_CLASS_TX31 = 301 !! TS30,TM30 family member, 1 ".

    ! TPS_DEVICE_TYPE: TPS device configuration type.
    ! -- TPS1x00 common.
    integer, parameter, public :: GEOCOM_TPS_DEVICE_T      = int(z'00000') !! Theodolite without built-in EDM.
    integer, parameter, public :: GEOCOM_TPS_DEVICE_MOT    = int(z'00004') !! Motorized device.
    integer, parameter, public :: GEOCOM_TPS_DEVICE_ATR    = int(z'00008') !! Automatic Target Recognition.
    integer, parameter, public :: GEOCOM_TPS_DEVICE_EGL    = int(z'00010') !! Electronic Guide Light.
    integer, parameter, public :: GEOCOM_TPS_DEVICE_DB     = int(z'00020') !! Reserved (Database, not GSI).
    integer, parameter, public :: GEOCOM_TPS_DEVICE_DL     = int(z'00040') !! Diode laser.
    integer, parameter, public :: GEOCOM_TPS_DEVICE_LP     = int(z'00080') !! Laser plumbed.
    ! -- TPS1000 specific.
    integer, parameter, public :: GEOCOM_TPS_DEVICE_TC1    = int(z'00001') !! Tachymeter (TCW1).
    integer, parameter, public :: GEOCOM_TPS_DEVICE_TC2    = int(z'00002') !! Tachymeter (TCW2).
    ! -- TPS1100/TPS1200 specific.
    integer, parameter, public :: GEOCOM_TPS_DEVICE_TC     = int(z'00001') !! Tachymeter (TCW3).
    integer, parameter, public :: GEOCOM_TPS_DEVICE_TCR    = int(z'00002') !! Tachymeter (TCW3 with red laser).
    integer, parameter, public :: GEOCOM_TPS_DEVICE_ATC    = int(z'00100') !! Autocollimation lamp (used only PMU).
    integer, parameter, public :: GEOCOM_TPS_DEVICE_LPNT   = int(z'00200') !! Laserpointer.
    integer, parameter, public :: GEOCOM_TPS_DEVICE_RL_EXT = int(z'00400') !! Reflectorless EDM with extended range (Pinpoint R100, R300).
    integer, parameter, public :: GEOCOM_TPS_DEVICE_PS     = int(z'00800') !! Power Search.
    ! -- TPSSim specific.
    integer, parameter, public :: GEOCOM_TPS_DEVICE_SIM    = int(z'04000') !! Runs on simulation, no hardware.

    ! TPS_REFLESS_CLASS: Reflectorless class.
    integer, parameter, public :: GEOCOM_TPS_REFLESS_NONE  = 0
    integer, parameter, public :: GEOCOM_TPS_REFLESS_R100  = 1 !! Pinpoint R100.
    integer, parameter, public :: GEOCOM_TPS_REFLESS_R300  = 2 !! Pinpoint R300.
    integer, parameter, public :: GEOCOM_TPS_REFLESS_R400  = 3 !! Pinpoint R400.
    integer, parameter, public :: GEOCOM_TPS_REFLESS_R1000 = 4 !! Pinpoint R1000.

    ! **************************************************************************
    ! EDM - ELECTRONIC DISTANCE MEASUREMENT.
    ! **************************************************************************
    ! EDM_EGLINTENSITY_TYPE: Intensity of Electronic Guidelight.
    integer, parameter, public :: GEOCOM_EDM_EGLINTEN_OFF  = 0
    integer, parameter, public :: GEOCOM_EDM_EGLINTEN_LOW  = 1
    integer, parameter, public :: GEOCOM_EDM_EGLINTEN_MID  = 2
    integer, parameter, public :: GEOCOM_EDM_EGLINTEN_HIGH = 3

    ! EDM_MODE: EDM measurement mode.
    integer, parameter, public :: GEOCOM_EDM_MODE_NOT_USED   = 0  !! Init value.
    integer, parameter, public :: GEOCOM_EDM_SINGLE_TAPE     = 1  !! IR Standard Reflector Tape.
    integer, parameter, public :: GEOCOM_EDM_SINGLE_STANDARD = 2  !! IR Standard.
    integer, parameter, public :: GEOCOM_EDM_SINGLE_FAST     = 3  !! IR Fast.
    integer, parameter, public :: GEOCOM_EDM_SINGLE_LRANGE   = 4  !! LO Standard.
    integer, parameter, public :: GEOCOM_EDM_SINGLE_SRANGE   = 5  !! RL Standard.
    integer, parameter, public :: GEOCOM_EDM_CONT_STANDARD   = 6  !! Standard repeated measurement.
    integer, parameter, public :: GEOCOM_EDM_CONT_DYNAMIC    = 7  !! IR Tacking.
    integer, parameter, public :: GEOCOM_EDM_CONT_REFLESS    = 8  !! RL Tracking.
    integer, parameter, public :: GEOCOM_EDM_CONT_FAST       = 9  !! Fast repeated measurement.
    integer, parameter, public :: GEOCOM_EDM_AVERAGE_IR      = 10 !! IR Average.
    integer, parameter, public :: GEOCOM_EDM_AVERAGE_SR      = 11 !! RL Average.
    integer, parameter, public :: GEOCOM_EDM_AVERAGE_LR      = 12 !! LO Average.
    integer, parameter, public :: GEOCOM_EDM_PRECISE_IR      = 13 !! IR Precise (TM30, TS30).
    integer, parameter, public :: GEOCOM_EDM_PRECISE_TAPE    = 14 !! IR Precise Reflector Tape (TM30, TS30). 

    ! **************************************************************************
    ! FTR - FILE TRANSFER.
    ! **************************************************************************
    integer, parameter, public :: GEOCOM_FTR_MAX_BLOCKSIZE = 450 !! Max. block size.

    ! FTR_DEVICETYPE: Device type.
    integer, parameter, public :: GEOCOM_FTR_DEVICE_INTERNAL = 0 !! Internal memory.
    integer, parameter, public :: GEOCOM_FTR_DEVICE_PCPARD   = 1 !! Memory card.

    ! FTR_FILETYPE: File type.
    integer, parameter, public :: GEOCOM_FTR_FILE_IMAGES = 170

    ! **************************************************************************
    ! IMG - IMAGE PROCESSING.
    ! **************************************************************************
    ! IMG_MEM_TYPE: Memory device type.
    integer, parameter, public :: GEOCOM_IMG_INTERNAL_MEMORY = int(z'0') !! Internal memory module.
    integer, parameter, public :: GEOCOM_IMG_PC_CARD         = int(z'1') !! External PC Card.

    integer, parameter, public :: GEOCOM_IMG_MAX_FILE_PREFIX_LEN = 20 !! Length of file name prefix.

    ! **************************************************************************
    ! MOT - MOTORISATION.
    ! **************************************************************************
    ! MOT_LOCK_STATUS: Lock conditions.
    integer, parameter, public :: GEOCOM_MOT_LOCKED_OUT = 0 !! Locked out.
    integer, parameter, public :: GEOCOM_MOT_LOCKED_IN  = 1 !! Locked in.
    integer, parameter, public :: GEOCOM_MOT_PREDICTION = 2 !! Prediction mode.

    ! MOT_STOPMODE: Controller stop mode.
    integer, parameter, public :: GEOCOM_MOT_NORMAL     = 0 !! Slow down with current acceleration.
    integer, parameter, public :: GEOCOM_MOT_SHUTDOWN   = 1 !! Slow down by switch off power supply.

    ! MOT_MODE: Controller configuration.
    integer, parameter, public :: GEOCOM_MOT_POSIT      = 0 !! Configured for relative positioning.
    integer, parameter, public :: GEOCOM_MOT_OCONST     = 1 !! Configured for constant speed.
    integer, parameter, public :: GEOCOM_MOT_MANUPOS    = 2 !! Configured for manual positioning (default setting).
    integer, parameter, public :: GEOCOM_MOT_LOCK       = 3 !! Configured as "Lock-In" controller.
    integer, parameter, public :: GEOCOM_MOT_BREAK      = 4 !! Configured as "Brake" controller.
    integer, parameter, public :: GEOCOM_MOT_TERM       = 7 !! Terminates the controller task.

    ! **************************************************************************
    ! TMC - THEODOLITE MEASUREMENT AND CALCULATION.
    ! **************************************************************************
    ! TMC_INCLINE_PRG: Inclination sensor measurement program.
    integer, parameter, public :: GEOCOM_TMC_MEA_INC         = 0  !! Use sensor (a priori sigma).
    integer, parameter, public :: GEOCOM_TMC_AUTO_INC        = 1  !! Automatic mode (sensor/plane).
    integer, parameter, public :: GEOCOM_TMC_PLANE_INC       = 2  !! Use plane (a priori sigma).

    ! TMC_MEASURE_PRG: TMC measurement mode.
    integer, parameter, public :: GEOCOM_TMC_STOP            = 0  !! Stop measurement program
    integer, parameter, public :: GEOCOM_TMC_DEF_DIST        = 1  !! Default DIST-measurement program.
    integer, parameter, public :: GEOCOM_TMC_CLEAR           = 3  !! TMC_STOP and clear data.
    integer, parameter, public :: GEOCOM_TMC_SIGNAL          = 4  !! Signal measurement (test function).
    integer, parameter, public :: GEOCOM_TMC_DO_MEASURE      = 6  !! (Re-)start measurement task.
    integer, parameter, public :: GEOCOM_TMC_RTRK_DIST       = 8  !! Distance-TRK measurement program.
    integer, parameter, public :: GEOCOM_TMC_RED_TRK_DIST    = 10 !! Reflectorless tracking.
    integer, parameter, public :: GEOCOM_TMC_FREQUENCY       = 11 !! Frequency measurement (test).

    ! TMC_FACE_DEF: Face position.
    integer, parameter, public :: GEOCOM_TMC_FACE_NORMAL = 0 !! Face in normal position.
    integer, parameter, public :: GEOCOM_TMC_FACE_TURN   = 1 !! Face turned.

    ! TMC_FACE: Actual face.
    integer, parameter, public :: GEOCOM_TMC_FACE_1 = 0 !! Position 1 of telescope.
    integer, parameter, public :: GEOCOM_TMC_FACE_2 = 1 !! Position 2 of telescope.

    ! **************************************************************************
    ! SUP - SUPERVISOR.
    ! **************************************************************************
    ! SUP_AUTO_POWER: Automatic shutdown mechanism for the system.
    integer, parameter, public :: GEOCOM_SUP_POWER_DISABLED = 0 !! Instrument remains on.
    integer, parameter, public :: GEOCOM_SUP_POWER_OFF      = 2 !! Turns off mechanism.

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
    public :: dm_geocom_api_request_get_angular_correction_status
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
    public :: dm_geocom_api_request_get_geometric_ppm
    public :: dm_geocom_api_request_get_height
    public :: dm_geocom_api_request_get_image_config
    public :: dm_geocom_api_request_get_incline_correction
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
    ! public :: dm_geocom_api_request_get_simple_coordinates
    ! public :: dm_geocom_api_request_get_simple_meta
    ! public :: dm_geocom_api_request_get_slope_distance_correction
    ! public :: dm_geocom_api_request_get_software_version
    ! public :: dm_geocom_api_request_get_software_version2
    ! public :: dm_geocom_api_request_get_station
    ! public :: dm_geocom_api_request_get_target_type
    ! public :: dm_geocom_api_request_get_timeout
    ! public :: dm_geocom_api_request_get_tolerance
    ! public :: dm_geocom_api_request_get_user_atr_mode
    ! public :: dm_geocom_api_request_get_user_local_mode
    ! public :: dm_geocom_api_request_get_user_prism_definition
    ! public :: dm_geocom_api_request_get_user_spiral
    ! public :: dm_geocom_api_request_list
    ! public :: dm_geocom_api_request_lock_in
    ! public :: dm_geocom_api_request_measure_distance_angle
    public :: dm_geocom_api_request_null
    ! public :: dm_geocom_api_request_ps_enable_range
    ! public :: dm_geocom_api_request_ps_search_next
    ! public :: dm_geocom_api_request_ps_search_window
    ! public :: dm_geocom_api_request_ps_set_range
    ! public :: dm_geocom_api_request_search
    ! public :: dm_geocom_api_request_search_target
    ! public :: dm_geocom_api_request_set_angle_correction
    ! public :: dm_geocom_api_request_set_atmospheric_correction
    ! public :: dm_geocom_api_request_set_atmospheric_ppm
    ! public :: dm_geocom_api_request_set_atr_setting
    ! public :: dm_geocom_api_request_set_binary_mode
    ! public :: dm_geocom_api_request_set_config
    ! public :: dm_geocom_api_request_set_date_time
    ! public :: dm_geocom_api_request_set_double_precision
    ! public :: dm_geocom_api_request_set_edm_mode
    ! public :: dm_geocom_api_request_set_egl_intensity
    ! public :: dm_geocom_api_request_set_fine_adjust_mode
    ! public :: dm_geocom_api_request_set_geometric_ppm
    ! public :: dm_geocom_api_request_set_height
    ! public :: dm_geocom_api_request_set_image_config
    ! public :: dm_geocom_api_request_set_incline_correction
    ! public :: dm_geocom_api_request_set_laser_pointer
    ! public :: dm_geocom_api_request_set_measurement_program
    ! public :: dm_geocom_api_request_set_offset
    ! public :: dm_geocom_api_request_set_orientation
    public :: dm_geocom_api_request_set_position
    ! public :: dm_geocom_api_request_set_prism_constant
    ! public :: dm_geocom_api_request_set_prism_type
    ! public :: dm_geocom_api_request_set_prism_type_v2
    ! public :: dm_geocom_api_request_set_reduced_atr_fov
    ! public :: dm_geocom_api_request_set_prism_constant
    ! public :: dm_geocom_api_request_set_refraction_mode
    ! public :: dm_geocom_api_request_set_search_area
    ! public :: dm_geocom_api_request_set_station
    ! public :: dm_geocom_api_request_set_target_type
    ! public :: dm_geocom_api_request_set_timeout
    ! public :: dm_geocom_api_request_set_tolerance
    ! public :: dm_geocom_api_request_set_user_atr_mode
    ! public :: dm_geocom_api_request_set_user_local_mode
    ! public :: dm_geocom_api_request_set_user_prism_definition
    ! public :: dm_geocom_api_request_set_user_spiral
    ! public :: dm_geocom_api_request_set_velocity
    ! public :: dm_geocom_api_request_setup_download
    ! public :: dm_geocom_api_request_setup_list
    ! public :: dm_geocom_api_request_start_controller
    ! public :: dm_geocom_api_request_stop_controller
    public :: dm_geocom_api_request_switch_off
    public :: dm_geocom_api_request_switch_on
    public :: dm_geocom_api_request_take_image
contains
    ! **************************************************************************
    ! PUBLIC PROCEDURES.
    ! **************************************************************************
    pure subroutine dm_geocom_api_request(request, code, arguments, pattern, responses, mode)
        !! Prepares a DMPACK request type by setting request command, response
        !! pattern, response delimiter, and response definition array.
        type(request_type),  intent(out)          :: request      !! Prepared request type.
        integer,             intent(in)           :: code         !! GeoCOM request code.
        character(len=*),    intent(in), optional :: arguments    !! GeoCOM request arguments.
        character(len=*),    intent(in), optional :: pattern      !! Regular expression pattern that matches the raw response.
        type(response_type), intent(in), optional :: responses(:) !! Array of response types.
        integer,             intent(in), optional :: mode         !! Request mode.

        integer :: n

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
    ! GEOCOM REQUEST PREPARATION PROCEDURES.
    ! **************************************************************************
    pure subroutine dm_geocom_api_request_abort_download(request)
        !! Request of `FTR_AbortDownload` procedure. Creates request to abort
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
        integer, parameter :: REQCODE = 23305

        type(request_type), intent(out) :: request !! Prepared request.

        call dm_geocom_api_request(request, REQCODE, pattern=GEOCOM_GRC_PATTERN, responses=GEOCOM_GRC_RESPONSES)
    end subroutine dm_geocom_api_request_abort_download

    pure subroutine dm_geocom_api_request_abort_list(request)
        !! Request of `FTR_AbortList` procedure. Creates request to aborts or
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
        integer, parameter :: REQCODE = 23308

        type(request_type), intent(out) :: request !! Prepared request.

        call dm_geocom_api_request(request, REQCODE, pattern=GEOCOM_GRC_PATTERN, responses=GEOCOM_GRC_RESPONSES)
    end subroutine dm_geocom_api_request_abort_list

    pure subroutine dm_geocom_api_request_beep_alarm(request)
        !! Request of `BMM_BeepAlarm` procedure. Creates request to output an
        !! alarm signal (triple beep).
        !!
        !! The instrument returns the following responses:
        !!
        !! * `grc` – GeoCOM return code.
        !!
        !! | Property       | Values                                           |
        !! |----------------|--------------------------------------------------|
        !! | Instruments    | TPS1200, TM30/TS30, TS16                         |
        !! | ASCII request  | `%R1Q,11004:`                                    |
        !! | ASCII response | `%R1P,0,0:<grc>`                                 |
        !!
        integer, parameter :: REQCODE = 11004

        type(request_type), intent(out) :: request !! Prepared request.

        call dm_geocom_api_request(request, REQCODE, pattern=GEOCOM_GRC_PATTERN, responses=GEOCOM_GRC_RESPONSES)
    end subroutine dm_geocom_api_request_beep_alarm

    pure subroutine dm_geocom_api_request_beep_normal(request)
        !! Request of `BMM_BeepNormal` procedure. Creates request to output an
        !! alarm signal (single beep).
        !!
        !! The instrument returns the following responses:
        !!
        !! * `grc` – GeoCOM return code.
        !!
        !! | Property       | Values                                           |
        !! |----------------|--------------------------------------------------|
        !! | Instruments    | TPS1200, TM30/TS30, TS16                         |
        !! | ASCII request  | `%R1Q,11003:`                                    |
        !! | ASCII response | `%R1P,0,0:<grc>`                                 |
        !!
        integer, parameter :: REQCODE = 11003

        type(request_type), intent(out) :: request !! Prepared request.

        call dm_geocom_api_request(request, REQCODE, pattern=GEOCOM_GRC_PATTERN, responses=GEOCOM_GRC_RESPONSES)
    end subroutine dm_geocom_api_request_beep_normal

    pure subroutine dm_geocom_api_request_beep_off(request)
        !! Request of `IOS_BeepOff` procedure. Creates request to stop an
        !! active beep signal.
        !!
        !! The instrument returns the following responses:
        !!
        !! * `grc` – GeoCOM return code.
        !!
        !! | Property       | Values                                           |
        !! |----------------|--------------------------------------------------|
        !! | Instruments    | TPS1200, TM30/TS30, TS16                         |
        !! | ASCII request  | `%R1Q,20000:`                                    |
        !! | ASCII response | `%R1P,0,0:<grc>`                                 |
        !!
        integer, parameter :: REQCODE = 20000

        type(request_type), intent(out) :: request !! Prepared request.

        call dm_geocom_api_request(request, REQCODE, pattern=GEOCOM_GRC_PATTERN, responses=GEOCOM_GRC_RESPONSES)
    end subroutine dm_geocom_api_request_beep_off

    pure subroutine dm_geocom_api_request_beep_on(request, intensity)
        !! Request of `IOS_BeepOn` procedure. Creates request for continuous
        !! beep signal of given intensity.
        !!
        !! The instrument returns the following responses:
        !!
        !! * `grc` – GeoCOM return code.
        !!
        !! | Property       | Values                                           |
        !! |----------------|--------------------------------------------------|
        !! | Instruments    | TPS1200, TM30/TS30, TS16                         |
        !! | ASCII request  | `%R1Q,20001:<intensity>`                         |
        !! | ASCII response | `%R1P,0,0:<grc>`                                 |
        !!
        integer, parameter :: REQCODE = 20001

        type(request_type), intent(out) :: request   !! Prepared request.
        integer,            intent(in)  :: intensity !! Intensity of beep, from 0 to 100.

        character(len=80) :: args

        write (args, '(i0)') intensity
        call dm_geocom_api_request(request, REQCODE, args, GEOCOM_GRC_PATTERN, GEOCOM_GRC_RESPONSES)
    end subroutine dm_geocom_api_request_beep_on

    pure subroutine dm_geocom_api_request_change_face(request, pos_mode, atr_mode)
        !! Request of `AUT_ChangeFace` procedure. Creates request for turning
        !! the telescope to the other face.
        !!
        !! If `pos_mode` is `GEOCOM_AUT_NORMAL`, uses the current value of the
        !! compensator. For positioning distances > 25 gon, this mode might tend
        !! to inaccuracy. If set to `GEOCOM_AUT_PRECISE`, tries to measure the
        !! exact inclination of the target. Tends to long position time.
        !!
        !! If `atr_mode` is `GEOCOM_AUT_POSITION`, uses conventional position
        !! to other face. If set to `GEOCOM_AUT_TARGET`, tries to position into
        !! a target in the destination area. This mode requires activated ATR.
        !!
        !! The instrument returns the following responses:
        !!
        !! * `grc` – GeoCOM return code.
        !!
        !! | Property       | Values                                           |
        !! |----------------|--------------------------------------------------|
        !! | Instruments    | TPS1200, TM30/TS30, TS16                         |
        !! | ASCII request  | `%R1Q,9028:<pos_mode>,<atr_mode>,0`              |
        !! | ASCII response | `%R1P,0,0:<grc>`                                 |
        !!
        integer, parameter :: REQCODE = 9028

        type(request_type), intent(out) :: request  !! Prepared request.
        integer,            intent(in)  :: pos_mode !! Position mode (`AUT_POSMODE`).
        integer,            intent(in)  :: atr_mode !! ATR mode (`AUT_ATRMODE`).

        character(len=80) :: args

        write (args, '(i0, ",", i0, ",0")') pos_mode, atr_mode
        call dm_geocom_api_request(request, REQCODE, args, GEOCOM_GRC_PATTERN, GEOCOM_GRC_RESPONSES)
    end subroutine dm_geocom_api_request_change_face

    pure subroutine dm_geocom_api_request_delete(request, device_type, file_type, day, month, year, file_name)
        !! Request of `FTR_Delete` procedure. Creates request for deleting one
        !! or more files.
        !!
        !! Wildcards may be used to delete multiple files. If the deletion date
        !! is valid, only files older than the deletion date are deleted.
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
        integer,          parameter :: REQCODE = 23309
        character(len=*), parameter :: PATTERN = '(?<grc>\d+),(?<nfiles>\d+)'

        type(request_type),     intent(out) :: request     !! Prepared request.
        integer,                intent(in)  :: device_type !! Internal memory or memory card (`FTR_DEVICETYPE`).
        integer,                intent(in)  :: file_type   !! Type of file (`FTR_FILETYPE`).
        integer,                intent(in)  :: day         !! Day.
        integer,                intent(in)  :: month       !! Month.
        integer,                intent(in)  :: year        !! Year.
        character(len=*),       intent(in)  :: file_name   !! Name of file to delete.

        character(len=80)   :: args
        type(response_type) :: responses(2)

        write (args, '(5(i0, ","), a)') device_type, file_type, day, month, year, file_name

        responses = [ &
            response_type('grc',    type=RESPONSE_TYPE_INT32), &
            response_type('nfiles', type=RESPONSE_TYPE_INT32)  &
        ]

        call dm_geocom_api_request(request, REQCODE, args, PATTERN, responses)
    end subroutine dm_geocom_api_request_delete

    pure subroutine dm_geocom_api_request_do_measure(request, prog, mode)
        !! Request of `TMC_DoMeasure` procedure. Creates request for trying a
        !! distance measurement. This command does not return any values.
        !!
        !! The argument `prog` may be one of the following TMC measurement
        !! modes:
        !!
        !! * `GEOCOM_TMC_STOP
        !! * `GEOCOM_TMC_DEF_DIST`
        !! * `GEOCOM_TMC_CLEAR`
        !! * `GEOCOM_TMC_SIGNAL`
        !! * `GEOCOM_TMC_DO_MEASURE`
        !! * `GEOCOM_TMC_RTRK_DIST`
        !! * `GEOCOM_TMC_RED_TRK_DIST`
        !! * `GEOCOM_TMC_FREQUENCY`
        !!
        !! The argument `mode` may be one of the following inclination sensor
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
        !! | Instruments    | TPS1200, TM30/TS30, TS16                         |
        !! | ASCII request  | `%R1Q,2008:<prog>,<mode>`                         |
        !! | ASCII response | `%R1P,0,0:<grc>`                                 |
        !!
        integer, parameter :: REQCODE = 2008

        type(request_type), intent(out) :: request !! Prepared request.
        integer,            intent(in)  :: prog    !! TMC measurement program (`TMC_MEASURE_PRG`).
        integer,            intent(in)  :: mode    !! Inclination sensor measurement mode (`TMC_INCLINE_PRG`).

        character(len=80) :: args

        write (args, '(i0, ",", i0)') prog, mode
        call dm_geocom_api_request(request, REQCODE, args, GEOCOM_GRC_PATTERN, GEOCOM_GRC_RESPONSES)
    end subroutine dm_geocom_api_request_do_measure

    pure subroutine dm_geocom_api_request_download(request, block_number)
        !! Request of `FTR_Download` procedure. Creates request to get a
        !! single block of data. The `FTR_SetupDownload` command has to be
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
        integer,          parameter :: REQCODE = 23304
        character(len=*), parameter :: PATTERN = '(?<grc>\d+),(?<blockval>\d+),(?<blocklen>\d+)'
        integer,          parameter :: MODE    = REQUEST_MODE_GEOCOM_FILE

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

        call dm_geocom_api_request(request, REQCODE, args, PATTERN, responses, mode=MODE)
    end subroutine dm_geocom_api_request_download

    pure subroutine dm_geocom_api_request_fine_adjust(request, search_hz, search_v)
        !! Request of `AUT_FineAdjust` procedure. Creates request for
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
        !! procedure call. After positioning, the lock mode will be active. The
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
        !! | Instruments    | TPS1200, TM30/TS30, TS16                         |
        !! | ASCII request  | `%R1Q,9037:<search_hz>,<search_v>`               |
        !! | ASCII response | `%R1P,0,0:<grc>`                                 |
        !!
        integer, parameter :: REQCODE = 9027

        type(request_type), intent(out) :: request   !! Prepared request.
        real(kind=r8),      intent(in)  :: search_hz !! Search range, Hz axis [rad].
        real(kind=r8),      intent(in)  :: search_v  !! Search range, V axis [rad].

        character(len=80) :: args

        write (args, '(2(f0.12, ","), "0")') search_hz, search_v
        call dm_geocom_api_request(request, REQCODE, args, GEOCOM_GRC_PATTERN, GEOCOM_GRC_RESPONSES)
    end subroutine dm_geocom_api_request_fine_adjust

    pure subroutine dm_geocom_api_request_get_angle(request, mode)
        !! Request of `TMC_GetAngle5` procedure. Creates request for returning
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
        !! | Instruments    | TPS1200, TM30/TS30, TS16                         |
        !! | ASCII request  | `%R1Q,2107:<mode>`                               |
        !! | ASCII response | `%R1P,0,0:<grc>,<hz>,<v>`                        |
        !!
        integer,          parameter :: REQCODE = 2107
        character(len=*), parameter :: PATTERN = '(?<grc>\d+),(?<hz>[\d\.]+),(?<v>[\d\.]+)'

        type(request_type), intent(out) :: request !! Prepared request.
        integer,            intent(in)  :: mode    !! Inclination sensor measurement mode (`TMC_INCLINE_PRG`).

        character(len=80)   :: args
        type(response_type) :: responses(3)

        write (args, '(i0)') mode

        responses = [ &
            response_type('grc', unit=' ',   type=RESPONSE_TYPE_INT32),  & ! GeoCOM return code.
            response_type('hz',  unit='rad', type=RESPONSE_TYPE_REAL64), & ! Horizontal angle [rad].
            response_type('v',   unit='rad', type=RESPONSE_TYPE_REAL64)  & ! Vertical angle [rad].
        ]

        call dm_geocom_api_request(request, REQCODE, args, PATTERN, responses)
    end subroutine dm_geocom_api_request_get_angle

    pure subroutine dm_geocom_api_request_get_angle_complete(request, mode)
        !! Request of `TMC_GetAngle1` procedure. Creates request for returning
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
        !! * `angtime` – Moment of measurement [ms].
        !! * `xinc`    – Transverse axis inclination [rad].
        !! * `linc`    – Longitude axis inclidation [rad].
        !! * `incacc`  – Inclination accuracy [rad].
        !! * `inctime` – Moment of measurement [ms].
        !! * `face`    – Face position of telescope.
        !!
        !! | Property       | Values                                                                               |
        !! |----------------|--------------------------------------------------------------------------------------|
        !! | Instruments    | TPS1200, TM30/TS30, TS16                                                             |
        !! | ASCII request  | `%R1Q,2003:<mode>`                                                                   |
        !! | ASCII response | `%R1P,0,0:<grc>,<hz>,<v>,<angacc>,<angtime>,<xinc>,<linc>,<incacc>,<inctime>,<face>` |
        !!
        integer,          parameter :: REQCODE = 2003
        character(len=*), parameter :: PATTERN = &
            '(?<grc>\d+),(?<hz>[-\d\.]+),(?<v>[-\d\.]+),(?<angacc>[-\d\.]+),(?<angtime>\d+),' // &
            '(?<xinc>[-\d\.]+),(?<linc>[-\d\.]+),(?<incacc>[-\d\.]+),(?<inctime>\d+),(?<face>\d+)'

        type(request_type), intent(out) :: request !! Prepared request.
        integer,            intent(in)  :: mode    !! Inclination sensor measurement mode (`TMC_INCLINE_PRG`).

        character(len=80)   :: args
        type(response_type) :: responses(10)

        write (args, '(i0)') mode

        responses = [ &
            response_type('grc',     unit=' ',   type=RESPONSE_TYPE_INT32),  & ! GeoCOM return code.
            response_type('hz',      unit='rad', type=RESPONSE_TYPE_REAL64), & ! Horizontal angle [rad].
            response_type('v',       unit='rad', type=RESPONSE_TYPE_REAL64), & ! Vertical angle [rad].
            response_type('angacc',  unit='rad', type=RESPONSE_TYPE_REAL64), & ! Accuracy of angles [rad].
            response_type('angtime', unit='ms',  type=RESPONSE_TYPE_INT64),  & ! Moment of measurement [ms].
            response_type('xinc',    unit='rad', type=RESPONSE_TYPE_REAL64), & ! Transverse axis inclination [rad].
            response_type('linc',    unit='rad', type=RESPONSE_TYPE_REAL64), & ! Longitude axis inclidation [rad].
            response_type('incacc',  unit='rad', type=RESPONSE_TYPE_REAL64), & ! Inclination accuracy [rad].
            response_type('inctime', unit='ms',  type=RESPONSE_TYPE_INT64),  & ! Moment of measurement [ms].
            response_type('face',    unit=' ',   type=RESPONSE_TYPE_INT32)   & ! Face position of telescope.
        ]

        call dm_geocom_api_request(request, REQCODE, args, PATTERN, responses)
    end subroutine dm_geocom_api_request_get_angle_complete

    pure subroutine dm_geocom_api_request_get_angular_correction_status(request)
        !! Request of `TMC_GetAngSwitch` procedure. Creates request for
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
        !! | Instruments    | TPS1200, TM30/TS30, TS16                             |
        !! | ASCII request  | `%R1Q,2014:`                                         |
        !! | ASCII response | `%R1P,0,0:<grc>,<inccor>,<stdcor>,<colcor>,<tilcor>` |
        !!
        integer,          parameter :: REQCODE = 2014
        character(len=*), parameter :: PATTERN = &
            '(?<grc>\d+),(?<inccor>\d+),(?<stdcor>\d+),(?<colcor>\d+),(?<tilcor>\d+)'

        type(request_type), intent(out) :: request !! Prepared request.
        type(response_type)             :: responses(5)

        responses = [ &
            response_type('grc',    type=RESPONSE_TYPE_INT32),   & ! GeoCOM return code.
            response_type('inccor', type=RESPONSE_TYPE_LOGICAL), & ! Inclination correction on/off [bool].
            response_type('stdcor', type=RESPONSE_TYPE_LOGICAL), & ! Standing axis correction on/off [bool].
            response_type('colcor', type=RESPONSE_TYPE_LOGICAL), & ! Collimation error correction on/off [bool].
            response_type('tilcor', type=RESPONSE_TYPE_LOGICAL)  & ! Tilting axis correction on/off [bool].
        ]

        call dm_geocom_api_request(request, REQCODE, pattern=PATTERN, responses=responses)
    end subroutine dm_geocom_api_request_get_angular_correction_status

    pure subroutine dm_geocom_api_request_get_atmospheric_correction(request)
        !! Request of `TMC_GetAtmCorr` procedure. Creates request for getting
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
        !! | Instruments    | TPS1200, TM30/TS30, TS16                                 |
        !! | ASCII request  | `%R1Q,2029:`                                             |
        !! | ASCII response | `%R1P,0,0:<grc>,<lambda>,<pressure>,<drytemp>,<wettemp>` |
        !!
        integer,          parameter :: REQCODE = 2029
        character(len=*), parameter :: PATTERN = &
            '(?<grc>\d+),(?<lambda>[-\d\.]+),(?<pressure>[-\d\.]+),(?<drytemp>[-\d\.]+),(?<wettemp>[-\d\.]+)'

        type(request_type), intent(out) :: request !! Prepared request.
        type(response_type)             :: responses(5)

        responses = [ &
            response_type('grc',      unit=' ',    type=RESPONSE_TYPE_INT32),  & ! GeoCOM return code.
            response_type('lambda',   unit='m',    type=RESPONSE_TYPE_REAL64), & ! Wave length of the EDM transmitter [m].
            response_type('pressure', unit='mbar', type=RESPONSE_TYPE_REAL64), & ! Atmospheric pressure [mbar].
            response_type('drytemp',  unit='degC', type=RESPONSE_TYPE_REAL64), & ! Dry temperature [°C].
            response_type('wettemp',  unit='degC', type=RESPONSE_TYPE_REAL64)  & ! Wet temperature [°C].
        ]

        call dm_geocom_api_request(request, REQCODE, pattern=PATTERN, responses=responses)
    end subroutine dm_geocom_api_request_get_atmospheric_correction

    pure subroutine dm_geocom_api_request_get_atmospheric_ppm(request)
        !! Request of `TMC_GetAtmPpm` procedure. Creates request for getting
        !! the atmospheric ppm correction factor.
        !!
        !! The instrument returns the following responses:
        !!
        !! * `grc`    – GeoCOM return code.
        !! * `atmppm` – Atmospheric ppm correction factor.
        !!
        !! | Property       | Values                                           |
        !! |----------------|--------------------------------------------------|
        !! | Instruments    | TPS1200, TM30/TS30, TS16                         |
        !! | ASCII request  | `%R1Q,2151:`                                     |
        !! | ASCII response | `%R1P,0,0:<grc>,<atmppm>`                        |
        !!
        integer,          parameter :: REQCODE = 2151
        character(len=*), parameter :: PATTERN = '(?<grc>\d+),(?<atmppm>[-\d\.]+)'

        type(request_type), intent(out) :: request !! Prepared request.
        type(response_type)             :: responses(2)

        responses = [ &
            response_type('grc',    unit=' ',   type=RESPONSE_TYPE_INT32), &
            response_type('atmppm', unit='ppm', type=RESPONSE_TYPE_REAL64) &
        ]

        call dm_geocom_api_request(request, REQCODE, pattern=PATTERN, responses=responses)
    end subroutine dm_geocom_api_request_get_atmospheric_ppm

    pure subroutine dm_geocom_api_request_get_atr_error(request)
        !! Request of `TMC_IfDataAzeCorrError` procedure. Creates request for
        !! getting the ATR error status.
        !!
        !! The instrument returns the following responses:
        !!
        !! * `grc`    – GeoCOM return code.
        !! * `atrerr` – ATR correction error occured [bool].
        !!
        !! | Property       | Values                                           |
        !! |----------------|--------------------------------------------------|
        !! | Instruments    | TPS1200, TM30/TS30, TS16                         |
        !! | ASCII request  | `%R1Q,2114:`                                     |
        !! | ASCII response | `%R1P,0,0:<grc>,<atrerr>`                        |
        !!
        integer,          parameter :: REQCODE = 2114
        character(len=*), parameter :: PATTERN = '(?<grc>\d+),(?<atrerr>\d+)'

        type(request_type), intent(out) :: request !! Prepared request.
        type(response_type)             :: responses(2)

        responses = [ &
            response_type('grc',    type=RESPONSE_TYPE_INT32),  &
            response_type('atrerr', type=RESPONSE_TYPE_LOGICAL) &
        ]

        call dm_geocom_api_request(request, REQCODE, pattern=PATTERN, responses=responses)
    end subroutine dm_geocom_api_request_get_atr_error

    pure subroutine dm_geocom_api_request_get_atr_setting(request)
        !! Request of `BAP_GetATRSetting` procedure. Creates request for
        !! getting the current ATR Low-Vis mode.
        !!
        !! The instrument returns the following responses:
        !!
        !! * `grc`    – GeoCOM return code.
        !! * `atrset` – ATR setting (`BAP_ATRSETTING`).
        !!
        !! | Property       | Values                                           |
        !! |----------------|--------------------------------------------------|
        !! | Instruments    | TPS1200, TM30/TS30, TS16                         |
        !! | ASCII request  | `%R1Q,17034:`                                    |
        !! | ASCII response | `%R1P,0,0:<grc>,<atrset>`                        |
        !!
        integer,          parameter :: REQCODE = 17034
        character(len=*), parameter :: PATTERN = '(?<grc>\d+),(?<atrset>\d+)'

        type(request_type), intent(out) :: request !! Prepared request.
        type(response_type)             :: responses(2)

        responses = [ &
            response_type('grc',    type=RESPONSE_TYPE_INT32), &
            response_type('atrset', type=RESPONSE_TYPE_INT32)  &
        ]

        call dm_geocom_api_request(request, REQCODE, pattern=PATTERN, responses=responses)
    end subroutine dm_geocom_api_request_get_atr_setting

    pure subroutine dm_geocom_api_request_get_binary_mode(request)
        !! Request of `COM_GetBinaryAvailable` procedure. Creates request for
        !! getting the binary attribute of the server.
        !!
        !! The instrument returns the following responses:
        !!
        !! * `grc`     – GeoCOM return code.
        !! * `binmode` – Binary operation is enabled [bool].
        !!
        !! | Property       | Values                                           |
        !! |----------------|--------------------------------------------------|
        !! | Instruments    | TPS1200, TM30/TS30, TS16                         |
        !! | ASCII request  | `%R1Q,113:`                                      |
        !! | ASCII response | `%R1P,0,0:<grc>,<binmode>`                       |
        !!
        integer,          parameter :: REQCODE = 113
        character(len=*), parameter :: PATTERN = '(?<grc>\d+),(?<binmode>\d+)'

        type(request_type), intent(out) :: request !! Prepared request.
        type(response_type)             :: responses(2)

        responses = [ &
            response_type('grc',     type=RESPONSE_TYPE_INT32),  &
            response_type('binmode', type=RESPONSE_TYPE_LOGICAL) &
        ]

        call dm_geocom_api_request(request, REQCODE, pattern=PATTERN, responses=responses)
    end subroutine dm_geocom_api_request_get_binary_mode

    pure subroutine dm_geocom_api_request_get_config(request)
        !! Request of `SUP_GetConfig` procedure. Creates request for getting
        !! the power management configuration status. The power timeout
        !! specifies the time after which the device switches into the mode
        !! indicated by `autopwr`.
        !!
        !! The instrument returns the following responses:
        !!
        !! * `grc`     – GeoCOM return code.
        !! * `autopwr` – Currently activated shut-down mode (`SUP_AUTO_POWER`).
        !! * `pwrtime` – Power timeout [ms].
        !!
        !! | Property       | Values                                           |
        !! |----------------|--------------------------------------------------|
        !! | Instruments    | TPS1200, TM30/TS30, TS16                         |
        !! | ASCII request  | `%R1Q,14001:`                                    |
        !! | ASCII response | `%R1P,0,0:<grc>,0,<autopwr>,<pwrtime>`           |
        !!
        integer,          parameter :: REQCODE = 14001
        character(len=*), parameter :: PATTERN = '(?<grc>\d+),\d+,(?<autopwr>\d+),(?<pwrtime>\d+)'

        type(request_type), intent(out) :: request !! Prepared request.
        type(response_type)             :: responses(3)

        responses = [ &
            response_type('grc',     unit=' ',  type=RESPONSE_TYPE_INT32), &
            response_type('autopwr', unit=' ',  type=RESPONSE_TYPE_INT32), &
            response_type('pwrtime', unit='ms', type=RESPONSE_TYPE_INT32)  &
        ]

        call dm_geocom_api_request(request, REQCODE, pattern=PATTERN, responses=responses)
    end subroutine dm_geocom_api_request_get_config

    pure subroutine dm_geocom_api_request_get_coordinate(request, mode, wait_time)
        !! Request of `TMC_GetCoordinate` procedure. Creates request for
        !! getting the coordinates of a measured point.
        !!
        !! This function conducts an angle and, in dependence of the selected
        !! `mode`, an inclination measurement, and the calculates the
        !! coordinates of the measured point with the last distance.
        !!
        !! The argument `wait_time` specifies the delay to wait for the
        !! distance measurement to finish. Single and tracking measurements are
        !! supported. The quality of the result is returned in the GeoCOM
        !! return code.
        !!
        !! The instrument returns the following responses:
        !!
        !! * `grc`    – GeoCOM return code.
        !! * `e`      – E coordinate [m].
        !! * `n`      – N coordinate [m]
        !! * `h`      – H coordinate [m].
        !! * `ctime`  – Timestamp of distance measurement [ms].
        !! * `econt`  – E coordinate (continuously) [m].
        !! * `ncont`  – N coordinate (continuously) [m].
        !! * `hcont`  – H coordinate (continuously) [m].
        !! * `ctimec` – Timestamp of continuous measurement [m].
        !!
        !! | Property       | Values                                                                |
        !! |----------------|-----------------------------------------------------------------------|
        !! | Instruments    | TPS1200, TM30/TS30, TS16                                              |
        !! | ASCII request  | `%R1Q,2082:<wait_time>,<mode>`                                        |
        !! | ASCII response | `%R1P,0,0:<grc>,<e>,<n>,<h>,<ctime>,<econt>,<ncont>,<hcont>,<ctimec>` |
        !!
        integer,          parameter :: REQCODE = 2082
        character(len=*), parameter :: PATTERN = &
            '(?<grc>\d+),(?<e>[-\d\.]+),(?<n>[-\d\.]+),(?<h>[-\d\.]+),(?<ctime>\d+),' // &
            '(?<econt>[-\d\.]+),(?<ncont>[-\d\.]+),(?<hcont>[-\d\.]+),(?<ctimec>\d+)'

        type(request_type), intent(out) :: request   !! Prepared request.
        integer,            intent(in)  :: mode      !! Inclination measurement mode (`TMC_INCLINE_PRG`).
        integer,            intent(in)  :: wait_time !! Delay to wait [ms].

        character(len=80)   :: args
        type(response_type) :: responses(9)

        write (args, '(i0, ",", i0)') wait_time, mode

        responses = [ &
            response_type('grc',    unit=' ',  type=RESPONSE_TYPE_INT32),  & ! GeoCOM return code.
            response_type('e',      unit='m',  type=RESPONSE_TYPE_REAL64), & ! E coordinate [m].
            response_type('n',      unit='m',  type=RESPONSE_TYPE_REAL64), & ! N coordinate [m]
            response_type('h',      unit='m',  type=RESPONSE_TYPE_REAL64), & ! H coordinate [m].
            response_type('ctime',  unit='ms', type=RESPONSE_TYPE_INT64),  & ! Timestamp of distance measurement [ms].
            response_type('econt',  unit='m',  type=RESPONSE_TYPE_REAL64), & ! E coordinate (continuously) [m].
            response_type('ncont',  unit='m',  type=RESPONSE_TYPE_REAL64), & ! N coordinate (continuously) [m].
            response_type('hcont',  unit='m',  type=RESPONSE_TYPE_REAL64), & ! H coordinate (continuously) [m].
            response_type('ctimec', unit='ms', type=RESPONSE_TYPE_INT64)   & ! Timestamp of continuous measurement [m].
        ]

        call dm_geocom_api_request(request, REQCODE, args, PATTERN, responses)
    end subroutine dm_geocom_api_request_get_coordinate

    pure subroutine dm_geocom_api_request_get_date_time(request)
        !! Request of `CSV_GetDateTime` procedure. Creates request for getting
        !! the current date and time of the instrument. A possible response may
        !! look like `%R1P,0,0:0,1996,'07','19','10','13','2f'`.
        !!
        !! The instrument returns the following responses:
        !!
        !! * `grc`    – GeoCOM return code.
        !! * `year`   – Year.
        !! * `month`  – Month [byte].
        !! * `day`    – Day [byte].
        !! * `hour`   – Hours [byte].
        !! * `minute` – Minutes [byte].
        !! * `second` – Seconds [byte].
        !!
        !! | Property       | Values                                                                   |
        !! |----------------|--------------------------------------------------------------------------|
        !! | Instruments    | TPS1200, TM30/TS30, TS16                                                 |
        !! | ASCII request  | `%R1Q,5008:`                                                             |
        !! | ASCII response | `%R1P,0,0:<grc>,<year>,'<month>','<day>','<hour>','<minute>','<second>'` |
        !!
        integer,          parameter :: REQCODE = 5008
        character(len=*), parameter :: PATTERN = &
            "(?<grc>\d+),(?<year>\d+),'(?<month>[0-9a-f]+)','(?<day>[0-9a-f]+)'," // &
            "'(?<hour>[0-9a-f]+)','(?<minute>[0-9a-f]+)','(?<second>[0-9a-f]+)'"

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

        call dm_geocom_api_request(request, REQCODE, pattern=PATTERN, responses=responses)
    end subroutine dm_geocom_api_request_get_date_time

    pure subroutine dm_geocom_api_request_get_date_time_centi(request)
        !! Request of `CSV_GetDateTimeCentiSec` procedure. Creates request for
        !! getting the current date and time of the instrument, including
        !! centiseconds.
        !!
        !! The instrument returns the following responses:
        !!
        !! * `grc`      – GeoCOM return code.
        !! * `year`     – Year.
        !! * `month`    – Month.
        !! * `day`      – Day.
        !! * `hour`     – Hours.
        !! * `minute`   – Minutes.
        !! * `second`   – Seconds.
        !! * `centisec` – Centiseconds.
        !!
        !! | Property       | Values                                                                    |
        !! |----------------|---------------------------------------------------------------------------|
        !! | Instruments    | TPS1200, TM30/TS30, TS16                                                  |
        !! | ASCII request  | `%R1Q,5117:`                                                              |
        !! | ASCII response | `%R1P,0,0:<grc>,<year>,<month>,<day>,<hour>,<minute>,<second>,<centisec>` |
        !!
        integer,          parameter :: REQCODE = 5117
        character(len=*), parameter :: PATTERN = &
            '(?<grc>\d+),(?<year>\d+),(?<month>\d+),(?<day>\d+),(?<hour>\d+),' // &
            '(?<minute>\d+),(?<second>\d+),(?<centisec>\d+)'

        type(request_type), intent(out) :: request !! Prepared request.
        type(response_type)             :: responses(8)

        responses = [ &
            response_type('grc',      type=RESPONSE_TYPE_INT32), &
            response_type('year',     type=RESPONSE_TYPE_INT32), &
            response_type('month',    type=RESPONSE_TYPE_INT32), &
            response_type('day',      type=RESPONSE_TYPE_INT32), &
            response_type('hour',     type=RESPONSE_TYPE_INT32), &
            response_type('minute',   type=RESPONSE_TYPE_INT32), &
            response_type('second',   type=RESPONSE_TYPE_INT32), &
            response_type('centisec', type=RESPONSE_TYPE_INT32)  &
        ]

        call dm_geocom_api_request(request, REQCODE, pattern=PATTERN, responses=responses)
    end subroutine dm_geocom_api_request_get_date_time_centi

    pure subroutine dm_geocom_api_request_get_device_config(request)
        !! Request of `CSV_GetDeviceConfig` procedure. Creates request for
        !! getting the instrument configuration.
        !!
        !! The instrument returns the following responses:
        !!
        !! * `grc`      – GeoCOM return code.
        !! * `devclass` – Device precision class (`TPS_DEVICE_CLASS`).
        !! * `devtype`  – Device configuration type (`TPS_DEVICE_TYPE`).
        !!
        !! | Property       | Values                                           |
        !! |----------------|--------------------------------------------------|
        !! | Instruments    | TPS1200, TM30/TS30, TS16                         |
        !! | ASCII request  | `%R1Q,5035:`                                     |
        !! | ASCII response | `%R1P,0,0:<grc>,<devclass>,<devtype>`            |
        !!
        integer,          parameter :: REQCODE = 5035
        character(len=*), parameter :: PATTERN = '(?<grc>\d+),(?<devclass>\d+),(?<devtype>\d+)'

        type(request_type), intent(out) :: request !! Prepared request.
        type(response_type)             :: responses(3)

        responses = [ &
            response_type('grc',      type=RESPONSE_TYPE_INT32), &
            response_type('devclass', type=RESPONSE_TYPE_INT32), &
            response_type('devtype',  type=RESPONSE_TYPE_INT32)  &
        ]

        call dm_geocom_api_request(request, REQCODE, pattern=PATTERN, responses=responses)
    end subroutine dm_geocom_api_request_get_device_config

    pure subroutine dm_geocom_api_request_get_double_precision(request)
        !! Request of `COM_GetDoublePrecision` procedure. Creates request for
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
        !! | Instruments    | TPS1200, TM30/TS30, TS16                         |
        !! | ASCII request  | `%R1Q,108:`                                      |
        !! | ASCII response | `%R1P,0,0:<grc>,<ndigits>`                       |
        !!
        integer,          parameter :: REQCODE = 108
        character(len=*), parameter :: PATTERN = '(?<grc>\d+),(?<ndigits>\d+)'

        type(request_type), intent(out) :: request !! Prepared request.
        type(response_type)             :: responses(2)

        responses = [ &
            response_type('grc',     type=RESPONSE_TYPE_INT32), &
            response_type('ndigits', type=RESPONSE_TYPE_INT32)  &
        ]

        call dm_geocom_api_request(request, REQCODE, pattern=PATTERN, responses=responses)
    end subroutine dm_geocom_api_request_get_double_precision

    pure subroutine dm_geocom_api_request_get_edm_mode(request)
        !! Request of `TMC_GetEdmMode` procedure. Creates request for getting
        !! the EDM measurement mode.
        !!
        !! The instrument returns the following responses:
        !!
        !! * `grc`     – GeoCOM return code.
        !! * `edmmode` – EDM mode (`EDM_MODE`).
        !!
        !! | Property       | Values                                           |
        !! |----------------|--------------------------------------------------|
        !! | Instruments    | TPS1200, TM30/TS30, TS16                         |
        !! | ASCII request  | `%R1Q,2021:`                                     |
        !! | ASCII response | `%R1P,0,0:<grc>,<edmmode>`                       |
        !!
        integer,          parameter :: REQCODE = 2021
        character(len=*), parameter :: PATTERN = '(?<grc>\d+),(?<edmmode>\d+)'

        type(request_type), intent(out) :: request !! Prepared request.
        type(response_type)             :: responses(2)

        responses = [ &
            response_type('grc',     type=RESPONSE_TYPE_INT32), &
            response_type('edmmode', type=RESPONSE_TYPE_INT32)  &
        ]

        call dm_geocom_api_request(request, REQCODE, pattern=PATTERN, responses=responses)
    end subroutine dm_geocom_api_request_get_edm_mode

    pure subroutine dm_geocom_api_request_get_egl_intensity(request)
        !! Request of `EDM_GetEglIntensity` procedure. Creates request for
        !! getting the value of the intensity of the electronic guide light
        !! (EGL).
        !!
        !! The instrument returns the following responses:
        !!
        !! * `grc`    – GeoCOM return code.
        !! * `eglint` – EDM EGL intensity (`EDM_EGLINTENSITY_TYPE`).
        !!
        !! | Property       | Values                                           |
        !! |----------------|--------------------------------------------------|
        !! | Instruments    | TPS1200, TM30/TS30, TS16                         |
        !! | ASCII request  | `%R1Q,1058:`                                     |
        !! | ASCII response | `%R1P,0,0:<grc>,<eglint>`                        |
        !!
        integer,          parameter :: REQCODE = 1058
        character(len=*), parameter :: PATTERN = '(?<grc>\d+),(?<eglint>\d+)'

        type(request_type), intent(out) :: request !! Prepared request.
        type(response_type)             :: responses(2)

        responses = [ &
            response_type('grc',    type=RESPONSE_TYPE_INT32), &
            response_type('eglint', type=RESPONSE_TYPE_INT32)  &
        ]

        call dm_geocom_api_request(request, REQCODE, pattern=PATTERN, responses=responses)
    end subroutine dm_geocom_api_request_get_egl_intensity

    pure subroutine dm_geocom_api_request_get_face(request)
        !! Request of `TMC_GetFace` procedure. Creates request for getting the
        !! face of the current telescope position.
        !!
        !! The instrument returns the following responses:
        !!
        !! * `grc`  – GeoCOM return code.
        !! * `face` – Telescope face (`TMC_FACE`).
        !!
        !! | Property       | Values                                           |
        !! |----------------|--------------------------------------------------|
        !! | Instruments    | TPS1200, TM30/TS30, TS16                         |
        !! | ASCII request  | `%R1Q,2026:`                                     |
        !! | ASCII response | `%R1P,0,0:<grc>,<face>`                          |
        !!
        integer,          parameter :: REQCODE = 2026
        character(len=*), parameter :: PATTERN = '(?<grc>\d+),(?<face>\d+)'

        type(request_type), intent(out) :: request !! Prepared request.
        type(response_type)             :: responses(2)

        responses = [ &
            response_type('grc',  type=RESPONSE_TYPE_INT32), &
            response_type('face', type=RESPONSE_TYPE_INT32)  &
        ]

        call dm_geocom_api_request(request, REQCODE, pattern=PATTERN, responses=responses)
    end subroutine dm_geocom_api_request_get_face

    pure subroutine dm_geocom_api_request_get_fine_adjust_mode(request)
        !! Request of `AUT_GetFineAdjustMode` procedure. Creates request for
        !! getting the fine adjustment positioning mode.
        !!
        !! The instrument returns the following responses:
        !!
        !! * `grc`     – GeoCOM return code.
        !! * `adjmode` – Fine adjustment positioning mode (`AUT_ADJMODE`).
        !!
        !! | Property       | Values                                           |
        !! |----------------|--------------------------------------------------|
        !! | Instruments    | TPS1200, TM30/TS30, TS16                         |
        !! | ASCII request  | `%R1Q,9030:`                                     |
        !! | ASCII response | `%R1P,0,0:<grc>,<adjmode>`                       |
        !!
        integer,          parameter :: REQCODE = 9030
        character(len=*), parameter :: PATTERN = '(?<grc>\d+),(?<adjmode>\d+)'

        type(request_type), intent(out) :: request !! Prepared request.
        type(response_type)             :: responses(2)

        responses = [ &
            response_type('grc',     type=RESPONSE_TYPE_INT32), &
            response_type('adjmode', type=RESPONSE_TYPE_INT32)  &
        ]

        call dm_geocom_api_request(request, REQCODE, pattern=PATTERN, responses=responses)
    end subroutine dm_geocom_api_request_get_fine_adjust_mode

    pure subroutine dm_geocom_api_request_get_full_measurement(request, wait_time, mode)
        !! Request of `TMC_GetFullMeas` procedure. Creates request to query
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
        !! * `disttime` – Time of distance measurement [ms].
        !!
        !! | Property       | Values                                                                           |
        !! |----------------|----------------------------------------------------------------------------------|
        !! | Instruments    | TPS1200, TM30/TS30, TS16                                                         |
        !! | ASCII request  | `%R1Q,2167:<wait_time>,<mode>`                                                   |
        !! | ASCII response | `%R1P,0,0:<grc>,<hz>,<v>,<angacc>,<xinc>,<linc>,<incacc>,<sdist>,<disttime>` |
        !!
        integer,          parameter :: REQCODE = 2167
        character(len=*), parameter :: PATTERN = &
            '(?<grc>\d+),(?<hz>[-\d\.]+),(?<v>[-\d\.]+),(?<angacc>[-\d\.]+),(?<xinc>[-\d\.]+),' // &
            '(?<linc>[-\d\.]+),(?<incacc>[-\d\.]+),(?<sdist>[-\d\.]+),(?<disttime>[-\d\.]+)'

        type(request_type), intent(out) :: request   !! Prepared request.
        integer,            intent(in)  :: wait_time !! Delay to wait for the distance measurement to finish [ms].
        integer,            intent(in)  :: mode      !! Inclination measurement mode (`TMC_INCLINE_PRG`).

        character(len=80)   :: args
        type(response_type) :: responses(9)

        write (args, '(i0, ",", i0)') wait_time, mode

        responses = [ &
            response_type('grc',      unit=' ',   type=RESPONSE_TYPE_INT32),  & ! GeoCOM return code.
            response_type('hz',       unit='rad', type=RESPONSE_TYPE_REAL64), & ! Horizontal angle [rad].
            response_type('v',        unit='rad', type=RESPONSE_TYPE_REAL64), & ! Vertical angle [rad].
            response_type('angacc',   unit='rad', type=RESPONSE_TYPE_REAL64), & ! Accuracy of angles [rad].
            response_type('xinc',     unit='rad', type=RESPONSE_TYPE_REAL64), & ! Cross inclination [rad].
            response_type('linc',     unit='rad', type=RESPONSE_TYPE_REAL64), & ! Length inclination [rad].
            response_type('incacc',   unit='rad', type=RESPONSE_TYPE_REAL64), & ! Inclination accuracy [rad].
            response_type('sdist',    unit='m',   type=RESPONSE_TYPE_REAL64), & ! Distance measurement [m].
            response_type('disttime', unit='ms',  type=RESPONSE_TYPE_REAL64)  & ! Time of distance measurement [ms].
        ]

        call dm_geocom_api_request(request, REQCODE, args, PATTERN, responses)
    end subroutine dm_geocom_api_request_get_full_measurement

    pure subroutine dm_geocom_api_request_get_geometric_ppm(request)
        !! Request of `TMC_GeoPpm` procedure. Creates request for getting the
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
        integer,          parameter :: REQCODE = 2154
        character(len=*), parameter :: PATTERN = &
             '(?<grc>\d+),(?<geomauto>\d+),(?<scalefcm>[-\d\.]+),(?<offsetcm>[-\d\.]+),' // &
             '(?<hredppm>[-\d\.]+),(?<indippm>[-\d\.]+)'

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

        call dm_geocom_api_request(request, REQCODE, pattern=PATTERN, responses=responses)
    end subroutine dm_geocom_api_request_get_geometric_ppm

    pure subroutine dm_geocom_api_request_get_height(request)
        !! Request of `TMC_GetHeight` procedure. Creates request for getting
        !! the current reflector height.
        !!
        !! The instrument returns the following responses:
        !!
        !! * `grc`     – GeoCOM return code.
        !! * `rheight` – Reflector height [m].
        !!
        !! | Property       | Values                                           |
        !! |----------------|--------------------------------------------------|
        !! | Instruments    | TPS1200, TM30/TS30, TS16                         |
        !! | ASCII request  | `%R1Q,2011:`                                     |
        !! | ASCII response | `%R1P,0,0:<grc>,<height>`                       |
        !!
        integer,          parameter :: REQCODE = 2011
        character(len=*), parameter :: PATTERN = '(?<grc>\d+),(?<rheight>[-\d\.]+)'

        type(request_type), intent(out) :: request !! Prepared request.
        type(response_type)             :: responses(2)

        responses = [ &
            response_type('grc',     unit=' ', type=RESPONSE_TYPE_INT32), &
            response_type('rheight', unit='m', type=RESPONSE_TYPE_REAL64) &
        ]

        call dm_geocom_api_request(request, REQCODE, pattern=PATTERN, responses=responses)
    end subroutine dm_geocom_api_request_get_height

    pure subroutine dm_geocom_api_request_get_image_config(request, mem_type)
        !! Request of `IMG_GetTccConfig` procedure. Creates request to read
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
        integer,          parameter :: REQCODE = 23400
        character(len=*), parameter :: PATTERN = '(?<grc>\d+),(?<imageno>\d+),(?<quality>\d+),(?<subfunc>\d+),(?<fnprefix>.+)'

        type(request_type), intent(out) :: request  !! Prepared request.
        integer,            intent(in)  :: mem_type !! Memory device type (`IMG_MEM_TYPE`).

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

        call dm_geocom_api_request(request, REQCODE, args, PATTERN, responses)
    end subroutine dm_geocom_api_request_get_image_config

    pure subroutine dm_geocom_api_request_get_incline_correction(request)
        !! Request of `TMC_GetInclineSwitch` procedure. Creates request for
        !! getting the dual-axis compensator status.
        !!
        !! The instrument returns the following responses:
        !!
        !! * `grc`    – GeoCOM return code.
        !! * `inccor` – Compensator is enabled [bool].
        !!
        !! | Property       | Values                                           |
        !! |----------------|--------------------------------------------------|
        !! | Instruments    | TPS1200, TM30/TS30, TS16                         |
        !! | ASCII request  | `%R1Q,2007:`                                     |
        !! | ASCII response | `%R1P,0,0:<grc>,<inccor>`                        |
        !!
        integer,          parameter :: REQCODE = 2007
        character(len=*), parameter :: PATTERN = '(?<grc>\d+),(?<inccor>\d+)'

        type(request_type), intent(out) :: request  !! Prepared request.
        type(response_type)             :: responses(2)

        responses = [ &
            response_type('grc',   type=RESPONSE_TYPE_INT32),  &
            response_type('inccor',type=RESPONSE_TYPE_LOGICAL) &
        ]

        call dm_geocom_api_request(request, REQCODE, pattern=PATTERN, responses=responses)
    end subroutine dm_geocom_api_request_get_incline_correction

    pure subroutine dm_geocom_api_request_get_inclination_error(request)
        !! Request of `TMC_IfDataIncCorrError` procedure. Creates request for
        !! getting the inclination error status.
        !!
        !! The instrument returns the following responses:
        !!
        !! * `grc`    – GeoCOM return code.
        !! * `incerr` – Last measurement not incline-corrected [bool].
        !!
        !! | Property       | Values                                           |
        !! |----------------|--------------------------------------------------|
        !! | Instruments    | TPS1200, TM30/TS30, TS16                         |
        !! | ASCII request  | `%R1Q,2115:`                                     |
        !! | ASCII response | `%R1P,0,0:<grc>,<incerr>`                        |
        !!
        integer,          parameter :: REQCODE = 2115
        character(len=*), parameter :: PATTERN = '(?<grc>\d+),(?<incerr>\d+)'

        type(request_type), intent(out) :: request !! Prepared request.
        type(response_type)             :: responses(2)

        responses = [ &
            response_type('grc',    type=RESPONSE_TYPE_INT32),  &
            response_type('incerr', type=RESPONSE_TYPE_LOGICAL) &
        ]

        call dm_geocom_api_request(request, REQCODE, pattern=PATTERN, responses=responses)
    end subroutine dm_geocom_api_request_get_inclination_error

    pure subroutine dm_geocom_api_request_get_instrument_name(request)
        !! Request of `CSV_GetInstrumentName` procedure. Creates request for
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
        integer,          parameter :: REQCODE = 5004
        character(len=*), parameter :: PATTERN = '(?<grc>\d+),(?<name>.+)'

        type(request_type), intent(out) :: request !! Prepared request.
        type(response_type)             :: responses(2)

        responses = [ &
            response_type('grc',  type=RESPONSE_TYPE_INT32), &
            response_type('name', type=RESPONSE_TYPE_STRING) &
        ]

        call dm_geocom_api_request(request, REQCODE, pattern=PATTERN, responses=responses)
    end subroutine dm_geocom_api_request_get_instrument_name

    pure subroutine dm_geocom_api_request_get_instrument_number(request)
        !! Request of `CSV_GetInstrumentNo` procedure. Creates request for
        !! getting the factory defined instrument number.
        !!
        !! The instrument returns the following responses:
        !!
        !! * `grc`      – GeoCOM return code.
        !! * `serialno` – Serial number of the instrument (integer).
        !!
        !! | Property       | Values                                           |
        !! |----------------|--------------------------------------------------|
        !! | Instruments    | TPS1200, TM30/TS30, TS16                         |
        !! | ASCII request  | `%R1Q,5003:`                                     |
        !! | ASCII response | `%R1P,0,0:<grc>,<serialno>`                      |
        !!
        integer,          parameter :: REQCODE = 5003
        character(len=*), parameter :: PATTERN = '(?<grc>\d+),(?<serialno>\d+)'

        type(request_type), intent(out) :: request !! Prepared request.
        type(response_type)             :: responses(2)

        responses = [ &
            response_type('grc',      type=RESPONSE_TYPE_INT32), &
            response_type('serialno', type=RESPONSE_TYPE_INT32)  &
        ]

        call dm_geocom_api_request(request, REQCODE, pattern=PATTERN, responses=responses)
    end subroutine dm_geocom_api_request_get_instrument_number

    pure subroutine dm_geocom_api_request_get_internal_temperature(request)
        !! Request of `CSV_GetIntTemp` procedure. Creates request for getting
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
        !! | Instruments    | TPS1200, TM30/TS30, TS16                         |
        !! | ASCII request  | `%R1Q,5011:`                                     |
        !! | ASCII response | `%R1P,0,0:<grc>,<temp>`                          |
        !!
        integer,          parameter :: REQCODE = 5011
        character(len=*), parameter :: PATTERN = '(?<grc>\d+),(?<temp>\d+)'

        type(request_type), intent(out) :: request !! Prepared request.
        type(response_type)             :: responses(2)

        responses = [ &
            response_type('grc',  type=RESPONSE_TYPE_INT32), &
            response_type('temp', type=RESPONSE_TYPE_INT32)  &
        ]

        call dm_geocom_api_request(request, REQCODE, pattern=PATTERN, responses=responses)
    end subroutine dm_geocom_api_request_get_internal_temperature

    pure subroutine dm_geocom_api_request_get_lock_status(request)
        !! Request of `MOT_ReadLockStatus` procedure. Creates request for
        !! returning the condition of the Lock-In control.
        !!
        !! The instrument returns the following responses:
        !!
        !! * `grc`      – GeoCOM return code.
        !! * `lockstat` – Lock status (`MOT_LOCK_STATUS`).
        !!
        !! | Property       | Values                                           |
        !! |----------------|--------------------------------------------------|
        !! | Instruments    | TPS1200, TM30/TS30, TS16                         |
        !! | ASCII request  | `%R1Q,6021:`                                     |
        !! | ASCII response | `%R1P,0,0:<grc>,<lockstat>`                      |
        !!
        integer,          parameter :: REQCODE = 6021
        character(len=*), parameter :: PATTERN = '(?<grc>\d+),(?<lockstat>\d+)'

        type(request_type), intent(out) :: request !! Prepared request.
        type(response_type)             :: responses(2)

        responses = [ &
            response_type('grc',      type=RESPONSE_TYPE_INT32), &
            response_type('lockstat', type=RESPONSE_TYPE_INT32)  &
        ]

        call dm_geocom_api_request(request, REQCODE, pattern=PATTERN, responses=responses)
    end subroutine dm_geocom_api_request_get_lock_status

    pure subroutine dm_geocom_api_request_get_measurement_program(request)
        !! Request of `BAP_GetMeasPrg` procedure. Creates request for getting
        !! the distance measurement program of the instrument.
        !!
        !! The instrument returns the following responses:
        !!
        !! * `grc`     – GeoCOM return code.
        !! * `measprg` – Measurement program (`BAP_USER_MEASPRG`).
        !!
        !! | Property       | Values                                           |
        !! |----------------|--------------------------------------------------|
        !! | Instruments    | TPS1200, TM30/TS30, TS16                         |
        !! | ASCII request  | `%R1Q,17018:`                                    |
        !! | ASCII response | `%R1P,0,0:<grc>,<measprg>`                       |
        !!
        integer,          parameter :: REQCODE = 17018
        character(len=*), parameter :: PATTERN = '(?<grc>\d+),(?<measprg>\d+)'

        type(request_type), intent(out) :: request !! Prepared request.
        type(response_type)             :: responses(2)

        responses = [ &
            response_type('grc',     type=RESPONSE_TYPE_INT32), &
            response_type('measprg', type=RESPONSE_TYPE_INT32)  &
        ]

        call dm_geocom_api_request(request, REQCODE, pattern=PATTERN, responses=responses)
    end subroutine dm_geocom_api_request_get_measurement_program

    pure subroutine dm_geocom_api_request_get_power(request)
        !! Request of `CSV_CheckPower` procedure. Creates request for checking
        !! the available power.
        !!
        !! The instrument returns the following responses:
        !!
        !! * `grc`      – GeoCOM return code.
        !! * `battlife` – Battery capacity [%].
        !! * `pwrsrc`   – Power source (`CSV_POWER_PATH`).
        !! * `pwrsug`   – Not supported (`CSV_POWER_PATH`).
        !!
        !! | Property       | Values                                           |
        !! |----------------|--------------------------------------------------|
        !! | Instruments    | TPS1200, TM30/TS30, TS16                         |
        !! | ASCII request  | `%R1Q,5039:`                                     |
        !! | ASCII response | `%R1P,0,0:<grc>,<battlife>,<pwrsrc>, <pwrsug>`   |
        !!
        integer,          parameter :: REQCODE = 5039
        character(len=*), parameter :: PATTERN = '(?<grc>\d+),(?<battlife>\d+),(?<pwrsrc>\d+),(?<pwrsug>\d+)'

        type(request_type), intent(out) :: request !! Prepared request.
        type(response_type)             :: responses(4)

        responses = [ &
            response_type('grc',      unit=' ', type=RESPONSE_TYPE_INT32), &
            response_type('battlife', unit='%', type=RESPONSE_TYPE_INT32), &
            response_type('pwrsrc',   unit=' ', type=RESPONSE_TYPE_INT32), &
            response_type('pwrsug',   unit=' ', type=RESPONSE_TYPE_INT32)  &
        ]

        call dm_geocom_api_request(request, REQCODE, pattern=PATTERN, responses=responses)
    end subroutine dm_geocom_api_request_get_power

    pure subroutine dm_geocom_api_request_get_prism_constant(request)
        !! Request of `TMC_GetPrismCorr` procedure. Creates request for
        !! getting the prism constant.
        !!
        !! The instrument returns the following responses:
        !!
        !! * `grc`     – GeoCOM return code.
        !! * `reflcor` – Prism correction constant [m].
        !!
        !! | Property       | Values                                           |
        !! |----------------|--------------------------------------------------|
        !! | Instruments    | TPS1200, TM30/TS30, TS16                         |
        !! | ASCII request  | `%R1Q,2023:`                                     |
        !! | ASCII response | `%R1P,0,0:<grc>,<reflcor>`                       |
        !!
        integer,          parameter :: REQCODE = 2023
        character(len=*), parameter :: PATTERN = '(?<grc>\d+),(?<reflcor>[-\d\.]+)'

        type(request_type), intent(out) :: request !! Prepared request.
        type(response_type)             :: responses(2)

        responses = [ &
            response_type('grc',     unit=' ', type=RESPONSE_TYPE_INT32), &
            response_type('reflcor', unit='m', type=RESPONSE_TYPE_REAL64) &
        ]

        call dm_geocom_api_request(request, REQCODE, pattern=PATTERN, responses=responses)
    end subroutine dm_geocom_api_request_get_prism_constant

    pure subroutine dm_geocom_api_request_get_prism_definition(request, type)
        !! Request of `BAP_GetPrismDef` procedure. Creates request for getting
        !! the default prism definition.
        !!
        !! The instrument returns the following responses:
        !!
        !! * `grc`      – GeoCOM return code.
        !! * `reflname` – Prism name [string].
        !! * `reflcor`  – Prism correction constant [m].
        !! * `refltype` – Prism type.
        !!
        !! | Property       | Values                                           |
        !! |----------------|--------------------------------------------------|
        !! | Instruments    | TPS1200, TM30/TS30, TS16                         |
        !! | ASCII request  | `%R1Q,17023:<type>`                              |
        !! | ASCII response | `%R1P,0,0:<grc>,<reflname>,<reflcor>,<refltype>` |
        !!
        integer,          parameter :: REQCODE = 17023
        character(len=*), parameter :: PATTERN = '(?<grc>\d+),(?<reflname>.+),(?<reflcor>[-\d\.]+),(?<refltype>\d+)'

        type(request_type), intent(out) :: request !! Prepared request.
        integer,            intent(in)  :: type    !! Prism type (`BAP_PRISMTYPE`).

        character(len=80)   :: args
        type(response_type) :: responses(4)

        write (args, '(i0)') type

        responses = [ &
            response_type('grc',      unit=' ', type=RESPONSE_TYPE_INT32),  &
            response_type('reflname', unit=' ', type=RESPONSE_TYPE_STRING), &
            response_type('reflcor',  unit='m', type=RESPONSE_TYPE_REAL64), &
            response_type('refltype', unit=' ', type=RESPONSE_TYPE_INT32)   &
        ]

        call dm_geocom_api_request(request, REQCODE, args, PATTERN, responses)
    end subroutine dm_geocom_api_request_get_prism_definition

    pure subroutine dm_geocom_api_request_get_prism_type(request)
        !! Request of `TMC_GetPrismType` procedure. Creates request for
        !! getting the default prism type.
        !!
        !! The instrument returns the following responses:
        !!
        !! * `grc`      – GeoCOM return code.
        !! * `refltype` – Prism type (`BAP_PRISMTYPE`).
        !!
        !! | Property       | Values                                           |
        !! |----------------|--------------------------------------------------|
        !! | Instruments    | TPS1200, TM30/TS30, TS16                         |
        !! | ASCII request  | `%R1Q,17009:`                                    |
        !! | ASCII response | `%R1P,0,0:<grc>,<refltype>`                      |
        !!
        integer,          parameter :: REQCODE = 17009
        character(len=*), parameter :: PATTERN = '(?<grc>\d+),(?<refltype>\d+)'

        type(request_type), intent(out) :: request !! Prepared request.
        type(response_type)             :: responses(2)

        responses = [ &
            response_type('grc',      type=RESPONSE_TYPE_INT32), &
            response_type('refltype', type=RESPONSE_TYPE_INT32)  &
        ]

        call dm_geocom_api_request(request, REQCODE, pattern=PATTERN, responses=responses)
    end subroutine dm_geocom_api_request_get_prism_type

    pure subroutine dm_geocom_api_request_get_prism_type_v2(request)
        !! Request of `TMC_GetPrismType2` procedure. Creates request for
        !! getting the default or user prism type.
        !!
        !! The instrument returns the following responses:
        !!
        !! * `grc`      – GeoCOM return code.
        !! * `refltype` – Prism type (`BAP_PRISMTYPE`).
        !!
        !! | Property       | Values                                           |
        !! |----------------|--------------------------------------------------|
        !! | Instruments    | TPS1200, TM30/TS30, TS16                         |
        !! | ASCII request  | `%R1Q,17031:`                                    |
        !! | ASCII response | `%R1P,0,0:<grc>,<refltype>`                      |
        !!
        integer,          parameter :: REQCODE = 17031
        character(len=*), parameter :: PATTERN = '(?<grc>\d+),(?<refltype>\d+)'

        type(request_type), intent(out) :: request !! Prepared request.
        type(response_type)             :: responses(2)

        responses = [ &
            response_type('grc',      type=RESPONSE_TYPE_INT32), &
            response_type('refltype', type=RESPONSE_TYPE_INT32)  &
        ]

        call dm_geocom_api_request(request, REQCODE, pattern=PATTERN, responses=responses)
    end subroutine dm_geocom_api_request_get_prism_type_v2

    pure subroutine dm_geocom_api_request_get_quick_distance(request)
        !! Request of `TMC_QuickDist` procedure. Creates request for returning
        !! the slope distance and both angles.
        !!
        !! The function starts an EDM tracking measurement, and waits until a
        !! distance has been measured. Then, it returns the angles and the
        !! slope distance, but no coordinates. If no distance could be
        !! measured, only angles and an error code are returned. A measurement
        !! may be aborted by calling `TMC_DoMeasure`.
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
        !! | Instruments    | TPS1200, TM30/TS30, TS16                         |
        !! | ASCII request  | `%R1Q,2117:`                                     |
        !! | ASCII response | `%R1P,0,0:<grc>,<hz>,<v>,<sdist>`            |
        !!
        integer,          parameter :: REQCODE = 2117
        character(len=*), parameter :: PATTERN = '(?<grc>\d+),(?<hz>[-\d\.]+),(?<v>[-\d\.]+),(?<sdist>[-\d\.]+)'

        type(request_type), intent(out) :: request !! Prepared request.
        type(response_type)             :: responses(4)

        responses = [ &
            response_type('grc',   unit=' ',   type=RESPONSE_TYPE_INT32),  &
            response_type('hz',    unit='rad', type=RESPONSE_TYPE_REAL64), &
            response_type('v',     unit='rad', type=RESPONSE_TYPE_REAL64), &
            response_type('sdist', unit='m',   type=RESPONSE_TYPE_REAL64)  &
        ]

        call dm_geocom_api_request(request, REQCODE, pattern=PATTERN, responses=responses)
    end subroutine dm_geocom_api_request_get_quick_distance

    pure subroutine dm_geocom_api_request_get_reduced_atr_fov(request)
        !! Request of `BAP_GetRedATRFov` procedure. Creates request for
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
        integer,          parameter :: REQCODE = 17036
        character(len=*), parameter :: PATTERN = '(?<grc>\d+),(?<atrfov>\d+)'

        type(request_type), intent(out) :: request !! Prepared request.
        type(response_type)             :: responses(2)

        responses = [ &
            response_type('grc',    type=RESPONSE_TYPE_INT32),  &
            response_type('atrfov', type=RESPONSE_TYPE_LOGICAL) &
        ]

        call dm_geocom_api_request(request, REQCODE, pattern=PATTERN, responses=responses)
    end subroutine dm_geocom_api_request_get_reduced_atr_fov

    pure subroutine dm_geocom_api_request_get_reflectorless_class(request)
        !! Request of `CSV_GetReflectorlessClass` procedure. Creates request
        !! for getting the RL type.
        !!
        !! The function returns the class of the reflectorless and long-range
        !! distance measurement of the instrument.
        !!
        !! The instrument returns the following responses:
        !!
        !! * `grc`     – GeoCOM return code.
        !! * `rlclass` – Reflectorless class (`TPS_REFLESS_CLASS`).
        !!
        !! | Property       | Values                                           |
        !! |----------------|--------------------------------------------------|
        !! | Instruments    | TPS1200, TM30/TS30, TS16                         |
        !! | ASCII request  | `%R1Q,5100:`                                     |
        !! | ASCII response | `%R1P,0,0:<grc>,<rlclass>`                       |
        !!
        integer,          parameter :: REQCODE = 5100
        character(len=*), parameter :: PATTERN = '(?<grc>\d+),(?<rlclass>\d+)'

        type(request_type), intent(out) :: request !! Prepared request.
        type(response_type)             :: responses(2)

        responses = [ &
            response_type('grc',     type=RESPONSE_TYPE_INT32), &
            response_type('rlclass', type=RESPONSE_TYPE_INT32)  &
        ]

        call dm_geocom_api_request(request, REQCODE, pattern=PATTERN, responses=responses)
    end subroutine dm_geocom_api_request_get_reflectorless_class

    pure subroutine dm_geocom_api_request_get_refraction_mode(request)
        !! Request of `TMC_GetRefractiveMethod` procedure. Creates request for
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
        !! | Instruments    | TPS1200, TM30/TS30, TS16                         |
        !! | ASCII request  | `%R1Q,2091:`                                     |
        !! | ASCII response | `%R1P,0,0:<grc>,<refrmode>`                      |
        !!
        integer,          parameter :: REQCODE = 2091
        character(len=*), parameter :: PATTERN = '(?<grc>\d+),(?<refrmode>\d+)'

        type(request_type), intent(out) :: request !! Prepared request.
        type(response_type)             :: responses(2)

        responses = [ &
            response_type('grc',      type=RESPONSE_TYPE_INT32), &
            response_type('refrmode', type=RESPONSE_TYPE_INT32)  &
        ]

        call dm_geocom_api_request(request, REQCODE, pattern=PATTERN, responses=responses)
    end subroutine dm_geocom_api_request_get_refraction_mode

    pure subroutine dm_geocom_api_request_get_search_area(request)
        !! Request of `AUT_GetSearchArea` procedure. Creates request for
        !! getting the dimensions of the Power Search window.
        !!
        !! This command is valid for all instruments, but has only effects for
        !! instruments equipped with Power Search.
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
        !! | Instruments    | TPS1200, TM30/TS30, TS16                                            |
        !! | ASCII request  | `%R1Q,9042:`                                                        |
        !! | ASCII response | `%R1P,0,0:<grc>,<centerhz>,<centerv>,<rangehz>,<rangev>,<userarea>` |
        !!
        integer,          parameter :: REQCODE = 9042
        character(len=*), parameter :: PATTERN = &
            '(?<grc>\d+),(?<centerhz>[-\d\.]+),(?<centerv>[-\d\.]+),(?<rangehz>[-\d\.]+),' // &
            '(?<rangev>[-\d\.]+),(?<userarea>\d+)'

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

        call dm_geocom_api_request(request, REQCODE, pattern=PATTERN, responses=responses)
    end subroutine dm_geocom_api_request_get_search_area

    pure subroutine dm_geocom_api_request_get_signal(request)
        !! Request of `TMC_GetSignal` procedure. Creates request for
        !! getting the EDM signal intensity.
        !!
        !! The function returns the intensity of the EDM signal. The function
        !! can only perform a measurement if the signal measurement program is
        !! activated. Start the signal measurement program with `TMC_DoMeasure` in
        !! program `TMC_SIGNAL`. After the measurement, the EDM must be
        !! switched off with program `TMC_CLEAR`. While measuring, there is no
        !! angle data available.
        !!
        !! The instrument returns the following responses:
        !!
        !! * `grc`     – GeoCOM return code.
        !! * `sigint`  – Signal intensity of EDM [%].
        !! * `sigtime` – Timestamp [ms].
        !!
        !! | Property       | Values                                           |
        !! |----------------|--------------------------------------------------|
        !! | Instruments    | TPS1200, TM30/TS30, TS16                         |
        !! | ASCII request  | `%R1Q,2022:`                                     |
        !! | ASCII response | `%R1P,0,0:<grc>,<sigint>,<sigtime>`              |
        !!
        integer,          parameter :: REQCODE = 2022
        character(len=*), parameter :: PATTERN = '(?<grc>\d+),(?<sigint>[-\d\.]+),(?<sigtime>\d+)'

        type(request_type), intent(out) :: request !! Prepared request.
        type(response_type)             :: responses(3)

        responses = [ &
            response_type('grc',     unit=' ',  type=RESPONSE_TYPE_INT32),  &
            response_type('sigint',  unit='%',  type=RESPONSE_TYPE_REAL64), &
            response_type('sigtime', unit='ms', type=RESPONSE_TYPE_LOGICAL) &
        ]

        call dm_geocom_api_request(request, REQCODE, pattern=PATTERN, responses=responses)
    end subroutine dm_geocom_api_request_get_signal

    ! pure subroutine dm_geocom_api_request_get_simple_coordinates(wait_time, e, n, h, prog)
    ! pure subroutine dm_geocom_api_request_get_simple_meta(wait_time, only_angle, slope_dist, mode)
    ! pure subroutine dm_geocom_api_request_get_slope_distance_correction(ppm_corr, prism_corr)
    ! pure subroutine dm_geocom_api_request_get_software_version(release, version, subversion)
    ! pure subroutine dm_geocom_api_request_get_software_version2(release, version, subversion)
    ! pure subroutine dm_geocom_api_request_get_station(station)
    ! pure subroutine dm_geocom_api_request_get_target_type(type)
    ! pure subroutine dm_geocom_api_request_get_timeout(hz, v)
    ! pure subroutine dm_geocom_api_request_get_tolerance(hz, v)
    ! pure subroutine dm_geocom_api_request_get_user_atr_mode(mode)
    ! pure subroutine dm_geocom_api_request_get_user_local_mode(mode)

    pure subroutine dm_geocom_api_request_get_user_prism_definition(request, name)
        !! Request of `BAP_GetUserPrismDef` procedure. Creates request for
        !! getting the user prism definition.
        !!
        !! The instrument returns the following responses:
        !!
        !! * `grc`      – GeoCOM return code.
        !! * `reflcor`  – Prism correction constant [m].
        !! * `refltype` – Prism type (`BAP_PRISMTYPE`).
        !! * `refluser` – Name of creator [string].
        !!
        !! | Property       | Values                                           |
        !! |----------------|--------------------------------------------------|
        !! | Instruments    | TPS1200, TM30/TS30, TS16                         |
        !! | ASCII request  | `%R1Q,17033:<name>`                              |
        !! | ASCII response | `%R1P,0,0:<grc>,<reflcor>,<refltype>,<refluser>` |
        !!
        integer,          parameter :: REQCODE = 17033
        character(len=*), parameter :: PATTERN = '(?<grc>\d+),(?<reflcor>[-\d\.]+),(?<refltype>\d+),(?<refluser>.+)'

        type(request_type), intent(out) :: request !! Prepared request.
        character(len=*),   intent(in)  :: name    !! Prism name.

        type(response_type) :: responses(4)

        responses = [ &
            response_type('grc',      unit=' ', type=RESPONSE_TYPE_INT32),  &
            response_type('reflcor',  unit='m', type=RESPONSE_TYPE_REAL64), &
            response_type('refltype', unit=' ', type=RESPONSE_TYPE_INT32),  &
            response_type('refluser', unit=' ', type=RESPONSE_TYPE_STRING)  &
        ]

        call dm_geocom_api_request(request, REQCODE, name, PATTERN, responses)
    end subroutine dm_geocom_api_request_get_user_prism_definition

    ! pure subroutine dm_geocom_api_request_get_user_spiral(hz, v)
    ! pure subroutine dm_geocom_api_request_list(next, last, dir_info)
    ! pure subroutine dm_geocom_api_request_lock_in()
    ! pure subroutine dm_geocom_api_request_measure_distance_angle(mode, hz, v, slope_dist)

    pure subroutine dm_geocom_api_request_null(request)
        !! Request of `COM_NullProc` procedure. Creates request for checking
        !! the communication.
        !!
        !! The instrument returns the following responses:
        !!
        !! * `grc` – GeoCOM return code.
        !!
        !! | Property       | Values                                           |
        !! |----------------|--------------------------------------------------|
        !! | Instruments    | TPS1200, TM30/TS30, TS16                         |
        !! | ASCII request  | `%R1Q,0:`                                        |
        !! | ASCII response | `%R1P,0,0:<grc>`                                 |
        !!
        integer, parameter :: REQCODE = 0

        type(request_type), intent(out) :: request !! Prepared request.

        call dm_geocom_api_request(request, REQCODE, pattern=GEOCOM_GRC_PATTERN, responses=GEOCOM_GRC_RESPONSES)
    end subroutine dm_geocom_api_request_null

    ! pure subroutine dm_geocom_api_request_ps_enable_range(enable)
    ! pure subroutine dm_geocom_api_request_ps_search_next(direction, swing)
    ! pure subroutine dm_geocom_api_request_ps_search_window()
    ! pure subroutine dm_geocom_api_request_ps_set_range(min_dist, max_dist)
    ! pure subroutine dm_geocom_api_request_search(hz, v)
    ! pure subroutine dm_geocom_api_request_search_target()
    ! pure subroutine dm_geocom_api_request_set_angle_correction(incline, stand_axis, collimation, tilt_axis)
    ! pure subroutine dm_geocom_api_request_set_atmospheric_correction(atmos)
    ! pure subroutine dm_geocom_api_request_set_atmospheric_ppm(ppm)
    ! pure subroutine dm_geocom_api_request_set_atr_setting(setting)
    ! pure subroutine dm_geocom_api_request_set_binary_mode(available)
    ! pure subroutine dm_geocom_api_request_set_config(auto_power, timeout)
    ! pure subroutine dm_geocom_api_request_set_date_time(dt)
    ! pure subroutine dm_geocom_api_request_set_double_precision(ndigits)
    ! pure subroutine dm_geocom_api_request_set_edm_mode(mode)
    ! pure subroutine dm_geocom_api_request_set_egl_intensity(intensity)
    ! pure subroutine dm_geocom_api_request_set_fine_adjust_mode(mode)
    ! pure subroutine dm_geocom_api_request_set_geometric_ppm(automatic, scale_factor, offset, height, individual)
    ! pure subroutine dm_geocom_api_request_set_height(height)
    ! pure subroutine dm_geocom_api_request_set_image_config(mem_type, parameters)
    ! pure subroutine dm_geocom_api_request_set_incline_correction(mode)
    ! pure subroutine dm_geocom_api_request_set_laser_pointer(enabled)
    ! pure subroutine dm_geocom_api_request_set_measurement_program(prog)
    ! pure subroutine dm_geocom_api_request_set_offset(slope_dist, height, mode)

    pure subroutine dm_geocom_api_request_set_position(request, hz, v, pos_mode, atr_mode)
        !! Request of `AUT_MakePositioning` procedure. Creates request for for
        !! turning the telescope to a specified position.
        !!
        !! If `pos_mode` is `GEOCOM_AUT_NORMAL`, uses the current value of the
        !! compensator. For positioning distances > 25 gon, this mode might tend
        !! to inaccuracy. If set to `GEOCOM_AUT_PRECISE`, tries to measure the
        !! exact inclination of the target. Tends to long position time.
        !!
        !! If `atr_mode` is `GEOCOM_AUT_POSITION`, uses conventional position
        !! to other face. If set to `GEOCOM_AUT_TARGET`, tries to position into
        !! a target in the destination area. This mode requires activated ATR.
        !!
        !! The instrument returns the following responses:
        !!
        !! * `grc` – GeoCOM return code.
        !!
        !! | Property       | Values                                           |
        !! |----------------|--------------------------------------------------|
        !! | Instruments    | TPS1200, TM30/TS30, TS16                         |
        !! | ASCII request  | `%R1Q,9027:<hz>,<v>,<pos_mode>,<atr_mode>,0`     |
        !! | ASCII response | `%R1P,0,0:<grc>`                                 |
        !!
        integer, parameter :: REQCODE = 9027

        type(request_type), intent(out) :: request  !! Prepared request.
        real(kind=r8),      intent(in)  :: hz       !! Horizontal angle [rad].
        real(kind=r8),      intent(in)  :: v        !! Vertical angle [rad].
        integer,            intent(in)  :: pos_mode !! Position mode (`AUT_POSMODE`).
        integer,            intent(in)  :: atr_mode !! ATR mode (`AUT_ATRMODE`).

        character(len=80) :: args

        write (args, '(2(f0.12, ","), 2(i0, ","), "0")') hz, v, pos_mode, atr_mode
        call dm_geocom_api_request(request, REQCODE, args, GEOCOM_GRC_PATTERN, GEOCOM_GRC_RESPONSES)
    end subroutine dm_geocom_api_request_set_position

    ! pure subroutine dm_geocom_api_request_set_orientation(hz)
    ! pure subroutine dm_geocom_api_request_set_prism_constant(prism_corr)
    ! pure subroutine dm_geocom_api_request_set_prism_type(type)
    ! pure subroutine dm_geocom_api_request_set_prism_type_v2(type, name)
    ! pure subroutine dm_geocom_api_request_set_reduced_atr_fov(mode)
    ! pure subroutine dm_geocom_api_request_set_refraction_mode(method)
    ! pure subroutine dm_geocom_api_request_set_search_area(area)
    ! pure subroutine dm_geocom_api_request_set_station(station)
    ! pure subroutine dm_geocom_api_request_set_target_type(type)
    ! pure subroutine dm_geocom_api_request_set_timeout(hz, v)
    ! pure subroutine dm_geocom_api_request_set_tolerance(hz, v)
    ! pure subroutine dm_geocom_api_request_set_user_atr_mode(mode)
    ! pure subroutine dm_geocom_api_request_set_user_local_mode(mode)
    ! pure subroutine dm_geocom_api_request_set_user_prism_definition(name, add_const, type, creator)
    ! pure subroutine dm_geocom_api_request_set_user_spiral(hz, v)
    ! pure subroutine dm_geocom_api_request_set_velocity(omega)
    ! pure subroutine dm_geocom_api_request_setup_download(device_type, file_type, file_name, block_size, nblocks)
    ! pure subroutine dm_geocom_api_request_setup_list(device_type, file_type, search_path)
    ! pure subroutine dm_geocom_api_request_start_controller(mode)
    ! pure subroutine dm_geocom_api_request_stop_controller(mode)

    pure subroutine dm_geocom_api_request_switch_off(request, mode)
        !! Request of `COM_SwitchOffTPS` procedure. Creates request for
        !! turning the instrument off.
        !!
        !! The argument `mode` has to be one of the following:
        !!
        !! * `COM_TPS_STOP_SHUT_DOWN` – Power down instrument.
        !! * `COM_TPS_STOP_SLEEP`     – Sleep mode (unsupported by TPS1200).
        !!
        !! The instrument returns the following responses:
        !!
        !! * `grc` – GeoCOM return code.
        !!
        !! | Property       | Values                                           |
        !! |----------------|--------------------------------------------------|
        !! | Instruments    | TPS1200, TM30/TS30, TS16                         |
        !! | ASCII request  | `%R1Q,112:<mode>`                                |
        !! | ASCII response | `%R1P,0,0:<grc>`                                 |
        !!
        integer, parameter :: REQCODE = 112

        type(request_type), intent(out) :: request !! Prepared request.
        integer,            intent(in)  :: mode    !! Switch off mode (`COM_TPS_STOP_MODE`).

        character(len=80) :: args

        write (args, '(i0)') mode
        call dm_geocom_api_request(request, REQCODE, args, GEOCOM_GRC_PATTERN, GEOCOM_GRC_RESPONSES)
    end subroutine dm_geocom_api_request_switch_off

    pure subroutine dm_geocom_api_request_switch_on(request, mode)
        !! Request of `COM_SwitchOffTPS` procedure. Creates request for
        !! turning the instrument off.
        !!
        !! The argument `mode` has to be one of the following:
        !!
        !! * `COM_TPS_STARTUP_LOCAL`  – Not supported by TPS1200.
        !! * `COM_TPS_STARTUP_REMOTE` – Online mode (RPC is enabled).
        !!
        !! The instrument returns the following responses:
        !!
        !! * `grc` – GeoCOM return code.
        !!
        !! | Property       | Values                                           |
        !! |----------------|--------------------------------------------------|
        !! | Instruments    | TPS1200, TM30/TS30, TS16                         |
        !! | ASCII request  | `%R1Q,111:<mode>`                                |
        !! | ASCII response | `%R1P,0,0:5` (if switched on)                    |
        !!
        integer, parameter :: REQCODE = 111

        type(request_type), intent(out) :: request !! Prepared request.
        integer,            intent(in)  :: mode    !! Switch on mode (`COM_TPS_STARTUP_MODE`).

        character(len=80) :: args

        write (args, '(i0)') mode
        call dm_geocom_api_request(request, REQCODE, args)
    end subroutine dm_geocom_api_request_switch_on

    pure subroutine dm_geocom_api_request_take_image(request, mem_type)
        !! Request of `IMG_TakeTccImage` procedure. Creates request for
        !! capturing a telescope image.
        !!
        !! The memory type `mem_type` has to be one of the following:
        !!
        !! * `IMG_INTERNAL_MEMORY` – Internal memory module.
        !! * `IMG_PC_CARD`         – External PC card.
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
        integer,          parameter :: REQCODE = 23402
        character(len=*), parameter :: PATTERN = '(?<grc>\d+),(?<imageno>\d+)'

        type(request_type), intent(out) :: request  !! Prepared request.
        integer,            intent(in)  :: mem_type !! Memory type (`IMG_MEM_TYPE`).

        character(len=80)   :: args
        type(response_type) :: responses(2)

        responses = [ &
            response_type('grc',     type=RESPONSE_TYPE_INT32), &
            response_type('imageno', type=RESPONSE_TYPE_INT64)  &
        ]

        write (args, '(i0)') mem_type
        call dm_geocom_api_request(request, REQCODE, args, PATTERN, responses)
    end subroutine dm_geocom_api_request_take_image
end module dm_geocom_api
