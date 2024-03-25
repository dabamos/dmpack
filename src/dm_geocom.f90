! Author:  Philipp Engel
! Licence: ISC
module dm_geocom
    !! Object-oriented GeoCOM API for Fortran.
    !!
    !! The API provided by DMPACK does not follow the official Leica GeoCOM API
    !! for C/C++ and Visual Basic. Functions are given more memorable names,
    !! without any sub-system prefix. Structured types have been removed
    !! altogether. If invalid parameters are passed to the GeoCOM methods, they
    !! will be replaced with their default values. Open the serial port with
    !! argument `verbose` set to `.true.` to output error messages to standard
    !! error.
    !!
    !! The following example opens the TTY `/dev/ttyUSB0` at 115,200 baud, and
    !! calls the null procedure of the instrument (`COM_NullProc`):
    !!
    !! ```fortran
    !! integer            :: rc     ! DMPACK return code.
    !! type(geocom_class) :: geocom ! GeoCOM object.
    !!
    !! call geocom%open('/dev/ttyUSB0', GEOCOM_COM_BAUD_115200, verbose=.true., error=rc)
    !! if (dm_is_error(rc)) error stop
    !!
    !! call geocom%null()
    !! print '(i0, ": ", a)', geocom%code(), geocom%message()
    !!
    !! call geocom%close()
    !! ```
    !!
    !! ## GeoCOM Parameters and Named Types
    !!
    !! * `GEOCOM_AUT_CLOCKWISE`     – Direction close-wise.
    !! * `GEOCOM_AUT_ANTICLOCKWISE` – Direction counter clock-wise.
    !!
    !! * `GEOCOM_IOS_BEEP_STDINTENS` – Standard intensity of beep expressed as percentage.
    !!
    !! ### GEOCOM_AUT_ADJMODE
    !!
    !! * `GEOCOM_AUT_NORM_MODE`   – Angle tolerance.
    !! * `GEOCOM_AUT_POINT_MODE`  – Point tolerance.
    !! * `GEOCOM_AUT_DEFINE_MODE` – System independent positioning tolerance.
    !!
    !! ### GEOCOM_AUT_ATRMODE
    !!
    !! * `GEOCOM_AUT_POSITION` – Positioning to Hz and V angle.
    !! * `GEOCOM_AUT_TARGET`   – Positioning to a target in the env. of the Hz and V angle.
    !!
    !! ### GEOCOM_AUT_POSMODE
    !!
    !! * `GEOCOM_AUT_NORMAL`  – Fast positioning mode.
    !! * `GEOCOM_AUT_PRECISE` – Exact positioning mode.
    !! * `GEOCOM_AUT_FAST`    – For TM30/TS30.
    !!
    !! ### GEOCOM_BAP_TRSETTING
    !!
    !! * `GEOCOM_BAP_ATRSET_NORMAL`     – ATR is using no special flags or modes.
    !! * `GEOCOM_BAP_ATRSET_LOWVIS_ON`  – ATR low vis mode on.
    !! * `GEOCOM_BAP_ATRSET_LOWVIS_AON` – ATR low vis mode always on.
    !! * `GEOCOM_BAP_ATRSET_SRANGE_ON`  – ATR high reflectivity mode on.
    !! * `GEOCOM_BAP_ATRSET_SRANGE_AON` – ATR high reflectivity mode always on.
    !!
    !! ### GEOCOM_BAP_MEASURE_PRG
    !!
    !! * `GEOCOM_BAP_NO_MEAS`    – No measurements, take last one.
    !! * `GEOCOM_BAP_NO_DIST`    – No dist. measurement, angles only.
    !! * `GEOCOM_BAP_DEF_DIST`   – Default distance measurements.
    !! * `GEOCOM_BAP_CLEAR_DIST` – Clear distances.
    !! * `GEOCOM_BAP_STOP_TRK`   – Stop tracking.
    !!
    !! ### GEOCOM_BAP_PRISMTYPE
    !!
    !! * `GEOCOM_BAP_PRISM_ROUND`        – Leica Circular Prism.
    !! * `GEOCOM_BAP_PRISM_MINI`         – Leica Mini Prism.
    !! * `GEOCOM_BAP_PRISM_TAPE`         – Leica Reflector Tape.
    !! * `GEOCOM_BAP_PRISM_360`          – Leica 360° Prism.
    !! * `GEOCOM_BAP_PRISM_USER1`        – Not supported by TPS1200.
    !! * `GEOCOM_BAP_PRISM_USER2`        – Not supported by TPS1200.
    !! * `GEOCOM_BAP_PRISM_USER3`        – Not supported by TPS1200.
    !! * `GEOCOM_BAP_PRISM_360_MINI`     – Leica Mini 360° Prism
    !! * `GEOCOM_BAP_PRISM_MINI_ZERO`    – Leica Mini Zero Prism.
    !! * `GEOCOM_BAP_PRISM_USER`         – User Defined Prism.
    !! * `GEOCOM_BAP_PRISM_NDS_TAPE`     – Leica HDS Target.
    !! * `GEOCOM_BAP_PRISM_GRZ121_ROUND` – GRZ121 360º Prism for Machine Guidance.
    !! * `GEOCOM_BAP_PRISM_MA_MPR122`    – MPR122 360º Prism for Machine Guidance.
    !!
    !! ### GEOCOM_BAP_REFLTYPE
    !!
    !! * `GEOCOM_BAP_REFL_UNDEF` – Reflector not defined.
    !! * `GEOCOM_BAP_REFL_PRISM` – Reflector prism.
    !! * `GEOCOM_BAP_REFL_TAPE`  – Reflector tape.
    !!
    !! ### GEOCOM_BAP_USER_MEASPRG
    !!
    !! * `GEOCOM_BAP_SINGLE_REF_STANDARD`  – IR standard.
    !! * `GEOCOM_BAP_SINGLE_REF_FAST`      – IR fast.
    !! * `GEOCOM_BAP_SINGLE_REF_VISIBLE`   – LO standard.
    !! * `GEOCOM_BAP_SINGLE_RLESS_VISIBLE` – RL standard.
    !! * `GEOCOM_BAP_CONT_REF_STANDARD`    – IR tracking.
    !! * `GEOCOM_BAP_CONT_REF_FAST`        – Not supported by TPS1200.
    !! * `GEOCOM_BAP_CONT_RLESS_VISIBLE`   – RL fast tracking.
    !! * `GEOCOM_BAP_AVG_REF_STANDARD`     – IR average.
    !! * `GEOCOM_BAP_AVG_REF_VISIBLE`      – LO average.
    !! * `GEOCOM_BAP_AVG_RLESS_VISIBLE`    – RL average.
    !! * `GEOCOM_BAP_CONT_REF_SYNCHRO`     – IR synchro tracking.
    !! * `GEOCOM_BAP_SINGLE_REF_PRECISE`   – IR precise (TM30/TS30).
    !!
    !! ### GEOCOM_BAP_TARGET_TYPE
    !!
    !! * `GEOCOM_BAP_REFL_USE`  – With reflector.
    !! * `GEOCOM_BAP_REFL_LESS` – Without reflector.
    !!
    !! ### GEOCOM_COM_BAUD_RATE
    !!
    !! * `GEOCOM_COM_BAUD_38400`  – 38400 baud.
    !! * `GEOCOM_COM_BAUD_19200`  – 19200 baud (default rate).
    !! * `GEOCOM_COM_BAUD_9600`   – 9600 baud.
    !! * `GEOCOM_COM_BAUD_4800`   – 4800 baud.
    !! * `GEOCOM_COM_BAUD_2400`   – 2400 baud.
    !! * `GEOCOM_COM_BAUD_115200` – 115200 baud.
    !! * `GEOCOM_COM_BAUD_57600`  – 57600 baud.
    !!
    !! ### GEOCOM_COM_FORMAT
    !!
    !! * `GEOCOM_COM_ASCII`  – ASCII protocol.
    !! * `GEOCOM_COM_BINARY` – Binary protocol.
    !!
    !! ### GEOCOM_COM_TPS_STARTUP_MODE
    !!
    !! * `GEOCOM_COM_STARTUP_LOCAL`  – Not supported by TPS1200.
    !! * `GEOCOM_COM_STARTUP_REMOTE` – RPC is enabled (online mode).
    !!
    !! ### GEOCOM_COM_TPS_STOP_MODE
    !!
    !! * `GEOCOM_COM_STOP_SHUT_DOWN` – Power down instrument.
    !! * `GEOCOM_COM_STOP_SLEEP`     – Not supported by TPS1200.
    !!
    !! ### GEOCOM_CSV_POWER_PATH
    !!
    !! * `GEOCOM_CSV_EXTERNAL_POWER` – Power source is external.
    !! * `GEOCOM_CSV_INTERNAL_POWER` – Power source is the internal battery.
    !!
    !! ### GEOCOM_TPS_DEVICE_CLASS
    !!
    !! * `GEOCOM_TPS_CLASS_1100` – TPS1000 family member, 1 mgon, 3 ".
    !! * `GEOCOM_TPS_CLASS_1700` – TPS1000 family member, 0.5 mgon, 1.5 ".
    !! * `GEOCOM_TPS_CLASS_1800` – TPS1000 family member, 0.3 mgon, 1 ".
    !! * `GEOCOM_TPS_CLASS_5000` – TPS2000 family member.
    !! * `GEOCOM_TPS_CLASS_6000` – TPS2000 family member.
    !! * `GEOCOM_TPS_CLASS_1500` – TPS1000 family member.
    !! * `GEOCOM_TPS_CLASS_2003` – TPS2000 family member.
    !! * `GEOCOM_TPS_CLASS_5005` – TPS5000 family member.
    !! * `GEOCOM_TPS_CLASS_5100` – TPS5000 family member.
    !! * `GEOCOM_TPS_CLASS_1102` – TPS1100 family member, 2 ".
    !! * `GEOCOM_TPS_CLASS_1103` – TPS1100 family member, 3 ".
    !! * `GEOCOM_TPS_CLASS_1105` – TPS1100 family member, 5 ".
    !! * `GEOCOM_TPS_CLASS_1101` – TPS1100 family member, 1 ".
    !! * `GEOCOM_TPS_CLASS_1202` – TPS1200 family member, 2 ".
    !! * `GEOCOM_TPS_CLASS_1203` – TPS1200 family member, 3 ".
    !! * `GEOCOM_TPS_CLASS_1205` – TPS1200 family member, 5 ".
    !! * `GEOCOM_TPS_CLASS_1201` – TPS1200 family member, 1 ".
    !! * `GEOCOM_TPS_CLASS_TX30` – TS30,TM30 family member, 0.5 ".
    !! * `GEOCOM_TPS_CLASS_TX31` – TS30,TM30 family member, 1 ".
    !!
    !! ### GEOCOM_TPS_DEVICE_TYPE
    !!
    !! * `GEOCOM_TPS_DEVICE_T`      – Theodolite without built-in EDM.
    !! * `GEOCOM_TPS_DEVICE_MOT`    – Motorized device.
    !! * `GEOCOM_TPS_DEVICE_ATR`    – Automatic Target Recognition.
    !! * `GEOCOM_TPS_DEVICE_EGL`    – Electronic Guide Light.
    !! * `GEOCOM_TPS_DEVICE_DB`     – Reserved (Database, not GSI).
    !! * `GEOCOM_TPS_DEVICE_DL`     – Diode laser.
    !! * `GEOCOM_TPS_DEVICE_LP`     – Laser plumbed.
    !! * `GEOCOM_TPS_DEVICE_TC1`    – Tachymeter (TCW1).
    !! * `GEOCOM_TPS_DEVICE_TC2`    – Tachymeter (TCW2).
    !! * `GEOCOM_TPS_DEVICE_TC`     – Tachymeter (TCW3).
    !! * `GEOCOM_TPS_DEVICE_TCR`    – Tachymeter (TCW3 with red laser).
    !! * `GEOCOM_TPS_DEVICE_ATC`    – Autocollimation lamp (used only PMU).
    !! * `GEOCOM_TPS_DEVICE_LPNT`   – Laserpointer.
    !! * `GEOCOM_TPS_DEVICE_RL_EXT` – Reflectorless EDM with extended range (Pinpoint R100, R300).
    !! * `GEOCOM_TPS_DEVICE_PS`     – Power Search.
    !! * `GEOCOM_TPS_DEVICE_SIM`    – Runs on simulation, no hardware.
    !!
    !! ### GEOCOM_TPS_REFLESS_CLASS
    !!
    !! * `GEOCOM_TPS_REFLESS_NONE`  – None.
    !! * `GEOCOM_TPS_REFLESS_R100`  – Pinpoint R100.
    !! * `GEOCOM_TPS_REFLESS_R300`  – Pinpoint R300.
    !! * `GEOCOM_TPS_REFLESS_R400`  – Pinpoint R400.
    !! * `GEOCOM_TPS_REFLESS_R1000` – Pinpoint R1000.
    !!
    !! ### GEOCOM_EDM_EGLINTENSITY_TYPE
    !!
    !! * `GEOCOM_EDM_EGLINTEN_OFF`  – Off.
    !! * `GEOCOM_EDM_EGLINTEN_LOW`  – Low intensity.
    !! * `GEOCOM_EDM_EGLINTEN_MID`  – Medium intensity.
    !! * `GEOCOM_EDM_EGLINTEN_HIGH` – High intensity.
    !!
    !! ### GEOCOM_EDM_MODE
    !!
    !! * `GEOCOM_EDM_MODE_NOT_USED`   – Initial value.
    !! * `GEOCOM_EDM_SINGLE_TAPE`     – IR Standard Reflector Tape.
    !! * `GEOCOM_EDM_SINGLE_STANDARD` – IR Standard.
    !! * `GEOCOM_EDM_SINGLE_FAST`     – IR Fast.
    !! * `GEOCOM_EDM_SINGLE_LRANGE`   – LO Standard.
    !! * `GEOCOM_EDM_SINGLE_SRANGE`   – RL Standard.
    !! * `GEOCOM_EDM_CONT_STANDARD`   – Standard repeated measurement.
    !! * `GEOCOM_EDM_CONT_DYNAMIC`    – IR Tacking.
    !! * `GEOCOM_EDM_CONT_REFLESS`    – RL Tracking.
    !! * `GEOCOM_EDM_CONT_FAST`       – Fast repeated measurement.
    !! * `GEOCOM_EDM_AVERAGE_IR`      – IR Average.
    !! * `GEOCOM_EDM_AVERAGE_SR`      – RL Average.
    !! * `GEOCOM_EDM_AVERAGE_LR`      – LO Average.
    !! * `GEOCOM_EDM_PRECISE_IR`      – IR Precise (TM30, TS30).
    !! * `GEOCOM_EDM_PRECISE_TAPE`    – IR Precise Reflector Tape (TM30, TS30).
    !!
    !! ### GEOCOM_FTR_DEVICETYPE
    !!
    !! * `GEOCOM_FTR_DEVICE_INTERNAL` – Internal memory.
    !! * `GEOCOM_FTR_DEVICE_PCPARD`   – Memory card.
    !!
    !! ### GEOCOM_FTR_FILETYPE
    !!
    !! * `GEOCOM_FTR_FILE_UNKNOWN` – Undocumented.
    !! * `GEOCOM_FTR_FILE_IMAGES`  – Extension wildcard: `*.jpg`.
    !!
    !! ### GEOCOM_IMG_MEM_TYPE
    !!
    !! * `GEOCOM_IMG_INTERNAL_MEMORY` – Internal memory module.
    !! * `GEOCOM_IMG_PC_CARD`         – External PC Card.
    !!
    !! ### GEOCOM_MOT_LOCK_STATUS
    !!
    !! * `GEOCOM_MOT_LOCKED_OUT` – Locked out.
    !! * `GEOCOM_MOT_LOCKED_IN`  – Locked in.
    !! * `GEOCOM_MOT_PREDICTION` – Prediction mode.
    !!
    !! ### GEOCOM_MOT_MODE
    !!
    !! * `GEOCOM_MOT_POSIT`   – Configured for relative positioning.
    !! * `GEOCOM_MOT_OCONST`  – Configured for constant speed.
    !! * `GEOCOM_MOT_MANUPOS` – Configured for manual positioning (default setting).
    !! * `GEOCOM_MOT_LOCK`    – Configured as “Lock-in” controller.
    !! * `GEOCOM_MOT_BREAK`   – Configured as “Brake” controller.
    !! * `GEOCOM_MOT_TERM`    – Terminates the controller task.
    !!
    !! ### GEOCOM_MOT_STOPMODE
    !!
    !! * `GEOCOM_MOT_NORMAL`   – Slow down with current acceleration.
    !! * `GEOCOM_MOT_SHUTDOWN` – Slow down by switch off power supply.
    !!
    !! ### GEOCOM_SUP_AUTO_POWER
    !!
    !! * `GEOCOM_SUP_POWER_DISABLED` – Instrument remains on.
    !! * `GEOCOM_SUP_POWER_OFF`      – Turns off mechanism.
    !!
    !! ### GEOCOM_TMC_FACE
    !!
    !! * `GEOCOM_TMC_FACE_1` – Position 1 of telescope.
    !! * `GEOCOM_TMC_FACE_2` – Position 2 of telescope.
    !!
    !! ### GEOCOM_TMC_FACE_DEF
    !!
    !! * `GEOCOM_TMC_FACE_NORMAL` – Face in normal position.
    !! * `GEOCOM_TMC_FACE_TURN`   – Face turned.
    !!
    !! ### GEOCOM_TMC_INCLINE_PRG
    !!
    !! * `GEOCOM_TMC_MEA_INC`      – Use sensor (a priori sigma).
    !! * `GEOCOM_TMC_AUTO_INC`     – Automatic mode (sensor/plane).
    !! * `GEOCOM_TMC_PLANE_INC`    – Use plane (a priori sigma).
    !!
    !! ### GEOCOM_TMC_MEASURE_PRG
    !!
    !! * `GEOCOM_TMC_STOP`         – Stop measurement program.
    !! * `GEOCOM_TMC_DEF_DIST`     – Default distance measurement program.
    !! * `GEOCOM_TMC_CLEAR`        – `GEOCOM_TMC_STOP` and clear data.
    !! * `GEOCOM_TMC_SIGNAL`       – Signal measurement (test function).
    !! * `GEOCOM_TMC_DO_MEASURE`   – (Re-)start measurement task.
    !! * `GEOCOM_TMC_RTRK_DIST`    – Distance-TRK measurement program.
    !! * `GEOCOM_TMC_RED_TRK_DIST` – Reflectorless tracking.
    !! * `GEOCOM_TMC_FREQUENCY`    – Frequency measurement (test).
    !!
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
        integer            :: baud    = GEOCOM_COM_BAUD_19200    !! GeoCOM baud rate enumerator (`GEOCOM_COM_BAUD_RATE`).
        integer            :: grc     = GRC_OK                   !! Last GeoCOM return code.
        integer            :: rc      = E_NONE                   !! Last DMPACK return code.
        logical            :: verbose = .false.                  !! Print error messages to standard error.
        type(request_type) :: request                            !! Last request sent to sensor.
        type(tty_type)     :: tty                                !! TTY type for serial connection.
    contains
        private
        ! Private class methods.
        procedure         :: output       => geocom_output       !! Outputs error message in verbose mode.
        procedure         :: reset        => geocom_reset        !! Resets request and error codes.
        ! Public class methods.
        procedure, public :: baud_rate    => geocom_baud_rate    !! Returns current baud rate.
        procedure, public :: close        => geocom_close        !! Closes TTY.
        procedure, public :: code         => geocom_code         !! Returns last GeoCOM code.
        procedure, public :: error        => geocom_error        !! Returns last DMPACK error.
        procedure, public :: last_request => geocom_last_request !! Returns last request sent to sensor.
        procedure, public :: message      => geocom_message      !! Returns message associated with GeoCOM code.
        procedure, public :: open         => geocom_open         !! Opens TTY.
        procedure, public :: path         => geocom_path         !! Returns TTY path.
        procedure, public :: send         => geocom_send         !! Sends raw request to sensor.
        ! Public GeoCOM-specific methods.
        procedure, public :: abort_download                => geocom_abort_download
        procedure, public :: abort_list                    => geocom_abort_list
        procedure, public :: beep_alarm                    => geocom_beep_alarm
        procedure, public :: beep_normal                   => geocom_beep_normal
        procedure, public :: beep_off                      => geocom_beep_off
        procedure, public :: beep_on                       => geocom_beep_on
        procedure, public :: change_face                   => geocom_change_face
        procedure, public :: delete                        => geocom_delete
        procedure, public :: do_measure                    => geocom_do_measure
        procedure, public :: download                      => geocom_download
        procedure, public :: fine_adjust                   => geocom_fine_adjust
        procedure, public :: get_angle                     => geocom_get_angle
        procedure, public :: get_angle_complete            => geocom_get_angle_complete
        procedure, public :: get_angle_correction          => geocom_get_angle_correction
        procedure, public :: get_atmospheric_correction    => geocom_get_atmospheric_correction
        procedure, public :: get_atmospheric_ppm           => geocom_get_atmospheric_ppm
        procedure, public :: get_atr_error                 => geocom_get_atr_error
        procedure, public :: get_atr_setting               => geocom_get_atr_setting
        procedure, public :: get_binary_mode               => geocom_get_binary_mode
        procedure, public :: get_config                    => geocom_get_config
        procedure, public :: get_coordinate                => geocom_get_coordinate
        procedure, public :: get_date_time                 => geocom_get_date_time
        procedure, public :: get_date_time_centi           => geocom_get_date_time_centi
        procedure, public :: get_device_config             => geocom_get_device_config
        procedure, public :: get_double_precision          => geocom_get_double_precision
        procedure, public :: get_edm_mode                  => geocom_get_edm_mode
        procedure, public :: get_egl_intensity             => geocom_get_egl_intensity
        procedure, public :: get_face                      => geocom_get_face
        procedure, public :: get_fine_adjust_mode          => geocom_get_fine_adjust_mode
        procedure, public :: get_full_measurement          => geocom_get_full_measurement
        procedure, public :: get_geocom_version            => geocom_get_geocom_version
        procedure, public :: get_geometric_ppm             => geocom_get_geometric_ppm
        procedure, public :: get_height                    => geocom_get_height
        procedure, public :: get_image_config              => geocom_get_image_config
        procedure, public :: get_inclination_correction    => geocom_get_inclination_correction
        procedure, public :: get_inclination_error         => geocom_get_inclination_error
        procedure, public :: get_instrument_name           => geocom_get_instrument_name
        procedure, public :: get_instrument_number         => geocom_get_instrument_number
        procedure, public :: get_internal_temperature      => geocom_get_internal_temperature
        procedure, public :: get_lock_status               => geocom_get_lock_status
        procedure, public :: get_measurement_program       => geocom_get_measurement_program
        procedure, public :: get_power                     => geocom_get_power
        procedure, public :: get_prism_constant            => geocom_get_prism_constant
        procedure, public :: get_prism_definition          => geocom_get_prism_definition
       !procedure, public :: get_prism_type                => geocom_get_prism_type
       !procedure, public :: get_prism_type_v2             => geocom_get_prism_type_v2
       !procedure, public :: get_quick_distance            => geocom_get_quick_distance
       !procedure, public :: get_reduced_atr_fov           => geocom_get_reduced_atr_fov
       !procedure, public :: get_reflectorless_class       => geocom_get_reflectorless_class
       !procedure, public :: get_refraction_mode           => geocom_get_refraction_mode
       !procedure, public :: get_search_area               => geocom_get_search_area
       !procedure, public :: get_signal                    => geocom_get_signal
       !procedure, public :: get_simple_coordinates        => geocom_get_simple_coordinates
       !procedure, public :: get_simple_measurement        => geocom_get_simple_measurement
       !procedure, public :: get_slope_distance_correction => geocom_get_slope_distance_correction
       !procedure, public :: get_software_version          => geocom_get_software_version
       !procedure, public :: get_station                   => geocom_get_station
       !procedure, public :: get_target_type               => geocom_get_target_type
       !procedure, public :: get_timeout                   => geocom_get_timeout
       !procedure, public :: get_tolerance                 => geocom_get_tolerance
       !procedure, public :: get_user_atr_mode             => geocom_get_user_atr_mode
       !procedure, public :: get_user_lock_mode            => geocom_get_user_lock_mode
       !procedure, public :: get_user_prism_definition     => geocom_get_user_prism_definition
       !procedure, public :: get_user_spiral               => geocom_get_user_spiral
       !procedure, public :: list                          => geocom_list
       !procedure, public :: lock_in                       => geocom_lock_in
       !procedure, public :: measure_distance_angle        => geocom_measure_distance_angle
        procedure, public :: null                          => geocom_null
       !procedure, public :: ps_enable_range               => geocom_ps_enable_range
       !procedure, public :: ps_search_next                => geocom_ps_search_next
       !procedure, public :: ps_search_window              => geocom_ps_search_window
       !procedure, public :: ps_set_range                  => geocom_ps_set_range
       !procedure, public :: search                        => geocom_search
       !procedure, public :: search_target                 => geocom_search_target
       !procedure, public :: set_angle_correction          => geocom_set_angle_correction
       !procedure, public :: set_atmospheric_correction    => geocom_set_atmospheric_correction
       !procedure, public :: set_atmospheric_ppm           => geocom_set_atmospheric_ppm
       !procedure, public :: set_atr_mode                  => geocom_set_atr_mode
       !procedure, public :: set_binary_mode               => geocom_set_binary_mode
       !procedure, public :: set_config                    => geocom_set_config
       !procedure, public :: set_date_time                 => geocom_set_date_time
       !procedure, public :: set_distance                  => geocom_set_distance
       !procedure, public :: set_double_precision          => geocom_set_double_precision
       !procedure, public :: set_edm_mode                  => geocom_set_edm_mode
       !procedure, public :: set_egl_intensity             => geocom_set_egl_intensity
       !procedure, public :: set_fine_adjust_mode          => geocom_set_fine_adjust_mode
       !procedure, public :: set_geometric_ppm             => geocom_set_geometric_ppm
       !procedure, public :: set_height                    => geocom_set_height
       !procedure, public :: set_image_config              => geocom_set_image_config
       !procedure, public :: set_inclination_correction    => geocom_set_inclination_correction
       !procedure, public :: set_laser_pointer             => geocom_set_laser_pointer
       !procedure, public :: set_measurement_program       => geocom_set_measurement_program
       !procedure, public :: set_orientation               => geocom_set_orientation
       !procedure, public :: set_position                  => geocom_set_position
       !procedure, public :: set_positioning_timeout       => geocom_set_positioning_timeout
       !procedure, public :: set_prism_constant            => geocom_set_prism_constant
       !procedure, public :: set_prism_type                => geocom_set_prism_type
       !procedure, public :: set_prism_type_v2             => geocom_set_prism_type_v2
       !procedure, public :: set_reduced_atr_fov           => geocom_set_reduced_atr_fov
       !procedure, public :: set_refraction_mode           => geocom_set_refraction_mode
       !procedure, public :: set_search_area               => geocom_set_search_area
       !procedure, public :: set_station                   => geocom_set_station
       !procedure, public :: set_target_type               => geocom_set_target_type
       !procedure, public :: set_tolerance                 => geocom_set_tolerance
       !procedure, public :: set_user_atr_mode             => geocom_set_user_atr_mode
       !procedure, public :: set_user_lock_mode            => geocom_set_user_lock_mode
       !procedure, public :: set_user_prism_definition     => geocom_set_user_prism_definition
       !procedure, public :: set_user_spiral               => geocom_set_user_spiral
       !procedure, public :: set_velocity                  => geocom_set_velocity
       !procedure, public :: setup_download                => geocom_setup_download
       !procedure, public :: setup_list                    => geocom_setup_list
       !procedure, public :: start_controller              => geocom_start_controller
       !procedure, public :: stop_controller               => geocom_stop_controller
       !procedure, public :: switch_off                    => geocom_switch_off
       !procedure, public :: switch_on                     => geocom_switch_on
       !procedure, public :: take_image                    => geocom_take_image
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
    private :: geocom_output
    private :: geocom_reset
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
    private :: geocom_get_angle_correction
    private :: geocom_get_atmospheric_correction
    private :: geocom_get_atmospheric_ppm
    private :: geocom_get_atr_error
    private :: geocom_get_atr_setting
    private :: geocom_get_binary_mode
    private :: geocom_get_config
    private :: geocom_get_coordinate
    private :: geocom_get_date_time
    private :: geocom_get_date_time_centi
    private :: geocom_get_device_config
    private :: geocom_get_double_precision
    private :: geocom_get_edm_mode
    private :: geocom_get_egl_intensity
    private :: geocom_get_face
    private :: geocom_get_fine_adjust_mode
    private :: geocom_get_full_measurement
    private :: geocom_get_geocom_version
    private :: geocom_get_geometric_ppm
    private :: geocom_get_height
    private :: geocom_get_image_config
    private :: geocom_get_inclination_correction
    private :: geocom_get_inclination_error
    private :: geocom_get_instrument_name
    private :: geocom_get_instrument_number
    private :: geocom_get_internal_temperature
    private :: geocom_get_lock_status
    private :: geocom_get_measurement_program
    private :: geocom_get_power
    private :: geocom_get_prism_constant
    private :: geocom_get_prism_definition
   !private :: geocom_get_prism_type
   !private :: geocom_get_prism_type_v2
   !private :: geocom_get_quick_distance
   !private :: geocom_get_reduced_atr_fov
   !private :: geocom_get_reflectorless_class
   !private :: geocom_get_refraction_mode
   !private :: geocom_get_search_area
   !private :: geocom_get_signal
   !private :: geocom_get_simple_coordinates
   !private :: geocom_get_simple_measurement
   !private :: geocom_get_slope_distance_correction
   !private :: geocom_get_software_version
   !private :: geocom_get_station
   !private :: geocom_get_target_type
   !private :: geocom_get_timeout
   !private :: geocom_get_tolerance
   !private :: geocom_get_user_atr_mode
   !private :: geocom_get_user_lock_mode
   !private :: geocom_get_user_prism_definition
   !private :: geocom_get_user_spiral
   !private :: geocom_list
   !private :: geocom_lock_in
   !private :: geocom_measure_distance_angle
    private :: geocom_null
   !private :: geocom_ps_enable_range
   !private :: geocom_ps_search_next
   !private :: geocom_ps_search_window
   !private :: geocom_ps_set_range
   !private :: geocom_search
   !private :: geocom_search_target
   !private :: geocom_set_angle_correction
   !private :: geocom_set_atmospheric_correction
   !private :: geocom_set_atmospheric_ppm
   !private :: geocom_set_atr_mode
   !private :: geocom_set_binary_mode
   !private :: geocom_set_config
   !private :: geocom_set_date_time
   !private :: geocom_set_distance
   !private :: geocom_set_double_precision
   !private :: geocom_set_edm_mode
   !private :: geocom_set_egl_intensity
   !private :: geocom_set_fine_adjust_mode
   !private :: geocom_set_geometric_ppm
   !private :: geocom_set_height
   !private :: geocom_set_image_config
   !private :: geocom_set_inclination_correction
   !private :: geocom_set_laser_pointer
   !private :: geocom_set_measurement_program
   !private :: geocom_set_orientation
   !private :: geocom_set_position
   !private :: geocom_set_positioning_timeout
   !private :: geocom_set_prism_constant
   !private :: geocom_set_prism_type
   !private :: geocom_set_prism_type_v2
   !private :: geocom_set_reduced_atr_fov
   !private :: geocom_set_refraction_mode
   !private :: geocom_set_search_area
   !private :: geocom_set_station
   !private :: geocom_set_target_type
   !private :: geocom_set_tolerance
   !private :: geocom_set_user_atr_mode
   !private :: geocom_set_user_lock_mode
   !private :: geocom_set_user_prism_definition
   !private :: geocom_set_user_spiral
   !private :: geocom_set_velocity
   !private :: geocom_setup_download
   !private :: geocom_setup_list
   !private :: geocom_start_controller
   !private :: geocom_stop_controller
   !private :: geocom_switch_off
   !private :: geocom_switch_on
   !private :: geocom_take_image
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
        character(len=:), allocatable             :: message !! Message associated with last GeoCOM return code.

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
        !! * `GEOCOM_COM_BAUD_19200`  –  19200 baud (instrument default).
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
        !! * `E_SYSTEM` if setting the TTY attributes failed.
        !!
        use :: dm_file, only: dm_file_exists

        integer, parameter :: WAIT_TIME = 3 !! Retry wait time [sec].

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
                call this%output(rc, 'TTY already connected')
                exit tty_block
            end if

            ! Initialise TTY type.
            call this%reset()

            this%verbose = .false.
            if (present(verbose)) this%verbose = verbose

            ! Validate and set baud rate.
            this%baud = dm_geocom_type_validated(GEOCOM_COM_BAUD_RATE, baud_rate, error=rc)

            if (dm_is_error(rc)) then
                call this%output(rc, 'invalid baud rate')
                exit tty_block
            end if

            ! Verify TTY device exists.
            rc = E_NOT_FOUND
            if (.not. dm_file_exists(path)) then
                call this%output(rc, 'TTY ' // trim(path) // ' not found')
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
                if (dm_is_ok(rc)) exit

                call this%output(rc, 'failed to open TTY ' // trim(path) // ' (attempt ' // &
                                     dm_itoa(i + 1) // ' of ' // dm_itoa(n + 1) // ')')

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

        if (dm_is_error(this%rc)) then
            call this%output(this%rc, 'invalid request parameters detected in request ' // request%name)
        end if

        tty_block: block
            ! Verify that TTY is not connected yet.
            rc = E_IO
            if (.not. dm_tty_connected(this%tty)) then
                call this%output(rc, 'TTY not connected')
                exit tty_block
            end if

            ! Set initial response errors.
            rc = dm_request_set_response_error(request, E_INCOMPLETE)

            if (dm_is_error(rc)) then
                call this%output(rc, 'failed to initialize responses')
                exit tty_block
            end if

            ! Prepare request.
            request%timestamp = dm_time_now()

            ! Flush buffers and send request to sensor.
            rc = dm_tty_flush(this%tty)
            rc = dm_tty_write(this%tty, request)

            if (dm_is_error(rc)) then
                call this%output(rc, 'failed to write to TTY ' // trim(this%tty%path))
                exit tty_block
            end if

            ! Read response from sensor.
            rc = dm_tty_read(this%tty, request)

            if (dm_is_error(rc)) then
                call this%output(rc, 'failed to read from TTY ' // trim(this%tty%path))
                exit tty_block
            end if

            ! Parse raw response and extract response values.
            rc = dm_regex_request(request)

            if (dm_is_error(rc)) then
                call this%output(rc, 'regular expression pattern of request ' // &
                                     trim(request%name) // ' does not match')
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
    ! PRIVATE METHODS.
    ! **************************************************************************
    subroutine geocom_output(this, error, message)
        !! Outputs error message to `stderr` if verbose mode is enabled.
        class(geocom_class), intent(inout) :: this    !! GeoCOM object.
        integer,             intent(in)    :: error   !! DMPACK error code.
        character(len=*),    intent(in)    :: message !! Error message.

        if (.not. this%verbose) return
        call dm_error_out(error, message)
    end subroutine geocom_output

    subroutine geocom_reset(this)
        !! Resets object: clears return codes and last request.
        class(geocom_class), intent(inout) :: this !! GeoCOM object.

        this%rc      = E_NONE
        this%grc     = GRC_OK
        this%request = request_type()
    end subroutine geocom_reset

    ! **************************************************************************
    ! PUBLIC GEOCOM METHODS.
    ! **************************************************************************
    subroutine geocom_abort_download(this, delay)
        !! Sends *FTR_AbortDownload* request to sensor. Aborts or ends the file
        !! download command.
        class(geocom_class), intent(inout)        :: this  !! GeoCOM object.
        integer,             intent(in), optional :: delay !! Request delay [msec].

        type(request_type) :: request

        call this%reset()
        call dm_geocom_api_request_abort_download(request)
        call this%send(request, delay)
    end subroutine geocom_abort_download

    subroutine geocom_abort_list(this, delay)
        !! Sends *FTR_AbortList* request to sensor. Aborts or ends the file
        !! list command.
        class(geocom_class), intent(inout)        :: this  !! GeoCOM object.
        integer,             intent(in), optional :: delay !! Request delay [msec].

        type(request_type) :: request

        call this%reset()
        call dm_geocom_api_request_abort_list(request)
        call this%send(request, delay)
    end subroutine geocom_abort_list

    subroutine geocom_beep_alarm(this, delay)
        !! Sends *BMM_BeepAlarm* request to sensor. Outputs an alarm signal
        !! (triple beep).
        class(geocom_class), intent(inout)        :: this  !! GeoCOM object.
        integer,             intent(in), optional :: delay !! Request delay [msec].

        type(request_type) :: request

        call this%reset()
        call dm_geocom_api_request_beep_alarm(request)
        call this%send(request, delay)
    end subroutine geocom_beep_alarm

    subroutine geocom_beep_normal(this, delay)
        !! Sends *BMM_BeepNormal* request to sensor. Outputs an alarm signal
        !! (single beep).
        class(geocom_class), intent(inout)        :: this  !! GeoCOM object.
        integer,             intent(in), optional :: delay !! Request delay [msec].

        type(request_type) :: request

        call this%reset()
        call dm_geocom_api_request_beep_normal(request)
        call this%send(request, delay)
    end subroutine geocom_beep_normal

    subroutine geocom_beep_off(this, delay)
        !! Sends *IOS_BeepOff* request to sensor. Stops an active beep signal.
        class(geocom_class), intent(inout)        :: this  !! GeoCOM object.
        integer,             intent(in), optional :: delay !! Request delay [msec].

        type(request_type) :: request

        call this%reset()
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

        call this%reset()

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
        integer            :: rc1, rc2
        type(request_type) :: request

        call this%reset()

        pos_mode_ = dm_geocom_type_validated(GEOCOM_AUT_POSMODE, pos_mode, verbose=this%verbose, error=rc1)
        atr_mode_ = dm_geocom_type_validated(GEOCOM_AUT_ATRMODE, atr_mode, verbose=this%verbose, error=rc2)
        this%rc   = max(rc1, rc2)

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
        integer            :: rc1, rc2
        type(request_type) :: request

        call this%reset()

        device_type_ = dm_geocom_type_validated(GEOCOM_FTR_DEVICETYPE, device_type, verbose=this%verbose, error=rc1)
        file_type_   = dm_geocom_type_validated(GEOCOM_FTR_FILETYPE,   file_type,   verbose=this%verbose, error=rc2)
        this%rc      = max(rc1, rc2)

        day_   = max(0, min(255, day))
        month_ = max(0, min(255, month))
        year_  = max(0, min(255, year))

        call dm_geocom_api_request_delete(request, device_type_, file_type_, day_, month_, year_, file_name)
        call this%send(request, delay)

        if (present(nfiles)) call dm_request_get(this%request, 'nfiles', nfiles, default=0)
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
        integer            :: rc1, rc2
        type(request_type) :: request

        call this%reset()

        tmc_prog_ = dm_geocom_type_validated(GEOCOM_TMC_MEASURE_PRG, tmc_prog, verbose=this%verbose, error=rc1)
        inc_mode_ = dm_geocom_type_validated(GEOCOM_TMC_INCLINE_PRG, inc_mode, verbose=this%verbose, error=rc2)
        this%rc   = max(rc1, rc2)

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

        call this%reset()

        block_number_ = max(0, min(65535, block_number))

        call dm_geocom_api_request_download(request, block_number_)
        call this%send(request, delay)

        call dm_request_get(this%request, 'blockval', block_value,  default=achar(0))
        call dm_request_get(this%request, 'blocklen', block_length, default=0)
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

        call this%reset()
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

        call this%reset()

        inc_mode_ = dm_geocom_type_validated(GEOCOM_TMC_INCLINE_PRG, inc_mode, verbose=this%verbose, error=this%rc)

        call dm_geocom_api_request_get_angle(request, inc_mode_)
        call this%send(request, delay)

        call dm_request_get(this%request, 'hz', hz, default=0.0_r8)
        call dm_request_get(this%request, 'v',  v,  default=0.0_r8)
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
        integer(kind=i8),    intent(out), optional :: angle_time     !! Moment of measurement [msec].
        real(kind=r8),       intent(out), optional :: trans_inc      !! Transverse axis inclination [rad].
        real(kind=r8),       intent(out), optional :: long_inc       !! Longitude axis inclidation [rad].
        real(kind=r8),       intent(out), optional :: inc_accuracy   !! Inclination accuracy [rad].
        integer(kind=i8),    intent(out), optional :: inc_time       !! Moment of measurement [msec].
        integer,             intent(out), optional :: face           !! Face position of telescope (`GEOCOM_TMC_FACE`).
        integer,             intent(in),  optional :: inc_mode       !! Inclination measurement mode (`GEOCOM_TMC_INCLINE_PRG`).
        integer,             intent(in),  optional :: delay          !! Request delay [msec].

        integer            :: inc_mode_
        type(request_type) :: request

        call this%reset()

        inc_mode_ = dm_geocom_type_validated(GEOCOM_TMC_INCLINE_PRG, inc_mode, verbose=this%verbose, error=this%rc)

        call dm_geocom_api_request_get_angle_complete(request, inc_mode_)
        call this%send(request, delay)

        call dm_request_get(this%request, 'hz', hz, default=0.0_r8)
        call dm_request_get(this%request, 'v',  v,  default=0.0_r8)

        if (present(angle_accuracy)) call dm_request_get(this%request, 'angacc',  angle_accuracy, default=0.0_r8)
        if (present(angle_time))     call dm_request_get(this%request, 'angtime', angle_time,     default=0_i8)
        if (present(trans_inc))      call dm_request_get(this%request, 'xinc',    trans_inc,      default=0.0_r8)
        if (present(long_inc))       call dm_request_get(this%request, 'linc',    long_inc,       default=0.0_r8)
        if (present(inc_accuracy))   call dm_request_get(this%request, 'incacc',  inc_accuracy,   default=0.0_r8)
        if (present(inc_time))       call dm_request_get(this%request, 'inctime', inc_time,       default=0_i8)
        if (present(face))           call dm_request_get(this%request, 'face',    face,           default=0)
    end subroutine geocom_get_angle_complete

    subroutine geocom_get_angle_correction(this, incline, stand_axis, collimation, tilt_axis, delay)
        !! Sends *TMC_GetAngSwitch* request to sensor. The function returns the
        !! angular correction status.
        class(geocom_class), intent(inout)         :: this        !! GeoCOM object.
        logical,             intent(out), optional :: incline     !! Inclination correction enabled.
        logical,             intent(out), optional :: stand_axis  !! Standing axis correction enabled.
        logical,             intent(out), optional :: collimation !! Collimation error correction enabled.
        logical,             intent(out), optional :: tilt_axis   !! Tilting axis correction enabled.
        integer,             intent(in),  optional :: delay       !! Request delay [msec].

        type(request_type) :: request

        call this%reset()
        call dm_geocom_api_request_get_angle_correction(request)
        call this%send(request, delay)

        if (present(incline))     call dm_request_get(this%request, 'inccor', incline,     default=.false.)
        if (present(stand_axis))  call dm_request_get(this%request, 'stdcor', stand_axis,  default=.false.)
        if (present(collimation)) call dm_request_get(this%request, 'colcor', collimation, default=.false.)
        if (present(tilt_axis))   call dm_request_get(this%request, 'tilcor', tilt_axis,   default=.false.)
    end subroutine geocom_get_angle_correction

    subroutine geocom_get_atmospheric_correction(this, lambda, pressure, dry_temp, wet_temp, delay)
        !! Sends *TMC_GetAtmCorr* request to sensor. The function returns the
        !! atmospheric correction parameters.
        class(geocom_class), intent(inout)         :: this     !! GeoCOM object.
        real(kind=r8),       intent(out), optional :: lambda   !! Wave length of the EDM transmitter [m].
        real(kind=r8),       intent(out), optional :: pressure !! Atmospheric pressure [mbar].
        real(kind=r8),       intent(out), optional :: dry_temp !! Dry temperature [°C].
        real(kind=r8),       intent(out), optional :: wet_temp !! Wet temperature [°C].
        integer,             intent(in),  optional :: delay    !! Request delay [msec].

        type(request_type) :: request

        call this%reset()
        call dm_geocom_api_request_get_atmospheric_correction(request)
        call this%send(request, delay)

        if (present(lambda))   call dm_request_get(this%request, 'lambda',   lambda,   default=0.0_r8)
        if (present(pressure)) call dm_request_get(this%request, 'pressure', pressure, default=0.0_r8)
        if (present(dry_temp)) call dm_request_get(this%request, 'drytemp',  dry_temp, default=0.0_r8)
        if (present(wet_temp)) call dm_request_get(this%request, 'wettemp',  wet_temp, default=0.0_r8)
    end subroutine geocom_get_atmospheric_correction

    subroutine geocom_get_atmospheric_ppm(this, ppm, delay)
        !! Sends *TMC_GetAtmPpm* request to sensor. The function returns the
        !! atmospheric ppm correction factor in `ppm`.
        class(geocom_class), intent(inout)        :: this  !! GeoCOM object.
        real(kind=r8),       intent(out)          :: ppm   !! Atmospheric ppm correction factor [ppm].
        integer,             intent(in), optional :: delay !! Request delay [msec].

        type(request_type) :: request

        call this%reset()
        call dm_geocom_api_request_get_atmospheric_ppm(request)
        call this%send(request, delay)
        call dm_request_get(this%request, 'atmppm', ppm, default=0.0_r8)
    end subroutine geocom_get_atmospheric_ppm

    subroutine geocom_get_atr_error(this, error, delay)
        !! Sends *TMC_IfDataAzeCorrError* request to sensor. The function returns the
        !! ATR error status in `error`.
        class(geocom_class), intent(inout)         :: this  !! GeoCOM object.
        logical,             intent(out)           :: error !! ATR correction error occured.
        integer,             intent(in),  optional :: delay !! Request delay [msec].

        type(request_type) :: request

        call this%reset()
        call dm_geocom_api_request_get_atr_error(request)
        call this%send(request, delay)
        call dm_request_get(this%request, 'atrerr', error, default=.false.)
    end subroutine geocom_get_atr_error

    subroutine geocom_get_atr_setting(this, setting, delay)
        !! Sends *BAP_GetATRSetting* request to sensor. The function returns
        !! the current ATR Low-Vis mode in `setting`.
        class(geocom_class), intent(inout)         :: this    !! GeoCOM object.
        integer,             intent(out)           :: setting !! ATR setting (`GEOCOM_BAP_ATRSETTING`).
        integer,             intent(in),  optional :: delay   !! Request delay [msec].

        type(request_type) :: request

        call this%reset()
        call dm_geocom_api_request_get_atr_setting(request)
        call this%send(request, delay)
        call dm_request_get(this%request, 'atrset', setting, default=0)
    end subroutine geocom_get_atr_setting

    subroutine geocom_get_binary_mode(this, enabled, delay)
        !! Sends *COM_GetBinaryAvailable* request to sensor. The function returns the
        !! binary attribute of the server in `enabled`.
        class(geocom_class), intent(inout)         :: this    !! GeoCOM object.
        logical,             intent(out)           :: enabled !! Binary operation is enabled.
        integer,             intent(in),  optional :: delay   !! Request delay [msec].

        type(request_type) :: request

        call this%reset()
        call dm_geocom_api_request_get_binary_mode(request)
        call this%send(request, delay)
        call dm_request_get(this%request, 'binmode', enabled, default=.false.)
    end subroutine geocom_get_binary_mode

    subroutine geocom_get_config(this, auto_power, timeout, delay)
        !! Send *SUP_GetConfig* request to sensor. The function returns the
        !! power management configuration status. The power timeout `timeout`
        !! specifies the time after which the device switches into the mode
        !! indicated by `auto_power`.
        class(geocom_class), intent(inout)         :: this       !! GeoCOM object.
        integer,             intent(out)           :: auto_power !! Currently activated shut-down mode (`GEOCOM_SUP_AUTO_POWER`).
        integer,             intent(out)           :: timeout    !! Power timeout [msec].
        integer,             intent(in),  optional :: delay      !! Request delay [msec].

        type(request_type) :: request

        call this%reset()
        call dm_geocom_api_request_get_config(request)
        call this%send(request, delay)
        call dm_request_get(this%request, 'autopwr', auto_power, default=0)
        call dm_request_get(this%request, 'pwrtime', timeout,    default=0)
    end subroutine geocom_get_config

    subroutine geocom_get_coordinate(this, easting, northing, height, time, cont_easting, cont_northing, &
                                     cont_height, cont_time, wait_time, inc_mode, delay)
        !! Sends *TMC_GetCoordinate* request to sensor. The function returns
        !! the coordinates of a measured point.
        !!
        !! The API call conducts an angle and, in dependence of the selected
        !! `inc_mode`, an inclination measurement, and then calculates the
        !! coordinates of the measured point with the last distance.
        !!
        !! The argument `wait_time` specifies the delay to wait for the
        !! distance measurement to finish. Single and tracking measurements are
        !! supported. The quality of the result is returned in the GeoCOM
        !! return code.
        class(geocom_class), intent(inout)         :: this          !! GeoCOM object.
        real(kind=r8),       intent(out), optional :: easting       !! E coordinate [m].
        real(kind=r8),       intent(out), optional :: northing      !! N coordinate [m]
        real(kind=r8),       intent(out), optional :: height        !! H coordinate [m].
        integer(kind=i8),    intent(out), optional :: time          !! Timestamp of distance measurement [msec].
        real(kind=r8),       intent(out), optional :: cont_easting  !! E coordinate (continuously) [m].
        real(kind=r8),       intent(out), optional :: cont_northing !! N coordinate (continuously) [m].
        real(kind=r8),       intent(out), optional :: cont_height   !! H coordinate (continuously) [m].
        integer(kind=i8),    intent(out), optional :: cont_time     !! Timestamp of continuous measurement [msec].
        integer,             intent(in),  optional :: wait_time     !! Delay to wait for the distance measurement to finish [msec].
        integer,             intent(in),  optional :: inc_mode      !! Inclination measurement mode (`GEOCOM_TMC_INCLINE_PRG`).
        integer,             intent(in),  optional :: delay         !! Request delay [msec].

        integer            :: inc_mode_, wait_time_
        type(request_type) :: request

        call this%reset()

        wait_time_ = 0
        if (present(wait_time)) wait_time_ = max(0, wait_time)

        inc_mode_ = dm_geocom_type_validated(GEOCOM_TMC_INCLINE_PRG, inc_mode, verbose=this%verbose, error=this%rc)

        call dm_geocom_api_request_get_coordinate(request, wait_time_, inc_mode_)
        call this%send(request, delay)

        if (present(easting))       call dm_request_get(this%request, 'east',    easting,       default=0.0_r8)
        if (present(northing))      call dm_request_get(this%request, 'north',   northing,      default=0.0_r8)
        if (present(height))        call dm_request_get(this%request, 'height',  height,        default=0.0_r8)
        if (present(time))          call dm_request_get(this%request, 'ctime',   time,          default=0_i8)
        if (present(cont_easting))  call dm_request_get(this%request, 'eastc',   cont_easting,  default=0.0_r8)
        if (present(cont_northing)) call dm_request_get(this%request, 'northc',  cont_northing, default=0.0_r8)
        if (present(cont_height))   call dm_request_get(this%request, 'heightc', cont_height,   default=0.0_r8)
        if (present(cont_time))     call dm_request_get(this%request, 'ctimec',  cont_time,     default=0_i8)
    end subroutine geocom_get_coordinate

    subroutine geocom_get_date_time(this, year, month, day, hour, minute, second, delay)
        !! Sends *CSV_GetDateTime* request to sensor. The function returns
        !! current date and time of the instrument.
        class(geocom_class), intent(inout)         :: this   !! GeoCOM object.
        integer,             intent(out), optional :: year   !! Year.
        integer,             intent(out), optional :: month  !! Month.
        integer,             intent(out), optional :: day    !! Day of month.
        integer,             intent(out), optional :: hour   !! Hours.
        integer,             intent(out), optional :: minute !! Minutes.
        integer,             intent(out), optional :: second !! Seconds.
        integer,             intent(in),  optional :: delay  !! Request delay [msec].

        character          :: month_, day_, hour_, minute_, second_
        integer            :: year_
        type(request_type) :: request

        call this%reset()
        call dm_geocom_api_request_get_date_time(request)
        call this%send(request, delay)

        call dm_request_get(this%request, 'year',   year_,   default=0)
        call dm_request_get(this%request, 'month',  month_,  default=achar(0))
        call dm_request_get(this%request, 'day',    day_,    default=achar(0))
        call dm_request_get(this%request, 'hour',   hour_,   default=achar(0))
        call dm_request_get(this%request, 'minute', minute_, default=achar(0))
        call dm_request_get(this%request, 'second', second_, default=achar(0))

        if (present(year))   year   = year_
        if (present(month))  month  = iachar(month_)
        if (present(day))    day    = iachar(day_)
        if (present(hour))   hour   = iachar(hour_)
        if (present(minute)) minute = iachar(minute_)
        if (present(second)) second = iachar(second_)
    end subroutine geocom_get_date_time

    subroutine geocom_get_date_time_centi(this, year, month, day, hour, minute, second, csecond, delay)
        !! Sends *CSV_GetDateTimeCentiSec* request to sensor. The function
        !! returns the current date and time of the instrument, including
        !! centiseconds.
        class(geocom_class), intent(inout)         :: this    !! GeoCOM object.
        integer,             intent(out), optional :: year    !! Year.
        integer,             intent(out), optional :: month   !! Month.
        integer,             intent(out), optional :: day     !! Day of month.
        integer,             intent(out), optional :: hour    !! Hours.
        integer,             intent(out), optional :: minute  !! Minutes.
        integer,             intent(out), optional :: second  !! Seconds.
        integer,             intent(out), optional :: csecond !! Centiseconds.
        integer,             intent(in),  optional :: delay   !! Request delay [msec].

        type(request_type) :: request

        call this%reset()
        call dm_geocom_api_request_get_date_time_centi(request)
        call this%send(request, delay)

        if (present(year))    call dm_request_get(this%request, 'year',    year,    default=0)
        if (present(month))   call dm_request_get(this%request, 'month',   month,   default=0)
        if (present(day))     call dm_request_get(this%request, 'day',     day,     default=0)
        if (present(hour))    call dm_request_get(this%request, 'hour',    hour,    default=0)
        if (present(minute))  call dm_request_get(this%request, 'minute',  minute,  default=0)
        if (present(second))  call dm_request_get(this%request, 'second',  second,  default=0)
        if (present(csecond)) call dm_request_get(this%request, 'csecond', csecond, default=0)
    end subroutine geocom_get_date_time_centi

    subroutine geocom_get_device_config(this, device_class, device_type, delay)
        !! Sends *CSV_GetDeviceConfig* request to sensor. The function returns
        !! the instrument configuration.
        class(geocom_class), intent(inout)        :: this         !! GeoCOM object.
        integer,             intent(out)          :: device_class !! Device precision class (`GEOCOM_TPS_DEVICE_CLASS`).
        integer,             intent(out)          :: device_type  !! Device configuration type (`GEOCOM_TPS_DEVICE_TYPE`).
        integer,             intent(in), optional :: delay        !! Request delay [msec].

        type(request_type) :: request

        call this%reset()
        call dm_geocom_api_request_get_device_config(request)
        call this%send(request, delay)

        call dm_request_get(this%request, 'devclass', device_class, default=0)
        call dm_request_get(this%request, 'devtype',  device_type,  default=0)
    end subroutine geocom_get_device_config

    subroutine geocom_get_double_precision(this, ndigits, delay)
        !! Sends *COM_GetDoublePrecision* request to sensor. The function
        !! returns the double precision setting – the number of digits to the
        !! right of the decimal point – when double floating-point values are
        !! transmitted in `ndigits`.
        class(geocom_class), intent(inout)        :: this    !! GeoCOM object.
        integer,             intent(out)          :: ndigits !! Number of digits to the right of the decimal point.
        integer,             intent(in), optional :: delay   !! Request delay [msec].

        type(request_type) :: request

        call this%reset()
        call dm_geocom_api_request_get_double_precision(request)
        call this%send(request, delay)
        call dm_request_get(this%request, 'ndigits', ndigits, default=0)
    end subroutine geocom_get_double_precision

    subroutine geocom_get_edm_mode(this, edm_mode, delay)
        !! Sends *TMC_GetEdmMode* request to sensor. The function returns the
        !! EDM measurement mode in `edm_mode`.
        class(geocom_class), intent(inout)        :: this     !! GeoCOM object.
        integer,             intent(out)          :: edm_mode !! EDM mode (`GEOCOM_EDM_MODE`).
        integer,             intent(in), optional :: delay    !! Request delay [msec].

        type(request_type) :: request

        call this%reset()
        call dm_geocom_api_request_get_edm_mode(request)
        call this%send(request, delay)
        call dm_request_get(this%request, 'edmmode', edm_mode, default=0)
    end subroutine geocom_get_edm_mode

    subroutine geocom_get_egl_intensity(this, intensity, delay)
        !! Sends *EDM_GetEglIntensity* request to sensor. The function returns
        !! the value of the intensity of the electronic guide light (EGL) in
        !! `intensity`.
        class(geocom_class), intent(inout)        :: this      !! GeoCOM object.
        integer,             intent(out)          :: intensity !! EDM EGL intensity (`GEOCOM_EDM_EGLINTENSITY_TYPE`).
        integer,             intent(in), optional :: delay     !! Request delay [msec].

        type(request_type) :: request

        call this%reset()
        call dm_geocom_api_request_get_egl_intensity(request)
        call this%send(request, delay)
        call dm_request_get(this%request, 'eglint', intensity, default=0)
    end subroutine geocom_get_egl_intensity

    subroutine geocom_get_face(this, face, delay)
        !! Sends *TMC_GetFace* request to sensor. The function returns the face
        !! of the current telescope position in `face`.
        class(geocom_class), intent(inout)        :: this  !! GeoCOM object.
        integer,             intent(out)          :: face  !! Telescope face (`GEOCOM_TMC_FACE`).
        integer,             intent(in), optional :: delay !! Request delay [msec].

        type(request_type) :: request

        call this%reset()
        call dm_geocom_api_request_get_face(request)
        call this%send(request, delay)
        call dm_request_get(this%request, 'face', face, default=0)
    end subroutine geocom_get_face

    subroutine geocom_get_fine_adjust_mode(this, adjust_mode, delay)
        !! Sends *AUT_GetFineAdjustMode* to sensor. The function returns the
        !! fine adjustment positioning mode in `adjust_mode`.
        class(geocom_class), intent(inout)        :: this        !! GeoCOM object.
        integer,             intent(out)          :: adjust_mode !! Fine adjustment positioning mode (`GEOCOM_AUT_ADJMODE`).
        integer,             intent(in), optional :: delay       !! Request delay [msec].

        type(request_type) :: request

        call this%reset()
        call dm_geocom_api_request_get_fine_adjust_mode(request)
        call this%send(request, delay)
        call dm_request_get(this%request, 'adjmode', adjust_mode, default=0)
    end subroutine geocom_get_fine_adjust_mode

    subroutine geocom_get_full_measurement(this, hz, v, angle_accuracy, cross_incl, length_incl, incl_accuracy, &
                                           slope_dist, dist_time, wait_time, inc_mode, delay)
        !! Sends *TMC_GetFullMeas* request to sensor. The function returns
        !! angle, inclination, and distance measurement data, including
        !! accuracy and measurement time.
        !!
        !! This command does not issue a new distance measurement. A distance
        !! measurement has to be started in advance. If the distance is valid,
        !! the function ignores `wait_time` and returns the results
        !! immediately. If no valid distance is available, and the measurement
        !! unit is not activated, the angle measurement result is retuned after
        !! the waiting time.
        class(geocom_class), intent(inout)         :: this           !! GeoCOM object.
        real(kind=r8),       intent(out), optional :: hz             !! Horizontal angle [rad].
        real(kind=r8),       intent(out), optional :: v              !! Vertical angle [rad].
        real(kind=r8),       intent(out), optional :: angle_accuracy !! Accuracy of angles [rad].
        real(kind=r8),       intent(out), optional :: cross_incl     !! Cross inclination [rad].
        real(kind=r8),       intent(out), optional :: length_incl    !! Length inclination [rad].
        real(kind=r8),       intent(out), optional :: incl_accuracy  !! Inclination accuracy [rad].
        real(kind=r8),       intent(out), optional :: slope_dist     !! Distance measurement [m].
        real(kind=r8),       intent(out), optional :: dist_time      !! Time of distance measurement [msec].
        integer,             intent(in),  optional :: wait_time      !! Delay to wait for the distance measurement to finish [msec].
        integer,             intent(in),  optional :: inc_mode       !! Inclination measurement mode (`GEOCOM_TMC_INCLINE_PRG`).
        integer,             intent(in),  optional :: delay          !! Request delay [msec].

        integer            :: inc_mode_, wait_time_
        type(request_type) :: request

        call this%reset()

        wait_time_ = 0
        if (present(wait_time)) wait_time_ = max(0, wait_time)

        inc_mode_ = dm_geocom_type_validated(GEOCOM_TMC_INCLINE_PRG, inc_mode, verbose=this%verbose, error=this%rc)

        call dm_geocom_api_request_get_full_measurement(request, wait_time_, inc_mode_)
        call this%send(request, delay)

        if (present(hz))             call dm_request_get(this%request, 'hz',       hz,             default=0.0_r8)
        if (present(v))              call dm_request_get(this%request, 'v',        v,              default=0.0_r8)
        if (present(angle_accuracy)) call dm_request_get(this%request, 'angacc',   angle_accuracy, default=0.0_r8)
        if (present(cross_incl))     call dm_request_get(this%request, 'xinc',     cross_incl,     default=0.0_r8)
        if (present(length_incl))    call dm_request_get(this%request, 'linc',     length_incl,    default=0.0_r8)
        if (present(incl_accuracy))  call dm_request_get(this%request, 'incacc',   incl_accuracy,  default=0.0_r8)
        if (present(slope_dist))     call dm_request_get(this%request, 'sdist',    slope_dist,     default=0.0_r8)
        if (present(dist_time))      call dm_request_get(this%request, 'disttime', dist_time,      default=0.0_r8)
    end subroutine geocom_get_full_measurement

    subroutine geocom_get_geocom_version(this, release, version, subversion, delay)
        !! Sends *COM_GetSWVersion* request to sensor. The function gets the
        !! GeoCOM server software version of the instrument.
        class(geocom_class), intent(inout)         :: this       !! GeoCOM object.
        integer,             intent(out), optional :: release    !! GeoCOM software release.
        integer,             intent(out), optional :: version    !! GeoCOM software version.
        integer,             intent(out), optional :: subversion !! GeoCOM software sub-version.
        integer,             intent(in),  optional :: delay      !! Request delay [msec].

        type(request_type) :: request

        call this%reset()
        call dm_geocom_api_request_get_geocom_version(request)
        call this%send(request, delay)

        if (present(release))    call dm_request_get(this%request, 'gcrel', release,    default=0)
        if (present(version))    call dm_request_get(this%request, 'gcver', version,    default=0)
        if (present(subversion)) call dm_request_get(this%request, 'gcsub', subversion, default=0)
    end subroutine geocom_get_geocom_version

    subroutine geocom_get_geometric_ppm(this, enabled, scale_factor, offset, height_ppm, individual_ppm, delay)
        !! Sends *TMC_GeoPpm* request to sensor. The function returns the
        !! geometric ppm correction factor.
        class(geocom_class), intent(inout)         :: this           !! GeoCOM object.
        logical,             intent(out), optional :: enabled        !! State of geometric ppm calculation.
        real(kind=r8),       intent(out), optional :: scale_factor   !! Scale factor on central meridian.
        real(kind=r8),       intent(out), optional :: offset         !! Offset from central meridian [m].
        real(kind=r8),       intent(out), optional :: height_ppm     !! Height above reference ppm value [ppm].
        real(kind=r8),       intent(out), optional :: individual_ppm !! Individual ppm value [ppm].
        integer,             intent(in),  optional :: delay          !! Request delay [msec].

        type(request_type) :: request

        call this%reset()
        call dm_geocom_api_request_get_geometric_ppm(request)
        call this%send(request, delay)

        if (present(enabled))        call dm_request_get(this%request, 'geomauto', enabled,        default=.false.)
        if (present(scale_factor))   call dm_request_get(this%request, 'scalefcm', scale_factor,   default=0.0_r8)
        if (present(offset))         call dm_request_get(this%request, 'offsetcm', offset,         default=0.0_r8)
        if (present(height_ppm))     call dm_request_get(this%request, 'hredppm',  height_ppm,     default=0.0_r8)
        if (present(individual_ppm)) call dm_request_get(this%request, 'indippm',  individual_ppm, default=0.0_r8)
    end subroutine geocom_get_geometric_ppm

    subroutine geocom_get_height(this, height, delay)
        !! Sends *TMC_GetHeight* request to sensor. The function gets the
        !! current reflector height.
        class(geocom_class), intent(inout)        :: this   !! GeoCOM object.
        real(kind=r8),       intent(out)          :: height !! Reflector height [m].
        integer,             intent(in), optional :: delay  !! Request delay [msec].

        type(request_type) :: request

        call this%reset()
        call dm_geocom_api_request_get_height(request)
        call this%send(request, delay)
        call dm_request_get(this%request, 'rheight', height, default=0.0_r8)
    end subroutine geocom_get_height

    subroutine geocom_get_image_config(this, mem_type, image_number, quality, sub_func, file_prefix, delay)
        !! Sends *IMG_GetTccConfig* request to sensor. The function reads the
        !! current image configuration. The response `sub_func` is a binary
        !! combination of the following settings:
        !!
        !! * `1` – Test image.
        !! * `2` – Automatic exposure time selection.
        !! * `4` – Two-times sub-sampling.
        !! * `8` – Four-times sub-sampling.
        !!
        !! If no memory device type is passed, `GEOCOM_IMG_INTERNAL_MEMORY` is
        !! selected. On error, the file prefix string is allocated but empty.
        !! The maximum string length is 20 characters
        !! (`GEOCOM_IMG_MAX_FILE_PREFIX_LEN`).
        use :: dm_regex, only: dm_regex_response_string

        class(geocom_class),           intent(inout)        :: this         !! GeoCOM object.
        integer,                       intent(out)          :: image_number !! Actual image number.
        integer,                       intent(out)          :: quality      !! JPEG compression quality factor (0 to 100).
        integer,                       intent(out)          :: sub_func     !! Binary combination of sub-function number.
        character(len=:), allocatable, intent(out)          :: file_prefix  !! File name prefix.
        integer,                       intent(in), optional :: mem_type     !! Memory device type (`GEOCOM_IMG_MEM_TYPE`).
        integer,                       intent(in), optional :: delay        !! Request delay [msec].

        integer            :: mem_type_
        type(request_type) :: request

        call this%reset()

        mem_type_ = dm_geocom_type_validated(GEOCOM_IMG_MEM_TYPE, mem_type, verbose=this%verbose, error=this%rc)

        call dm_geocom_api_request_get_image_config(request, mem_type_)
        call this%send(request, delay)

        call dm_request_get(this%request, 'imageno', image_number, default=0)
        call dm_request_get(this%request, 'quality', quality,      default=0)
        call dm_request_get(this%request, 'subfunc', sub_func,     default=0)

        if (dm_is_error(this%rc)) then
            file_prefix = ''
            return
        end if

        this%rc = dm_regex_response_string(this%request, 'fnprefix', file_prefix)
    end subroutine geocom_get_image_config

    subroutine geocom_get_inclination_correction(this, enabled, delay)
        !! Sends *TMC_GetInclineSwitch* request to sensor. The function gets
        !! the dual-axis compensator status.
        class(geocom_class), intent(inout)        :: this    !! GeoCOM object.
        logical,             intent(out)          :: enabled !! Compensator is enabled.
        integer,             intent(in), optional :: delay   !! Request delay [msec].

        type(request_type) :: request

        call this%reset()
        call dm_geocom_api_request_get_inclination_correction(request)
        call this%send(request, delay)
        call dm_request_get(this%request, 'inccor', enabled, default=.false.)
    end subroutine geocom_get_inclination_correction

    subroutine geocom_get_inclination_error(this, error, delay)
        !! Sends *TMC_IfDataIncCorrError* request to sensor. The function gets
        !! the inclination error status. If `error` is `.true.`, the last
        !! measurement is not incline-corrected.
        class(geocom_class), intent(inout)        :: this  !! GeoCOM object.
        logical,             intent(out)          :: error !! Last measurement not incline-corrected.
        integer,             intent(in), optional :: delay !! Request delay [msec].

        type(request_type) :: request

        call this%reset()
        call dm_geocom_api_request_get_inclination_error(request)
        call this%send(request, delay)
        call dm_request_get(this%request, 'incerr', error, default=.false.)
    end subroutine geocom_get_inclination_error

    subroutine geocom_get_instrument_name(this, name, delay)
        !! Sends *CSV_GetInstrumentName* request to sensor. The function gets
        !! the Leica-specific instrument name. On error, the name is allocated
        !! but empty.
        use :: dm_regex, only: dm_regex_response_string

        class(geocom_class),           intent(inout)        :: this  !! GeoCOM object.
        character(len=:), allocatable, intent(out)          :: name  !! Instrument name
        integer,                       intent(in), optional :: delay !! Request delay [msec].

        type(request_type) :: request

        call this%reset()
        call dm_geocom_api_request_get_instrument_name(request)
        call this%send(request, delay)

        if (dm_is_error(this%rc)) then
            name = ''
            return
        end if

        this%rc = dm_regex_response_string(this%request, 'name', name)
    end subroutine geocom_get_instrument_name

    subroutine geocom_get_instrument_number(this, number, delay)
        !! Sends *CSV_GetInstrumentNo* request to sensor. The function gets the
        !! factory defined instrument number.
        class(geocom_class), intent(inout)         :: this   !! GeoCOM object.
        integer,             intent(out)           :: number !! Serial number of the instrument.
        integer,             intent(in),  optional :: delay  !! Request delay [msec].

        type(request_type) :: request

        call this%reset()
        call dm_geocom_api_request_get_instrument_number(request)
        call this%send(request, delay)
        call dm_request_get(this%request, 'serialno', number, default=0)
    end subroutine geocom_get_instrument_number

    subroutine geocom_get_internal_temperature(this, temp, delay)
        !! Sends *CSV_GetIntTemp* request to sensor. The function gets the
        !! internal temperature of the instrument, measured on the mainboard
        !! side.
        class(geocom_class), intent(inout)        :: this  !! GeoCOM object.
        real(kind=r8),       intent(out)          :: temp  !! Instrument temperature [°C].
        integer,             intent(in), optional :: delay !! Request delay [msec].

        type(request_type) :: request

        call this%reset()
        call dm_geocom_api_request_get_internal_temperature(request)
        call this%send(request, delay)
        call dm_request_get(this%request, 'temp', temp, default=0.0_r8)
    end subroutine geocom_get_internal_temperature

    subroutine geocom_get_lock_status(this, status, delay)
        !! Sends *MOT_ReadLockStatus* request to sensor. The function gets the
        !! condition of the Lock-in control.
        class(geocom_class), intent(inout)        :: this   !! GeoCOM object.
        integer,             intent(out)          :: status !! Lock status (`GEOCOM_MOT_LOCK_STATUS`).
        integer,             intent(in), optional :: delay  !! Request delay [msec].

        type(request_type) :: request

        call this%reset()
        call dm_geocom_api_request_get_lock_status(request)
        call this%send(request, delay)
        call dm_request_get(this%request, 'lockstat', status, default=0)
    end subroutine geocom_get_lock_status

    subroutine geocom_get_measurement_program(this, prg, delay)
        !! Sends *BAP_GetMeasPrg* request to sensor. The function gets the
        !! distance measurement program of the instrument.
        class(geocom_class), intent(inout)         :: this  !! GeoCOM object.
        integer,             intent(out)           :: prg   !! Measurement program (`GEOCOM_BAP_USER_MEASPRG`).
        integer,             intent(in),  optional :: delay !! Request delay [msec].

        type(request_type) :: request

        call this%reset()
        call dm_geocom_api_request_get_measurement_program(request)
        call this%send(request, delay)
        call dm_request_get(this%request, 'measprg', prg, default=0)
    end subroutine geocom_get_measurement_program

    subroutine geocom_get_power(this, battery_life, power_source, power_suggest, delay)
        !! Sends *CSV_CheckPower* request to sensor. The function returns the
        !! available power.
        class(geocom_class), intent(inout)         :: this          !! GeoCOM object.
        integer,             intent(out), optional :: battery_life  !! Battery capacity [%].
        integer,             intent(out), optional :: power_source  !! Power source (`GEOCOM_CSV_POWER_PATH`).
        integer,             intent(out), optional :: power_suggest !! Not supported (`GEOCOM_CSV_POWER_PATH`).
        integer,             intent(in),  optional :: delay         !! Request delay [msec].

        type(request_type) :: request

        call this%reset()
        call dm_geocom_api_request_get_power(request)
        call this%send(request, delay)

        if (present(battery_life))  call dm_request_get(this%request, 'battlife', battery_life,  default=0)
        if (present(power_source))  call dm_request_get(this%request, 'powsrc',   power_source,  default=0)
        if (present(power_suggest)) call dm_request_get(this%request, 'powsug',   power_suggest, default=0)
    end subroutine geocom_get_power

    subroutine geocom_get_prism_constant(this, constant, delay)
        !! Sends *TMC_GetPrismCorr* request to sensor. The function gets the
        !! prism constant.
        class(geocom_class), intent(inout)        :: this     !! GeoCOM object.
        real(kind=r8),       intent(out)          :: constant !! Prism correction constant [m].
        integer,             intent(in), optional :: delay    !! Request delay [msec].

        type(request_type) :: request

        call this%reset()
        call dm_geocom_api_request_get_prism_constant(request)
        call this%send(request, delay)
        call dm_request_get(this%request, 'reflcor', constant, default=0.0_r8)
    end subroutine geocom_get_prism_constant

    subroutine geocom_get_prism_definition(this, prism_type, prism_name, prism_const, delay)
        !! Sends *BAP_GetPrismDef* request to sensor. Returns the default prism
        !! definition. The maximum prism name length is 16 characters
        !! (`GEOCOM_BAP_PRISMNAME_LEN`). On error, the string is allocated but
        !! empty.
        use :: dm_regex, only: dm_regex_response_string

        class(geocom_class),           intent(inout)        :: this        !! GeoCOM object.
        integer,                       intent(in)           :: prism_type  !! Prism type (`GEOCOM_BAP_PRISMTYPE`).
        character(len=:), allocatable, intent(out)          :: prism_name  !! Prism name.
        real(kind=r8),                 intent(out)          :: prism_const !! Prism correction constant [m].
        integer,                       intent(in), optional :: delay       !! Request delay [msec].

        integer            :: prism_type_
        type(request_type) :: request

        call this%reset()
        prism_type_ = dm_geocom_type_validated(GEOCOM_BAP_PRISMTYPE, prism_type, verbose=this%verbose, error=this%rc)
        call dm_geocom_api_request_get_prism_definition(request, prism_type_)
        call this%send(request, delay)

        call dm_request_get(this%request, 'prsmcor', prism_const, default=0.0_r8)

        if (dm_is_error(this%rc)) then
            prism_name = ''
            return
        end if

        this%rc = dm_regex_response_string(this%request, 'prsmname', prism_name)
    end subroutine geocom_get_prism_definition

    subroutine geocom_null(this, delay)
        !! Sends *COM_NullProc* request to sensor. API call for checking the
        !! communication.
        class(geocom_class), intent(inout)        :: this  !! GeoCOM object.
        integer,             intent(in), optional :: delay !! Request delay [msec].

        type(request_type) :: request

        call this%reset()
        call dm_geocom_api_request_null(request)
        call this%send(request, delay)
    end subroutine geocom_null
end module dm_geocom
