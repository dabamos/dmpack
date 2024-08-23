! Author:  Philipp Engel
! Licence: ISC
module dm_geocom
    !! Object-oriented GeoCOM API for Fortran.
    !!
    !! The API provided by DMPACK does not follow the official Leica GeoCOM API
    !! for C/C++ and Visual Basic. Functions are given more memorable names,
    !! without any sub-system prefix. Structured types have been removed
    !! altogether. If invalid parameters are passed to the GeoCOM methods, they
    !! will be replaced with their default values, and an error message is
    !! printed in verbose mode.
    !!
    !! Open the serial port with argument `verbose` set to `.true.` to output
    !! error messages to standard error.
    !!
    !! The following example opens the TTY `/dev/ttyUSB0` at 115,200 baud, calls
    !! the null procedure of the instrument (`COM_NullProc`), and outputs return
    !! code and associated error message:
    !!
    !! ```fortran
    !! integer            :: rc     ! DMPACK return code.
    !! type(geocom_class) :: geocom ! GeoCOM object.
    !!
    !! ! Open connection to instrument, quit on error.
    !! call geocom%open('/dev/ttyUSB0', GEOCOM_COM_BAUD_115200, verbose=.true., error=rc)
    !! call dm_error_out(rc, fatal=.true.)
    !!
    !! ! Call remote procedure COM_NullProc and output result.
    !! call geocom%null()
    !! print '(i0, ": ", a)', geocom%code(), geocom%message()
    !!
    !! ! Close connection.
    !! call geocom%close()
    !! ```
    !!
    !! ## API
    !!
    !! | GeoCOM API                  | DMPACK API                      |
    !! |-----------------------------|---------------------------------|
    !! | `AUS_GetUserAtrState`       | `get_user_atr_mode`             |
    !! | `AUS_GetUserLockState`      | `get_user_lock_mode`            |
    !! | `AUS_SetUserAtrState`       | `set_user_atr_mode`             |
    !! | `AUS_SetUserLockState`      | `set_user_lock_mode`            |
    !! | `AUT_ChangeFace`            | `change_face`                   |
    !! | `AUT_FineAdjust`            | `fine_adjust`                   |
    !! | `AUT_GetFineAdjustMode`     | `get_fine_adjust_mode`          |
    !! | `AUT_GetSearchArea`         | `get_search_area`               |
    !! | `AUT_GetUserSpiral`         | `get_user_spiral`               |
    !! | `AUT_LockIn`                | `lock_in`                       |
    !! | `AUT_MakePositioning`       | `set_position`                  |
    !! | `AUT_PS_EnableRange`        | `ps_enable_range`               |
    !! | `AUT_PS_SearchNext`         | `ps_search_next`                |
    !! | `AUT_PS_SearchWindow`       | `ps_search_window`              |
    !! | `AUT_PS_SetRange`           | `ps_set_range`                  |
    !! | `AUT_ReadTimeout`           | `get_timeout`                   |
    !! | `AUT_ReadTol`               | `get_tolerance`                 |
    !! | `AUT_Search`                | `get_search`                    |
    !! | `AUT_SetFineAdjustMode`     | `set_fine_adjust_mode`          |
    !! | `AUT_SetSearchArea`         | `set_search_area`               |
    !! | `AUT_SetTimeout`            | `set_positioning_timeout`       |
    !! | `AUT_SetTol`                | `set_tolerance`                 |
    !! | `AUT_SetUserSpiral`         | `set_user_spiral`               |
    !! | `BAP_GetATRSetting`         | `get_atr_setting`               |
    !! | `BAP_GetMeasPrg`            | `get_measurement_program`       |
    !! | `BAP_GetPrismDef`           | `get_prism_definition`          |
    !! | `BAP_GetRedATRFov`          | `get_reduced_atr_fov`           |
    !! | `BAP_GetTargetType`         | `get_target_type`               |
    !! | `BAP_GetUserPrismDef`       | `get_user_prism_definition`     |
    !! | `BAP_MeasDistanceAngle`     | `measure_distance_angle`        |
    !! | `BAP_SearchTarget`          | `search_target`                 |
    !! | `BAP_SetATRSetting`         | `set_atr_mode`                  |
    !! | `BAP_SetAtmCorr`            | `set_atmospheric_correction`    |
    !! | `BAP_SetAtmPpm`             | `set_atmospheric_ppm`           |
    !! | `BAP_SetMeasPrg`            | `set_measurement_program`       |
    !! | `BAP_SetPrismType`          | `set_prism_type`                |
    !! | `BAP_SetPrismType2`         | `set_prism_type_v2`             |
    !! | `BAP_SetRedATRFov`          | `set_reduced_atr_fov`           |
    !! | `BAP_SetTargetType`         | `set_target_type`               |
    !! | `BAP_SetUserPrismDef`       | `set_user_prism_definition`     |
    !! | `BMM_BeepAlarm`             | `beep_alarm`                    |
    !! | `BMM_BeepNormal`            | `beep_normal`                   |
    !! | `COM_GetBinaryAvailable`    | `get_binary_mode`               |
    !! | `COM_GetDoublePrecision`    | `get_double_precision`          |
    !! | `COM_GetSWVersion`          | `get_geocom_version`            |
    !! | `COM_NullProc`              | `null`                          |
    !! | `COM_SetBinaryAvailable`    | `set_binary_mode`               |
    !! | `COM_SetDoublePrecision`    | `set_double_precision`          |
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
    !! | `CSV_GetSWVersion`          | `get_software_version`          |
    !! | `CSV_SetDateTime`           | `set_date_time`                 |
    !! | `EDM_GetEglIntensity`       | `get_egl_intensity`             |
    !! | `EDM_Laserpointer`          | `set_laser_pointer`             |
    !! | `EDM_SetEglIntensity`       | `set_egl_intensity`             |
    !! | `FTR_AbortDownload`         | `abort_download`                |
    !! | `FTR_AbortList`             | `abort_list`                    |
    !! | `FTR_Delete`                | `delete`                        |
    !! | `FTR_Download`              | `download`                      |
    !! | `FTR_List`                  | `list`                          |
    !! | `FTR_SetupDownload`         | `setup_download`                |
    !! | `FTR_SetupList`             | `setup_list`                    |
    !! | `IMG_GetTccConfig`          | `get_image_config`              |
    !! | `IMG_SetTccConfig`          | `set_image_config`              |
    !! | `IMG_TakeTccImage`          | `take_image`                    |
    !! | `IOS_BeepOff`               | `beep_off`                      |
    !! | `IOS_BeepOn`                | `beep_on`                       |
    !! | `MOT_ReadLockStatus`        | `get_lock_status`               |
    !! | `MOT_SetVelocity`           | `set_velocity`                  |
    !! | `MOT_StartController`       | `start_controller`              |
    !! | `MOT_StopController`        | `stop_controller`               |
    !! | `SUP_GetConfig`             | `get_config`                    |
    !! | `SUP_SetConfig`             | `set_config`                    |
    !! | `TMC_DoMeasure`             | `do_measure`                    |
    !! | `TMC_GeoPpm`                | `get_geometric_ppm`             |
    !! | `TMC_GetAngSwitch`          | `get_angle_correction`          |
    !! | `TMC_GetAngle1`             | `get_angle_complete`            |
    !! | `TMC_GetAngle5`             | `get_angle`                     |
    !! | `TMC_GetAtmCorr`            | `get_atmospheric_correction`    |
    !! | `TMC_GetAtmPpm`             | `get_atmospheric_ppm`           |
    !! | `TMC_GetCoordinate`         | `get_coordinate`                |
    !! | `TMC_GetEdmMode`            | `get_edm_mode`                  |
    !! | `TMC_GetFace`               | `get_face`                      |
    !! | `TMC_GetFullMeas`           | `get_full_measurement`          |
    !! | `TMC_GetHeight`             | `get_height`                    |
    !! | `TMC_GetInclineSwitch`      | `get_inclination_correction`    |
    !! | `TMC_GetPrismCorr`          | `get_prism_constant`            |
    !! | `TMC_GetPrismType`          | `get_prism_type`                |
    !! | `TMC_GetPrismType2`         | `get_prism_type_v2`             |
    !! | `TMC_GetQuickDist`          | `get_quick_distance`            |
    !! | `TMC_GetRefractiveMethod`   | `get_refraction_mode`           |
    !! | `TMC_GetSignal`             | `get_signal`                    |
    !! | `TMC_GetSimpleCoord`        | `get_simple_coordinates`        |
    !! | `TMC_GetSimpleMea`          | `get_simple_measurement`        |
    !! | `TMC_GetSlopeDistCorr`      | `get_slope_distance_correction` |
    !! | `TMC_GetStation`            | `get_station`                   |
    !! | `TMC_IfDataAzeCorrError`    | `get_atr_error`                 |
    !! | `TMC_IfDataIncCorrError`    | `get_inclination_error`         |
    !! | `TMC_QuickDist`             | `get_quick_distance`            |
    !! | `TMC_SetAngSwitch`          | `set_angle_correction`          |
    !! | `TMC_SetEdmMode`            | `set_edm_mode`                  |
    !! | `TMC_SetGeoPpm`             | `set_geometric_ppm`             |
    !! | `TMC_SetHandDist`           | `set_distance`                  |
    !! | `TMC_SetHeight`             | `set_height`                    |
    !! | `TMC_SetInclineSwitch`      | `set_inclination_correction`    |
    !! | `TMC_SetOrientation`        | `set_orientation`               |
    !! | `TMC_SetPrismCorr`          | `set_prism_constant`            |
    !! | `TMC_SetRefractiveMethod`   | `set_refraction_mode`           |
    !! | `TMC_SetStation`            | `set_station`                   |
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
    !! * `GEOCOM_TMC_MEA_INC`   – Use sensor (a priori sigma).
    !! * `GEOCOM_TMC_AUTO_INC`  – Automatic mode (sensor/plane).
    !! * `GEOCOM_TMC_PLANE_INC` – Use plane (a priori sigma).
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
        procedure, public :: get_prism_type                => geocom_get_prism_type
        procedure, public :: get_prism_type_v2             => geocom_get_prism_type_v2
        procedure, public :: get_quick_distance            => geocom_get_quick_distance
        procedure, public :: get_reduced_atr_fov           => geocom_get_reduced_atr_fov
        procedure, public :: get_reflectorless_class       => geocom_get_reflectorless_class
        procedure, public :: get_refraction_mode           => geocom_get_refraction_mode
        procedure, public :: get_search_area               => geocom_get_search_area
        procedure, public :: get_signal                    => geocom_get_signal
        procedure, public :: get_simple_coordinates        => geocom_get_simple_coordinates
        procedure, public :: get_simple_measurement        => geocom_get_simple_measurement
        procedure, public :: get_slope_distance_correction => geocom_get_slope_distance_correction
        procedure, public :: get_software_version          => geocom_get_software_version
        procedure, public :: get_station                   => geocom_get_station
        procedure, public :: get_target_type               => geocom_get_target_type
        procedure, public :: get_timeout                   => geocom_get_timeout
        procedure, public :: get_tolerance                 => geocom_get_tolerance
        procedure, public :: get_user_atr_mode             => geocom_get_user_atr_mode
        procedure, public :: get_user_lock_mode            => geocom_get_user_lock_mode
        procedure, public :: get_user_prism_definition     => geocom_get_user_prism_definition
        procedure, public :: get_user_spiral               => geocom_get_user_spiral
        procedure, public :: list                          => geocom_list
        procedure, public :: lock_in                       => geocom_lock_in
        procedure, public :: measure_distance_angle        => geocom_measure_distance_angle
        procedure, public :: null                          => geocom_null
        procedure, public :: ps_enable_range               => geocom_ps_enable_range
        procedure, public :: ps_search_next                => geocom_ps_search_next
        procedure, public :: ps_search_window              => geocom_ps_search_window
        procedure, public :: ps_set_range                  => geocom_ps_set_range
        procedure, public :: search                        => geocom_search
        procedure, public :: search_target                 => geocom_search_target
        procedure, public :: set_angle_correction          => geocom_set_angle_correction
        procedure, public :: set_atmospheric_correction    => geocom_set_atmospheric_correction
        procedure, public :: set_atmospheric_ppm           => geocom_set_atmospheric_ppm
        procedure, public :: set_atr_mode                  => geocom_set_atr_mode
        procedure, public :: set_binary_mode               => geocom_set_binary_mode
        procedure, public :: set_config                    => geocom_set_config
        procedure, public :: set_date_time                 => geocom_set_date_time
        procedure, public :: set_distance                  => geocom_set_distance
        procedure, public :: set_double_precision          => geocom_set_double_precision
        procedure, public :: set_edm_mode                  => geocom_set_edm_mode
        procedure, public :: set_egl_intensity             => geocom_set_egl_intensity
        procedure, public :: set_fine_adjust_mode          => geocom_set_fine_adjust_mode
        procedure, public :: set_geometric_ppm             => geocom_set_geometric_ppm
        procedure, public :: set_height                    => geocom_set_height
        procedure, public :: set_image_config              => geocom_set_image_config
        procedure, public :: set_inclination_correction    => geocom_set_inclination_correction
        procedure, public :: set_laser_pointer             => geocom_set_laser_pointer
        procedure, public :: set_measurement_program       => geocom_set_measurement_program
        procedure, public :: set_orientation               => geocom_set_orientation
        procedure, public :: set_position                  => geocom_set_position
        procedure, public :: set_positioning_timeout       => geocom_set_positioning_timeout
        procedure, public :: set_prism_constant            => geocom_set_prism_constant
        procedure, public :: set_prism_type                => geocom_set_prism_type
        procedure, public :: set_prism_type_v2             => geocom_set_prism_type_v2
        procedure, public :: set_reduced_atr_fov           => geocom_set_reduced_atr_fov
        procedure, public :: set_refraction_mode           => geocom_set_refraction_mode
        procedure, public :: set_search_area               => geocom_set_search_area
        procedure, public :: set_station                   => geocom_set_station
        procedure, public :: set_target_type               => geocom_set_target_type
        procedure, public :: set_tolerance                 => geocom_set_tolerance
        procedure, public :: set_user_atr_mode             => geocom_set_user_atr_mode
        procedure, public :: set_user_lock_mode            => geocom_set_user_lock_mode
        procedure, public :: set_user_prism_definition     => geocom_set_user_prism_definition
        procedure, public :: set_user_spiral               => geocom_set_user_spiral
        procedure, public :: set_velocity                  => geocom_set_velocity
        procedure, public :: setup_download                => geocom_setup_download
        procedure, public :: setup_list                    => geocom_setup_list
        procedure, public :: start_controller              => geocom_start_controller
        procedure, public :: stop_controller               => geocom_stop_controller
        procedure, public :: switch_off                    => geocom_switch_off
        procedure, public :: switch_on                     => geocom_switch_on
        procedure, public :: take_image                    => geocom_take_image
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
    private :: geocom_get_prism_type
    private :: geocom_get_prism_type_v2
    private :: geocom_get_quick_distance
    private :: geocom_get_reduced_atr_fov
    private :: geocom_get_reflectorless_class
    private :: geocom_get_refraction_mode
    private :: geocom_get_search_area
    private :: geocom_get_signal
    private :: geocom_get_simple_coordinates
    private :: geocom_get_simple_measurement
    private :: geocom_get_slope_distance_correction
    private :: geocom_get_software_version
    private :: geocom_get_station
    private :: geocom_get_target_type
    private :: geocom_get_timeout
    private :: geocom_get_tolerance
    private :: geocom_get_user_atr_mode
    private :: geocom_get_user_lock_mode
    private :: geocom_get_user_prism_definition
    private :: geocom_get_user_spiral
    private :: geocom_list
    private :: geocom_lock_in
    private :: geocom_measure_distance_angle
    private :: geocom_null
    private :: geocom_ps_enable_range
    private :: geocom_ps_search_next
    private :: geocom_ps_search_window
    private :: geocom_ps_set_range
    private :: geocom_search
    private :: geocom_search_target
    private :: geocom_set_angle_correction
    private :: geocom_set_atmospheric_correction
    private :: geocom_set_atmospheric_ppm
    private :: geocom_set_atr_mode
    private :: geocom_set_binary_mode
    private :: geocom_set_config
    private :: geocom_set_date_time
    private :: geocom_set_distance
    private :: geocom_set_double_precision
    private :: geocom_set_edm_mode
    private :: geocom_set_egl_intensity
    private :: geocom_set_fine_adjust_mode
    private :: geocom_set_geometric_ppm
    private :: geocom_set_height
    private :: geocom_set_image_config
    private :: geocom_set_inclination_correction
    private :: geocom_set_laser_pointer
    private :: geocom_set_measurement_program
    private :: geocom_set_orientation
    private :: geocom_set_position
    private :: geocom_set_positioning_timeout
    private :: geocom_set_prism_constant
    private :: geocom_set_prism_type
    private :: geocom_set_prism_type_v2
    private :: geocom_set_reduced_atr_fov
    private :: geocom_set_refraction_mode
    private :: geocom_set_search_area
    private :: geocom_set_station
    private :: geocom_set_target_type
    private :: geocom_set_tolerance
    private :: geocom_set_user_atr_mode
    private :: geocom_set_user_lock_mode
    private :: geocom_set_user_prism_definition
    private :: geocom_set_user_spiral
    private :: geocom_set_velocity
    private :: geocom_setup_download
    private :: geocom_setup_list
    private :: geocom_start_controller
    private :: geocom_stop_controller
    private :: geocom_switch_off
    private :: geocom_switch_on
    private :: geocom_take_image
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

        integer :: grc_

        grc_ = this%grc
        if (present(grc)) grc_ = grc
        message = dm_geocom_error_message(grc_)
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
        !! The procedure returns the following error codes:
        !!
        !! * `E_EXIST` if TTY is already connected.
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

        integer :: baud_rate_, retries_
        integer :: i, rc

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

            ! Convert GeoCOM baud rate parameter to TTY baud rate parameter.
            rc = E_INVALID
            select case (baud_rate)
                case (GEOCOM_COM_BAUD_2400)
                    baud_rate_ = TTY_B2400
                case (GEOCOM_COM_BAUD_4800)
                    baud_rate_ = TTY_B4800
                case (GEOCOM_COM_BAUD_9600)
                    baud_rate_ = TTY_B9600
                case (GEOCOM_COM_BAUD_19200)
                    baud_rate_ = TTY_B19200
                case (GEOCOM_COM_BAUD_38400)
                    baud_rate_ = TTY_B38400
                case (GEOCOM_COM_BAUD_57600)
                    baud_rate_ = TTY_B57600
                case (GEOCOM_COM_BAUD_115200)
                    baud_rate_ = TTY_B115200
                case default
                    call this%output(rc, 'invalid baud rate')
                    exit tty_block
            end select

            ! Verify TTY device exists.
            rc = E_NOT_FOUND
            if (.not. dm_file_exists(path)) then
                call this%output(rc, 'TTY ' // trim(path) // ' not found')
                exit tty_block
            end if

            retries_ = 0
            if (present(retries)) retries_ = max(0, retries)

            ! Try to open TTY.
            do i = 0, retries_
                rc = dm_tty_open(tty       = this%tty, &
                                 path      = path, &
                                 baud_rate = baud_rate_, &
                                 byte_size = TTY_BYTE_SIZE8, &
                                 parity    = TTY_PARITY_NONE, &
                                 stop_bits = TTY_STOP_BITS1)
                if (dm_is_ok(rc)) exit

                call this%output(rc, 'failed to open TTY ' // trim(path) // ' (attempt ' // &
                                     dm_itoa(i + 1) // ' of ' // dm_itoa(retries_ + 1) // ')')

                ! Try again.
                if (i < retries_) call dm_sleep(WAIT_TIME)
            end do

            if (dm_is_error(rc)) call this%output(rc, 'could not open TTY ' // path)
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
        integer,             intent(in),  optional :: delay   !! Post-request delay [msec].
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
                call this%output(rc, 'regular expression pattern of request ' // trim(request%name) // &
                                     ' does not match response')
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
        integer,             intent(in), optional :: delay !! Post-request delay [msec].

        type(request_type) :: request

        call this%reset()
        call dm_geocom_api_request_abort_download(request)
        call this%send(request, delay)
    end subroutine geocom_abort_download

    subroutine geocom_abort_list(this, delay)
        !! Sends *FTR_AbortList* request to sensor. Aborts or ends the file
        !! list command.
        class(geocom_class), intent(inout)        :: this  !! GeoCOM object.
        integer,             intent(in), optional :: delay !! Post-request delay [msec].

        type(request_type) :: request

        call this%reset()
        call dm_geocom_api_request_abort_list(request)
        call this%send(request, delay)
    end subroutine geocom_abort_list

    subroutine geocom_beep_alarm(this, delay)
        !! Sends *BMM_BeepAlarm* request to sensor. Outputs an alarm signal
        !! (triple beep).
        class(geocom_class), intent(inout)        :: this  !! GeoCOM object.
        integer,             intent(in), optional :: delay !! Post-request delay [msec].

        type(request_type) :: request

        call this%reset()
        call dm_geocom_api_request_beep_alarm(request)
        call this%send(request, delay)
    end subroutine geocom_beep_alarm

    subroutine geocom_beep_normal(this, delay)
        !! Sends *BMM_BeepNormal* request to sensor. Outputs an alarm signal
        !! (single beep).
        class(geocom_class), intent(inout)        :: this  !! GeoCOM object.
        integer,             intent(in), optional :: delay !! Post-request delay [msec].

        type(request_type) :: request

        call this%reset()
        call dm_geocom_api_request_beep_normal(request)
        call this%send(request, delay)
    end subroutine geocom_beep_normal

    subroutine geocom_beep_off(this, delay)
        !! Sends *IOS_BeepOff* request to sensor. Stops an active beep signal.
        class(geocom_class), intent(inout)        :: this  !! GeoCOM object.
        integer,             intent(in), optional :: delay !! Post-request delay [msec].

        type(request_type) :: request

        call this%reset()
        call dm_geocom_api_request_beep_off(request)
        call this%send(request, delay)
    end subroutine geocom_beep_off

    subroutine geocom_beep_on(this, intensity, delay)
        !! Sends *IOS_BeepOn* request to sensor. Outputs continuous beep signal
        !! of given intensity (between 0 and 100). If no intensity is given,
        !! the default `GEOCOM_IOS_BEEP_STDINTENS` is used.
        class(geocom_class), intent(inout)        :: this      !! GeoCOM object.
        integer,             intent(in), optional :: intensity !! Intensity of beep, from 0 to 100.
        integer,             intent(in), optional :: delay     !! Post-request delay [msec].

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
        !! to inaccuracy. If set to `GEOCOM_AUT_PRECISE`, it tries to measure
        !! the exact inclination of the target. Tends to long positioning time.
        !!
        !! If `atr_mode` is `GEOCOM_AUT_POSITION`, uses conventional position to
        !! other face. If set to `GEOCOM_AUT_TARGET`, tries to position into a
        !! target in the destination area. This mode requires activated ATR.
        class(geocom_class), intent(inout)        :: this     !! GeoCOM object.
        integer,             intent(in)           :: pos_mode !! Position mode (`GEOCOM_AUT_POSMODE`).
        integer,             intent(in)           :: atr_mode !! ATR mode (`GEOCOM_AUT_ATRMODE`).
        integer,             intent(in), optional :: delay    !! Post-request delay [msec].

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
        integer,             intent(in),  optional :: delay       !! Post-request delay [msec].

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
        integer,             intent(in), optional :: delay    !! Post-request delay [msec].

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
        !! data. The `setup_download()` method has to be called first.
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
        integer,             intent(in), optional :: delay        !! Post-request delay [msec].

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
        !! procedure call. After positioning, the Lock mode will be active. The
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
        integer,             intent(in), optional :: delay     !! Post-request delay [msec].

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
        integer,             intent(in), optional :: delay    !! Post-request delay [msec].

        integer            :: inc_mode_
        type(request_type) :: request

        call this%reset()
        inc_mode_ = dm_geocom_type_validated(GEOCOM_TMC_INCLINE_PRG, inc_mode, verbose=this%verbose, error=this%rc)
        call dm_geocom_api_request_get_angle(request, inc_mode_)
        call this%send(request, delay)

        call dm_request_get(this%request, 'hz', hz, default=0.0_r8)
        call dm_request_get(this%request, 'v',  v,  default=0.0_r8)
    end subroutine geocom_get_angle

    subroutine geocom_get_angle_complete(this, hz, v, angle_accuracy, angle_time, cross_inc, length_inc, &
                                         inc_accuracy, inc_time, face, inc_mode, delay)
        !! Sends *TMC_GetAngle1* request to sensor. Performs a complete angle
        !! measurement. The procedure starts an angle and, depending on the
        !! configuration, an inclination measurement, and returns the results.
        !! This function sets inclination mode `GEOCOM_TMC_MEA_INC` by default.
        class(geocom_class), intent(inout)         :: this           !! GeoCOM object.
        real(kind=r8),       intent(out)           :: hz             !! Horizontal angle [rad].
        real(kind=r8),       intent(out)           :: v              !! Vertical angle [rad].
        real(kind=r8),       intent(out), optional :: angle_accuracy !! Accuracy of angles [rad].
        integer(kind=i8),    intent(out), optional :: angle_time     !! Moment of measurement [msec].
        real(kind=r8),       intent(out), optional :: cross_inc      !! Transverse axis inclination [rad].
        real(kind=r8),       intent(out), optional :: length_inc     !! Longitude axis inclidation [rad].
        real(kind=r8),       intent(out), optional :: inc_accuracy   !! Inclination accuracy [rad].
        integer(kind=i8),    intent(out), optional :: inc_time       !! Moment of measurement [msec].
        integer,             intent(out), optional :: face           !! Face position of telescope (`GEOCOM_TMC_FACE`).
        integer,             intent(in),  optional :: inc_mode       !! Inclination measurement mode (`GEOCOM_TMC_INCLINE_PRG`).
        integer,             intent(in),  optional :: delay          !! Post-request delay [msec].

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
        if (present(cross_inc))      call dm_request_get(this%request, 'xinc',    cross_inc,      default=0.0_r8)
        if (present(length_inc))     call dm_request_get(this%request, 'linc',    length_inc,     default=0.0_r8)
        if (present(inc_accuracy))   call dm_request_get(this%request, 'incacc',  inc_accuracy,   default=0.0_r8)
        if (present(inc_time))       call dm_request_get(this%request, 'inctime', inc_time,       default=0_i8)
        if (present(face))           call dm_request_get(this%request, 'face',    face,           default=0)
    end subroutine geocom_get_angle_complete

    subroutine geocom_get_angle_correction(this, incline, stand_axis, collimation, tilt_axis, delay)
        !! Sends *TMC_GetAngSwitch* request to sensor. The procedure returns the
        !! angular correction status.
        class(geocom_class), intent(inout)         :: this        !! GeoCOM object.
        logical,             intent(out), optional :: incline     !! Inclination correction enabled.
        logical,             intent(out), optional :: stand_axis  !! Standing axis correction enabled.
        logical,             intent(out), optional :: collimation !! Collimation error correction enabled.
        logical,             intent(out), optional :: tilt_axis   !! Tilting axis correction enabled.
        integer,             intent(in),  optional :: delay       !! Post-request delay [msec].

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
        !! Sends *TMC_GetAtmCorr* request to sensor. The procedure returns the
        !! atmospheric correction parameters.
        class(geocom_class), intent(inout)         :: this     !! GeoCOM object.
        real(kind=r8),       intent(out), optional :: lambda   !! Wave length of the EDM transmitter [m].
        real(kind=r8),       intent(out), optional :: pressure !! Atmospheric pressure [mbar].
        real(kind=r8),       intent(out), optional :: dry_temp !! Dry temperature [°C].
        real(kind=r8),       intent(out), optional :: wet_temp !! Wet temperature [°C].
        integer,             intent(in),  optional :: delay    !! Post-request delay [msec].

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
        !! Sends *TMC_GetAtmPpm* request to sensor. The procedure returns the
        !! atmospheric ppm correction factor in `ppm`.
        class(geocom_class), intent(inout)        :: this  !! GeoCOM object.
        real(kind=r8),       intent(out)          :: ppm   !! Atmospheric ppm correction factor [ppm].
        integer,             intent(in), optional :: delay !! Post-request delay [msec].

        type(request_type) :: request

        call this%reset()
        call dm_geocom_api_request_get_atmospheric_ppm(request)
        call this%send(request, delay)
        call dm_request_get(this%request, 'atmppm', ppm, default=0.0_r8)
    end subroutine geocom_get_atmospheric_ppm

    subroutine geocom_get_atr_error(this, error, delay)
        !! Sends *TMC_IfDataAzeCorrError* request to sensor. The procedure returns the
        !! ATR error status in `error`.
        class(geocom_class), intent(inout)        :: this  !! GeoCOM object.
        logical,             intent(out)          :: error !! ATR correction error occured.
        integer,             intent(in), optional :: delay !! Post-request delay [msec].

        type(request_type) :: request

        call this%reset()
        call dm_geocom_api_request_get_atr_error(request)
        call this%send(request, delay)
        call dm_request_get(this%request, 'atrerr', error, default=.false.)
    end subroutine geocom_get_atr_error

    subroutine geocom_get_atr_setting(this, setting, delay)
        !! Sends *BAP_GetATRSetting* request to sensor. The procedure returns
        !! the current ATR Low-Vis mode in `setting`.
        class(geocom_class), intent(inout)        :: this    !! GeoCOM object.
        integer,             intent(out)          :: setting !! ATR setting (`GEOCOM_BAP_ATRSETTING`).
        integer,             intent(in), optional :: delay   !! Post-request delay [msec].

        type(request_type) :: request

        call this%reset()
        call dm_geocom_api_request_get_atr_setting(request)
        call this%send(request, delay)
        call dm_request_get(this%request, 'atrset', setting, default=0)
    end subroutine geocom_get_atr_setting

    subroutine geocom_get_binary_mode(this, enabled, delay)
        !! Sends *COM_GetBinaryAvailable* request to sensor. The procedure returns the
        !! binary attribute of the server in `enabled`.
        class(geocom_class), intent(inout)        :: this    !! GeoCOM object.
        logical,             intent(out)          :: enabled !! Binary operation is enabled.
        integer,             intent(in), optional :: delay   !! Post-request delay [msec].

        type(request_type) :: request

        call this%reset()
        call dm_geocom_api_request_get_binary_mode(request)
        call this%send(request, delay)
        call dm_request_get(this%request, 'binmode', enabled, default=.false.)
    end subroutine geocom_get_binary_mode

    subroutine geocom_get_config(this, auto_power, timeout, delay)
        !! Send *SUP_GetConfig* request to sensor. The procedure returns the
        !! power management configuration status. The power timeout `timeout`
        !! specifies the time after which the device switches into the mode
        !! indicated by `auto_power`.
        class(geocom_class), intent(inout)        :: this       !! GeoCOM object.
        integer,             intent(out)          :: auto_power !! Currently activated shut-down mode (`GEOCOM_SUP_AUTO_POWER`).
        integer,             intent(out)          :: timeout    !! Power timeout [msec].
        integer,             intent(in), optional :: delay      !! Post-request delay [msec].

        type(request_type) :: request

        call this%reset()
        call dm_geocom_api_request_get_config(request)
        call this%send(request, delay)
        call dm_request_get(this%request, 'autopwr', auto_power, default=0)
        call dm_request_get(this%request, 'pwrtime', timeout,    default=0)
    end subroutine geocom_get_config

    subroutine geocom_get_coordinate(this, easting, northing, height, time, cont_easting, cont_northing, &
                                     cont_height, cont_time, wait_time, inc_mode, delay)
        !! Sends *TMC_GetCoordinate* request to sensor. The procedure returns
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
        integer,             intent(in),  optional :: delay         !! Post-request delay [msec].

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
        !! Sends *CSV_GetDateTime* request to sensor. The procedure returns
        !! current date and time of the instrument.
        class(geocom_class), intent(inout)         :: this   !! GeoCOM object.
        integer,             intent(out), optional :: year   !! Year.
        integer,             intent(out), optional :: month  !! Month.
        integer,             intent(out), optional :: day    !! Day of month.
        integer,             intent(out), optional :: hour   !! Hours.
        integer,             intent(out), optional :: minute !! Minutes.
        integer,             intent(out), optional :: second !! Seconds.
        integer,             intent(in),  optional :: delay  !! Post-request delay [msec].

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
        !! Sends *CSV_GetDateTimeCentiSec* request to sensor. The procedure
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
        integer,             intent(in),  optional :: delay   !! Post-request delay [msec].

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
        !! Sends *CSV_GetDeviceConfig* request to sensor. The procedure returns
        !! the instrument configuration.
        class(geocom_class), intent(inout)        :: this         !! GeoCOM object.
        integer,             intent(out)          :: device_class !! Device precision class (`GEOCOM_TPS_DEVICE_CLASS`).
        integer,             intent(out)          :: device_type  !! Device configuration type (`GEOCOM_TPS_DEVICE_TYPE`).
        integer,             intent(in), optional :: delay        !! Post-request delay [msec].

        type(request_type) :: request

        call this%reset()
        call dm_geocom_api_request_get_device_config(request)
        call this%send(request, delay)
        call dm_request_get(this%request, 'devclass', device_class, default=0)
        call dm_request_get(this%request, 'devtype',  device_type,  default=0)
    end subroutine geocom_get_device_config

    subroutine geocom_get_double_precision(this, ndigits, delay)
        !! Sends *COM_GetDoublePrecision* request to sensor. The procedure
        !! returns the double precision setting – the number of digits to the
        !! right of the decimal point – when double floating-point values are
        !! transmitted in `ndigits`.
        class(geocom_class), intent(inout)        :: this    !! GeoCOM object.
        integer,             intent(out)          :: ndigits !! Number of digits to the right of the decimal point.
        integer,             intent(in), optional :: delay   !! Post-request delay [msec].

        type(request_type) :: request

        call this%reset()
        call dm_geocom_api_request_get_double_precision(request)
        call this%send(request, delay)
        call dm_request_get(this%request, 'ndigits', ndigits, default=0)
    end subroutine geocom_get_double_precision

    subroutine geocom_get_edm_mode(this, edm_mode, delay)
        !! Sends *TMC_GetEdmMode* request to sensor. The procedure returns the
        !! EDM measurement mode in `edm_mode`.
        class(geocom_class), intent(inout)        :: this     !! GeoCOM object.
        integer,             intent(out)          :: edm_mode !! EDM mode (`GEOCOM_EDM_MODE`).
        integer,             intent(in), optional :: delay    !! Post-request delay [msec].

        type(request_type) :: request

        call this%reset()
        call dm_geocom_api_request_get_edm_mode(request)
        call this%send(request, delay)
        call dm_request_get(this%request, 'edmmode', edm_mode, default=0)
    end subroutine geocom_get_edm_mode

    subroutine geocom_get_egl_intensity(this, intensity, delay)
        !! Sends *EDM_GetEglIntensity* request to sensor. The procedure returns
        !! the value of the intensity of the electronic guide light (EGL) in
        !! `intensity`.
        class(geocom_class), intent(inout)        :: this      !! GeoCOM object.
        integer,             intent(out)          :: intensity !! EDM EGL intensity (`GEOCOM_EDM_EGLINTENSITY_TYPE`).
        integer,             intent(in), optional :: delay     !! Post-request delay [msec].

        type(request_type) :: request

        call this%reset()
        call dm_geocom_api_request_get_egl_intensity(request)
        call this%send(request, delay)
        call dm_request_get(this%request, 'eglint', intensity, default=0)
    end subroutine geocom_get_egl_intensity

    subroutine geocom_get_face(this, face, delay)
        !! Sends *TMC_GetFace* request to sensor. The procedure returns the face
        !! of the current telescope position in `face`.
        class(geocom_class), intent(inout)        :: this  !! GeoCOM object.
        integer,             intent(out)          :: face  !! Telescope face (`GEOCOM_TMC_FACE`).
        integer,             intent(in), optional :: delay !! Post-request delay [msec].

        type(request_type) :: request

        call this%reset()
        call dm_geocom_api_request_get_face(request)
        call this%send(request, delay)
        call dm_request_get(this%request, 'face', face, default=0)
    end subroutine geocom_get_face

    subroutine geocom_get_fine_adjust_mode(this, adjust_mode, delay)
        !! Sends *AUT_GetFineAdjustMode* to sensor. The procedure returns the
        !! fine adjustment positioning mode in `adjust_mode`.
        class(geocom_class), intent(inout)        :: this        !! GeoCOM object.
        integer,             intent(out)          :: adjust_mode !! Fine adjustment positioning mode (`GEOCOM_AUT_ADJMODE`).
        integer,             intent(in), optional :: delay       !! Post-request delay [msec].

        type(request_type) :: request

        call this%reset()
        call dm_geocom_api_request_get_fine_adjust_mode(request)
        call this%send(request, delay)
        call dm_request_get(this%request, 'adjmode', adjust_mode, default=0)
    end subroutine geocom_get_fine_adjust_mode

    subroutine geocom_get_full_measurement(this, hz, v, angle_accuracy, cross_inc, length_inc, inc_accuracy, &
                                           slope_dist, dist_time, wait_time, inc_mode, delay)
        !! Sends *TMC_GetFullMeas* request to sensor. The procedure returns
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
        real(kind=r8),       intent(out), optional :: cross_inc      !! Cross inclination [rad].
        real(kind=r8),       intent(out), optional :: length_inc     !! Length inclination [rad].
        real(kind=r8),       intent(out), optional :: inc_accuracy   !! Inclination accuracy [rad].
        real(kind=r8),       intent(out), optional :: slope_dist     !! Distance measurement [m].
        real(kind=r8),       intent(out), optional :: dist_time      !! Time of distance measurement [msec].
        integer,             intent(in),  optional :: wait_time      !! Delay to wait for the distance measurement to finish [msec].
        integer,             intent(in),  optional :: inc_mode       !! Inclination measurement mode (`GEOCOM_TMC_INCLINE_PRG`).
        integer,             intent(in),  optional :: delay          !! Post-request delay [msec].

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
        if (present(cross_inc))      call dm_request_get(this%request, 'xinc',     cross_inc,      default=0.0_r8)
        if (present(length_inc))     call dm_request_get(this%request, 'linc',     length_inc,     default=0.0_r8)
        if (present(inc_accuracy))   call dm_request_get(this%request, 'incacc',   inc_accuracy,   default=0.0_r8)
        if (present(slope_dist))     call dm_request_get(this%request, 'sdist',    slope_dist,     default=0.0_r8)
        if (present(dist_time))      call dm_request_get(this%request, 'disttime', dist_time,      default=0.0_r8)
    end subroutine geocom_get_full_measurement

    subroutine geocom_get_geocom_version(this, release, version, subversion, delay)
        !! Sends *COM_GetSWVersion* request to sensor. The procedure returns
        !! the GeoCOM server software version of the instrument.
        class(geocom_class), intent(inout)         :: this       !! GeoCOM object.
        integer,             intent(out), optional :: release    !! GeoCOM software release.
        integer,             intent(out), optional :: version    !! GeoCOM software version.
        integer,             intent(out), optional :: subversion !! GeoCOM software sub-version.
        integer,             intent(in),  optional :: delay      !! Post-request delay [msec].

        type(request_type) :: request

        call this%reset()
        call dm_geocom_api_request_get_geocom_version(request)
        call this%send(request, delay)

        if (present(release))    call dm_request_get(this%request, 'gcrel', release,    default=0)
        if (present(version))    call dm_request_get(this%request, 'gcver', version,    default=0)
        if (present(subversion)) call dm_request_get(this%request, 'gcsub', subversion, default=0)
    end subroutine geocom_get_geocom_version

    subroutine geocom_get_geometric_ppm(this, enabled, scale_factor, offset, height_ppm, individual_ppm, delay)
        !! Sends *TMC_GeoPpm* request to sensor. The procedure returns the
        !! geometric ppm correction factor.
        class(geocom_class), intent(inout)         :: this           !! GeoCOM object.
        logical,             intent(out), optional :: enabled        !! State of geometric ppm calculation.
        real(kind=r8),       intent(out), optional :: scale_factor   !! Scale factor on central meridian.
        real(kind=r8),       intent(out), optional :: offset         !! Offset from central meridian [m].
        real(kind=r8),       intent(out), optional :: height_ppm     !! Height above reference ppm value [ppm].
        real(kind=r8),       intent(out), optional :: individual_ppm !! Individual ppm value [ppm].
        integer,             intent(in),  optional :: delay          !! Post-request delay [msec].

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
        !! Sends *TMC_GetHeight* request to sensor. The procedure returns the
        !! current reflector height.
        class(geocom_class), intent(inout)        :: this   !! GeoCOM object.
        real(kind=r8),       intent(out)          :: height !! Reflector height [m].
        integer,             intent(in), optional :: delay  !! Post-request delay [msec].

        type(request_type) :: request

        call this%reset()
        call dm_geocom_api_request_get_height(request)
        call this%send(request, delay)
        call dm_request_get(this%request, 'rheight', height, default=0.0_r8)
    end subroutine geocom_get_height

    subroutine geocom_get_image_config(this, mem_type, image_number, quality, sub_func, file_prefix, delay)
        !! Sends *IMG_GetTccConfig* request to sensor. The procedure reads the
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
        integer,                       intent(in), optional :: delay        !! Post-request delay [msec].

        integer            :: mem_type_
        type(request_type) :: request

        call this%reset()
        mem_type_ = dm_geocom_type_validated(GEOCOM_IMG_MEM_TYPE, mem_type, verbose=this%verbose, error=this%rc)
        call dm_geocom_api_request_get_image_config(request, mem_type_)
        call this%send(request, delay)

        call dm_request_get(this%request, 'imageno', image_number, default=0)
        call dm_request_get(this%request, 'quality', quality,      default=0)
        call dm_request_get(this%request, 'subfunc', sub_func,     default=0)
        this%rc = dm_regex_response_string(this%request, 'fnprefix', file_prefix)
    end subroutine geocom_get_image_config

    subroutine geocom_get_inclination_correction(this, enabled, delay)
        !! Sends *TMC_GetInclineSwitch* request to sensor. The procedure
        !! returns the dual-axis compensator status.
        class(geocom_class), intent(inout)        :: this    !! GeoCOM object.
        logical,             intent(out)          :: enabled !! Compensator is enabled.
        integer,             intent(in), optional :: delay   !! Post-request delay [msec].

        type(request_type) :: request

        call this%reset()
        call dm_geocom_api_request_get_inclination_correction(request)
        call this%send(request, delay)
        call dm_request_get(this%request, 'inccor', enabled, default=.false.)
    end subroutine geocom_get_inclination_correction

    subroutine geocom_get_inclination_error(this, error, delay)
        !! Sends *TMC_IfDataIncCorrError* request to sensor. The procedure
        !! returns the inclination error status. If `error` is `.true.`, the
        !! last measurement is not incline-corrected.
        class(geocom_class), intent(inout)        :: this  !! GeoCOM object.
        logical,             intent(out)          :: error !! Last measurement not incline-corrected.
        integer,             intent(in), optional :: delay !! Post-request delay [msec].

        type(request_type) :: request

        call this%reset()
        call dm_geocom_api_request_get_inclination_error(request)
        call this%send(request, delay)
        call dm_request_get(this%request, 'incerr', error, default=.false.)
    end subroutine geocom_get_inclination_error

    subroutine geocom_get_instrument_name(this, name, delay)
        !! Sends *CSV_GetInstrumentName* request to sensor. The procedure
        !! returns the Leica-specific instrument name. On error, the name is
        !! allocated but empty.
        use :: dm_regex, only: dm_regex_response_string

        class(geocom_class),           intent(inout)        :: this  !! GeoCOM object.
        character(len=:), allocatable, intent(out)          :: name  !! Instrument name
        integer,                       intent(in), optional :: delay !! Post-request delay [msec].

        type(request_type) :: request

        call this%reset()
        call dm_geocom_api_request_get_instrument_name(request)
        call this%send(request, delay)
        this%rc = dm_regex_response_string(this%request, 'name', name)
    end subroutine geocom_get_instrument_name

    subroutine geocom_get_instrument_number(this, number, delay)
        !! Sends *CSV_GetInstrumentNo* request to sensor. The procedure returns
        !! the factory defined instrument number.
        class(geocom_class), intent(inout)        :: this   !! GeoCOM object.
        integer,             intent(out)          :: number !! Serial number of the instrument.
        integer,             intent(in), optional :: delay  !! Post-request delay [msec].

        type(request_type) :: request

        call this%reset()
        call dm_geocom_api_request_get_instrument_number(request)
        call this%send(request, delay)
        call dm_request_get(this%request, 'serialno', number, default=0)
    end subroutine geocom_get_instrument_number

    subroutine geocom_get_internal_temperature(this, temp, delay)
        !! Sends *CSV_GetIntTemp* request to sensor. The procedure returns the
        !! internal temperature of the instrument, measured on the mainboard
        !! side.
        class(geocom_class), intent(inout)        :: this  !! GeoCOM object.
        real(kind=r8),       intent(out)          :: temp  !! Instrument temperature [°C].
        integer,             intent(in), optional :: delay !! Post-request delay [msec].

        type(request_type) :: request

        call this%reset()
        call dm_geocom_api_request_get_internal_temperature(request)
        call this%send(request, delay)
        call dm_request_get(this%request, 'temp', temp, default=0.0_r8)
    end subroutine geocom_get_internal_temperature

    subroutine geocom_get_lock_status(this, status, delay)
        !! Sends *MOT_ReadLockStatus* request to sensor. The procedure returns
        !! the condition of the Lock-in control.
        class(geocom_class), intent(inout)        :: this   !! GeoCOM object.
        integer,             intent(out)          :: status !! Lock status (`GEOCOM_MOT_LOCK_STATUS`).
        integer,             intent(in), optional :: delay  !! Post-request delay [msec].

        type(request_type) :: request

        call this%reset()
        call dm_geocom_api_request_get_lock_status(request)
        call this%send(request, delay)
        call dm_request_get(this%request, 'lockstat', status, default=0)
    end subroutine geocom_get_lock_status

    subroutine geocom_get_measurement_program(this, prg, delay)
        !! Sends *BAP_GetMeasPrg* request to sensor. The procedure returns the
        !! distance measurement program of the instrument.
        class(geocom_class), intent(inout)        :: this  !! GeoCOM object.
        integer,             intent(out)          :: prg   !! Measurement program (`GEOCOM_BAP_USER_MEASPRG`).
        integer,             intent(in), optional :: delay !! Post-request delay [msec].

        type(request_type) :: request

        call this%reset()
        call dm_geocom_api_request_get_measurement_program(request)
        call this%send(request, delay)
        call dm_request_get(this%request, 'measprg', prg, default=0)
    end subroutine geocom_get_measurement_program

    subroutine geocom_get_power(this, battery_life, power_source, power_suggest, delay)
        !! Sends *CSV_CheckPower* request to sensor. The procedure returns the
        !! available power.
        class(geocom_class), intent(inout)         :: this          !! GeoCOM object.
        integer,             intent(out), optional :: battery_life  !! Battery capacity [%].
        integer,             intent(out), optional :: power_source  !! Power source (`GEOCOM_CSV_POWER_PATH`).
        integer,             intent(out), optional :: power_suggest !! Not supported (`GEOCOM_CSV_POWER_PATH`).
        integer,             intent(in),  optional :: delay         !! Post-request delay [msec].

        type(request_type) :: request

        call this%reset()
        call dm_geocom_api_request_get_power(request)
        call this%send(request, delay)

        if (present(battery_life))  call dm_request_get(this%request, 'battlife', battery_life,  default=0)
        if (present(power_source))  call dm_request_get(this%request, 'powsrc',   power_source,  default=0)
        if (present(power_suggest)) call dm_request_get(this%request, 'powsug',   power_suggest, default=0)
    end subroutine geocom_get_power

    subroutine geocom_get_prism_constant(this, prism_const, delay)
        !! Sends *TMC_GetPrismCorr* request to sensor. The procedure returns
        !! the prism constant.
        class(geocom_class), intent(inout)        :: this        !! GeoCOM object.
        real(kind=r8),       intent(out)          :: prism_const !! Prism correction constant [m].
        integer,             intent(in), optional :: delay       !! Post-request delay [msec].

        type(request_type) :: request

        call this%reset()
        call dm_geocom_api_request_get_prism_constant(request)
        call this%send(request, delay)
        call dm_request_get(this%request, 'prsmcor', prism_const, default=0.0_r8)
    end subroutine geocom_get_prism_constant

    subroutine geocom_get_prism_definition(this, prism_type, prism_name, prism_const, delay)
        !! Sends *BAP_GetPrismDef* request to sensor. The procedure returns the
        !! default prism definition. The maximum prism name length is 16
        !! characters (`GEOCOM_BAP_PRISMNAME_LEN`). On error, the string is
        !! allocated but empty.
        use :: dm_regex, only: dm_regex_response_string

        class(geocom_class),           intent(inout)        :: this        !! GeoCOM object.
        integer,                       intent(in)           :: prism_type  !! Prism type (`GEOCOM_BAP_PRISMTYPE`).
        character(len=:), allocatable, intent(out)          :: prism_name  !! Prism name.
        real(kind=r8),                 intent(out)          :: prism_const !! Prism correction constant [m].
        integer,                       intent(in), optional :: delay       !! Post-request delay [msec].

        integer            :: prism_type_
        type(request_type) :: request

        call this%reset()
        prism_type_ = dm_geocom_type_validated(GEOCOM_BAP_PRISMTYPE, prism_type, verbose=this%verbose, error=this%rc)
        call dm_geocom_api_request_get_prism_definition(request, prism_type_)
        call this%send(request, delay)

        call dm_request_get(this%request, 'prsmcor', prism_const, default=0.0_r8)
        this%rc = dm_regex_response_string(this%request, 'prsmname', prism_name)
    end subroutine geocom_get_prism_definition

    subroutine geocom_get_prism_type(this, prism_type, delay)
        !! Sends *TMC_GetPrismType* request to sensor. The procedure returns the
        !! default prism type.
        class(geocom_class), intent(inout)        :: this       !! GeoCOM object.
        integer,             intent(out)          :: prism_type !! Prism type (`GEOCOM_BAP_PRISMTYPE`).
        integer,             intent(in), optional :: delay      !! Post-request delay [msec].

        type(request_type) :: request

        call this%reset()
        call dm_geocom_api_request_get_prism_type(request)
        call this%send(request, delay)
        call dm_request_get(this%request, 'prsmtype', prism_type, default=0)
    end subroutine geocom_get_prism_type

    subroutine geocom_get_prism_type_v2(this, prism_type, delay)
        !! Sends *TMC_GetPrismType2* request to sensor. The procedure returns
        !! the default or user prism type.
        class(geocom_class), intent(inout)        :: this       !! GeoCOM object.
        integer,             intent(out)          :: prism_type !! Prism type (`GEOCOM_BAP_PRISMTYPE`).
        integer,             intent(in), optional :: delay      !! Post-request delay [msec].

        type(request_type) :: request

        call this%reset()
        call dm_geocom_api_request_get_prism_type_v2(request)
        call this%send(request, delay)
        call dm_request_get(this%request, 'prsmtype', prism_type, default=0)
    end subroutine geocom_get_prism_type_v2

    subroutine geocom_get_quick_distance(this, hz, v, slope_dist, delay)
        !! Sends *TMC_QuickDist* request to sensor. The procedure returns the
        !! slope distance and both angles.
        !!
        !! The procedure starts an EDM tracking measurement, and waits until a
        !! distance has been measured. Then, it returns the angles and the
        !! slope distance, but no coordinates. If no distance could be
        !! measured, only angles and an error code are returned. A measurement
        !! may be aborted by calling method `do_measure()`.
        class(geocom_class), intent(inout)         :: this       !! GeoCOM object.
        real(kind=r8),       intent(out), optional :: hz         !! Horizontal angle [rad].
        real(kind=r8),       intent(out), optional :: v          !! Vertical angle [rad].
        real(kind=r8),       intent(out), optional :: slope_dist !! Slope distance [m].
        integer,             intent(in),  optional :: delay      !! Post-request delay [msec].

        type(request_type) :: request

        call this%reset()
        call dm_geocom_api_request_get_quick_distance(request)
        call this%send(request, delay)

        if (present(hz))         call dm_request_get(this%request, 'hz',    hz,         default=0.0_r8)
        if (present(v))          call dm_request_get(this%request, 'v',     v,          default=0.0_r8)
        if (present(slope_dist)) call dm_request_get(this%request, 'sdist', slope_dist, default=0.0_r8)
    end subroutine geocom_get_quick_distance

    subroutine geocom_get_reduced_atr_fov(this, enabled, delay)
        !! Sends *BAP_GetRedATRFov* request to sensor. The procedure returns
        !! whether or not reduced field of view is used by ATR.
        class(geocom_class), intent(inout)        :: this    !! GeoCOM object.
        logical,             intent(out)          :: enabled !! ATR uses reduced field of view (about 1/9)
        integer,             intent(in), optional :: delay   !! Post-request delay [msec].

        type(request_type) :: request

        call this%reset()
        call dm_geocom_api_request_get_reduced_atr_fov(request)
        call this%send(request, delay)
        call dm_request_get(this%request, 'atrfov', enabled, default=.false.)
    end subroutine geocom_get_reduced_atr_fov

    subroutine geocom_get_reflectorless_class(this, class, delay)
        !! Sends *CSV_GetReflectorlessClass* request to sensor. The procedure
        !! returns the class of the reflectorless and long-range distance
        !! measurement of the instrument.
        class(geocom_class), intent(inout)        :: this  !! GeoCOM object.
        integer,             intent(out)          :: class !! Reflectorless class (`GEOCOM_TPS_REFLESS_CLASS`).
        integer,             intent(in), optional :: delay !! Post-request delay [msec].

        type(request_type) :: request

        call this%reset()
        call dm_geocom_api_request_get_reflectorless_class(request)
        call this%send(request, delay)
        call dm_request_get(this%request, 'rlclass', class, default=0)
    end subroutine geocom_get_reflectorless_class

    subroutine geocom_get_refraction_mode(this, mode, delay)
        !! Sends *TMC_GetRefractiveMethod* request to sensor. The procedure
        !! returns the refraction model. Changing the method is not indicated
        !! on the interface of the instrument.
        class(geocom_class), intent(inout)        :: this  !! GeoCOM object.
        integer,             intent(out)          :: mode  !! Refraction mode (`1` for world, `2` for Australia).
        integer,             intent(in), optional :: delay !! Post-request delay [msec].

        type(request_type) :: request

        call this%reset()
        call dm_geocom_api_request_get_refraction_mode(request)
        call this%send(request, delay)
        call dm_request_get(this%request, 'refrmode', mode, default=0)
    end subroutine geocom_get_refraction_mode

    subroutine geocom_get_search_area(this, center_hz, center_v, range_hz, range_v, user_area, delay)
        !! Sends *AUT_GetSearchArea* request to sensor. The procedure returns
        !! the dimensions of the PowerSearch window.
        !!
        !! This command is valid for all instruments, but has only effects for
        !! instruments equipped with PowerSearch.
        class(geocom_class), intent(inout)         :: this      !! GeoCOM object.
        real(kind=r8),       intent(out), optional :: center_hz !! Hz angle of search area (center) [rad].
        real(kind=r8),       intent(out), optional :: center_v  !! V angle of search area (center) [rad].
        real(kind=r8),       intent(out), optional :: range_hz  !! Width of search area [rad].
        real(kind=r8),       intent(out), optional :: range_v   !! Max. height of search area [rad].
        logical,             intent(out), optional :: user_area !! User-defined search area is active.
        integer,             intent(in),  optional :: delay     !! Post-request delay [msec].

        type(request_type) :: request

        call this%reset()
        call dm_geocom_api_request_get_search_area(request)
        call this%send(request, delay)

        if (present(center_hz)) call dm_request_get(this%request, 'centerhz', center_hz, default=0.0_r8)
        if (present(center_v))  call dm_request_get(this%request, 'centerv',  center_v,  default=0.0_r8)
        if (present(range_hz))  call dm_request_get(this%request, 'rangehz',  range_hz,  default=0.0_r8)
        if (present(range_v))   call dm_request_get(this%request, 'rangev',   range_v,   default=0.0_r8)
        if (present(user_area)) call dm_request_get(this%request, 'userarea', user_area, default=.false.)
    end subroutine geocom_get_search_area

    subroutine geocom_get_signal(this, intensity, time, delay)
        !! Sends *TMC_GetSignal* request to sensor. The procedure returns the
        !! EDM signal intensity.
        !!
        !! The procedure can only perform a measurement if the signal
        !! measurement program is activated. Start the signal measurement
        !! program with method `do_measure()` in program `GEOCOM_TMC_SIGNAL`.
        !! After the measurement, the EDM must be switched off with program
        !! `GEOCOM_TMC_CLEAR`. While measuring, there is no angle data
        !! available.
        class(geocom_class), intent(inout)         :: this      !! GeoCOM object.
        real(kind=r8),       intent(out)           :: intensity !! Signal intensity of EDM [%].
        integer,             intent(out), optional :: time      !! Timestamp [msec].
        integer,             intent(in),  optional :: delay     !! Post-request delay [msec].

        type(request_type) :: request

        call this%reset()
        call dm_geocom_api_request_get_signal(request)
        call this%send(request, delay)
        call dm_request_get(this%request, 'sigint', intensity, default=0.0_r8)
        if (present(time)) call dm_request_get(this%request, 'sigtime', time, default=0)
    end subroutine geocom_get_signal

    subroutine geocom_get_simple_coordinates(this, easting, northing, height, wait_time, inc_mode, delay)
        !! Sends *TMC_GetSimpleCoord* request to sensor. The procedure returns
        !! the cartesian coordinates if a valid distance is set. The argument
        !! `wait_time` sets the maximum time to wait for a valid distance.
        !! Without a valid distance, the coordinates are set to 0.0, and an
        !! error is returned. The coordinate calculation requires inclination
        !! results. The argument `inc_mode` sets the inclination measurement
        !! mode (`GEOCOM_TMC_INCLINE_PRG`).
        class(geocom_class), intent(inout)        :: this      !! GeoCOM object.
        real(kind=r8),       intent(out)          :: easting   !! Easting [m].
        real(kind=r8),       intent(out)          :: northing  !! Northing [m].
        real(kind=r8),       intent(out)          :: height    !! Orthometric height [m].
        integer,             intent(in), optional :: wait_time !! Delay to wait for the distance measurement to finish [msec].
        integer,             intent(in), optional :: inc_mode  !! Inclination measurement mode (`GEOCOM_TMC_INCLINE_PRG`).
        integer,             intent(in), optional :: delay     !! Post-request delay [msec].

        integer            :: inc_mode_, wait_time_
        type(request_type) :: request

        call this%reset()
        wait_time_ = 0
        if (present(wait_time)) wait_time_ = max(0, wait_time)
        inc_mode_ = dm_geocom_type_validated(GEOCOM_TMC_INCLINE_PRG, inc_mode, verbose=this%verbose, error=this%rc)
        call dm_geocom_api_request_get_simple_coordinates(request, wait_time_, inc_mode_)
        call this%send(request, delay)

        call dm_request_get(this%request, 'east',   easting,  default=0.0_r8)
        call dm_request_get(this%request, 'north',  northing, default=0.0_r8)
        call dm_request_get(this%request, 'height', height,   default=0.0_r8)
    end subroutine geocom_get_simple_coordinates

    subroutine geocom_get_simple_measurement(this, hz, v, slope_dist, wait_time, inc_mode, delay)
        !! Sends *TMC_GetSimpleMea* request to sensor. The procedure returns the
        !! values of the angle and distance measurement. The argument
        !! `wait_time` sets the maximum time to wait for a valid distance. If a
        !! distance is available, the wait time is ignored.
        class(geocom_class), intent(inout)        :: this       !! GeoCOM object.
        real(kind=r8),       intent(out)          :: hz         !! Horizontal angle [rad].
        real(kind=r8),       intent(out)          :: v          !! Vertical angle [rad].
        real(kind=r8),       intent(out)          :: slope_dist !! Slope distance [m].
        integer,             intent(in), optional :: wait_time  !! Delay to wait for the distance measurement to finish [msec].
        integer,             intent(in), optional :: inc_mode   !! Inclination measurement mode (`GEOCOM_TMC_INCLINE_PRG`).
        integer,             intent(in), optional :: delay      !! Post-request delay [msec].

        integer            :: inc_mode_, wait_time_
        type(request_type) :: request

        call this%reset()
        wait_time_ = 0
        if (present(wait_time)) wait_time_ = max(0, wait_time)
        inc_mode_ = dm_geocom_type_validated(GEOCOM_TMC_INCLINE_PRG, inc_mode, verbose=this%verbose, error=this%rc)
        call dm_geocom_api_request_get_simple_measurement(request, wait_time_, inc_mode_)
        call this%send(request, delay)

        call dm_request_get(this%request, 'hz',    hz,         default=0.0_r8)
        call dm_request_get(this%request, 'v',     v,          default=0.0_r8)
        call dm_request_get(this%request, 'sdist', slope_dist, default=0.0_r8)
    end subroutine geocom_get_simple_measurement

    subroutine geocom_get_slope_distance_correction(this, dist_ppm, prism_const, delay)
        !! Sends *TMC_GetSlopeDistCorr* request to sensor. The procedure returns
        !! the total ppm (atmospheric ppm + geometric ppm) and prism
        !! correction constant.
        class(geocom_class), intent(inout)        :: this        !! GeoCOM object.
        real(kind=r8),       intent(out)          :: dist_ppm    !! Total correction of distance [ppm].
        real(kind=r8),       intent(out)          :: prism_const !! Correction of the reflector [m].
        integer,             intent(in), optional :: delay       !! Post-request delay [msec].

        type(request_type) :: request

        call this%reset()
        call dm_geocom_api_request_get_slope_distance_correction(request)
        call this%send(request, delay)

        call dm_request_get(this%request, 'distppm', dist_ppm,    default=0.0_r8)
        call dm_request_get(this%request, 'prsmcor', prism_const, default=0.0_r8)
    end subroutine geocom_get_slope_distance_correction

    subroutine geocom_get_software_version(this, release, version, subversion, delay)
        !! Sends *CSV_GetSWVersion* request to sensor. The procedure returns the
        !! system software version of the instrument.
        class(geocom_class), intent(inout)        :: this       !! GeoCOM object.
        integer,             intent(out)          :: release    !! Software release.
        integer,             intent(out)          :: version    !! Software version.
        integer,             intent(out)          :: subversion !! Software sub-version.
        integer,             intent(in), optional :: delay      !! Post-request delay [msec].

        type(request_type) :: request

        call this%reset()
        call dm_geocom_api_request_get_software_version(request)
        call this%send(request, delay)

        call dm_request_get(this%request, 'swrel', release,    default=0)
        call dm_request_get(this%request, 'swver', version,    default=0)
        call dm_request_get(this%request, 'swsub', subversion, default=0)
    end subroutine geocom_get_software_version

    subroutine geocom_get_station(this, easting, northing, height, instr_height, delay)
        !! Sends *TMC_GetStation* request to sensor. The procedure returns the
        !! station coordinates of the instrument.
        class(geocom_class), intent(inout)        :: this         !! GeoCOM object.
        real(kind=r8),       intent(out)          :: easting      !! Station easting coordinate [m].
        real(kind=r8),       intent(out)          :: northing     !! Station northing coordinate [m].
        real(kind=r8),       intent(out)          :: height       !! Station height coordinate [m].
        real(kind=r8),       intent(out)          :: instr_height !! Instrument height [m].
        integer,             intent(in), optional :: delay        !! Post-request delay [msec].

        type(request_type) :: request

        call this%reset()
        call dm_geocom_api_request_get_station(request)
        call this%send(request, delay)

        call dm_request_get(this%request, 'east0',   easting,      default=0.0_r8)
        call dm_request_get(this%request, 'north0',  northing,     default=0.0_r8)
        call dm_request_get(this%request, 'height0', height,       default=0.0_r8)
        call dm_request_get(this%request, 'heighti', instr_height, default=0.0_r8)
    end subroutine geocom_get_station

    subroutine geocom_get_target_type(this, target_type, delay)
        !! Sends *BAP_GetTargetType* request to sensor. The procedure returns
        !! the EDM type for distance measurements: reflector (IR) or
        !! reflectorless (RL).
        class(geocom_class), intent(inout)        :: this        !! GeoCOM object.
        integer,             intent(out)          :: target_type !! Target type (`GEOCOM_BAP_TARGET_TYPE`).
        integer,             intent(in), optional :: delay       !! Post-request delay [msec].

        type(request_type) :: request

        call this%reset()
        call dm_geocom_api_request_get_target_type(request)
        call this%send(request, delay)
        call dm_request_get(this%request, 'tartype', target_type, default=0)
    end subroutine geocom_get_target_type

    subroutine geocom_get_timeout(this, time_hz, time_v, delay)
        !! Sends *AUT_ReadTimeout* request to sensor. The procedure returns the
        !! maximum time to perform positioning.
        class(geocom_class), intent(inout)        :: this    !! GeoCOM object.
        integer(kind=i8),    intent(out)          :: time_hz !! Positioning timeout in Hz [sec].
        integer(kind=i8),    intent(out)          :: time_v  !! Positioning timeout in V [sec].
        integer,             intent(in), optional :: delay   !! Post-request delay [msec].

        type(request_type) :: request

        call this%reset()
        call dm_geocom_api_request_get_timeout(request)
        call this%send(request, delay)
        call dm_request_get(this%request, 'timehz', time_hz, default=0_i8)
        call dm_request_get(this%request, 'timev',  time_v,  default=0_i8)
    end subroutine geocom_get_timeout

    subroutine geocom_get_tolerance(this, tolerance_hz, tolerance_v, delay)
        !! Sends *AUT_ReadTol* request to sensor. The procedure returns the
        !! positioning tolerances of the Hz and V instrument axis.
        class(geocom_class), intent(inout)        :: this         !! GeoCOM object.
        real(kind=r8),       intent(out)          :: tolerance_hz !! Positioning tolerance in Hz [rad].
        real(kind=r8),       intent(out)          :: tolerance_v  !! Positioning tolerance in V [rad].
        integer,             intent(in), optional :: delay        !! Post-request delay [msec].

        type(request_type) :: request

        call this%reset()
        call dm_geocom_api_request_get_tolerance(request)
        call this%send(request, delay)
        call dm_request_get(this%request, 'tolhz', tolerance_hz, default=0.0_r8)
        call dm_request_get(this%request, 'tolv',  tolerance_v,  default=0.0_r8)
    end subroutine geocom_get_tolerance

    subroutine geocom_get_user_atr_mode(this, enabled, delay)
        !! Sends *AUS_GetUserAtrState* request to sensor. The procedure returns
        !! the status of the ATR mode.
        class(geocom_class), intent(inout)        :: this    !! GeoCOM object.
        logical,             intent(out)          :: enabled !! ATR mode is enabled.
        integer,             intent(in), optional :: delay   !! Post-request delay [msec].

        type(request_type) :: request

        call this%reset()
        call dm_geocom_api_request_get_user_atr_mode(request)
        call this%send(request, delay)
        call dm_request_get(this%request, 'atr', enabled, default=.false.)
    end subroutine geocom_get_user_atr_mode

    subroutine geocom_get_user_lock_mode(this, enabled, delay)
        !! Sends *AUS_GetUserLockState* request to sensor. The procedure
        !! returns the status of the Lock mode
        class(geocom_class), intent(inout)        :: this    !! GeoCOM object.
        logical,             intent(out)          :: enabled !! Lock mode is enabled.
        integer,             intent(in), optional :: delay   !! Post-request delay [msec].

        type(request_type) :: request

        call this%reset()
        call dm_geocom_api_request_get_user_lock_mode(request)
        call this%send(request, delay)
        call dm_request_get(this%request, 'lock', enabled, default=.false.)
    end subroutine geocom_get_user_lock_mode

    subroutine geocom_get_user_prism_definition(this, name, prism_const, prism_type, prism_user, delay)
        !! Sends *BAP_GetUserPrismDef* request to sensor. The procedure returns
        !! the user prism definition (prism constant, prism type, and name of
        !! creator).
        use :: dm_regex, only: dm_regex_response_string

        class(geocom_class),           intent(inout)         :: this        !! GeoCOM object.
        character(len=*),              intent(in)            :: name        !! Prism definition name.
        real(kind=r8),                 intent(out), optional :: prism_const !! Prism correction constant [m].
        integer,                       intent(out), optional :: prism_type  !! Prism type (`GEOCOM_BAP_PRISMTYPE`).
        character(len=:), allocatable, intent(out), optional :: prism_user  !! Name of creator.
        integer,                       intent(in),  optional :: delay       !! Post-request delay [msec].

        type(request_type) :: request

        call this%reset()
        call dm_geocom_api_request_get_user_prism_definition(request, name)
        call this%send(request, delay)

        if (present(prism_const)) call dm_request_get(this%request, 'prsmcor',  prism_const, default=0.0_r8)
        if (present(prism_type))  call dm_request_get(this%request, 'prsmtype', prism_type,  default=0)
        if (present(prism_user))  this%rc = dm_regex_response_string(this%request, 'prsmuser', prism_user)
    end subroutine geocom_get_user_prism_definition

    subroutine geocom_get_user_spiral(this, range_hz, range_v, delay)
        !! Sends *AUT_GetUserSpiral* request to sensor. The procedure returns
        !! the current dimensions of the searching spiral. Requires at least a
        !! TCA instrument.
        class(geocom_class), intent(inout)        :: this     !! GeoCOM object.
        real(kind=r8),       intent(out)          :: range_hz !! Horizontal angle [rad].
        real(kind=r8),       intent(out)          :: range_v  !! Vertical angle [rad].
        integer,             intent(in), optional :: delay    !! Post-request delay [msec].

        type(request_type) :: request

        call this%reset()
        call dm_geocom_api_request_get_user_spiral(request)
        call this%send(request, delay)
        call dm_request_get(this%request, 'rangehz', range_hz, default=0.0_r8)
        call dm_request_get(this%request, 'rangev',  range_v,  default=0.0_r8)
    end subroutine geocom_get_user_spiral

    subroutine geocom_list(this, next, last, name, size, year, month, day, hour, minute, second, delay)
        !! Sends *FTR_List* request to sensor. The procedure returns file
        !! information. If `next` is `.false.`, the first file entry is
        !! returned, else the next. On error, the file name `name` is
        !! allocated, but may be empty.
        use :: dm_regex, only: dm_regex_response_string

        class(geocom_class),           intent(inout)         :: this   !! GeoCOM object.
        logical,                       intent(in)            :: next   !! First or next entry.
        logical,                       intent(out), optional :: last   !! File is last entry.
        character(len=:), allocatable, intent(out), optional :: name   !! File name, max. 80 characters long.
        integer(kind=i8),              intent(out), optional :: size   !! File size [bytes].
        integer,                       intent(out), optional :: year   !! UTC modification year.
        integer,                       intent(out), optional :: month  !! UTC modification month.
        integer,                       intent(out), optional :: day    !! UTC modification day.
        integer,                       intent(out), optional :: hour   !! UTC modification hour.
        integer,                       intent(out), optional :: minute !! UTC modification minute
        integer,                       intent(out), optional :: second !! UTC modification second.
        integer,                       intent(in),  optional :: delay  !! Post-request delay [msec].

        character          :: year_, month_, day_, hour_, minute_, second_
        type(request_type) :: request

        call this%reset()
        call dm_geocom_api_request_list(request, next)
        call this%send(request, delay)

        if (present(last)) call dm_request_get(this%request, 'last',  last, default=.false.)
        if (present(name)) this%rc = dm_regex_response_string(this%request, 'fname', name)
        if (present(size)) call dm_request_get(this%request, 'fsize', size, default=0_i8)

        call dm_request_get(this%request, 'year',   year_,   default=achar(0))
        call dm_request_get(this%request, 'month',  month_,  default=achar(0))
        call dm_request_get(this%request, 'day',    day_,    default=achar(0))
        call dm_request_get(this%request, 'hour',   hour_,   default=achar(0))
        call dm_request_get(this%request, 'minute', minute_, default=achar(0))
        call dm_request_get(this%request, 'second', second_, default=achar(0))

        if (present(year))   year   = iachar(year_)
        if (present(month))  month  = iachar(month_)
        if (present(day))    day    = iachar(day_)
        if (present(hour))   hour   = iachar(hour_)
        if (present(minute)) minute = iachar(minute_)
        if (present(second)) second = iachar(second_)
    end subroutine geocom_list

    subroutine geocom_lock_in(this, delay)
        !! Sends *AUT_LockIn* request to sensor. The procedure will start the
        !! target tracking if the Lock mode is activated
        !! (method `set_user_lock_mode()`). The `fine_adjust()` method call
        !! must have finished successfully before executing this procedure.
        class(geocom_class), intent(inout)        :: this  !! GeoCOM object.
        integer,             intent(in), optional :: delay !! Post-request delay [msec].

        type(request_type) :: request

        call this%reset()
        call dm_geocom_api_request_lock_in(request)
        call this%send(request, delay)
    end subroutine geocom_lock_in

    subroutine geocom_measure_distance_angle(this, hz, v, slope_dist, dist_mode, delay)
        !! Sends *BAP_MeasDistanceAngle* request to sensor. The procedure
        !! measures Hz, V angles and a single distance.
        !!
        !! The API function measures angles and a single distance depending on
        !! the distance measurement mode `dist_mode`. It is not suited for
        !! continuous measurements (Lock mode and TRK mode), and uses the
        !! current automation settings.
        !!
        !! Distance measurement mode `GEOCOM_BAP_DEF_DIST` is used by default.
        class(geocom_class), intent(inout)        :: this       !! GeoCOM object.
        real(kind=r8),       intent(out)          :: hz         !! Horizontal angle [rad].
        real(kind=r8),       intent(out)          :: v          !! Vertical angle [rad].
        real(kind=r8),       intent(out)          :: slope_dist !! Slope distance [m].
        integer,             intent(in), optional :: dist_mode  !! Distance measurement mode (`GEOCOM_BAP_MEASURE_PRG`).
        integer,             intent(in), optional :: delay      !! Post-request delay [msec].

        integer            :: dist_mode_
        type(request_type) :: request

        call this%reset()
        dist_mode_ = dm_geocom_type_validated(GEOCOM_BAP_MEASURE_PRG, dist_mode, verbose=this%verbose, error=this%rc)
        call dm_geocom_api_request_measure_distance_angle(request, dist_mode_)
        call this%send(request, delay)

        call dm_request_get(this%request, 'hz',    hz,         default=0.0_r8)
        call dm_request_get(this%request, 'v',     v,          default=0.0_r8)
        call dm_request_get(this%request, 'sdist', slope_dist, default=0.0_r8)
    end subroutine geocom_measure_distance_angle

    subroutine geocom_null(this, delay)
        !! Sends *COM_NullProc* request to sensor. API call for checking the
        !! communication.
        class(geocom_class), intent(inout)        :: this  !! GeoCOM object.
        integer,             intent(in), optional :: delay !! Post-request delay [msec].

        type(request_type) :: request

        call this%reset()
        call dm_geocom_api_request_null(request)
        call this%send(request, delay)
    end subroutine geocom_null

    subroutine geocom_ps_enable_range(this, enabled, delay)
        !! Sends *AUT_PS_EnableRange* request to sensor. The procedure enabled or
        !! disables the predefined PowerSearch window including the PowerSearch
        !! range limits set by method `ps_set_range()` (requires GeoCOM robotic
        !! licence). If `enabled` is `.false.`, the default range is set to
        !! ≤ 400 m.
        class(geocom_class), intent(inout)        :: this    !! GeoCOM object.
        logical,             intent(in)           :: enabled !! Enable PowerSearch.
        integer,             intent(in), optional :: delay   !! Post-request delay [msec].

        type(request_type) :: request

        call this%reset()
        call dm_geocom_api_request_ps_enable_range(request, enabled)
        call this%send(request, delay)
    end subroutine geocom_ps_enable_range

    subroutine geocom_ps_search_next(this, direction, swing, delay)
        !! Sends *AUT_PS_SearchNext* request to sensor. The procedure searches
        !! for the next target.
        !!
        !! The procedure executes the 360° default PowerSearch and searches for
        !! the next targets. A previously defined PowerSearch window (method
        !! `set_search_area()`) is not taken into account. Use method
        !! `ps_search_window()` first.
        !!
        !! The argument `direction` may be one of the following enumerators:
        !!
        !! * `GEOCOM_AUT_CLOCKWISE`     – Direction close-wise (`1`).
        !! * `GEOCOM_AUT_ANTICLOCKWISE` – Direction counter clock-wise (`-1`).
        !!
        class(geocom_class), intent(inout)        :: this      !! GeoCOM object.
        integer,             intent(in)           :: direction !! Searching direction (`1` for clockwise, `-1` for counter-clockwise).
        logical,             intent(in)           :: swing     !! Searching starts –10 gon to the given direction.
        integer,             intent(in), optional :: delay     !! Post-request delay [msec].

        integer            :: direction_
        type(request_type) :: request

        call this%reset()
        direction_ = max(-1, min(1, direction))
        call dm_geocom_api_request_ps_search_next(request, direction_, swing)
        call this%send(request, delay)
    end subroutine geocom_ps_search_next

    subroutine geocom_ps_search_window(this, delay)
        !! Sends *AUT_PS_SearchWindow* request to sensor. The procedure starts
        !! PowerSearch in the window defined by method calls `set_search_area()`
        !! and `ps_set_range()` (requires GeoCOM robotic licence).
        class(geocom_class), intent(inout)        :: this  !! GeoCOM object.
        integer,             intent(in), optional :: delay !! Post-request delay [msec].

        type(request_type) :: request

        call this%reset()
        call dm_geocom_api_request_ps_search_window(request)
        call this%send(request, delay)
    end subroutine geocom_ps_search_window

    subroutine geocom_ps_set_range(this, min_dist, max_dist, delay)
        !! Sends *AUT_PS_SetRange* request to sensor. Creates request for setting
        !! the PowerSearch range.
        class(geocom_class), intent(inout)        :: this     !! GeoCOM object.
        integer,             intent(in)           :: min_dist !! Min. distance to prism (≥ 0) [m].
        integer,             intent(in)           :: max_dist !! Max. distance to prism (≤ 400, ≥ `min_dist` + 10) [m].
        integer,             intent(in), optional :: delay    !! Post-request delay [msec].

        type(request_type) :: request

        call this%reset()
        call dm_geocom_api_request_ps_set_range(request, min_dist, max_dist)
        call this%send(request, delay)
    end subroutine geocom_ps_set_range

    subroutine geocom_search(this, search_hz, search_v, delay)
        !! Sends *AUT_Search* request to sensor. The function performs an
        !! automatic target search within the given search area (requires
        !! GeoCOM robotic licence).
        !!
        !! The search is terminated once the prism appears in the field of view
        !! of the ATR sensor. If no prism is found in the specified area, the
        !! instrument turns back into the initial position. For an exact
        !! positioning onto the prism centre, use the fine-adjust API call
        !! afterwards (method `fine_adjust()`).
        !!
        !! If the search range of method `fine_adjust()` is expanded, target
        !! search and fine positioning are done in one step.
        class(geocom_class), intent(inout)        :: this      !! GeoCOM object.
        real(kind=r8),       intent(in)           :: search_hz !! Horizontal search region [rad].
        real(kind=r8),       intent(in)           :: search_v  !! Vertical search region [rad].
        integer,             intent(in), optional :: delay     !! Post-request delay [msec].

        type(request_type) :: request

        call this%reset()
        call dm_geocom_api_request_search(request, search_hz, search_v)
        call this%send(request, delay)
    end subroutine geocom_search

    subroutine geocom_search_target(this, delay)
        !! Sends *BAP_SearchTarget* request to sensor. The procedure searches
        !! for a target in the ATR search window.
        class(geocom_class), intent(inout)        :: this  !! GeoCOM object.
        integer,             intent(in), optional :: delay !! Post-request delay [msec].

        type(request_type) :: request

        call this%reset()
        call dm_geocom_api_request_search_target(request)
        call this%send(request, delay)
    end subroutine geocom_search_target

    subroutine geocom_set_angle_correction(this, incline, stand_axis, collimation, tilt_axis, delay)
        !! Sends *TMC_SetAngSwitch* request to sensor. The procedure turns
        !! angle corrections on or off.
        class(geocom_class), intent(inout)        :: this        !! GeoCOM object.
        logical,             intent(in)           :: incline     !! Enable inclination correction.
        logical,             intent(in)           :: stand_axis  !! Enable standard axis correction.
        logical,             intent(in)           :: collimation !! Enable collimation correction.
        logical,             intent(in)           :: tilt_axis   !! Enable tilt axis correction.
        integer,             intent(in), optional :: delay       !! Post-request delay [msec].

        type(request_type) :: request

        call this%reset()
        call dm_geocom_api_request_set_angle_correction(request, incline, stand_axis, collimation, tilt_axis)
        call this%send(request, delay)
    end subroutine geocom_set_angle_correction

    subroutine geocom_set_atmospheric_correction(this, lambda, pressure, dry_temp, wet_temp, delay)
        !! Sends *BAP_SetAtmCorr* request to sensor. The procedure sets the
        !! atmospheric correction parameters. The argument `lambda` should be
        !! queried with method `get_atmospheric_correction()`.
        class(geocom_class), intent(inout)        :: this     !! GeoCOM object.
        real(kind=r8),       intent(in)           :: lambda   !! Wave-length of EDM transmitter [m].
        real(kind=r8),       intent(in)           :: pressure !! Atmospheric pressure [mbar].
        real(kind=r8),       intent(in)           :: dry_temp !! Dry temperature [°C].
        real(kind=r8),       intent(in)           :: wet_temp !! Wet temperature [°C].
        integer,             intent(in), optional :: delay    !! Post-request delay [msec].

        type(request_type) :: request

        call this%reset()
        call dm_geocom_api_request_set_atmospheric_correction(request, lambda, pressure, dry_temp, wet_temp)
        call this%send(request, delay)
    end subroutine geocom_set_atmospheric_correction

    subroutine geocom_set_atmospheric_ppm(this, atm_ppm, delay)
        !! Sends *BAP_SetAtmPpm* request to sensor. The procedure sets the
        !! atmospheric ppm correction factor.
        class(geocom_class), intent(inout)        :: this    !! GeoCOM object.
        real(kind=r8),       intent(in)           :: atm_ppm !! Atmospheric ppm correction factor [ppm].
        integer,             intent(in), optional :: delay   !! Post-request delay [msec].

        type(request_type) :: request

        call this%reset()
        call dm_geocom_api_request_set_atmospheric_ppm(request, atm_ppm)
        call this%send(request, delay)
    end subroutine geocom_set_atmospheric_ppm

    subroutine geocom_set_atr_mode(this, atr_mode, delay)
        !! Sends *BAP_SetATRSetting* request to sensor. The procedure sets the
        !! ATR low-vis mode.
        !!
        !! The argument `atr_mode` must be one of the following enumerators:
        !!
        !! * `GEOCOM_BAP_ATRSET_NORMAL`     – No special flags or modes.
        !! * `GEOCOM_BAP_ATRSET_LOWVIS_ON`  – ATR low-vis mode on.
        !! * `GEOCOM_BAP_ATRSET_LOWVIS_AON` – ATR low-vis mode always on.
        !! * `GEOCOM_BAP_ATRSET_SRANGE_ON`  – ATR high-reflectivity mode on.
        !! * `GEOCOM_BAP_ATRSET_SRANGE_AON` – ATR high-reflectivity mode always on.
        !!
        !! An invalid mode is replaced with `GEOCOM_BAP_ATRSET_NORMAL`.
        class(geocom_class), intent(inout)        :: this     !! GeoCOM object.
        integer,             intent(in)           :: atr_mode !! ATR low-vis mode (`GEOCOM_BAP_ATRSETTING`).
        integer,             intent(in), optional :: delay    !! Post-request delay [msec].

        integer            :: atr_mode_
        type(request_type) :: request

        call this%reset()
        atr_mode_ = dm_geocom_type_validated(GEOCOM_BAP_ATRSETTING, atr_mode, verbose=this%verbose, error=this%rc)
        call dm_geocom_api_request_set_atr_mode(request, atr_mode_)
        call this%send(request, delay)
    end subroutine geocom_set_atr_mode

    subroutine geocom_set_binary_mode(this, enabled, delay)
        !! Sends *COM_SetBinaryAvailable* request to sensor. The procedure sets
        !! the ability of the GeoCOM server to handle binary communication (not
        !! supported by DMPACK).
        class(geocom_class), intent(inout)        :: this    !! GeoCOM object.
        logical,             intent(in)           :: enabled !! Enable binary communication.
        integer,             intent(in), optional :: delay   !! Post-request delay [msec].

        type(request_type) :: request

        call this%reset()
        call dm_geocom_api_request_set_binary_mode(request, enabled)
        call this%send(request, delay)
    end subroutine geocom_set_binary_mode

    subroutine geocom_set_config(this, auto_power, timeout, delay)
        !! Sends *SUP_SetConfig* request to sensor. The procedure sets the
        !! power management configuration.
        !!
        !! The argument `timeout` sets the duration after which the instrument
        !! switches into the mode `auto_power` (`GEOCOM_SUP_AUTO_POWER`) when
        !! no user activity occured (key press, GeoCOM communication). The
        !! value must be between 60,000 m/s (1 min) and 6,000,000 m/s (100 min).
        class(geocom_class), intent(inout)        :: this       !! GeoCOM object.
        integer,             intent(in)           :: auto_power !! Power-off mode (`GEOCOM_SUP_AUTO_POWER`).
        integer,             intent(in)           :: timeout    !! Timeout [msec].
        integer,             intent(in), optional :: delay      !! Post-request delay [msec].

        type(request_type) :: request

        call this%reset()
        call dm_geocom_api_request_set_config(request, auto_power, timeout)
        call this%send(request, delay)
    end subroutine geocom_set_config

    subroutine geocom_set_date_time(this, year, month, day, hour, minute, second, delay)
        !! Sends *CSV_SetDateTime* request to sensor. The procedure sets the
        !! date and time of the instrument.
        class(geocom_class), intent(inout)        :: this    !! GeoCOM object.
        integer,             intent(in)           :: year    !! Year (`YYYY`).
        integer,             intent(in)           :: month   !! Month (`MM`).
        integer,             intent(in)           :: day     !! Day of month (`DD`).
        integer,             intent(in)           :: hour    !! Hour (`hh`).
        integer,             intent(in)           :: minute  !! Minute (`mm`).
        integer,             intent(in)           :: second  !! Second (`ss`).
        integer,             intent(in), optional :: delay   !! Post-request delay [msec].

        type(request_type) :: request

        call this%reset()
        call dm_geocom_api_request_set_date_time(request, year, month, day, hour, minute, second)
        call this%send(request, delay)
    end subroutine geocom_set_date_time

    subroutine geocom_set_distance(this, slope_dist, height_offset, inc_mode, delay)
        !! Sends *TMC_SetHandDist* request to sensor. The procedure sets the
        !! slope distance and height offset.
        !!
        !! The API function is used to set the manually measured slope distance
        !! and height offset for a following measurement. Additionally, an
        !! inclination and an angle measurement are started to determine the
        !! coordinates of the target. The vertical angle is corrected to π/2 or
        !! 3π/2, depending on the face of the instrument. The previously
        !! measured distance is cleared.
        !!
        !! The argument `inc_mode` must be one of the following enumerators:
        !!
        !! * `GEOCOM_TMC_MEA_INC`   – Use sensor (a priori sigma).
        !! * `GEOCOM_TMC_AUTO_INC`  – Automatic mode (sensor/plane).
        !! * `GEOCOM_TMC_PLANE_INC` – Use plane (a priori sigma).
        !!
        class(geocom_class), intent(inout)        :: this          !! GeoCOM object.
        real(kind=r8),       intent(in)           :: slope_dist    !! Slope distance [m].
        real(kind=r8),       intent(in)           :: height_offset !! Height offset [m].
        integer,             intent(in), optional :: inc_mode      !! Inclination measurement mode (`GEOCOM_TMC_INCLINE_PRG`).
        integer,             intent(in), optional :: delay         !! Post-request delay [msec].

        integer            :: inc_mode_
        type(request_type) :: request

        call this%reset()
        inc_mode_ = dm_geocom_type_validated(GEOCOM_TMC_INCLINE_PRG, inc_mode, verbose=this%verbose, error=this%rc)
        call dm_geocom_api_request_set_distance(request, slope_dist, height_offset, inc_mode_)
        call this%send(request, delay)
    end subroutine geocom_set_distance

    subroutine geocom_set_double_precision(this, ndigits, delay)
        !! Sends *COM_SetDoublePrecision* request to sensor. The procedure sets
        !! the double precision.
        !!
        !! The API function sets the precision – the number of digits right of
        !! the decimal – when double floating-point values are transmitted. The
        !! default precision is 15 digits. The setting is only valid for the
        !! ASCII transmission mode. Trailing zeroes will not be sent by the
        !! instrument. For example, if `ndigits` is set to 3 and the exact
        !! value is 1.99975, the resulting value will be 2.0.
        class(geocom_class), intent(inout)        :: this    !! GeoCOM object.
        integer,             intent(in)           :: ndigits !! Number of digits right to the comma.
        integer,             intent(in), optional :: delay   !! Post-request delay [msec].

        type(request_type) :: request

        call this%reset()
        call dm_geocom_api_request_set_double_precision(request, ndigits)
        call this%send(request, delay)
    end subroutine geocom_set_double_precision

    subroutine geocom_set_edm_mode(this, edm_mode, delay)
        !! Sends *TMC_SetEdmMode* request to sensor. The procedure sets the EDM
        !! measurement mode. The EDM mode is used by method `do_measure()` in
        !! program `GEOCOM_TMC_DEF_DIST`.
        class(geocom_class), intent(inout)        :: this     !! GeoCOM object.
        integer,             intent(in)           :: edm_mode !! EDM measurement mode (`GEOCOM_EDM_MODE`).
        integer,             intent(in), optional :: delay    !! Post-request delay [msec].

        integer            :: edm_mode_
        type(request_type) :: request

        call this%reset()
        edm_mode_ = dm_geocom_type_validated(GEOCOM_EDM_MODE, edm_mode, verbose=this%verbose, error=this%rc)
        call dm_geocom_api_request_set_edm_mode(request, edm_mode_)
        call this%send(request, delay)
    end subroutine geocom_set_edm_mode

    subroutine geocom_set_egl_intensity(this, intensity, delay)
        !! Sends *EDM_SetEglIntensity* request to sensor. The procedure sets
        !! the intensity of the electronic guide light.
        !!
        !! The argument `intensity` must be one of the following enumerators:
        !!
        !! * `GEOCOM_EDM_EGLINTEN_OFF`
        !! * `GEOCOM_EDM_EGLINTEN_LOW`
        !! * `GEOCOM_EDM_EGLINTEN_MID`
        !! * `GEOCOM_EDM_EGLINTEN_HIGH`
        !!
        class(geocom_class), intent(inout)        :: this      !! GeoCOM object.
        integer,             intent(in)           :: intensity !! EGL intensity (`GEOCOM_EDM_EGLINTENSITY_TYPE`).
        integer,             intent(in), optional :: delay     !! Post-request delay [msec].

        integer            :: intensity_
        type(request_type) :: request

        call this%reset()
        intensity_ = dm_geocom_type_validated(GEOCOM_EDM_EGLINTENSITY_TYPE, intensity, verbose=this%verbose, error=this%rc)
        call dm_geocom_api_request_set_egl_intensity(request, intensity_)
        call this%send(request, delay)
    end subroutine geocom_set_egl_intensity

    subroutine geocom_set_fine_adjust_mode(this, adj_mode, delay)
        !! Sends *AUT_SetFineAdjustMode* request to sensor. The procedure sets
        !! the fine adjust positioning mode.
        !!
        !! The API function sets the positioning tolerances relating to angle
        !! accuracy or point accuracy for the fine adjust (requires GeoCOM
        !! robotic licence). If a target is near or held by hand, it is
        !! recommended to set the adjust mode to `GEOCOM_AUT_POINT_MODE`.
        !!
        !! The argument `adj_mode` has to be either `GEOCOM_AUT_NORM_MODE` or
        !! `GEOCOM_AUT_POINT_MODE`.
        class(geocom_class), intent(inout)        :: this     !! GeoCOM object.
        integer,             intent(in)           :: adj_mode !! Fine adjust positioning mode (`GEOCOM_AUT_ADJMODE`).
        integer,             intent(in), optional :: delay    !! Post-request delay [msec].

        integer            :: adj_mode_
        type(request_type) :: request

        call this%reset()
        adj_mode_ = dm_geocom_type_validated(GEOCOM_AUT_ADJMODE, adj_mode, verbose=this%verbose, error=this%rc)
        call dm_geocom_api_request_set_fine_adjust_mode(request, adj_mode_)
        call this%send(request, delay)
    end subroutine geocom_set_fine_adjust_mode

    subroutine geocom_set_geometric_ppm(this, enabled, scale_factor, offset, height_ppm, individual_ppm, delay)
        !! Sends *TMC_SetGeoPpm* request to sensor. The procedure sets the
        !! geometric ppm correction factor.
        class(geocom_class), intent(inout)        :: this           !! GeoCOM object.
        logical,             intent(in)           :: enabled        !! Enable geometric ppm calculation.
        real(kind=r8),       intent(in)           :: scale_factor   !! Scale factor on central meridian.
        real(kind=r8),       intent(in)           :: offset         !! Offset from central meridian [m].
        real(kind=r8),       intent(in)           :: height_ppm     !! Ppm value due to height above reference.
        real(kind=r8),       intent(in)           :: individual_ppm !! Individual ppm value.
        integer,             intent(in), optional :: delay          !! Post-request delay [msec].

        type(request_type) :: request

        call this%reset()
        call dm_geocom_api_request_set_geometric_ppm(request, enabled, scale_factor, offset, height_ppm, individual_ppm)
        call this%send(request, delay)
    end subroutine geocom_set_geometric_ppm

    subroutine geocom_set_height(this, height, delay)
        !! Sends *TMC_SetHeight* request to sensor. The procedure sets a new
        !! reflector height.
        class(geocom_class), intent(inout)        :: this   !! GeoCOM object.
        real(kind=r8),       intent(in)           :: height !! Reflector height [m].
        integer,             intent(in), optional :: delay  !! Post-request delay [msec].

        type(request_type) :: request

        call this%reset()
        call dm_geocom_api_request_set_height(request, height)
        call this%send(request, delay)
    end subroutine geocom_set_height

    subroutine geocom_set_image_config(this, mem_type, image_number, quality, sub_function, prefix, delay)
        !! Sends *IMG_SetTccConfig* request to sensor. The procedure sets the
        !! image configuration.
        !!
        !! The argument `sub_function` may be a binary combination of the
        !! following settings:
        !!
        !! * `1` – Test image.
        !! * `2` – Automatic exposure-time selection.
        !! * `3` – Two-times sub-sampling.
        !! * `4` – Four-times sub-sampling.
        !!
        class(geocom_class), intent(inout)        :: this         !! GeoCOM object.
        integer,             intent(in)           :: mem_type     !! Memory device type (`GEOCOM_IMG_MEM_TYPE`).
        integer,             intent(in)           :: image_number !! Actual image number.
        integer,             intent(in)           :: quality      !! JPEG compression factor (0 – 100).
        integer,             intent(in)           :: sub_function !! Additional sub-functions to call.
        character(len=*),    intent(in)           :: prefix       !! File name prefix.
        integer,             intent(in), optional :: delay        !! Post-request delay [msec].

        integer            :: mem_type_
        type(request_type) :: request

        call this%reset()
        mem_type_ = dm_geocom_type_validated(GEOCOM_IMG_MEM_TYPE, mem_type, verbose=this%verbose, error=this%rc)
        call dm_geocom_api_request_set_image_config(request, mem_type_, image_number, quality, sub_function, prefix)
        call this%send(request, delay)
    end subroutine geocom_set_image_config

    subroutine geocom_set_inclination_correction(this, enabled, delay)
        !! Sends *TMC_SetInclineSwitch* request to sensor. The procedure turns
        !! the dual-axis compensator on or off.
        class(geocom_class), intent(inout)        :: this    !! GeoCOM object.
        logical,             intent(in)           :: enabled !! Enable dual-axis compensator.
        integer,             intent(in), optional :: delay   !! Post-request delay [msec].

        type(request_type) :: request

        call this%reset()
        call dm_geocom_api_request_set_inclination_correction(request, enabled)
        call this%send(request, delay)
    end subroutine geocom_set_inclination_correction

    subroutine geocom_set_laser_pointer(this, enabled, delay)
        !! Sends *EDM_Laserpointer* request to sensor. The procedure turns the
        !! laser pointer on or off.
        !!
        !! The API function is only available on models which support
        !! reflectorless distance measurement.
        class(geocom_class), intent(inout)        :: this    !! GeoCOM object.
        logical,             intent(in)           :: enabled !! Enable laser pointer.
        integer,             intent(in), optional :: delay   !! Post-request delay [msec].

        type(request_type) :: request

        call this%reset()
        call dm_geocom_api_request_set_laser_pointer(request, enabled)
        call this%send(request, delay)
    end subroutine geocom_set_laser_pointer

    subroutine geocom_set_measurement_program(this, bap_prog, delay)
        !! Sends *BAP_SetMeasPrg* request to sensor. The procedure sets the
        !! distance measurement program, for example, for method
        !! `measure_distance_angle()`. The RL EDM type programs are not
        !! available on all instruments. Changing the measurement program may
        !! change the EDM type as well (IR, RL).
        class(geocom_class), intent(inout)        :: this     !! GeoCOM object.
        integer,             intent(in)           :: bap_prog !! Measurement program (`GEOCOM_BAP_USER_MEASPRG`).
        integer,             intent(in), optional :: delay    !! Post-request delay [msec].

        integer            :: bap_prog_
        type(request_type) :: request

        call this%reset()
        bap_prog_ = dm_geocom_type_validated(GEOCOM_BAP_USER_MEASPRG, bap_prog, verbose=this%verbose, error=this%rc)
        call dm_geocom_api_request_set_measurement_program(request, bap_prog_)
        call this%send(request, delay)
    end subroutine geocom_set_measurement_program

    subroutine geocom_set_orientation(this, hz, delay)
        !! Sends *TMC_SetOrientation* request to sensor. The procedure sets
        !! orientating the instrument in horizontal direction.
        !!
        !! The API function is a combination of an angle measurement to get the
        !! horizontal offset and setting the angle offset afterwards, in order
        !! to orientate to a target. Before the new orientation can be set, an
        !! existing distance must be cleared by calling method `do_measure()`
        !! with program `GEOCOM_TMC_CLEAR`.
        class(geocom_class), intent(inout)        :: this  !! GeoCOM object.
        real(kind=r8),       intent(in)           :: hz    !! Horizontal orientation [rad].
        integer,             intent(in), optional :: delay !! Post-request delay [msec].

        type(request_type) :: request

        call this%reset()
        call dm_geocom_api_request_set_orientation(request, hz)
        call this%send(request, delay)
    end subroutine geocom_set_orientation

    subroutine geocom_set_position(this, hz, v, pos_mode, atr_mode, delay)
        !! Sends *AUT_MakePositioning* request to sensor. The procedure turns
        !! the telescope to a specified position.
        !!
        !! If `pos_mode` is `GEOCOM_AUT_NORMAL`, uses the current value of the
        !! compensator. For positioning distances > 25 gon, this mode might tend
        !! to inaccuracy. If set to `GEOCOM_AUT_PRECISE`, tries to measure the
        !! exact inclination of the target. Tends to long position time.
        !!
        !! If `atr_mode` is `GEOCOM_AUT_POSITION`, uses conventional position to
        !! other face. If set to `GEOCOM_AUT_TARGET`, tries to position into a
        !! target in the destination area. This mode requires activated ATR.
        class(geocom_class), intent(inout)        :: this     !! GeoCOM object.
        real(kind=r8),       intent(in)           :: hz       !! Horizontal angle [rad].
        real(kind=r8),       intent(in)           :: v        !! Vertical angle [rad].
        integer,             intent(in)           :: pos_mode !! Position mode (`GEOCOM_AUT_POSMODE`).
        integer,             intent(in)           :: atr_mode !! ATR mode (`GEOCOM_AUT_ATRMODE`).
        integer,             intent(in), optional :: delay    !! Post-request delay [msec].

        integer            :: atr_mode_, pos_mode_
        integer            :: rc1, rc2
        type(request_type) :: request

        call this%reset()
        pos_mode_ = dm_geocom_type_validated(GEOCOM_AUT_POSMODE, pos_mode, verbose=this%verbose, error=rc1)
        atr_mode_ = dm_geocom_type_validated(GEOCOM_AUT_ATRMODE, atr_mode, verbose=this%verbose, error=rc2)
        this%rc   = max(rc1, rc2)
        call dm_geocom_api_request_set_position(request, hz, v, pos_mode_, atr_mode_)
        call this%send(request, delay)
    end subroutine geocom_set_position

    subroutine geocom_set_positioning_timeout(this, time_hz, time_v, delay)
        !! Sends *AUT_SetTimeout* request to sensor. The procedure sets the
        !! timeout for positioning.
        !!
        !! This function sets the maximum time to perform a positioning. The
        !! timeout is reset on 7 seconds after each power on. Valid value for
        !! `time_hz` and `time_v` are between 7.0 [sec] and 60.0 [sec].
        class(geocom_class), intent(inout)        :: this    !! GeoCOM object.
        real(kind=r8),       intent(in)           :: time_hz !! Timeout in Hz direction [s].
        real(kind=r8),       intent(in)           :: time_v  !! Timeout in V direction [s].
        integer,             intent(in), optional :: delay   !! Post-request delay [msec].

        type(request_type) :: request

        call this%reset()
        call dm_geocom_api_request_set_positioning_timeout(request, time_hz, time_v)
        call this%send(request, delay)
    end subroutine geocom_set_positioning_timeout

    subroutine geocom_set_prism_constant(this, prism_const, delay)
        !! Sends *TMC_SetPrismCorr* request to sensor. The procedure sets the
        !! prism constant. The method `set_prism_type()` overwrites this
        !! setting.
        class(geocom_class), intent(inout)        :: this        !! GeoCOM object.
        real(kind=r8),       intent(in)           :: prism_const !! Prism constant [mm].
        integer,             intent(in), optional :: delay       !! Post-request delay [msec].

        type(request_type) :: request

        call this%reset()
        call dm_geocom_api_request_set_prism_constant(request, prism_const)
        call this%send(request, delay)
    end subroutine geocom_set_prism_constant

    subroutine geocom_set_prism_type(this, prism_type, delay)
        !! Sends *BAP_SetPrismType* request to sensor. The procedure sets the
        !! default prism type for measurement with a reflector
        !! (`GEOCOM_BAP_PRISMTYPE`). It overwrites the prism constant set by
        !! method `set_prism_constant()`.
        class(geocom_class), intent(inout)        :: this       !! GeoCOM object.
        integer,             intent(in)           :: prism_type !! Prism type (`GEOCOM_BAP_PRISMTYPE`).
        integer,             intent(in), optional :: delay      !! Post-request delay [msec].

        integer            :: prism_type_
        type(request_type) :: request

        call this%reset()
        prism_type_ = dm_geocom_type_validated(GEOCOM_BAP_PRISMTYPE, prism_type, verbose=this%verbose, error=this%rc)
        call dm_geocom_api_request_set_prism_type(request, prism_type_)
        call this%send(request, delay)
    end subroutine geocom_set_prism_type

    subroutine geocom_set_prism_type_v2(this, prism_type, prism_name, delay)
        !! Sends *BAP_SetPrismType2* request to sensor. The procedure sets the
        !! default or user prism type for measurements with a reflector. It
        !! overwrites the prism constant set by method `set_prism_constant()`.
        !! The user-defined prism must have been added with method
        !! `set_user_prism_definition()` beforehand.
        class(geocom_class), intent(inout)        :: this       !! GeoCOM object.
        integer,             intent(in)           :: prism_type !! Prism type (`GEOCOM_BAP_PRISMTYPE`).
        character(len=*),    intent(in)           :: prism_name !! Prism name (required if prism type is `GEOCOM_BAP_PRISM_USER`).
        integer,             intent(in), optional :: delay      !! Post-request delay [msec].

        integer            :: prism_type_
        type(request_type) :: request

        call this%reset()
        prism_type_ = dm_geocom_type_validated(GEOCOM_BAP_PRISMTYPE, prism_type, verbose=this%verbose, error=this%rc)
        call dm_geocom_api_request_set_prism_type_v2(request, prism_type_, prism_name)
        call this%send(request, delay)
    end subroutine geocom_set_prism_type_v2

    subroutine geocom_set_reduced_atr_fov(this, enabled, delay)
        !! Sends *BAP_SetRedATRFov* request to sensor. The procedure sets the
        !! reduced ATR field of view. If `enabled` is `.true.`, ATR uses
        !! reduced field of view (about 1/9), full field of view otherwise.
        class(geocom_class), intent(inout)        :: this    !! GeoCOM object.
        logical,             intent(in)           :: enabled !! Use reduced field of view.
        integer,             intent(in), optional :: delay   !! Post-request delay [msec].

        type(request_type) :: request

        call this%reset()
        call dm_geocom_api_request_set_reduced_atr_fov(request, enabled)
        call this%send(request, delay)
    end subroutine geocom_set_reduced_atr_fov

    subroutine geocom_set_refraction_mode(this, mode, delay)
        !! Sends *TMC_SetRefractiveMethod* request to sensor. The procedure
        !! sets the refraction model. Mode `1` means method 1 for the rest of
        !! the world, mode `2` means method for Australia.
        class(geocom_class), intent(inout)        :: this  !! GeoCOM object.
        integer,             intent(in)           :: mode  !! Refraction data method (1 or 2).
        integer,             intent(in), optional :: delay !! Post-request delay [msec].

        integer            :: mode_
        type(request_type) :: request

        call this%reset()
        mode_ = max(1, min(2, mode))
        call dm_geocom_api_request_set_refraction_mode(request, mode_)
        call this%send(request, delay)
    end subroutine geocom_set_refraction_mode

    subroutine geocom_set_search_area(this, center_hz, center_v, range_hz, range_v, enabled, delay)
        !! Sends *AUT_SetSearchArea* request to sensor. The procedure sets the
        !! PowerSearch window, and activates it. The API call is valid for all
        !! instruments, but has effects only for those equipped with
        !! PowerSearch (requires GeoCOM robotic licence).
        class(geocom_class), intent(inout)        :: this      !! GeoCOM object.
        real(kind=r8),       intent(in)           :: center_hz !! Search area center Hz angle [rad].
        real(kind=r8),       intent(in)           :: center_v  !! Search area center V angle [rad].
        real(kind=r8),       intent(in)           :: range_hz  !! Search area range Hz angle [rad].
        real(kind=r8),       intent(in)           :: range_v   !! Search area range V angle [rad].
        logical,             intent(in)           :: enabled   !! Enable search area.
        integer,             intent(in), optional :: delay     !! Post-request delay [msec].

        type(request_type) :: request

        call this%reset()
        call dm_geocom_api_request_set_search_area(request, center_hz, center_v, range_hz, range_v, enabled)
        call this%send(request, delay)
    end subroutine geocom_set_search_area

    subroutine geocom_set_station(this, easting, northing, height, instr_height, delay)
        !! Sends *TMC_SetStation* request to sensor. The procedure sets the
        !! station coordinates of the instrument.
        class(geocom_class), intent(inout)        :: this         !! GeoCOM object.
        real(kind=r8),       intent(in)           :: easting      !! E coordinate [m].
        real(kind=r8),       intent(in)           :: northing     !! N coordinate [m].
        real(kind=r8),       intent(in)           :: height       !! H coordinate [m].
        real(kind=r8),       intent(in)           :: instr_height !! Instrument height [m].
        integer,             intent(in), optional :: delay        !! Post-request delay [msec].

        type(request_type) :: request

        call this%reset()
        call dm_geocom_api_request_set_station(request, easting, northing, height, instr_height)
        call this%send(request, delay)
    end subroutine geocom_set_station

    subroutine geocom_set_target_type(this, target_type, delay)
        !! Sends *BAP_SetTargetType* request to sensor. The procedure sets the
        !! EDM type (`GEOCOM_BAP_TARGET_TYPE`) for distance measurements:
        !! reflector (IR) or reflectorless (RL).
        !!
        !! For each EDM type, the EDM mode used last is remembered and actived
        !! if the EDM type is changed. If EDM type IR is selected, the
        !! automation mode used last is activated automatically. The method
        !! `set_measurement_program()` can also change the target type. The EDM
        !! type RL is not available on all instruments.
        class(geocom_class), intent(inout)        :: this        !! GeoCOM object.
        integer,             intent(in)           :: target_type !! Target type (`GEOCOM_BAP_TARGET_TYPE`).
        integer,             intent(in), optional :: delay       !! Post-request delay [msec].

        integer            :: target_type_
        type(request_type) :: request

        call this%reset()
        target_type_ = dm_geocom_type_validated(GEOCOM_BAP_TARGET_TYPE, target_type, verbose=this%verbose, error=this%rc)
        call dm_geocom_api_request_set_target_type(request, target_type_)
        call this%send(request, delay)
    end subroutine geocom_set_target_type

    subroutine geocom_set_tolerance(this, hz, v, delay)
        !! Sends *AUT_SetTol* request to sensor. The procedure sets the
        !! positioning tolerances of the Hz and V instrument axes (GeoCOM
        !! robotic licence required). The tolerances must be in the range of 1
        !! [cc] (1.57079E-06 [rad]) to 100 [cc] (1.57079E-04 [rad]).
        !!
        !! The maximum resolution of the angle measurement system depends on
        !! the instrument accuracy class. If smaller positioning tolerances are
        !! required, the positioning time can increase drastically.
        class(geocom_class), intent(inout)        :: this  !! GeoCOM object.
        real(kind=r8),       intent(in)           :: hz    !! Positioning tolerance in Hz direction [rad].
        real(kind=r8),       intent(in)           :: v     !! Positioning tolerance in V direction [rad].
        integer,             intent(in), optional :: delay !! Post-request delay [msec].

        type(request_type) :: request

        call this%reset()
        call dm_geocom_api_request_set_tolerance(request, hz, v)
        call this%send(request, delay)
    end subroutine geocom_set_tolerance

    subroutine geocom_set_user_atr_mode(this, enabled, delay)
        !! Sends *AUS_SetUserAtrState* request to sensor. The procedure
        !! activates or deactivates the ATR mode (requires GeoCOM robotic
        !! licence).
        !!
        !! If `enabled` is `.true.`, ATR mode is activated, and if Lock mode is
        !! enabled while the API call is made, Lock mode will !! change to ATR
        !! mode. If `enabled` is `.false.`, ATR mode is deactivated, and if
        !! Lock mode is enabled then it stays enabled.
        class(geocom_class), intent(inout)        :: this    !! GeoCOM object.
        logical,             intent(in)           :: enabled !! Enable ATR mode.
        integer,             intent(in), optional :: delay   !! Post-request delay [msec].

        type(request_type) :: request

        call this%reset()
        call dm_geocom_api_request_set_user_atr_mode(request, enabled)
        call this%send(request, delay)
    end subroutine geocom_set_user_atr_mode

    subroutine geocom_set_user_lock_mode(this, enabled, delay)
        !! Sends *AUS_SetUserLockState* request to sensor. The procedure
        !! activates or deactivates the Lock mode (GeoCOM robotic licence
        !! required).
        !!
        !! If `enabled` is `.true.`, Lock mode is activated. In order to lock
        !! and follow a moving target, call method `lock_in()`. If `enabled` is
        !! `.false.`, Lock mode is deactivated. Tracking of a moving target
        !! will be aborted, and the manual drive wheel is activated.
        class(geocom_class), intent(inout)        :: this    !! GeoCOM object.
        logical,             intent(in)           :: enabled !! Enable Lock mode.
        integer,             intent(in), optional :: delay   !! Post-request delay [msec].

        type(request_type) :: request

        call this%reset()
        call dm_geocom_api_request_set_user_lock_mode(request, enabled)
        call this%send(request, delay)
    end subroutine geocom_set_user_lock_mode

    subroutine geocom_set_user_prism_definition(this, prism_name, prism_const, refl_type, creator, delay)
        !! Sends *BAP_SetUserPrismDef* request to sensor. The procedure sets a
        !! user prism definition.
        class(geocom_class), intent(inout)        :: this        !! GeoCOM object.
        character(len=*),    intent(in)           :: prism_name  !! Prism name.
        real(kind=r8),       intent(in)           :: prism_const !! Prism correction constant [mm].
        integer,             intent(in)           :: refl_type   !! Reflector type (`GEOCOM_BAP_REFLTYPE`).
        character(len=*),    intent(in)           :: creator     !! Name of creator.
        integer,             intent(in), optional :: delay       !! Post-request delay [msec].

        integer            :: refl_type_
        type(request_type) :: request

        call this%reset()
        refl_type_ = dm_geocom_type_validated(GEOCOM_BAP_REFLTYPE, refl_type, verbose=this%verbose, error=this%rc)
        call dm_geocom_api_request_set_user_prism_definition(request, prism_name, prism_const, refl_type_, creator)
        call this%send(request, delay)
    end subroutine geocom_set_user_prism_definition

    subroutine geocom_set_user_spiral(this, hz, v, delay)
        !! Sends *AUT_SetUserSpiral* request to sensor. The procedure sets the
        !! dimensions of the ATR search window (GeoCOM robotic licence
        !! required).
        class(geocom_class), intent(inout)        :: this  !! GeoCOM object.
        real(kind=r8),       intent(in)           :: hz    !! ATR search window in Hz direction [rad].
        real(kind=r8),       intent(in)           :: v     !! ATR search window in V direction [rad].
        integer,             intent(in), optional :: delay !! Post-request delay [msec].

        type(request_type) :: request

        call this%reset()
        call dm_geocom_api_request_set_user_spiral(request, hz, v)
        call this%send(request, delay)
    end subroutine geocom_set_user_spiral

    subroutine geocom_set_velocity(this, omega_hz, omega_v, delay)
        !! Sends *MOT_SetVelocity* request to sensor. The procedure drives the
        !! instrument with constant speed (GeoCOM robotic licence required).
        !!
        !! The API function is used to set up the velocity of the motorisation.
        !! The method `start_controller()` must have been called with argument
        !! `GEOCOM_MOT_OCONST` before.
        !!
        !! The velocity in horizontal and vertical direction are in [rad/s].
        !! The maximum velocity is ±3.14 rad/s for TM30/TS30, and ±0.79 rad/s
        !! for TPS1100/TPS1200.
        class(geocom_class), intent(inout)        :: this     !! GeoCOM object.
        real(kind=r8),       intent(in)           :: omega_hz !! Velocity in Hz direction [rad/s].
        real(kind=r8),       intent(in)           :: omega_v  !! Velocity in V direction [rad/s].
        integer,             intent(in), optional :: delay    !! Post-request delay [msec].

        type(request_type) :: request

        call this%reset()
        call dm_geocom_api_request_set_velocity(request, omega_hz, omega_v)
        call this%send(request, delay)
    end subroutine geocom_set_velocity

    subroutine geocom_setup_download(this, device_type, file_type, file_name, block_size, nblocks, delay)
        !! Sends *FTR_SetupDownload* request to sensor. The procedure sets up a
        !! file download.
        !!
        !! This method must be called before `download()`. If the file type is
        !! `GEOCOM_FTR_FILE_UNKNOWN`, an additional file path is required.
        !!
        !! The argument `device_type` must be one of the following enumerators:
        !!
        !! * `GEOCOM_FTR_DEVICE_INTERNAL` – Internal memory (path `/ata1a/`).
        !! * `GEOCOM_FTR_DEVICE_PCPARD`   – CF Card (path `/ata0a/`).
        !!
        !! The argument `file_type` is usually `GEOCOM_FTR_FILE_IMAGES`. The
        !! maximum value for `block_size` is `GEOCOM_FTR_MAX_BLOCKSIZE`.
        class(geocom_class), intent(inout)        :: this        !! GeoCOM object.
        integer,             intent(in)           :: device_type !! Device type (`GEOCOM_FTR_DEVICETYPE`).
        integer,             intent(in)           :: file_type   !! File type (`GEOCOM_FTR_FILETYPE`).
        character(len=*),    intent(in)           :: file_name   !! File name with extension.
        integer,             intent(in)           :: block_size  !! Block size.
        integer,             intent(out)          :: nblocks     !! Number of blocks required to upload the file.
        integer,             intent(in), optional :: delay       !! Post-request delay [msec].

        integer            :: device_type_, file_type_
        integer            :: rc1, rc2
        type(request_type) :: request

        call this%reset()
        device_type_ = dm_geocom_type_validated(GEOCOM_FTR_DEVICETYPE, device_type, verbose=this%verbose, error=rc1)
        file_type_   = dm_geocom_type_validated(GEOCOM_FTR_FILETYPE,   file_type,   verbose=this%verbose, error=rc2)
        this%rc      = max(rc1, rc2)
        call dm_geocom_api_request_setup_download(request, device_type_, file_type_, file_name, block_size)
        call this%send(request, delay)
        call dm_request_get(this%request, 'nblocks', nblocks, default=0)
    end subroutine geocom_setup_download

    subroutine geocom_setup_list(this, delay, device_type, file_type, search_path)
        !! Sends *FTR_SetupList* request to sensor. The procedure sets up the
        !! device type, file type, and search path. It has to be called before
        !! `list()`.
        class(geocom_class), intent(inout)        :: this        !! GeoCOM object.
        integer,             intent(in)           :: device_type !! Device type (`GEOCOM_FTR_DEVICETYPE`).
        integer,             intent(in)           :: file_type   !! File type (`GEOCOM_FTR_FILETYPE`).
        character(len=*),    intent(in), optional :: search_path !! Optional search path, required for file type `GEOCOM_FTR_FILE_UNKNOWN`.
        integer,             intent(in), optional :: delay       !! Post-request delay [msec].

        integer            :: device_type_, file_type_
        integer            :: rc1, rc2
        type(request_type) :: request

        call this%reset()
        device_type_ = dm_geocom_type_validated(GEOCOM_FTR_DEVICETYPE, device_type, verbose=this%verbose, error=rc1)
        file_type_   = dm_geocom_type_validated(GEOCOM_FTR_FILETYPE,   file_type,   verbose=this%verbose, error=rc2)
        this%rc      = max(rc1, rc2)
        call dm_geocom_api_request_setup_list(request, device_type_, file_type_, search_path)
        call this%send(request, delay)
    end subroutine geocom_setup_list

    subroutine geocom_start_controller(this, start_mode, delay)
        !! Sends *MOT_StartController* request to sensor. The procedure starts
        !! the motor controller.
        !!
        !! If this function is used in combination with API call
        !! `set_velocity()`, the controller mode has to be `GEOCOM_MOT_OCONST`.
        !!
        !! The argument `start_mode` must be one of the following enumerators:
        !!
        !! * `GEOCOM_MOT_POSIT`   –  Relative positioning.
        !! * `GEOCOM_MOT_OCONST`  –  Constant speed.
        !! * `GEOCOM_MOT_MANUPOS` –  Manual positioning (default setting).
        !! * `GEOCOM_MOT_LOCK`    –  “Lock-in” controller.
        !! * `GEOCOM_MOT_BREAK`   –  “Brake” controller.
        !! * `GEOCOM_MOT_TERM`    –  Terminates the controller task.
        !!
        class(geocom_class), intent(inout)        :: this       !! GeoCOM object.
        integer,             intent(in)           :: start_mode !! Controller start mode (`GEOCOM_MOT_MODE`).
        integer,             intent(in), optional :: delay      !! Post-request delay [msec].

        integer            :: start_mode_
        type(request_type) :: request

        call this%reset()
        start_mode_ = dm_geocom_type_validated(GEOCOM_MOT_MODE, start_mode, verbose=this%verbose, error=this%rc)
        call dm_geocom_api_request_start_controller(request, start_mode_)
        call this%send(request, delay)
    end subroutine geocom_start_controller

    subroutine geocom_stop_controller(this, stop_mode, delay)
        !! Sends *MOT_StartController* request to sensor. The procedure stops
        !! the movement and the motor controller program.
        !!
        !! The argument `stop_mode` must be one of the following enumerators:
        !!
        !! * `GEOCOM_MOT_NORMAL`   – Slow down with current acceleration.
        !! * `GEOCOM_MOT_SHUTDOWN` – Slow down by switching off power supply.
        !!
        class(geocom_class), intent(inout)        :: this      !! GeoCOM object.
        integer,             intent(in)           :: stop_mode !! Controller stop mode (`GEOCOM_MOT_STOPMODE`).
        integer,             intent(in), optional :: delay     !! Post-request delay [msec].

        integer            :: stop_mode_
        type(request_type) :: request

        call this%reset()
        stop_mode_ = dm_geocom_type_validated(GEOCOM_MOT_STOPMODE, stop_mode, verbose=this%verbose, error=this%rc)
        call dm_geocom_api_request_stop_controller(request, stop_mode_)
        call this%send(request, delay)
    end subroutine geocom_stop_controller

    subroutine geocom_switch_off(this, stop_mode, delay)
        !! Sends *COM_SwitchOffTPS* request to sensor. The procedure turns the
        !! instrument off.
        !!
        !! The argument `stop_mode` has to be one of the following enumerators:
        !!
        !! * `GEOCOM_COM_TPS_STOP_SHUT_DOWN` – Power down instrument.
        !! * `GEOCOM_COM_TPS_STOP_SLEEP`     – Sleep mode (not supported by TPS1200).
        !!
        class(geocom_class), intent(inout)        :: this      !! GeoCOM object.
        integer,             intent(in)           :: stop_mode !! Switch-off mode (`GEOCOM_COM_TPS_STOP_MODE`).
        integer,             intent(in), optional :: delay     !! Post-request delay [msec].

        integer            :: stop_mode_
        type(request_type) :: request

        call this%reset()
        stop_mode_ = dm_geocom_type_validated(GEOCOM_COM_TPS_STOP_MODE, stop_mode, verbose=this%verbose, error=this%rc)
        call dm_geocom_api_request_switch_off(request, stop_mode_)
        call this%send(request, delay)
    end subroutine geocom_switch_off

    subroutine geocom_switch_on(this, start_mode, delay)
        !! Sends *COM_SwitchOnTPS* request to sensor. The procedure turns the
        !! instrument on.
        !!
        !! The argument `start_mode` has to be one of the following enumerators:
        !!
        !! * `GEOCOM_COM_TPS_STARTUP_LOCAL`  – Not supported by TPS1200.
        !! * `GEOCOM_COM_TPS_STARTUP_REMOTE` – Online mode (RPC is enabled).
        !!
        !! The response is not captured by the returned request.
        class(geocom_class), intent(inout)        :: this       !! GeoCOM object.
        integer,             intent(in)           :: start_mode !! Switch-on mode (`GEOCOM_COM_TPS_STARTUP_MODE`).
        integer,             intent(in), optional :: delay      !! Post-request delay [msec].

        integer            :: start_mode_
        type(request_type) :: request

        call this%reset()
        start_mode_ = dm_geocom_type_validated(GEOCOM_COM_TPS_STARTUP_MODE, start_mode, verbose=this%verbose, error=this%rc)
        call dm_geocom_api_request_switch_on(request, start_mode_)
        call this%send(request, delay)
    end subroutine geocom_switch_on

    subroutine geocom_take_image(this, mem_type, image_number, delay)
        !! Sends *IMG_TakeTccImage* request to sensor. The procedure captures a
        !! telescope image.
        !!
        !! The memory type `mem_type` has to be one of the following
        !! enumerators:
        !!
        !! * `GEOCOM_IMG_INTERNAL_MEMORY` – Internal memory module.
        !! * `GEOCOM_IMG_PC_CARD`         – External PC card.
        !!
        class(geocom_class), intent(inout)        :: this         !! GeoCOM object.
        integer,             intent(in)           :: mem_type     !! Memory type (`GEOCOM_IMG_MEM_TYPE`).
        integer(kind=i8),    intent(out)          :: image_number !! Number of the currently captured image.
        integer,             intent(in), optional :: delay        !! Post-request delay [msec].

        integer            :: mem_type_
        type(request_type) :: request

        call this%reset()
        mem_type_ = dm_geocom_type_validated(GEOCOM_IMG_MEM_TYPE, mem_type, verbose=this%verbose, error=this%rc)
        call dm_geocom_api_request_take_image(request, mem_type_)
        call this%send(request, delay)
        call dm_request_get(this%request, 'imageno', image_number, default=0_i8)
    end subroutine geocom_take_image
end module dm_geocom
