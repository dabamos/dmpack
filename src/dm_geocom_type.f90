! Author:  Philipp Engel
! Licence: ISC
module dm_geocom_type
    !! GeoCOM API types enumerators. All GeoCOM named parameters start with
    !! prefix `GEOCOM_`.
    use :: dm_error
    implicit none (type, external)
    private

    ! GeoCOM enumeration types.
    integer, parameter, public :: GEOCOM_AUT_ADJMODE           = 1  !! Fine-adjust position mode.
    integer, parameter, public :: GEOCOM_AUT_ATRMODE           = 2  !! Automatic target recognition mode.
    integer, parameter, public :: GEOCOM_AUT_POSMODE           = 3  !! Position precision.
    integer, parameter, public :: GEOCOM_BAP_ATRSETTING        = 4  !! ATR Low-Vis mode definition.
    integer, parameter, public :: GEOCOM_BAP_MEASURE_PRG       = 5  !! Measurement modes.
    integer, parameter, public :: GEOCOM_BAP_PRISMDEF          = 6  !! Prism definition.
    integer, parameter, public :: GEOCOM_BAP_PRISMTYPE         = 7  !! Prism type definition.
    integer, parameter, public :: GEOCOM_BAP_REFLTYPE          = 8  !! Reflector type definition.
    integer, parameter, public :: GEOCOM_BAP_TARGET_TYPE       = 9  !! Target type definition.
    integer, parameter, public :: GEOCOM_BAP_USER_MEASPRG      = 10 !! Distance measurement programs.
    integer, parameter, public :: GEOCOM_COM_BAUD_RATE         = 11 !! Baud rate.
    integer, parameter, public :: GEOCOM_COM_FORMAT            = 12 !! Transmission data format.
    integer, parameter, public :: GEOCOM_COM_TPS_STARTUP_MODE  = 13 !! Start mode.
    integer, parameter, public :: GEOCOM_COM_TPS_STOP_MODE     = 14 !! Stop mode.
    integer, parameter, public :: GEOCOM_CSV_POWER_PATH        = 15 !! Power sources.
    integer, parameter, public :: GEOCOM_EDM_EGLINTENSITY_TYPE = 16 !! Intensity of Electronic Guidelight.
    integer, parameter, public :: GEOCOM_EDM_MODE              = 17 !! EDM measurement mode.
    integer, parameter, public :: GEOCOM_FTR_DEVICETYPE        = 18 !! Device type.
    integer, parameter, public :: GEOCOM_FTR_FILETYPE          = 19 !! File type.
    integer, parameter, public :: GEOCOM_IMG_MEM_TYPE          = 20 !! Memory device type.
    integer, parameter, public :: GEOCOM_MOT_LOCK_STATUS       = 21 !! Lock conditions.
    integer, parameter, public :: GEOCOM_MOT_MODE              = 22 !! Controller configuration.
    integer, parameter, public :: GEOCOM_MOT_STOPMODE          = 23 !! Controller stop mode.
    integer, parameter, public :: GEOCOM_SUP_AUTO_POWER        = 24 !! Automatic shutdown mechanism for the system.
    integer, parameter, public :: GEOCOM_TMC_FACE              = 25 !! Actual face.
    integer, parameter, public :: GEOCOM_TMC_FACE_DEF          = 26 !! Face position.
    integer, parameter, public :: GEOCOM_TMC_INCLINE_PRG       = 27 !! Inclination sensor measurement program.
    integer, parameter, public :: GEOCOM_TMC_MEASURE_PRG       = 28 !! TMC measurement mode.
    integer, parameter, public :: GEOCOM_TPS_DEVICE_CLASS      = 29 !! TPS device precision class.
    integer, parameter, public :: GEOCOM_TPS_DEVICE_TYPE       = 30 !! TPS device configuration type.
    integer, parameter, public :: GEOCOM_TPS_REFLESS_CLASS     = 31 !! Reflectorless class.

    ! **************************************************************************
    ! AUT - AUTOMATION.
    ! **************************************************************************
    ! GEOCOM_AUT_ADJMODE
    integer, parameter, public :: GEOCOM_AUT_NORM_MODE   = 0            !! Angle tolerance.
    integer, parameter, public :: GEOCOM_AUT_POINT_MODE  = 1            !! Point tolerance.
    integer, parameter, public :: GEOCOM_AUT_DEFINE_MODE = 2            !! System independent positioning tolerance.

    ! GEOCOM_AUT_ATRMODE
    integer, parameter, public :: GEOCOM_AUT_POSITION = 0               !! Positioning to Hz and V angle.
    integer, parameter, public :: GEOCOM_AUT_TARGET   = 1               !! Positioning to a target in the env. of the Hz and V angle.

    ! GEOCOM_AUT_POSMODE
    integer, parameter, public :: GEOCOM_AUT_NORMAL  = 0                !! Fast positioning mode.
    integer, parameter, public :: GEOCOM_AUT_PRECISE = 1                !! Exact positioning mode.
    integer, parameter, public :: GEOCOM_AUT_FAST    = 2                !! For TM30/TS30.

    integer, parameter, public :: GEOCOM_AUT_CLOCKWISE     = 1          !! Direction close-wise.
    integer, parameter, public :: GEOCOM_AUT_ANTICLOCKWISE = -1         !! Direction counter clock-wise.

    ! **************************************************************************
    ! BAP - BASIC APPLICATIONS.
    ! **************************************************************************
    ! GEOCOM_BAP_ATRSETTING
    integer, parameter, public :: GEOCOM_BAP_ATRSET_NORMAL     = 0      !! ATR is using no special flags or modes.
    integer, parameter, public :: GEOCOM_BAP_ATRSET_LOWVIS_ON  = 1      !! ATR low vis mode on.
    integer, parameter, public :: GEOCOM_BAP_ATRSET_LOWVIS_AON = 2      !! ATR low vis mode always on.
    integer, parameter, public :: GEOCOM_BAP_ATRSET_SRANGE_ON  = 3      !! ATR high reflectivity mode on.
    integer, parameter, public :: GEOCOM_BAP_ATRSET_SRANGE_AON = 4      !! ATR high reflectivity mode always on.

    ! GEOCOM_BAP_MEASURE_PRG
    integer, parameter, public :: GEOCOM_BAP_NO_MEAS    = 0             !! No measurements, take last one.
    integer, parameter, public :: GEOCOM_BAP_NO_DIST    = 1             !! No dist. measurement, angles only.
    integer, parameter, public :: GEOCOM_BAP_DEF_DIST   = 2             !! Default distance measurements.
    integer, parameter, public :: GEOCOM_BAP_CLEAR_DIST = 5             !! Clear distances.
    integer, parameter, public :: GEOCOM_BAP_STOP_TRK   = 6             !! Stop tracking.

    ! GEOCOM_BAP_PRISMDEF
    integer, parameter, public :: GEOCOM_BAP_PRISMNAME_LEN = 16         !! Prism name string length.

    ! GEOCOM_BAP_PRISMTYPE
    integer, parameter, public :: GEOCOM_BAP_PRISM_ROUND        = 0     !! Leica Circular Prism.
    integer, parameter, public :: GEOCOM_BAP_PRISM_MINI         = 1     !! Leica Mini Prism.
    integer, parameter, public :: GEOCOM_BAP_PRISM_TAPE         = 2     !! Leica Reflector Tape.
    integer, parameter, public :: GEOCOM_BAP_PRISM_360          = 3     !! Leica 360° Prism.
    integer, parameter, public :: GEOCOM_BAP_PRISM_USER1        = 4     !! Not supported by TPS1200.
    integer, parameter, public :: GEOCOM_BAP_PRISM_USER2        = 5     !! Not supported by TPS1200.
    integer, parameter, public :: GEOCOM_BAP_PRISM_USER3        = 6     !! Not supported by TPS1200.
    integer, parameter, public :: GEOCOM_BAP_PRISM_360_MINI     = 7     !! Leica Mini 360° Prism
    integer, parameter, public :: GEOCOM_BAP_PRISM_MINI_ZERO    = 8     !! Leica Mini Zero Prism.
    integer, parameter, public :: GEOCOM_BAP_PRISM_USER         = 9     !! User Defined Prism.
    integer, parameter, public :: GEOCOM_BAP_PRISM_NDS_TAPE     = 10    !! Leica HDS Target.
    integer, parameter, public :: GEOCOM_BAP_PRISM_GRZ121_ROUND = 11    !! GRZ121 360º Prism for Machine Guidance.
    integer, parameter, public :: GEOCOM_BAP_PRISM_MA_MPR122    = 12    !! MPR122 360º Prism for Machine Guidance.

    ! GEOCOM_BAP_REFLTYPE
    integer, parameter, public :: GEOCOM_BAP_REFL_UNDEF = 0             !! Reflector not defined.
    integer, parameter, public :: GEOCOM_BAP_REFL_PRISM = 1             !! Reflector prism.
    integer, parameter, public :: GEOCOM_BAP_REFL_TAPE  = 2             !! Reflector tape.

    ! GEOCOM_BAP_USER_MEASPRG
    integer, parameter, public :: GEOCOM_BAP_SINGLE_REF_STANDARD  = 0   !! IR standard.
    integer, parameter, public :: GEOCOM_BAP_SINGLE_REF_FAST      = 1   !! IR fast.
    integer, parameter, public :: GEOCOM_BAP_SINGLE_REF_VISIBLE   = 2   !! LO standard.
    integer, parameter, public :: GEOCOM_BAP_SINGLE_RLESS_VISIBLE = 3   !! RL standard.
    integer, parameter, public :: GEOCOM_BAP_CONT_REF_STANDARD    = 4   !! IR tracking.
    integer, parameter, public :: GEOCOM_BAP_CONT_REF_FAST        = 5   !! Not supported by TPS1200.
    integer, parameter, public :: GEOCOM_BAP_CONT_RLESS_VISIBLE   = 6   !! RL fast tracking.
    integer, parameter, public :: GEOCOM_BAP_AVG_REF_STANDARD     = 7   !! IR average.
    integer, parameter, public :: GEOCOM_BAP_AVG_REF_VISIBLE      = 8   !! LO average.
    integer, parameter, public :: GEOCOM_BAP_AVG_RLESS_VISIBLE    = 9   !! RL average.
    integer, parameter, public :: GEOCOM_BAP_CONT_REF_SYNCHRO     = 10  !! IR synchro tracking.
    integer, parameter, public :: GEOCOM_BAP_SINGLE_REF_PRECISE   = 11  !! IR precise (TM30/TS30).

    ! GEOCOM_BAP_TARGET_TYPE
    integer, parameter, public :: GEOCOM_BAP_REFL_USE  = 0              !! With reflector.
    integer, parameter, public :: GEOCOM_BAP_REFL_LESS = 1              !! Without reflector.

    ! **************************************************************************
    ! BMM - BASIC MAN-MACHINE INTERFACE.
    ! **************************************************************************
    integer, parameter, public :: GEOCOM_IOS_BEEP_STDINTENS = 100       !! Standard intensity of beep expressed as percentage.

    ! **************************************************************************
    ! COM - COMMUNICATION SETTINGS.
    ! **************************************************************************
    ! GEOCOM_COM_BAUD_RATE
    integer, parameter, public :: GEOCOM_COM_BAUD_38400  = 0
    integer, parameter, public :: GEOCOM_COM_BAUD_19200  = 1    !! Default baud rate.
    integer, parameter, public :: GEOCOM_COM_BAUD_9600   = 2
    integer, parameter, public :: GEOCOM_COM_BAUD_4800   = 3
    integer, parameter, public :: GEOCOM_COM_BAUD_2400   = 4
    integer, parameter, public :: GEOCOM_COM_BAUD_115200 = 5
    integer, parameter, public :: GEOCOM_COM_BAUD_57600  = 6

    ! GEOCOM_COM_FORMAT
    integer, parameter, public :: GEOCOM_COM_ASCII  = 0         !! ASCII protocol.
    integer, parameter, public :: GEOCOM_COM_BINARY = 1         !! Binary protocol.

    ! GEOCOM_COM_TPS_STARTUP_MODE
    integer, parameter, public :: GEOCOM_COM_STARTUP_LOCAL  = 0 !! Not supported by TPS1200.
    integer, parameter, public :: GEOCOM_COM_STARTUP_REMOTE = 1 !! RPC is enabled (online mode).

    ! GEOCOM_COM_TPS_STOP_MODE
    integer, parameter, public :: GEOCOM_COM_STOP_SHUT_DOWN = 0 !! Power down instrument.
    integer, parameter, public :: GEOCOM_COM_STOP_SLEEP     = 1 !! Not supported by TPS1200.

    ! **************************************************************************
    ! CSV - CENTRAL SERVICES.
    ! **************************************************************************
    ! GEOCOM_CSV_POWER_PATH
    integer, parameter, public :: GEOCOM_CSV_EXTERNAL_POWER = 1             !! Power source is external.
    integer, parameter, public :: GEOCOM_CSV_INTERNAL_POWER = 2             !! Power source is the internal battery.

    ! GEOCOM_TPS_DEVICE_CLASS
    integer, parameter, public :: GEOCOM_TPS_CLASS_1100 = 0                 !! TPS1000 family member, 1 mgon, 3 ".
    integer, parameter, public :: GEOCOM_TPS_CLASS_1700 = 1                 !! TPS1000 family member, 0.5 mgon, 1.5 ".
    integer, parameter, public :: GEOCOM_TPS_CLASS_1800 = 2                 !! TPS1000 family member, 0.3 mgon, 1 ".
    integer, parameter, public :: GEOCOM_TPS_CLASS_5000 = 3                 !! TPS2000 family member.
    integer, parameter, public :: GEOCOM_TPS_CLASS_6000 = 4                 !! TPS2000 family member.
    integer, parameter, public :: GEOCOM_TPS_CLASS_1500 = 5                 !! TPS1000 family member.
    integer, parameter, public :: GEOCOM_TPS_CLASS_2003 = 6                 !! TPS2000 family member.
    integer, parameter, public :: GEOCOM_TPS_CLASS_5005 = 7                 !! TPS5000 family member.
    integer, parameter, public :: GEOCOM_TPS_CLASS_5100 = 8                 !! TPS5000 family member.
    integer, parameter, public :: GEOCOM_TPS_CLASS_1102 = 100               !! TPS1100 family member, 2 ".
    integer, parameter, public :: GEOCOM_TPS_CLASS_1103 = 101               !! TPS1100 family member, 3 ".
    integer, parameter, public :: GEOCOM_TPS_CLASS_1105 = 102               !! TPS1100 family member, 5 ".
    integer, parameter, public :: GEOCOM_TPS_CLASS_1101 = 103               !! TPS1100 family member, 1 ".
    integer, parameter, public :: GEOCOM_TPS_CLASS_1202 = 200               !! TPS1200 family member, 2 ".
    integer, parameter, public :: GEOCOM_TPS_CLASS_1203 = 201               !! TPS1200 family member, 3 ".
    integer, parameter, public :: GEOCOM_TPS_CLASS_1205 = 202               !! TPS1200 family member, 5 ".
    integer, parameter, public :: GEOCOM_TPS_CLASS_1201 = 203               !! TPS1200 family member, 1 ".
    integer, parameter, public :: GEOCOM_TPS_CLASS_TX30 = 300               !! TS30,TM30 family member, 0.5 ".
    integer, parameter, public :: GEOCOM_TPS_CLASS_TX31 = 301               !! TS30,TM30 family member, 1 ".

    ! GEOCOM_TPS_DEVICE_TYPE
    ! -- TPS1x00 common.
    integer, parameter, public :: GEOCOM_TPS_DEVICE_T      = int(z'00000')  !! Theodolite without built-in EDM.
    integer, parameter, public :: GEOCOM_TPS_DEVICE_MOT    = int(z'00004')  !! Motorized device.
    integer, parameter, public :: GEOCOM_TPS_DEVICE_ATR    = int(z'00008')  !! Automatic Target Recognition.
    integer, parameter, public :: GEOCOM_TPS_DEVICE_EGL    = int(z'00010')  !! Electronic Guide Light.
    integer, parameter, public :: GEOCOM_TPS_DEVICE_DB     = int(z'00020')  !! Reserved (Database, not GSI).
    integer, parameter, public :: GEOCOM_TPS_DEVICE_DL     = int(z'00040')  !! Diode laser.
    integer, parameter, public :: GEOCOM_TPS_DEVICE_LP     = int(z'00080')  !! Laser plumbed.
    ! -- TPS1000 specific.
    integer, parameter, public :: GEOCOM_TPS_DEVICE_TC1    = int(z'00001')  !! Tachymeter (TCW1).
    integer, parameter, public :: GEOCOM_TPS_DEVICE_TC2    = int(z'00002')  !! Tachymeter (TCW2).
    ! -- TPS1100/TPS1200 specific.
    integer, parameter, public :: GEOCOM_TPS_DEVICE_TC     = int(z'00001')  !! Tachymeter (TCW3).
    integer, parameter, public :: GEOCOM_TPS_DEVICE_TCR    = int(z'00002')  !! Tachymeter (TCW3 with red laser).
    integer, parameter, public :: GEOCOM_TPS_DEVICE_ATC    = int(z'00100')  !! Autocollimation lamp (used only PMU).
    integer, parameter, public :: GEOCOM_TPS_DEVICE_LPNT   = int(z'00200')  !! Laserpointer.
    integer, parameter, public :: GEOCOM_TPS_DEVICE_RL_EXT = int(z'00400')  !! Reflectorless EDM with extended range (Pinpoint R100, R300).
    integer, parameter, public :: GEOCOM_TPS_DEVICE_PS     = int(z'00800')  !! Power Search.
    ! -- TPSSim specific.
    integer, parameter, public :: GEOCOM_TPS_DEVICE_SIM    = int(z'04000')  !! Runs on simulation, no hardware.

    ! GEOCOM_TPS_REFLESS_CLASS
    integer, parameter, public :: GEOCOM_TPS_REFLESS_NONE  = 0
    integer, parameter, public :: GEOCOM_TPS_REFLESS_R100  = 1              !! Pinpoint R100.
    integer, parameter, public :: GEOCOM_TPS_REFLESS_R300  = 2              !! Pinpoint R300.
    integer, parameter, public :: GEOCOM_TPS_REFLESS_R400  = 3              !! Pinpoint R400.
    integer, parameter, public :: GEOCOM_TPS_REFLESS_R1000 = 4              !! Pinpoint R1000.

    ! **************************************************************************
    ! EDM - ELECTRONIC DISTANCE MEASUREMENT.
    ! **************************************************************************
    ! GEOCOM_EDM_EGLINTENSITY_TYPE
    integer, parameter, public :: GEOCOM_EDM_EGLINTEN_OFF  = 0
    integer, parameter, public :: GEOCOM_EDM_EGLINTEN_LOW  = 1
    integer, parameter, public :: GEOCOM_EDM_EGLINTEN_MID  = 2
    integer, parameter, public :: GEOCOM_EDM_EGLINTEN_HIGH = 3

    ! GEOCOM_EDM_MODE
    integer, parameter, public :: GEOCOM_EDM_MODE_NOT_USED   = 0    !! Initial value.
    integer, parameter, public :: GEOCOM_EDM_SINGLE_TAPE     = 1    !! IR Standard Reflector Tape.
    integer, parameter, public :: GEOCOM_EDM_SINGLE_STANDARD = 2    !! IR Standard.
    integer, parameter, public :: GEOCOM_EDM_SINGLE_FAST     = 3    !! IR Fast.
    integer, parameter, public :: GEOCOM_EDM_SINGLE_LRANGE   = 4    !! LO Standard.
    integer, parameter, public :: GEOCOM_EDM_SINGLE_SRANGE   = 5    !! RL Standard.
    integer, parameter, public :: GEOCOM_EDM_CONT_STANDARD   = 6    !! Standard repeated measurement.
    integer, parameter, public :: GEOCOM_EDM_CONT_DYNAMIC    = 7    !! IR Tacking.
    integer, parameter, public :: GEOCOM_EDM_CONT_REFLESS    = 8    !! RL Tracking.
    integer, parameter, public :: GEOCOM_EDM_CONT_FAST       = 9    !! Fast repeated measurement.
    integer, parameter, public :: GEOCOM_EDM_AVERAGE_IR      = 10   !! IR Average.
    integer, parameter, public :: GEOCOM_EDM_AVERAGE_SR      = 11   !! RL Average.
    integer, parameter, public :: GEOCOM_EDM_AVERAGE_LR      = 12   !! LO Average.
    integer, parameter, public :: GEOCOM_EDM_PRECISE_IR      = 13   !! IR Precise (TM30, TS30).
    integer, parameter, public :: GEOCOM_EDM_PRECISE_TAPE    = 14   !! IR Precise Reflector Tape (TM30, TS30).

    ! **************************************************************************
    ! FTR - FILE TRANSFER.
    ! **************************************************************************
    integer, parameter, public :: GEOCOM_FTR_MAX_BLOCKSIZE = 450    !! Max. block size.

    ! GEOCOM_FTR_DEVICETYPE
    integer, parameter, public :: GEOCOM_FTR_DEVICE_INTERNAL = 0    !! Internal memory.
    integer, parameter, public :: GEOCOM_FTR_DEVICE_PCPARD   = 1    !! Memory card.

    ! GEOCOM_FTR_FILETYPE
    integer, parameter, public :: GEOCOM_FTR_FILE_UNKNOWN = 0       !! Undocumented.
    integer, parameter, public :: GEOCOM_FTR_FILE_IMAGES  = 170     !! Extension wildcard: `*.jpg`.

    ! **************************************************************************
    ! IMG - IMAGE PROCESSING.
    ! **************************************************************************
    ! GEOCOM_IMG_MEM_TYPE
    integer, parameter, public :: GEOCOM_IMG_INTERNAL_MEMORY = int(z'0')    !! Internal memory module.
    integer, parameter, public :: GEOCOM_IMG_PC_CARD         = int(z'1')    !! External PC Card.

    integer, parameter, public :: GEOCOM_IMG_MAX_FILE_PREFIX_LEN = 20       !! Length of file name prefix.

    ! **************************************************************************
    ! MOT - MOTORISATION.
    ! **************************************************************************
    ! GEOCOM_MOT_LOCK_STATUS
    integer, parameter, public :: GEOCOM_MOT_LOCKED_OUT = 0     !! Locked out.
    integer, parameter, public :: GEOCOM_MOT_LOCKED_IN  = 1     !! Locked in.
    integer, parameter, public :: GEOCOM_MOT_PREDICTION = 2     !! Prediction mode.

    ! GEOCOM_MOT_MODE
    integer, parameter, public :: GEOCOM_MOT_POSIT   = 0        !! Configured for relative positioning.
    integer, parameter, public :: GEOCOM_MOT_OCONST  = 1        !! Configured for constant speed.
    integer, parameter, public :: GEOCOM_MOT_MANUPOS = 2        !! Configured for manual positioning (default setting).
    integer, parameter, public :: GEOCOM_MOT_LOCK    = 3        !! Configured as "Lock-In" controller.
    integer, parameter, public :: GEOCOM_MOT_BREAK   = 4        !! Configured as "Brake" controller.
    integer, parameter, public :: GEOCOM_MOT_TERM    = 7        !! Terminates the controller task.

    ! GEOCOM_MOT_STOPMODE
    integer, parameter, public :: GEOCOM_MOT_NORMAL   = 0       !! Slow down with current acceleration.
    integer, parameter, public :: GEOCOM_MOT_SHUTDOWN = 1       !! Slow down by switch off power supply.

    ! **************************************************************************
    ! SUP - SUPERVISOR.
    ! **************************************************************************
    ! GEOCOM_SUP_AUTO_POWER
    integer, parameter, public :: GEOCOM_SUP_POWER_DISABLED = 0 !! Instrument remains on.
    integer, parameter, public :: GEOCOM_SUP_POWER_OFF      = 2 !! Turns off mechanism.

    ! **************************************************************************
    ! TMC - THEODOLITE MEASUREMENT AND CALCULATION.
    ! **************************************************************************
    ! GEOCOM_TMC_FACE
    integer, parameter, public :: GEOCOM_TMC_FACE_1 = 0         !! Position 1 of telescope.
    integer, parameter, public :: GEOCOM_TMC_FACE_2 = 1         !! Position 2 of telescope.

    ! GEOCOM_TMC_FACE_DEF
    integer, parameter, public :: GEOCOM_TMC_FACE_NORMAL = 0    !! Face in normal position.
    integer, parameter, public :: GEOCOM_TMC_FACE_TURN   = 1    !! Face turned.

    ! GEOCOM_TMC_INCLINE_PRG
    integer, parameter, public :: GEOCOM_TMC_MEA_INC      = 0   !! Use sensor (a priori sigma).
    integer, parameter, public :: GEOCOM_TMC_AUTO_INC     = 1   !! Automatic mode (sensor/plane).
    integer, parameter, public :: GEOCOM_TMC_PLANE_INC    = 2   !! Use plane (a priori sigma).

    ! GEOCOM_TMC_MEASURE_PRG
    integer, parameter, public :: GEOCOM_TMC_STOP         = 0   !! Stop measurement program.
    integer, parameter, public :: GEOCOM_TMC_DEF_DIST     = 1   !! Default DIST-measurement program.
    integer, parameter, public :: GEOCOM_TMC_CLEAR        = 3   !! TMC_STOP and clear data.
    integer, parameter, public :: GEOCOM_TMC_SIGNAL       = 4   !! Signal measurement (test function).
    integer, parameter, public :: GEOCOM_TMC_DO_MEASURE   = 6   !! (Re-)start measurement task.
    integer, parameter, public :: GEOCOM_TMC_RTRK_DIST    = 8   !! Distance-TRK measurement program.
    integer, parameter, public :: GEOCOM_TMC_RED_TRK_DIST = 10  !! Reflectorless tracking.
    integer, parameter, public :: GEOCOM_TMC_FREQUENCY    = 11  !! Frequency measurement (test).

    ! Public procedures.
    public :: dm_geocom_type_validated
contains
    ! **************************************************************************
    ! PUBLIC PROCEDURES.
    ! **************************************************************************
    integer function dm_geocom_type_validated(type, value, default, error) result(n)
        !! Parameterisation function for GeoCOM enumeration types.
        !!
        !! Returns argument `value` if it is a valid enumerator of `type`, else
        !! the default value of that type. If argument `type` is not found or
        !! not supported, the function returns 0. If argument `default` is
        !! passed, it is returned on error.
        !!
        !! If argument `default` is not passed and `type` is valid, one of the
        !! following values is returned:
        !!
        !! | Type                           | Default Value                    |
        !! |--------------------------------|----------------------------------|
        !! | `GEOCOM_AUT_ADJMODE`           | `GEOCOM_AUT_NORM_MODE`           |
        !! | `GEOCOM_AUT_ATRMODE`           | `GEOCOM_AUT_POSITION`            |
        !! | `GEOCOM_AUT_POSMODE`           | `GEOCOM_AUT_NORMAL`              |
        !! | `GEOCOM_BAP_MEASURE_PRG`       | `GEOCOM_BAP_DEF_DIST`            |
        !! | `GEOCOM_BAP_PRISMTYPE`         | `GEOCOM_BAP_PRISM_ROUND`         |
        !! | `GEOCOM_BAP_REFLTYPE`          | `GEOCOM_BAP_REFL_UNDEF`          |
        !! | `GEOCOM_BAP_TARGET_TYPE`       | `GEOCOM_BAP_REFL_USE`            |
        !! | `GEOCOM_BAP_USER_MEASPRG`      | `GEOCOM_BAP_SINGLE_REF_STANDARD` |
        !! | `GEOCOM_COM_TPS_STARTUP_MODE`  | `GEOCOM_COM_STARTUP_REMOTE`      |
        !! | `GEOCOM_COM_TPS_STOP_MODE`     | `GEOCOM_COM_STOP_SHUT_DOWN`      |
        !! | `GEOCOM_EDM_EGLINTENSITY_TYPE` | `GEOCOM_EDM_EGLINTEN_OFF`        |
        !! | `GEOCOM_EDM_MODE`              | `GEOCOM_EDM_MODE_NOT_USED`       |
        !! | `GEOCOM_FTR_DEVICETYPE`        | `GEOCOM_FTR_DEVICE_INTERNAL`     |
        !! | `GEOCOM_FTR_FILETYPE`          | `GEOCOM_FTR_FILE_UNKNOWN`        |
        !! | `GEOCOM_IMG_MEM_TYPE`          | `GEOCOM_IMG_INTERNAL_MEMORY`     |
        !! | `GEOCOM_MOT_MODE`              | `GEOCOM_MOT_MANUPOS`             |
        !! | `GEOCOM_MOT_STOPMODE`          | `GEOCOM_MOT_NORMAL`              |
        !! | `GEOCOM_TMC_INCLINE_PRG`       | `GEOCOM_TMC_MEA_INC`             |
        !! | `GEOCOM_TMC_MEASURE_PRG`       | `GEOCOM_TMC_DEF_DIST`            |
        !!
        !! The function returns the following error codes in `error`:
        !!
        !! * `E_TYPE` if the type is not found or not supported.
        !! * `E_INVALID` if the value is not of given type.
        integer, intent(in)            :: type    !! GeoCOM enumeration type.
        integer, intent(in)            :: value   !! Enumerator to validate.
        integer, intent(in),  optional :: default !! Value to return on error.
        integer, intent(out), optional :: error   !! Error code.

        integer :: rc

        rc = E_INVALID
        n  = 0

        select case (type)
            case (GEOCOM_AUT_ADJMODE)
                select case (value)
                    case (GEOCOM_AUT_NORM_MODE,  &
                          GEOCOM_AUT_POINT_MODE, &
                          GEOCOM_AUT_DEFINE_MODE)
                        rc = E_NONE
                        n  = value
                    case default
                        n = GEOCOM_AUT_NORM_MODE
                end select

            case (GEOCOM_AUT_ATRMODE)
                select case (value)
                    case (GEOCOM_AUT_POSITION, &
                          GEOCOM_AUT_TARGET)
                        rc = E_NONE
                        n  = value
                    case default
                        n = GEOCOM_AUT_POSITION
                end select

            case (GEOCOM_AUT_POSMODE)
                select case (value)
                    case (GEOCOM_AUT_NORMAL,  &
                          GEOCOM_AUT_PRECISE, &
                          GEOCOM_AUT_FAST)
                        rc = E_NONE
                        n  = value
                    case default
                        n = GEOCOM_AUT_NORMAL
                end select

            case (GEOCOM_BAP_MEASURE_PRG)
                select case (value)
                    case (GEOCOM_BAP_NO_MEAS,    &
                          GEOCOM_BAP_NO_DIST,    &
                          GEOCOM_BAP_DEF_DIST,   &
                          GEOCOM_BAP_CLEAR_DIST, &
                          GEOCOM_BAP_STOP_TRK)
                        rc = E_NONE
                        n  = value
                    case default
                        n = GEOCOM_BAP_DEF_DIST
                end select

            case (GEOCOM_BAP_PRISMTYPE)
                select case (value)
                    case (GEOCOM_BAP_PRISM_ROUND,        &
                          GEOCOM_BAP_PRISM_MINI,         &
                          GEOCOM_BAP_PRISM_TAPE,         &
                          GEOCOM_BAP_PRISM_360,          &
                          GEOCOM_BAP_PRISM_USER1,        &
                          GEOCOM_BAP_PRISM_USER2,        &
                          GEOCOM_BAP_PRISM_USER3,        &
                          GEOCOM_BAP_PRISM_360_MINI,     &
                          GEOCOM_BAP_PRISM_MINI_ZERO,    &
                          GEOCOM_BAP_PRISM_USER,         &
                          GEOCOM_BAP_PRISM_NDS_TAPE,     &
                          GEOCOM_BAP_PRISM_GRZ121_ROUND, &
                          GEOCOM_BAP_PRISM_MA_MPR122)
                        rc = E_NONE
                        n  = value
                    case default
                        n = GEOCOM_BAP_PRISM_ROUND
                end select

            case (GEOCOM_BAP_REFLTYPE)
                select case (value)
                    case (GEOCOM_BAP_REFL_UNDEF, &
                          GEOCOM_BAP_REFL_PRISM, &
                          GEOCOM_BAP_REFL_TAPE)
                        rc = E_NONE
                        n  = value
                    case default
                        n = GEOCOM_BAP_REFL_UNDEF
                end select

            case (GEOCOM_BAP_TARGET_TYPE)
                select case (value)
                    case (GEOCOM_BAP_REFL_USE, &
                          GEOCOM_BAP_REFL_LESS)
                        rc = E_NONE
                        n  = value
                    case default
                        n = GEOCOM_BAP_REFL_USE
                end select

            case (GEOCOM_BAP_USER_MEASPRG)
                select case (value)
                    case (GEOCOM_BAP_SINGLE_REF_STANDARD,  &
                          GEOCOM_BAP_SINGLE_REF_FAST,      &
                          GEOCOM_BAP_SINGLE_REF_VISIBLE,   &
                          GEOCOM_BAP_SINGLE_RLESS_VISIBLE, &
                          GEOCOM_BAP_CONT_REF_STANDARD,    &
                          GEOCOM_BAP_CONT_REF_FAST,        &
                          GEOCOM_BAP_CONT_RLESS_VISIBLE,   &
                          GEOCOM_BAP_AVG_REF_STANDARD,     &
                          GEOCOM_BAP_AVG_REF_VISIBLE,      &
                          GEOCOM_BAP_AVG_RLESS_VISIBLE,    &
                          GEOCOM_BAP_CONT_REF_SYNCHRO,     &
                          GEOCOM_BAP_SINGLE_REF_PRECISE)
                        rc = E_NONE
                        n  = value
                    case default
                        n = GEOCOM_BAP_SINGLE_REF_STANDARD
                end select

            case (GEOCOM_COM_TPS_STARTUP_MODE)
                select case (value)
                    case (GEOCOM_COM_STARTUP_LOCAL, &
                          GEOCOM_COM_STARTUP_REMOTE)
                        rc = E_NONE
                        n  = value
                    case default
                        n = GEOCOM_COM_STARTUP_REMOTE
                end select

            case (GEOCOM_COM_TPS_STOP_MODE)
                select case (value)
                    case (GEOCOM_COM_STOP_SHUT_DOWN, &
                          GEOCOM_COM_STOP_SLEEP)
                        rc = E_NONE
                        n  = value
                    case default
                        n = GEOCOM_COM_STOP_SHUT_DOWN
                end select

            case (GEOCOM_EDM_EGLINTENSITY_TYPE)
                select case (value)
                    case (GEOCOM_EDM_EGLINTEN_OFF, &
                          GEOCOM_EDM_EGLINTEN_LOW, &
                          GEOCOM_EDM_EGLINTEN_MID, &
                          GEOCOM_EDM_EGLINTEN_HIGH)
                        rc = E_NONE
                        n  = value
                    case default
                        n = GEOCOM_EDM_EGLINTEN_OFF
                end select

            case (GEOCOM_EDM_MODE)
                select case (value)
                    case (GEOCOM_EDM_MODE_NOT_USED,   &
                          GEOCOM_EDM_SINGLE_TAPE,     &
                          GEOCOM_EDM_SINGLE_STANDARD, &
                          GEOCOM_EDM_SINGLE_FAST,     &
                          GEOCOM_EDM_SINGLE_LRANGE,   &
                          GEOCOM_EDM_SINGLE_SRANGE,   &
                          GEOCOM_EDM_CONT_STANDARD,   &
                          GEOCOM_EDM_CONT_DYNAMIC,    &
                          GEOCOM_EDM_CONT_REFLESS,    &
                          GEOCOM_EDM_CONT_FAST,       &
                          GEOCOM_EDM_AVERAGE_IR,      &
                          GEOCOM_EDM_AVERAGE_SR,      &
                          GEOCOM_EDM_AVERAGE_LR,      &
                          GEOCOM_EDM_PRECISE_IR,      &
                          GEOCOM_EDM_PRECISE_TAPE)
                        rc = E_NONE
                        n  = value
                    case default
                        n = GEOCOM_EDM_MODE_NOT_USED
                end select

            case (GEOCOM_FTR_DEVICETYPE)
                select case (value)
                    case (GEOCOM_FTR_DEVICE_INTERNAL, &
                          GEOCOM_FTR_DEVICE_PCPARD)
                        rc = E_NONE
                        n  = value
                    case default
                        n = GEOCOM_FTR_DEVICE_INTERNAL
                end select

            case (GEOCOM_FTR_FILETYPE)
                select case (value)
                    case (GEOCOM_FTR_FILE_UNKNOWN, &
                          GEOCOM_FTR_FILE_IMAGES)
                        rc = E_NONE
                        n  = value
                    case default
                        n = GEOCOM_FTR_FILE_UNKNOWN
                end select

            case (GEOCOM_IMG_MEM_TYPE)
                select case (value)
                    case (GEOCOM_IMG_INTERNAL_MEMORY, &
                          GEOCOM_IMG_PC_CARD)
                        rc = E_NONE
                        n  = value
                    case default
                        n = GEOCOM_IMG_INTERNAL_MEMORY
                end select

            case (GEOCOM_MOT_MODE)
                select case (value)
                    case (GEOCOM_MOT_POSIT,   &
                          GEOCOM_MOT_OCONST,  &
                          GEOCOM_MOT_MANUPOS, &
                          GEOCOM_MOT_LOCK,    &
                          GEOCOM_MOT_BREAK,   &
                          GEOCOM_MOT_TERM)
                        rc = E_NONE
                        n  = value
                    case default
                        n = GEOCOM_MOT_MANUPOS
                end select

            case (GEOCOM_MOT_STOPMODE)
                select case (value)
                    case (GEOCOM_MOT_NORMAL, &
                          GEOCOM_MOT_SHUTDOWN)
                        rc = E_NONE
                        n  = value
                    case default
                        n = GEOCOM_MOT_NORMAL
                end select

            case (GEOCOM_TMC_INCLINE_PRG)
                select case (value)
                    case (GEOCOM_TMC_MEA_INC,  &
                          GEOCOM_TMC_AUTO_INC, &
                          GEOCOM_TMC_PLANE_INC)
                        rc = E_NONE
                        n  = value
                    case default
                        n = GEOCOM_TMC_MEA_INC
                end select

            case (GEOCOM_TMC_MEASURE_PRG)
                select case (value)
                    case (GEOCOM_TMC_STOP,         &
                          GEOCOM_TMC_DEF_DIST,     &
                          GEOCOM_TMC_CLEAR,        &
                          GEOCOM_TMC_SIGNAL,       &
                          GEOCOM_TMC_DO_MEASURE,   &
                          GEOCOM_TMC_RTRK_DIST,    &
                          GEOCOM_TMC_RED_TRK_DIST, &
                          GEOCOM_TMC_FREQUENCY)
                        rc = E_NONE
                        n  = value
                    case default
                        n = GEOCOM_TMC_DEF_DIST
                end select

            case default
                rc = E_TYPE
        end select

        if (dm_is_error(rc) .and. present(default)) n = default
        if (present(error)) error = rc
    end function dm_geocom_type_validated
end module dm_geocom_type
