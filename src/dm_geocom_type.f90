! Author:  Philipp Engel
! Licence: ISC
module dm_geocom_type
    !! GeoCOM API enumerators. All GeoCOM named parameters start with prefix
    !! `GEOCOM_`.
    use :: dm_kind
    use :: dm_request
    use :: dm_response
    use :: dm_util
    implicit none (type, external)
    private

    ! **************************************************************************
    ! AUT - AUTOMATION.
    ! **************************************************************************
    ! AUT_ADJMODE: Fine-adjust position mode.
    integer, parameter, public :: GEOCOM_AUT_NORM_MODE   = 0            !! Angle tolerance.
    integer, parameter, public :: GEOCOM_AUT_POINT_MODE  = 1            !! Point tolerance.
    integer, parameter, public :: GEOCOM_AUT_DEFINE_MODE = 2            !! System independent positioning tolerance.

    ! AUT_ATRMODE: Automatic target recognition mode.
    integer, parameter, public :: GEOCOM_AUT_POSITION = 0               !! Positioning to Hz and V angle.
    integer, parameter, public :: GEOCOM_AUT_TARGET   = 1               !! Positioning to a target in the env. of the Hz and V angle.

    ! AUT_POSMODE: Position precision.
    integer, parameter, public :: GEOCOM_AUT_NORMAL  = 0                !! Fast positioning mode.
    integer, parameter, public :: GEOCOM_AUT_PRECISE = 1                !! Exact positioning mode.
    integer, parameter, public :: GEOCOM_AUT_FAST    = 2                !! For TM30/TS30.

    integer, parameter, public :: GEOCOM_AUT_CLOCKWISE     = 1          !! Direction close-wise.
    integer, parameter, public :: GEOCOM_AUT_ANTICLOCKWISE = -1         !! Direction counter clock-wise.

    ! **************************************************************************
    ! BAP - BASIC APPLICATIONS.
    ! **************************************************************************
    ! BAP_MEASURE_PRG: Measurement modes.
    integer, parameter, public :: GEOCOM_BAP_NO_MEAS    = 0             !! No measurements, take last one.
    integer, parameter, public :: GEOCOM_BAP_NO_DIST    = 1             !! No dist. measurement, angles only.
    integer, parameter, public :: GEOCOM_BAP_DEF_DIST   = 2             !! Default distance measurements.
    integer, parameter, public :: GEOCOM_BAP_CLEAR_DIST = 5             !! Clear distances.
    integer, parameter, public :: GEOCOM_BAP_STOP_TRK   = 6             !! Stop tracking.

    ! BAP_USER_MEASPRG: Distance measurement programs.
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

    ! BAP_PRISMTYPE: Prism type definition.
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

    ! BAP_REFLTYPE: Reflector type definition.
    integer, parameter, public :: GEOCOM_BAP_REFL_UNDEF = 0             !! Reflector not defined.
    integer, parameter, public :: GEOCOM_BAP_REFL_PRISM = 1             !! Reflector prism.
    integer, parameter, public :: GEOCOM_BAP_REFL_TAPE  = 2             !! Reflector tape.

    ! BAP_TARGET_TYPE: Target type definition.
    integer, parameter, public :: GEOCOM_BAP_REFL_USE  = 0              !! With reflector.
    integer, parameter, public :: GEOCOM_BAP_REFL_LESS = 1              !! Without reflector.

    ! BAP_ATRSETTING: ATR Low-Vis mode definition.
    integer, parameter, public :: GEOCOM_BAP_ATRSET_NORMAL     = 0      !! ATR is using no special flags or modes.
    integer, parameter, public :: GEOCOM_BAP_ATRSET_LOWVIS_ON  = 1      !! ATR low vis mode on.
    integer, parameter, public :: GEOCOM_BAP_ATRSET_LOWVIS_AON = 2      !! ATR low vis mode always on.
    integer, parameter, public :: GEOCOM_BAP_ATRSET_SRANGE_ON  = 3      !! ATR high reflectivity mode on.
    integer, parameter, public :: GEOCOM_BAP_ATRSET_SRANGE_AON = 4      !! ATR high reflectivity mode always on.

    ! BAP_PRISMDEF: Prism definition.
    integer, parameter, public :: GEOCOM_BAP_PRISMNAME_LEN = 16         !! Prism name string length.

    ! **************************************************************************
    ! BMM - BASIC MAN-MACHINE INTERFACE.
    ! **************************************************************************
    integer, parameter, public :: GEOCOM_IOS_BEEP_STDINTENS = 100       !! Standard intensity of beep expressed as percentage.

    ! **************************************************************************
    ! COM - COMMUNICATION SETTINGS.
    ! **************************************************************************
    ! COM_FORMAT: Transmission data format.
    integer, parameter, public :: GEOCOM_COM_ASCII  = 0         !! ASCII protocol.
    integer, parameter, public :: GEOCOM_COM_BINARY = 1         !! Binary protocol.

    ! COM_BAUD_RATE: Baud rate.
    integer, parameter, public :: GEOCOM_COM_BAUD_38400  = 0
    integer, parameter, public :: GEOCOM_COM_BAUD_19200  = 1    !! Default baud rate.
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
    integer, parameter, public :: GEOCOM_CSV_EXTERNAL_POWER = 1             !! Power source is external.
    integer, parameter, public :: GEOCOM_CSV_INTERNAL_POWER = 2             !! Power source is the internal battery.

    ! TPS_DEVICE_CLASS: TPS device precision class.
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

    ! TPS_DEVICE_TYPE: TPS device configuration type.
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

    ! TPS_REFLESS_CLASS: Reflectorless class.
    integer, parameter, public :: GEOCOM_TPS_REFLESS_NONE  = 0
    integer, parameter, public :: GEOCOM_TPS_REFLESS_R100  = 1              !! Pinpoint R100.
    integer, parameter, public :: GEOCOM_TPS_REFLESS_R300  = 2              !! Pinpoint R300.
    integer, parameter, public :: GEOCOM_TPS_REFLESS_R400  = 3              !! Pinpoint R400.
    integer, parameter, public :: GEOCOM_TPS_REFLESS_R1000 = 4              !! Pinpoint R1000.

    ! **************************************************************************
    ! EDM - ELECTRONIC DISTANCE MEASUREMENT.
    ! **************************************************************************
    ! EDM_EGLINTENSITY_TYPE: Intensity of Electronic Guidelight.
    integer, parameter, public :: GEOCOM_EDM_EGLINTEN_OFF  = 0
    integer, parameter, public :: GEOCOM_EDM_EGLINTEN_LOW  = 1
    integer, parameter, public :: GEOCOM_EDM_EGLINTEN_MID  = 2
    integer, parameter, public :: GEOCOM_EDM_EGLINTEN_HIGH = 3

    ! EDM_MODE: EDM measurement mode.
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

    ! FTR_DEVICETYPE: Device type.
    integer, parameter, public :: GEOCOM_FTR_DEVICE_INTERNAL = 0    !! Internal memory.
    integer, parameter, public :: GEOCOM_FTR_DEVICE_PCPARD   = 1    !! Memory card.

    ! FTR_FILETYPE: File type.
    integer, parameter, public :: GEOCOM_FTR_FILE_UNKNOWN = 0       !! Undocumented.
    integer, parameter, public :: GEOCOM_FTR_FILE_IMAGES  = 170     !! Extension wildcard: `*.jpg`.

    ! **************************************************************************
    ! IMG - IMAGE PROCESSING.
    ! **************************************************************************
    ! IMG_MEM_TYPE: Memory device type.
    integer, parameter, public :: GEOCOM_IMG_INTERNAL_MEMORY = int(z'0')    !! Internal memory module.
    integer, parameter, public :: GEOCOM_IMG_PC_CARD         = int(z'1')    !! External PC Card.

    integer, parameter, public :: GEOCOM_IMG_MAX_FILE_PREFIX_LEN = 20       !! Length of file name prefix.

    ! **************************************************************************
    ! MOT - MOTORISATION.
    ! **************************************************************************
    ! MOT_LOCK_STATUS: Lock conditions.
    integer, parameter, public :: GEOCOM_MOT_LOCKED_OUT = 0     !! Locked out.
    integer, parameter, public :: GEOCOM_MOT_LOCKED_IN  = 1     !! Locked in.
    integer, parameter, public :: GEOCOM_MOT_PREDICTION = 2     !! Prediction mode.

    ! MOT_STOPMODE: Controller stop mode.
    integer, parameter, public :: GEOCOM_MOT_NORMAL   = 0       !! Slow down with current acceleration.
    integer, parameter, public :: GEOCOM_MOT_SHUTDOWN = 1       !! Slow down by switch off power supply.

    ! MOT_MODE: Controller configuration.
    integer, parameter, public :: GEOCOM_MOT_POSIT   = 0        !! Configured for relative positioning.
    integer, parameter, public :: GEOCOM_MOT_OCONST  = 1        !! Configured for constant speed.
    integer, parameter, public :: GEOCOM_MOT_MANUPOS = 2        !! Configured for manual positioning (default setting).
    integer, parameter, public :: GEOCOM_MOT_LOCK    = 3        !! Configured as "Lock-In" controller.
    integer, parameter, public :: GEOCOM_MOT_BREAK   = 4        !! Configured as "Brake" controller.
    integer, parameter, public :: GEOCOM_MOT_TERM    = 7        !! Terminates the controller task.

    ! **************************************************************************
    ! TMC - THEODOLITE MEASUREMENT AND CALCULATION.
    ! **************************************************************************
    ! TMC_INCLINE_PRG: Inclination sensor measurement program.
    integer, parameter, public :: GEOCOM_TMC_MEA_INC      = 0   !! Use sensor (a priori sigma).
    integer, parameter, public :: GEOCOM_TMC_AUTO_INC     = 1   !! Automatic mode (sensor/plane).
    integer, parameter, public :: GEOCOM_TMC_PLANE_INC    = 2   !! Use plane (a priori sigma).

    ! TMC_MEASURE_PRG: TMC measurement mode.
    integer, parameter, public :: GEOCOM_TMC_STOP         = 0   !! Stop measurement program.
    integer, parameter, public :: GEOCOM_TMC_DEF_DIST     = 1   !! Default DIST-measurement program.
    integer, parameter, public :: GEOCOM_TMC_CLEAR        = 3   !! TMC_STOP and clear data.
    integer, parameter, public :: GEOCOM_TMC_SIGNAL       = 4   !! Signal measurement (test function).
    integer, parameter, public :: GEOCOM_TMC_DO_MEASURE   = 6   !! (Re-)start measurement task.
    integer, parameter, public :: GEOCOM_TMC_RTRK_DIST    = 8   !! Distance-TRK measurement program.
    integer, parameter, public :: GEOCOM_TMC_RED_TRK_DIST = 10  !! Reflectorless tracking.
    integer, parameter, public :: GEOCOM_TMC_FREQUENCY    = 11  !! Frequency measurement (test).

    ! TMC_FACE_DEF: Face position.
    integer, parameter, public :: GEOCOM_TMC_FACE_NORMAL = 0    !! Face in normal position.
    integer, parameter, public :: GEOCOM_TMC_FACE_TURN   = 1    !! Face turned.

    ! TMC_FACE: Actual face.
    integer, parameter, public :: GEOCOM_TMC_FACE_1 = 0         !! Position 1 of telescope.
    integer, parameter, public :: GEOCOM_TMC_FACE_2 = 1         !! Position 2 of telescope.

    ! **************************************************************************
    ! SUP - SUPERVISOR.
    ! **************************************************************************
    ! SUP_AUTO_POWER: Automatic shutdown mechanism for the system.
    integer, parameter, public :: GEOCOM_SUP_POWER_DISABLED = 0 !! Instrument remains on.
    integer, parameter, public :: GEOCOM_SUP_POWER_OFF      = 2 !! Turns off mechanism.

    ! Public procedures.
    public :: dm_geocom_type_aut_adjmode
    public :: dm_geocom_type_aut_atrmode
    public :: dm_geocom_type_aut_posmode
    public :: dm_geocom_type_bap_prismtype
    public :: dm_geocom_type_bap_refltype
    public :: dm_geocom_type_bap_target_type
    public :: dm_geocom_type_bap_user_measprg
    public :: dm_geocom_type_com_tps_startup_mode
    public :: dm_geocom_type_com_tps_stop_mode
    public :: dm_geocom_type_edm_eglintensity_type
    public :: dm_geocom_type_edm_mode
    public :: dm_geocom_type_ftr_devicetype
    public :: dm_geocom_type_ftr_filetype
    public :: dm_geocom_type_img_mem_type
    public :: dm_geocom_type_mot_mode
    public :: dm_geocom_type_mot_stopmode
    public :: dm_geocom_type_tmc_incline_prg
    public :: dm_geocom_type_tmc_measure_prg
contains
    ! **************************************************************************
    ! PUBLIC GEOCOM PARAMETER FUNCTIONS.
    ! **************************************************************************
    pure elemental integer function dm_geocom_type_aut_adjmode(param) result(n)
        !! Parameterisation function for enumeration `AUT_ADJMODE`. Returns
        !! `param` or `GEOCOM_AUT_NORM_MODE` if argument is invalid.
        integer, intent(in) :: param !! `AUT_ADJMODE`.

        select case (param)
            case (GEOCOM_AUT_NORM_MODE)
            case (GEOCOM_AUT_POINT_MODE)
            case (GEOCOM_AUT_DEFINE_MODE)
                n = param
            case default
                n = GEOCOM_AUT_NORM_MODE
        end select
    end function dm_geocom_type_aut_adjmode

    pure elemental integer function dm_geocom_type_aut_atrmode(param) result(n)
        !! Parameterisation function for enumeration `AUT_ATRMODE`. Returns
        !! `param` or `GEOCOM_AUT_POSITION` if argument is invalid.
        integer, intent(in) :: param !! `AUT_ATRMODE`.

        select case (param)
            case (GEOCOM_AUT_POSITION)
            case (GEOCOM_AUT_TARGET)
                n = param
            case default
                n = GEOCOM_AUT_POSITION
        end select
    end function dm_geocom_type_aut_atrmode

    pure elemental integer function dm_geocom_type_aut_posmode(param) result(n)
        !! Parameterisation function for enumeration `AUT_POSMODE`. Returns
        !! `param` or `GEOCOM_AUT_NORMAL` if argument is invalid.
        integer, intent(in) :: param !! `AUT_POSMODE`.

        select case (param)
            case (GEOCOM_AUT_NORMAL)
            case (GEOCOM_AUT_PRECISE)
            case (GEOCOM_AUT_FAST)
                n = param
            case default
                n = GEOCOM_AUT_NORMAL
        end select
    end function dm_geocom_type_aut_posmode

    pure elemental integer function dm_geocom_type_bap_prismtype(param) result(n)
        !! Parameterisation function for enumeration `BAP_PRISMTYPE`. Returns
        !! `param` or `GEOCOM_BAP_PRISM_ROUND` if argument is invalid.
        integer, intent(in) :: param !! `BAP_PRISMTYPE`.

        select case (param)
            case (GEOCOM_BAP_PRISM_ROUND)
            case (GEOCOM_BAP_PRISM_MINI)
            case (GEOCOM_BAP_PRISM_TAPE)
            case (GEOCOM_BAP_PRISM_360)
            case (GEOCOM_BAP_PRISM_USER1)
            case (GEOCOM_BAP_PRISM_USER2)
            case (GEOCOM_BAP_PRISM_USER3)
            case (GEOCOM_BAP_PRISM_360_MINI)
            case (GEOCOM_BAP_PRISM_MINI_ZERO)
            case (GEOCOM_BAP_PRISM_USER)
            case (GEOCOM_BAP_PRISM_NDS_TAPE)
            case (GEOCOM_BAP_PRISM_GRZ121_ROUND)
            case (GEOCOM_BAP_PRISM_MA_MPR122)
                n = param
            case default
                n = GEOCOM_BAP_PRISM_ROUND
        end select
    end function dm_geocom_type_bap_prismtype

    pure elemental integer function dm_geocom_type_bap_refltype(param) result(n)
        !! Parameterisation function for enumeration `BAP_REFLTYPE`. Returns
        !! `param` or `GEOCOM_BAP_REFL_UNDEF` if argument is invalid.
        integer, intent(in) :: param !! `BAP_REFLTYPE`.

        select case (param)
            case (GEOCOM_BAP_REFL_UNDEF)
            case (GEOCOM_BAP_REFL_PRISM)
            case (GEOCOM_BAP_REFL_TAPE)
                n = param
            case default
                n = GEOCOM_BAP_REFL_UNDEF
        end select
    end function dm_geocom_type_bap_refltype

    pure elemental integer function dm_geocom_type_bap_target_type(param) result(n)
        !! Parameterisation function for enumeration `BAP_TARGET_TYPE`. Returns
        !! `param` or `GEOCOM_BAP_REFL_USE` if argument is invalid.
        integer, intent(in) :: param !! `BAP_TARGET_TYPE`.

        select case (param)
            case (GEOCOM_BAP_REFL_USE)
            case (GEOCOM_BAP_REFL_LESS)
                n = param
            case default
                n = GEOCOM_BAP_REFL_USE
        end select
    end function dm_geocom_type_bap_target_type

    pure elemental integer function dm_geocom_type_bap_user_measprg(param) result(n)
        !! Parameterisation function for enumeration `BAP_USER_MEASPRG`. Returns
        !! `param` or `GEOCOM_BAP_SINGLE_REF_STANDARD` if argument is invalid.
        integer, intent(in) :: param !! `BAP_USER_MEASPRG`.

        select case (param)
            case (GEOCOM_BAP_SINGLE_REF_STANDARD)
            case (GEOCOM_BAP_SINGLE_REF_FAST)
            case (GEOCOM_BAP_SINGLE_REF_VISIBLE)
            case (GEOCOM_BAP_SINGLE_RLESS_VISIBLE)
            case (GEOCOM_BAP_CONT_REF_STANDARD)
            case (GEOCOM_BAP_CONT_REF_FAST)
            case (GEOCOM_BAP_CONT_RLESS_VISIBLE)
            case (GEOCOM_BAP_AVG_REF_STANDARD)
            case (GEOCOM_BAP_AVG_REF_VISIBLE)
            case (GEOCOM_BAP_AVG_RLESS_VISIBLE)
            case (GEOCOM_BAP_CONT_REF_SYNCHRO)
            case (GEOCOM_BAP_SINGLE_REF_PRECISE)
                n = param
            case default
                n = GEOCOM_BAP_SINGLE_REF_STANDARD
        end select
    end function dm_geocom_type_bap_user_measprg

    pure elemental integer function dm_geocom_type_com_tps_startup_mode(param) result(n)
        !! Parameterisation function for enumeration `COM_TPS_STARTUP_MODE`.
        !! Returns `param` or `GEOCOM_COM_STARTUP_REMOTE` if argument is invalid.
        integer, intent(in) :: param !! `COM_TPS_STARTUP_MODE`.

        select case (param)
            case (GEOCOM_COM_STARTUP_LOCAL)
            case (GEOCOM_COM_STARTUP_REMOTE)
                n = param
            case default
                n = GEOCOM_COM_STARTUP_REMOTE
        end select
    end function dm_geocom_type_com_tps_startup_mode

    pure elemental integer function dm_geocom_type_com_tps_stop_mode(param) result(n)
        !! Parameterisation function for enumeration `COM_TPS_STOP_MODE`.
        !! Returns `param` or `GEOCOM_COM_STOP_SHUT_DOWN` if argument is invalid.
        integer, intent(in) :: param !! `COM_TPS_STOP_MODE`.

        select case (param)
            case (GEOCOM_COM_STOP_SHUT_DOWN)
            case (GEOCOM_COM_STOP_SLEEP)
                n = param
            case default
                n = GEOCOM_COM_STOP_SHUT_DOWN
        end select
    end function dm_geocom_type_com_tps_stop_mode

    pure elemental integer function dm_geocom_type_edm_eglintensity_type(param) result(n)
        !! Parameterisation function for enumeration `EDM_EGLINTENSITY_TYPE`.
        !! Returns `param` or `GEOCOM_EDM_EGLINTEN_OFF` if argument is invalid.
        integer, intent(in) :: param !! `EDM_EGLINTENSITY_TYPE`.

        select case (param)
            case (GEOCOM_EDM_EGLINTEN_OFF)
            case (GEOCOM_EDM_EGLINTEN_LOW)
            case (GEOCOM_EDM_EGLINTEN_MID)
            case (GEOCOM_EDM_EGLINTEN_HIGH)
                n = param
            case default
                n = GEOCOM_EDM_MODE_NOT_USED
        end select
    end function dm_geocom_type_edm_eglintensity_type

    pure elemental integer function dm_geocom_type_edm_mode(param) result(n)
        !! Parameterisation function for enumeration `EDM_MODE`. Returns
        !! `param` or `GEOCOM_EDM_MODE_NOT_USED` if argument is invalid.
        integer, intent(in) :: param !! `EDM_MODE`.

        select case (param)
            case (GEOCOM_EDM_MODE_NOT_USED)
            case (GEOCOM_EDM_SINGLE_TAPE)
            case (GEOCOM_EDM_SINGLE_STANDARD)
            case (GEOCOM_EDM_SINGLE_FAST)
            case (GEOCOM_EDM_SINGLE_LRANGE)
            case (GEOCOM_EDM_SINGLE_SRANGE)
            case (GEOCOM_EDM_CONT_STANDARD)
            case (GEOCOM_EDM_CONT_DYNAMIC)
            case (GEOCOM_EDM_CONT_REFLESS)
            case (GEOCOM_EDM_CONT_FAST)
            case (GEOCOM_EDM_AVERAGE_IR)
            case (GEOCOM_EDM_AVERAGE_SR)
            case (GEOCOM_EDM_AVERAGE_LR)
            case (GEOCOM_EDM_PRECISE_IR)
            case (GEOCOM_EDM_PRECISE_TAPE)
                n = param
            case default
                n = GEOCOM_EDM_MODE_NOT_USED
        end select
    end function dm_geocom_type_edm_mode

    pure elemental integer function dm_geocom_type_ftr_devicetype(param) result(n)
        !! Parameterisation function for enumeration `FTR_DEVICETYPE`. Returns
        !! `param` or `GEOCOM_FTR_DEVICE_INTERNAL` if argument is invalid.
        integer, intent(in) :: param !! `FTR_DEVICETYPE`.

        select case (param)
            case (GEOCOM_FTR_DEVICE_INTERNAL)
            case (GEOCOM_FTR_DEVICE_PCPARD)
                n = param
            case default
                n = GEOCOM_FTR_DEVICE_INTERNAL
        end select
    end function dm_geocom_type_ftr_devicetype

    pure elemental integer function dm_geocom_type_ftr_filetype(param) result(n)
        !! Parameterisation function for enumeration `FTR_FILETYPE`. Returns
        !! `param` or `GEOCOM_FTR_FILE_UNKNOWN` if argument is invalid.
        integer, intent(in) :: param !! `FTR_FILETYPE`.

        select case (param)
            case (GEOCOM_FTR_FILE_UNKNOWN)
            case (GEOCOM_FTR_FILE_IMAGES)
                n = param
            case default
                n = GEOCOM_FTR_FILE_UNKNOWN
        end select
    end function dm_geocom_type_ftr_filetype

    pure elemental integer function dm_geocom_type_img_mem_type(param) result(n)
        !! Parameterisation function for enumeration `IMG_MEM_TYPE`. Returns
        !! `param` or `GEOCOM_IMG_INTERNAL_MEMORY` if argument is invalid.
        integer, intent(in) :: param !! `IMG_MEM_TYPE`.

        select case (param)
            case (GEOCOM_IMG_INTERNAL_MEMORY)
            case (GEOCOM_IMG_PC_CARD)
                n = param
            case default
                n = GEOCOM_IMG_INTERNAL_MEMORY
        end select
    end function dm_geocom_type_img_mem_type

    pure elemental integer function dm_geocom_type_mot_mode(param) result(n)
        !! Parameterisation function for enumeration `MOT_MODE`. Returns
        !! `param` or `GEOCOM_MOT_MANUPOS` if argument is invalid.
        integer, intent(in) :: param !! `MOT_MODE`.

        select case (param)
            case (GEOCOM_MOT_POSIT)
            case (GEOCOM_MOT_OCONST)
            case (GEOCOM_MOT_MANUPOS)
            case (GEOCOM_MOT_LOCK)
            case (GEOCOM_MOT_BREAK)
            case (GEOCOM_MOT_TERM)
                n = param
            case default
                n = GEOCOM_MOT_MANUPOS
        end select
    end function dm_geocom_type_mot_mode

    pure elemental integer function dm_geocom_type_mot_stopmode(param) result(n)
        !! Parameterisation function for enumeration `MOT_STOPMODE`. Returns
        !! `param` or `GEOCOM_MOT_NORMAL` if argument is invalid.
        integer, intent(in) :: param !! `MOT_STOPMODE`.

        select case (param)
            case (GEOCOM_MOT_NORMAL)
            case (GEOCOM_MOT_SHUTDOWN)
                n = param
            case default
                n = GEOCOM_MOT_NORMAL
        end select
    end function dm_geocom_type_mot_stopmode

    pure elemental integer function dm_geocom_type_tmc_incline_prg(param) result(n)
        !! Parameterisation function for enumeration `TMC_INCLINE_PRG`. Returns
        !! `param` or `GEOCOM_TMC_MEA_INC` if argument is invalid.
        integer, intent(in) :: param !! `TMC_INCLINE_PRG`.

        select case (param)
            case (GEOCOM_TMC_MEA_INC)
            case (GEOCOM_TMC_AUTO_INC)
            case (GEOCOM_TMC_PLANE_INC)
                n = param
            case default
                n = GEOCOM_TMC_MEA_INC
        end select
    end function dm_geocom_type_tmc_incline_prg

    pure elemental integer function dm_geocom_type_tmc_measure_prg(param) result(n)
        !! Parameterisation function for enumeration `TMC_MEASURE_PRG`. Returns
        !! `param` or `GEOCOM_TMC_DEF_DIST` if argument is invalid.
        integer, intent(in) :: param !! `TMC_MEASURE_PRG`.

        select case (param)
            case (GEOCOM_TMC_STOP)
            case (GEOCOM_TMC_DEF_DIST)
            case (GEOCOM_TMC_CLEAR)
            case (GEOCOM_TMC_SIGNAL)
            case (GEOCOM_TMC_DO_MEASURE)
            case (GEOCOM_TMC_RTRK_DIST)
            case (GEOCOM_TMC_RED_TRK_DIST)
            case (GEOCOM_TMC_FREQUENCY)
                n = param
            case default
                n = GEOCOM_TMC_DEF_DIST
        end select
    end function dm_geocom_type_tmc_measure_prg
end module dm_geocom_type
