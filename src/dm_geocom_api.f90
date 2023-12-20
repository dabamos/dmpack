! Author:  Philipp Engel
! Licence: ISC
module dm_geocom_api
    !! Parameters, types, and procedures for GeoCOM protocol handling.
    !!
    !! The GeoCOM API is divided into the following sub-systems:
    !!
    !! * AUT - Automation
    !! * BAP - Basic Applications
    !! * BMM - Basic Man-Machine Interface
    !! * COM - Communication Settings
    !! * CSV - Central Services
    !! * EDM - Electronic Distance Measurement
    !! * FTR - File Transfer
    !! * IMG - Image Processing
    !! * MOT - Motorisation
    !! * SUP - Supervisor
    !! * TMC - Theodolite Measurement and Calculation
    !!
    !! All other GeoCOM parameters and structured types the prefix `GEOCOM_` or
    !! `geocom_`.
    use :: dm_geocom_error
    use :: dm_kind
    use :: dm_request
    use :: dm_response
    implicit none (type, external)
    private

    ! **************************************************************************
    ! GEOCOM API CONSTANTS.
    ! **************************************************************************
    character(len=*),    parameter :: GEOCOM_DELIMITER        = '\r\n'                  !! Default GeoCOM delimiter.
    character(len=*),    parameter :: GEOCOM_GRC_PATTERN      = '%R1P,0,0:(?<grc>\d+)'  !! Default GeoCOM response pattern.
    type(response_type), parameter :: GEOCOM_GRC_RESPONSES(1) = [ response_type('rc') ] !! Default GeoCOM responses (GRC only).

    ! **************************************************************************
    ! COMMON.
    ! **************************************************************************
    ! ON_OFF_TYPE.
    integer, parameter, public :: GEOCOM_OFF = 0 !! Mode off.
    integer, parameter, public :: GEOCOM_ON  = 1 !! Mode on.

    integer, parameter, public :: GEOCOM_MOT_AXES    = 2 !! Number of motor axes.
    integer, parameter, public :: GEOCOM_MOT_HZ_AXLE = 1 !! Hz axis.
    integer, parameter, public :: GEOCOM_MOT_V_AXLE  = 2 !! V axis.

    ! **************************************************************************
    ! AUS - ALT USER.
    ! **************************************************************************
    type, public :: geocom_date_type
        !! DATE_TYPE: General date.
        integer :: year  = 0 !! Year.
        integer :: month = 0 !! Month in year, from 1 to 12.
        integer :: day   = 0 !! Day in month, from 1 to 31.
    end type geocom_date_type

    type, public :: geocom_time_type
        !! TIME_TYPE: General time.
        integer :: hour   = 0 !! 24 hours per day, from 0 to 23.
        integer :: minute = 0 !! Minutes, from 0 to 59.
        integer :: second = 0 !! Seconds, from 0 to 59.
    end type geocom_time_type

    type, public :: geocom_date_time_type
        !! DATIME: General date and time.
        type(geocom_date_type) :: date !! Date.
        type(geocom_time_type) :: time !! Time.
    end type geocom_date_time_type

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

    integer, parameter, public :: GEOCOM_AUT_CLOCKWISE     = 1  !! Direction closewise.
    integer, parameter, public :: GEOCOM_AUT_ANTICLOCKWISE = -1 !! Direction counter clockwise.

    ! AUT_DETENT: Automatic detent mode.
    type, public :: geocom_aut_detent_type
        !! Detent data.
        logical       :: enabled  = .false. !! Detent is active.
        real(kind=r8) :: positive = 0.0_r8  !! Detent in positive direction.
        real(kind=r8) :: negative = 0.0_r8  !! Detent in negative direction.
    end type geocom_aut_detent_type

    ! AUT_POSTOL: Positioning tolerance.
    type, public :: geocom_aut_pos_tol_type
        !! Positioning tolerance for Hz and V [rad].
        real(kind=r8) :: axes(GEOCOM_MOT_AXES) = 0.0_r8 !! Hz and V tolerance [rad].
    end type geocom_aut_pos_tol_type

    ! AUT_SEARCH_AREA: Search area.
    type, public :: geocom_aut_search_area_type
        !! Search spiral.
        logical       :: enabled   = .false. !! User defined search area is active.
        real(kind=r8) :: center_hz = 0.0_r8  !! Hz angle of search area - center [rad].
        real(kind=r8) :: center_v  = 0.0_r8  !! V angle of search area - center [rad].
        real(kind=r8) :: range_hz  = 0.0_r8  !! Width of search area [rad].
        real(kind=r8) :: range_v   = 0.0_r8  !! Max. height of search area [rad].
    end type geocom_aut_search_area_type

    ! AUT_SEARCH_SPIRAL: Search spiral.
    type, public :: geocom_aut_search_spiral_type
        !! Search spiral.
        real(kind=r8) :: range_hz = 0.0_r8 !! Width of search area [rad].
        real(kind=r8) :: range_v  = 0.0_r8 !! Max. height of search area [rad].
    end type geocom_aut_search_spiral_type

    ! AUT_TIMEOUT: Maximum position time.
    type, public :: geocom_aut_timeout_type
        !! Maximum positioning time for Hz and V [sec].
        real(kind=r8) :: axes(GEOCOM_MOT_AXES) = 0.0_r8 !! Max. Hz and V positioning time [sec].
    end type geocom_aut_timeout_type

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

    ! BAP_ATRSETTING: ATR Low Vis mode definition.
    integer, parameter, public :: GEOCOM_BAP_ATRSET_NORMAL     = 0 !! ATR is using no special flags or modes.
    integer, parameter, public :: GEOCOM_BAP_ATRSET_LOWVIS_ON  = 1 !! ATR low vis mode on.
    integer, parameter, public :: GEOCOM_BAP_ATRSET_LOWVIS_AON = 2 !! ATR low vis mode always on.
    integer, parameter, public :: GEOCOM_BAP_ATRSET_SRANGE_ON  = 3 !! ATR high reflectivity mode on.
    integer, parameter, public :: GEOCOM_BAP_ATRSET_SRANGE_AON = 4 !! ATR high reflectivity mode always on.

    ! BAP_PRISMDEF: Prism definition.
    integer, parameter, public :: GEOCOM_BAP_PRISMNAME_LEN = 16 !! Prism name string length.

    type, public :: geocom_bap_prism_type
        !! Prism type.
        character(len=GEOCOM_BAP_PRISMNAME_LEN) :: name      = ' '                   !! Prism name.
        integer                                 :: refl_type = GEOCOM_BAP_REFL_UNDEF !! Reflector type.
        real(kind=r8)                           :: add_const = 0.0_r8                !! Prism correction.
    end type geocom_bap_prism_type

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
    integer, parameter, public :: GEOCOM_TPS_CLASS_1100 = 0   !! TPS1000 family member, 1 mgon, 3".
    integer, parameter, public :: GEOCOM_TPS_CLASS_1700 = 1   !! TPS1000 family member, 0.5 mgon, 1.5".
    integer, parameter, public :: GEOCOM_TPS_CLASS_1800 = 2   !! TPS1000 family member, 0.3 mgon, 1".
    integer, parameter, public :: GEOCOM_TPS_CLASS_5000 = 3   !! TPS2000 family member.
    integer, parameter, public :: GEOCOM_TPS_CLASS_6000 = 4   !! TPS2000 family member.
    integer, parameter, public :: GEOCOM_TPS_CLASS_1500 = 5   !! TPS1000 family member.
    integer, parameter, public :: GEOCOM_TPS_CLASS_2003 = 6   !! TPS2000 family member.
    integer, parameter, public :: GEOCOM_TPS_CLASS_5005 = 7   !! TPS5000 family member.
    integer, parameter, public :: GEOCOM_TPS_CLASS_5100 = 8   !! TPS5000 family member.
    integer, parameter, public :: GEOCOM_TPS_CLASS_1102 = 100 !! TPS1100 family member, 2".
    integer, parameter, public :: GEOCOM_TPS_CLASS_1103 = 101 !! TPS1100 family member, 3".
    integer, parameter, public :: GEOCOM_TPS_CLASS_1105 = 102 !! TPS1100 family member, 5".
    integer, parameter, public :: GEOCOM_TPS_CLASS_1101 = 103 !! TPS1100 family member, 1".
    integer, parameter, public :: GEOCOM_TPS_CLASS_1202 = 200 !! TPS1200 family member, 2".
    integer, parameter, public :: GEOCOM_TPS_CLASS_1203 = 201 !! TPS1200 family member, 3".
    integer, parameter, public :: GEOCOM_TPS_CLASS_1205 = 202 !! TPS1200 family member, 5".
    integer, parameter, public :: GEOCOM_TPS_CLASS_1201 = 203 !! TPS1200 family member, 1".
    integer, parameter, public :: GEOCOM_TPS_CLASS_TX30 = 300 !! TS30,TM30 family member, 0.5".
    integer, parameter, public :: GEOCOM_TPS_CLASS_TX31 = 301 !! TS30,TM30 family member, 1".

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

    ! TPS_DEVICE: TPS device configuration type.
    type, public :: geocom_tps_device_type
        !! TPS device configuration type.
        integer :: class = 0 !! Device precision class (`TPS_DEVICE_CLASS`).
        integer :: type  = 0 !! Device configuration type (`TPS_DEVICE_TYPE`).
    end type geocom_tps_device_type

    ! **************************************************************************
    ! EDM - ELECTRONIC DISTANCE MEASUREMENT.
    ! **************************************************************************
    ! EDM_EGLINTENSITY_TYPE: Intensity of Electronic Guidelight.
    integer, parameter, public :: GEOCOM_EDM_EGLINTEN_OFF  = 0
    integer, parameter, public :: GEOCOM_EDM_EGLINTEN_LOW  = 1
    integer, parameter, public :: GEOCOM_EDM_EGLINTEN_MID  = 2
    integer, parameter, public :: GEOCOM_EDM_EGLINTEN_HIGH = 3

    ! **************************************************************************
    ! FTR - FILE TRANSFER.
    ! **************************************************************************
    integer, parameter, public :: GEOCOM_FTR_MAX_BLOCKSIZE = 450 !! Max. block size.

    ! FTR_DEVICETYPE: Device type.
    integer, parameter, public :: GEOCOM_FTR_DEVICE_INTERNAL = 0 !! Internal memory.
    integer, parameter, public :: GEOCOM_FTR_DEVICE_PCPARD   = 1 !! Memory card.

    ! FTR_FILETYPE: File type.
    integer, parameter, public :: GEOCOM_FTR_FILE_IMAGES = 170

    ! FTR_BLOCK: Block type.
    type, public :: geocom_ftr_block_type
        !! Block type.
        character :: bytes(GEOCOM_FTR_MAX_BLOCKSIZE) = ' '
        integer   :: length                          = 0
    end type geocom_ftr_block_type

    ! FTR_MODDATE: Modification date.
    type, public :: geocom_ftr_mod_date_type
        !! Modification date type.
        integer :: year  = 0 !! UTC date, year.
        integer :: month = 0 !! UTC date, month.
        integer :: day   = 0 !! UTC date, day.
    end type geocom_ftr_mod_date_type

    ! FTR_MODTIME: Modification time.
    type, public :: geocom_ftr_mod_time_type
        !! Modification time type.
        integer :: hour        = 0 !! Hours.
        integer :: minute      = 0 !! Minutes.
        integer :: second      = 0 !! Seconds.
        integer :: centisecond = 0 !! Centiseconds (0.01 sec).
    end type geocom_ftr_mod_time_type

    ! FTR_DIRINFO: Directory info.
    type, public :: geocom_ftr_dir_info_type
        !! Directory information type.
        character(len=80)              :: file_name = ' '
        integer(kind=i8)               :: file_size = 0_i8
        type(geocom_ftr_mod_time_type) :: mod_time
        type(geocom_ftr_mod_date_type) :: mod_date
    end type geocom_ftr_dir_info_type

    ! **************************************************************************
    ! IMG - IMAGE PROCESSING.
    ! **************************************************************************
    ! IMG_MEM_TYPE: Memory device type.
    integer, parameter, public :: GEOCOM_IMG_INTERNAL_MEMORY = int(z'0') !! Internal memory module.
    integer, parameter, public :: GEOCOM_IMG_PC_CARD         = int(z'1') !! External PC Card.

    integer, parameter, public :: GEOCOM_IMG_MAX_FILE_PREFIX_LEN = 20 !! Length of file name prefix.

    type, public :: geocom_img_tcc_config_type
        !! IMG_TCC_CONFIG: Image parameters.
        integer(kind=i8)                              :: image_number     = 0_i8 !! Image number.
        integer(kind=i8)                              :: quality          = 0_i8 !! Image quality.
        integer(kind=i8)                              :: sub_funct_number = 0_i8 !!
        character(len=GEOCOM_IMG_MAX_FILE_PREFIX_LEN) :: file_name_prefix = ' '  !! File name prefix.
    end type geocom_img_tcc_config_type

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

    type, public :: geocom_mot_com_pair_type
        !! MOT_COM_PAIR: Speed.
        real(kind=r8) :: axes(GEOCOM_MOT_AXES) = 0.0_r8 !! Values for horizontal (instrument) and vertical (telescope) speed.
    end type geocom_mot_com_pair_type

    ! **************************************************************************
    ! SUP - SUPERVISOR.
    ! **************************************************************************
    ! SUP_AUTO_POWER: Automatic shutdown mechanism for the system.
    integer, parameter, public :: GEOCOM_SUP_POWER_DISABLED = 0 !! Instrument remains on.
    integer, parameter, public :: GEOCOM_SUP_POWER_OFF      = 2 !! Turns off mechanism.

    ! **************************************************************************
    ! TMC - THEODOLITE MEASUREMENT AND CALCULATION.
    ! **************************************************************************
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

    type, public :: geocom_tmc_edm_frequency_type
        !! TMC_EDM_FREQUENCY: EDM frequency.
        real(kind=r8)    :: frequency = 0.0_r8 !! EDM frequency in Hz.
        integer(kind=i8) :: time      = 0_i8   !! Time of last measurement.
    end type geocom_tmc_edm_frequency_type

    type, public :: geocom_tmc_coordinate_type
        !! TMC_COORDINATE: Calculated coordinates based on distance measurement.
        real(kind=r8)    :: e               = 0.0_r8 !! E coordinate [m].
        real(kind=r8)    :: n               = 0.0_r8 !! N coordinate [m].
        real(kind=r8)    :: h               = 0.0_r8 !! H coordinate [m].
        integer(kind=i8) :: coord_time      = 0_i8   !! Timestamp of distance measurement [ms].
        real(kind=r8)    :: e_cont          = 0.0_r8 !! E coordinate (continuously) [m].
        real(kind=r8)    :: n_cont          = 0.0_r8 !! N coordinate (continuously) [m].
        real(kind=r8)    :: h_cont          = 0.0_r8 !! H coordinate (continuously) [m].
        integer(kind=i8) :: coord_cont_time = 0_i8   !! Timestamp of measurement [ms].
    end type geocom_tmc_coordinate_type

    type, public :: geocom_tmc_hz_v_ang_type
        !! TMC_HZ_V_ANG: Corrected angle data.
        real(kind=r8) :: hz = 0.0_r8 !! Horizontal angle [rad].
        real(kind=r8) :: v  = 0.0_r8 !! Vertical angle [rad].
    end type geocom_tmc_hz_v_ang_type

    type, public :: geocom_tmc_incline_type
        !! TMC_INCLINE: Inclination data.
        real(kind=r8)    :: cross_incline    = 0.0_r8 !! Transverse axis inclination [rad].
        real(kind=r8)    :: length_incline   = 0.0_r8 !! Longitude axis inclination [rad].
        real(kind=r8)    :: accuracy_incline = 0.0_r8 !! Inclination accuracy [rad].
        integer(kind=i8) :: incline_time     = 0_i8   !! Moment of measurement [ms].
    end type geocom_tmc_incline_type

    type, public :: geocom_tmc_angle_type
        !! TMC_ANGLE: Corrected angle data with inclination data.
        real(kind=r8)                 :: hz             = 0.0_r8 !! Horizontal angle [rad].
        real(kind=r8)                 :: v              = 0.0_r8 !! Vertical angle [rad].
        real(kind=r8)                 :: angle_accuracy = 0.0_r8 !! Accuracy of angles [rad].
        integer(kind=i8)              :: angle_time     = 0.0_r8 !! Moment of measurement [ms].
        type(geocom_tmc_incline_type) :: incline                 !! Corresponding inclination (TMC_INCLINE).
        integer                       :: face           = 0_i8   !! Telescope face (TMC_FACE).
    end type geocom_tmc_angle_type

    type, public :: geocom_tmc_offset_dist_type
        !! TMC_OFFSETDIST: Offset values for correction.
        real(kind=r8) :: length = 0.0_r8 !! Aim offset length.
        real(kind=r8) :: cross  = 0.0_r8 !! Aim offset cross.
        real(kind=r8) :: height = 0.0_r8 !! Aim offset height.
    end type geocom_tmc_offset_dist_type

    type, public :: geocom_tmc_height
        !! TMC_HEIGHT: Reflector height.
        real(kind=r8) :: height = 0.0_r8 !! Reflector height.
    end type geocom_tmc_height

    type, public :: geocom_tmc_atmos_temperature_type
        !! TMC_ATMOS_TEMPERATURE: Atmospheric correction data.
        real(kind=r8) :: lambda          = 0.0_r8 !! Wave length of the EDM transmitter [m].
        real(kind=r8) :: pressure        = 0.0_r8 !! Atmospheric pressure [mbar].
        real(kind=r8) :: dry_temperature = 0.0_r8 !! Dry temperature [°C].
        real(kind=r8) :: wet_temperature = 0.0_r8 !! Wet temperature [°C].
    end type geocom_tmc_atmos_temperature_type

    type, public :: geocom_tmc_refraction_type
        !! TMC_REFRACTION: Refraction control data.
        logical       :: enabled          = .false. !! Refraction correction on/off.
        real(kind=r8) :: earth_radius     = 0.0_r8  !! Radius of the earth [m].
        real(kind=r8) :: refractive_scale = 0.0_r8  !! Refraction coefficient.
    end type geocom_tmc_refraction_type

    type, public :: geocom_tmc_station_type
        !! TMC_STATION: Instrument station coordinates.
        real(kind=r8) :: e0 = 0.0_r8 !! Station easting coordinate [m].
        real(kind=r8) :: n0 = 0.0_r8 !! Station northing coordinate [m].
        real(kind=r8) :: h0 = 0.0_r8 !! Station height coordinate [m].
        real(kind=r8) :: hi = 0.0_r8 !! Instrument height [m].
    end type geocom_tmc_station_type

    type, public :: geocom_tmc_edm_signal_type
        !! TMC_EDM_SIGNAL: EDM signal information.
        real(kind=r8)    :: signal_intensity = 0.0_r8 !! Signal intensity of EDM [%].
        integer(kind=i8) :: time             = 0_i8   !! Timestamp [ms].
    end type geocom_tmc_edm_signal_type

    type, public :: geocom_tmc_ang_switch_type
        !! TMC_ANG_SWITCH: Correction switches.
        logical :: incline_corr     = .false. !! Inclination correction.
        logical :: stand_axis_corr  = .false. !! Standing axis correction.
        logical :: collimation_corr = .false. !! Collimation error correction.
        logical :: tilt_axis_corr   = .false. !! Tilting axis correction.
    end type geocom_tmc_ang_switch_type

    ! Public procedures.
    public :: dm_geocom_prepare_request
    public :: dm_geocom_prepare_request_abort_download
    public :: dm_geocom_prepare_request_abort_list
    public :: dm_geocom_prepare_request_beep_alarm
    public :: dm_geocom_prepare_request_beep_normal
    public :: dm_geocom_prepare_request_beep_off
    public :: dm_geocom_prepare_request_beep_on
    public :: dm_geocom_prepare_request_change_face
    public :: dm_geocom_prepare_request_delete
    public :: dm_geocom_prepare_request_do_measure
    public :: dm_geocom_prepare_request_do_position
    ! public :: dm_geocom_prepare_request_download
    ! public :: dm_geocom_prepare_request_fine_adjust
    ! public :: dm_geocom_prepare_request_get_angle1
    ! public :: dm_geocom_prepare_request_get_angle5
    ! public :: dm_geocom_prepare_request_get_angular_correction
    ! public :: dm_geocom_prepare_request_get_atmospheric_correction
    ! public :: dm_geocom_prepare_request_get_atmospheric_ppm
    ! public :: dm_geocom_prepare_request_get_atr_error
    ! public :: dm_geocom_prepare_request_get_atr_setting
    ! public :: dm_geocom_prepare_request_get_binary_available
    ! public :: dm_geocom_prepare_request_get_config
    ! public :: dm_geocom_prepare_request_get_coordinate
    ! public :: dm_geocom_prepare_request_get_date_time
    ! public :: dm_geocom_prepare_request_get_date_time_centi
    ! public :: dm_geocom_prepare_request_get_device_config
    public :: dm_geocom_prepare_request_get_double_precision
    ! public :: dm_geocom_prepare_request_get_edm_mode
    ! public :: dm_geocom_prepare_request_get_egl_intensity
    ! public :: dm_geocom_prepare_request_get_face
    ! public :: dm_geocom_prepare_request_get_fine_adjust_mode
    ! public :: dm_geocom_prepare_request_get_full_measurement
    ! public :: dm_geocom_prepare_request_get_geometric_ppm
    ! public :: dm_geocom_prepare_request_get_height
    ! public :: dm_geocom_prepare_request_get_image_config
    ! public :: dm_geocom_prepare_request_get_incline_correction
    ! public :: dm_geocom_prepare_request_get_incline_error
    ! public :: dm_geocom_prepare_request_get_instrument_name
    ! public :: dm_geocom_prepare_request_get_instrument_number
    ! public :: dm_geocom_prepare_request_get_internal_temperature
    ! public :: dm_geocom_prepare_request_get_lock_status
    ! public :: dm_geocom_prepare_request_get_measurement_program
    ! public :: dm_geocom_prepare_request_get_power
    ! public :: dm_geocom_prepare_request_get_prism_definition
    ! public :: dm_geocom_prepare_request_get_prism_type
    ! public :: dm_geocom_prepare_request_get_prism_type2
    ! public :: dm_geocom_prepare_request_get_quick_distance
    ! public :: dm_geocom_prepare_request_get_reduced_atr_fov
    ! public :: dm_geocom_prepare_request_get_reflectorless_class
    ! public :: dm_geocom_prepare_request_get_refractive_correction
    ! public :: dm_geocom_prepare_request_get_refractive_method
    ! public :: dm_geocom_prepare_request_get_search_area
    ! public :: dm_geocom_prepare_request_get_set_laser_pointer
    ! public :: dm_geocom_prepare_request_get_signal
    ! public :: dm_geocom_prepare_request_get_simple_coordinates
    ! public :: dm_geocom_prepare_request_get_simple_meta
    ! public :: dm_geocom_prepare_request_get_slope_distance_correction
    ! public :: dm_geocom_prepare_request_get_software_version
    ! public :: dm_geocom_prepare_request_get_software_version2
    ! public :: dm_geocom_prepare_request_get_station
    ! public :: dm_geocom_prepare_request_get_target_type
    ! public :: dm_geocom_prepare_request_get_timeout
    ! public :: dm_geocom_prepare_request_get_tolerance
    ! public :: dm_geocom_prepare_request_get_user_atr_mode
    ! public :: dm_geocom_prepare_request_get_user_local_mode
    ! public :: dm_geocom_prepare_request_get_user_prism_definition
    ! public :: dm_geocom_prepare_request_get_user_spiral
    ! public :: dm_geocom_prepare_request_list
    ! public :: dm_geocom_prepare_request_lock_in
    ! public :: dm_geocom_prepare_request_measure_distance_angle
    ! public :: dm_geocom_prepare_request_null_proc
    ! public :: dm_geocom_prepare_request_ps_enable_range
    ! public :: dm_geocom_prepare_request_ps_search_next
    ! public :: dm_geocom_prepare_request_ps_search_window
    ! public :: dm_geocom_prepare_request_ps_set_range
    ! public :: dm_geocom_prepare_request_search
    ! public :: dm_geocom_prepare_request_search_target
    ! public :: dm_geocom_prepare_request_set_angle_correction
    ! public :: dm_geocom_prepare_request_set_atmospheric_correction
    ! public :: dm_geocom_prepare_request_set_atmospheric_ppm
    ! public :: dm_geocom_prepare_request_set_atr_setting
    ! public :: dm_geocom_prepare_request_set_binary_available
    ! public :: dm_geocom_prepare_request_set_config
    ! public :: dm_geocom_prepare_request_set_date_time
    ! public :: dm_geocom_prepare_request_set_double_precision
    ! public :: dm_geocom_prepare_request_set_edm_mode
    ! public :: dm_geocom_prepare_request_set_egl_intensity
    ! public :: dm_geocom_prepare_request_set_fine_adjust_mode
    ! public :: dm_geocom_prepare_request_set_geometric_ppm
    ! public :: dm_geocom_prepare_request_set_height
    ! public :: dm_geocom_prepare_request_set_image_config
    ! public :: dm_geocom_prepare_request_set_incline_correction
    ! public :: dm_geocom_prepare_request_set_measurement_program
    ! public :: dm_geocom_prepare_request_set_offset
    ! public :: dm_geocom_prepare_request_set_orientation
    ! public :: dm_geocom_prepare_request_set_prism_correction
    ! public :: dm_geocom_prepare_request_set_prism_type
    ! public :: dm_geocom_prepare_request_set_prism_type2
    ! public :: dm_geocom_prepare_request_set_reduced_atr_fov
    ! public :: dm_geocom_prepare_request_set_refractive_correction
    ! public :: dm_geocom_prepare_request_set_refractive_method
    ! public :: dm_geocom_prepare_request_set_search_area
    ! public :: dm_geocom_prepare_request_set_station
    ! public :: dm_geocom_prepare_request_set_target_type
    ! public :: dm_geocom_prepare_request_set_timeout
    ! public :: dm_geocom_prepare_request_set_tolerance
    ! public :: dm_geocom_prepare_request_set_user_atr_mode
    ! public :: dm_geocom_prepare_request_set_user_local_mode
    ! public :: dm_geocom_prepare_request_set_user_prism_definition
    ! public :: dm_geocom_prepare_request_set_user_spiral
    ! public :: dm_geocom_prepare_request_set_velocity
    ! public :: dm_geocom_prepare_request_setup_download
    ! public :: dm_geocom_prepare_request_setup_list
    ! public :: dm_geocom_prepare_request_start_controller
    ! public :: dm_geocom_prepare_request_stop_controller
    ! public :: dm_geocom_prepare_request_switch_off
    ! public :: dm_geocom_prepare_request_switch_on
    ! public :: dm_geocom_prepare_request_take_image
contains
    ! **************************************************************************
    ! PUBLIC PROCEDURES.
    ! **************************************************************************
    pure subroutine dm_geocom_prepare_request(request, string, pattern, responses)
        !! Prepares a DMPACK request type by setting request command, response
        !! pattern, response delimiter, and response definition array.
        type(request_type),  intent(out) :: request      !! Prepared request type.
        character(len=*),    intent(in)  :: string       !! Request string to send to the sensor (with delimiter).
        character(len=*),    intent(in)  :: pattern      !! Regular expression pattern that matches the response.
        type(response_type), intent(in)  :: responses(:) !! Array of response types.

        integer :: i, n

        n = min(REQUEST_MAX_NRESPONSES, size(responses))

        request%request    = string           ! Request command.
        request%pattern    = pattern          ! Response pattern.
        request%delimiter  = GEOCOM_DELIMITER ! Response delimiter.
        request%nresponses = n                ! Number of responses.

        do i = 1, n
            request%responses(i) = responses(i)
        end do
    end subroutine dm_geocom_prepare_request

    ! **************************************************************************
    ! GEOCOM PREPARATION PROCEDURES.
    ! **************************************************************************
    pure subroutine dm_geocom_prepare_request_abort_download(request)
        !! Request of `FTR_AbortDownload` procedure.
        !!
        !! Creates request to abort or end the file download command.
        !!
        !! Responses:       grc
        !! Instruments:     TPS1200, TM30/TS30, TS16
        !! ASCII-Request:   `%R1Q,23305:`
        !! ASCII-Response:  `%R1P,0,0:<grc>`
        type(request_type), intent(out) :: request !! Prepared request.

        character(len=REQUEST_REQUEST_LEN) :: string

        write (string, '("%R1Q,23305:", a)') GEOCOM_DELIMITER
        call dm_geocom_prepare_request(request, string, GEOCOM_GRC_PATTERN, GEOCOM_GRC_RESPONSES)
    end subroutine dm_geocom_prepare_request_abort_download

    pure subroutine dm_geocom_prepare_request_abort_list(request)
        !! Request of `FTR_AbortList` procedure.
        !!
        !! Creates request to aborts or end the file list command.
        !!
        !! Responses:       grc
        !! Instruments:     TPS1200, TM30/TS30, TS16
        !! ASCII-Request:   `%R1Q,23308:`
        !! ASCII-Response:  `%R1P,0,0:<grc>`
        type(request_type), intent(out) :: request !! Prepared request.

        character(len=REQUEST_REQUEST_LEN) :: string

        write (string, '("%R1Q,23308:", a)') GEOCOM_DELIMITER
        call dm_geocom_prepare_request(request, string, GEOCOM_GRC_PATTERN, GEOCOM_GRC_RESPONSES)
    end subroutine dm_geocom_prepare_request_abort_list

    pure subroutine dm_geocom_prepare_request_beep_alarm(request)
        !! Request of `BMM_BeepAlarm` procedure.
        !!
        !! Creates request to output an alarm signal (triple beep).
        !!
        !! Responses:       grc
        !! Instruments:     TPS1200, TM30/TS30, TS16
        !! ASCII-Request:   `%R1Q,11004:`
        !! ASCII-Response:  `%R1P,0,0:<grc>`
        type(request_type), intent(out) :: request !! Prepared request.

        character(len=REQUEST_REQUEST_LEN) :: string

        write (string, '("%R1Q,11004:", a)') GEOCOM_DELIMITER
        call dm_geocom_prepare_request(request, string, GEOCOM_GRC_PATTERN, GEOCOM_GRC_RESPONSES)
    end subroutine dm_geocom_prepare_request_beep_alarm

    pure subroutine dm_geocom_prepare_request_beep_normal(request)
        !! Request of `BMM_BeepNormal` procedure.
        !!
        !! Creates request to output an alarm signal (single beep).
        !!
        !! Responses:       grc
        !! Instruments:     TPS1200, TM30/TS30, TS16
        !! ASCII-Request:   `%R1Q,11003:`
        !! ASCII-Response:  `%R1P,0,0:<grc>`
        type(request_type), intent(out) :: request !! Prepared request.

        character(len=REQUEST_REQUEST_LEN) :: string

        write (string, '("%R1Q,11003:", a)') GEOCOM_DELIMITER
        call dm_geocom_prepare_request(request, string, GEOCOM_GRC_PATTERN, GEOCOM_GRC_RESPONSES)
    end subroutine dm_geocom_prepare_request_beep_normal

    pure subroutine dm_geocom_prepare_request_beep_off(request)
        !! Request of `IOS_BeepOff` procedure.
        !!
        !! Creates request to stop an active beep signal.
        !!
        !! Responses:       grc
        !! Instruments:     TPS1200, TM30/TS30, TS16
        !! ASCII-Request:   `%R1Q,20000:`
        !! ASCII-Response:  `%R1P,0,0:<grc>`
        type(request_type), intent(out) :: request !! Prepared request.

        character(len=REQUEST_REQUEST_LEN) :: string

        write (string, '("%R1Q,20000:", a)') GEOCOM_DELIMITER
        call dm_geocom_prepare_request(request, string, GEOCOM_GRC_PATTERN, GEOCOM_GRC_RESPONSES)
    end subroutine dm_geocom_prepare_request_beep_off

    pure subroutine dm_geocom_prepare_request_beep_on(request, intensity)
        !! Request of `IOS_BeepOn` procedure.
        !!
        !! Creates request for continuous beep signal of given intensity. If
        !! no intensity is passed, the default (`GEOCOM_IOS_BEEP_STDINTENS`) is
        !! used.
        !!
        !! Responses:       grc
        !! Instruments:     TPS1200, TM30/TS30, TS16
        !! ASCII-Request:   `%R1Q,20001:<intensity>`
        !! ASCII-Response:  `%R1P,0,0:<grc>`
        type(request_type), intent(out)          :: request   !! Prepared request.
        integer,            intent(in), optional :: intensity !! Intensity of beep, from 0 to 100.

        character(len=REQUEST_REQUEST_LEN) :: string
        integer                            :: intensity_

        intensity_ = GEOCOM_IOS_BEEP_STDINTENS
        if (present(intensity)) intensity_ = max(0, min(GEOCOM_IOS_BEEP_STDINTENS, intensity))

        write (string, '("%R1Q,20001:", i0, a)') intensity_, GEOCOM_DELIMITER
        call dm_geocom_prepare_request(request, string, GEOCOM_GRC_PATTERN, GEOCOM_GRC_RESPONSES)
    end subroutine dm_geocom_prepare_request_beep_on

    pure subroutine dm_geocom_prepare_request_change_face(request, pos_mode, atr_mode)
        !! Request of `AUT_ChangeFace` procedure.
        !!
        !! Creates request for turning the telescope to the other face.
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
        !! By default, `pos_mode` is set to `GEOCOM_AUT_NORMAL`, and `atr_mode`
        !! to `GEOCOM_AUT_POSITION`.
        !!
        !! Responses:       grc
        !! Instruments:     TPS1200, TM30/TS30, TS16
        !! ASCII-Request:   `%R1Q,9028:<pos_mode>,<atr_mode>,0`
        !! ASCII-Response:  `%R1P,0,0:<grc>`
        type(request_type), intent(out)          :: request  !! Prepared request.
        integer,            intent(in), optional :: pos_mode !! Position mode (`AUT_POSMODE`).
        integer,            intent(in), optional :: atr_mode !! ATR mode (`AUT_ATRMODE`).

        character(len=REQUEST_REQUEST_LEN) :: string
        integer                            :: atr_mode_, pos_mode_

        pos_mode_ = GEOCOM_AUT_NORMAL
        atr_mode_ = GEOCOM_AUT_POSITION

        if (present(pos_mode)) pos_mode_ = pos_mode
        if (present(atr_mode)) atr_mode_ = atr_mode

        write (string, '("%R1Q,9028:", i1, ",", i1, ",0", a)') pos_mode_, atr_mode_, GEOCOM_DELIMITER
        call dm_geocom_prepare_request(request, string, GEOCOM_GRC_PATTERN, GEOCOM_GRC_RESPONSES)
    end subroutine dm_geocom_prepare_request_change_face

    pure subroutine dm_geocom_prepare_request_delete(request, device_type, file_type, date, file_name)
        !! Request of `FTR_Delete` procedure.
        !!
        !! Creates request for deleting one or more files. Wildcards may be used
        !! to delete multiple files. If the deletion date is valid, only files
        !! older than the deletion date are deleted.
        !!
        !! Responses:       grc, nfiles
        !! Instruments:     TPS1200, TM30/TS30, TS16
        !! ASCII-Request:   `%R1Q,23309:<device_type>,<file_type>,<day>,<month>,<year>,<file_name>`
        !! ASCII-Response:  `%R1P,0,0:<grc>,<nfiles>`
        type(request_type),     intent(out) :: request     !! Prepared request.
        integer,                intent(in)  :: device_type !! Internal memory or memory card (`FTR_DEVICETYPE`).
        integer,                intent(in)  :: file_type   !! Type of file (`FTR_FILETYPE`).
        type(geocom_date_type), intent(in)  :: date        !! Modification date (`FTR_MODDATE`).
        character(len=*),       intent(in)  :: file_name   !! Name of file to delete.

        character(len=REQUEST_PATTERN_LEN) :: pattern
        character(len=REQUEST_REQUEST_LEN) :: string
        type(response_type)                :: responses(2)

        write (string, '("%R1Q,23309:", 5(i0, ","), 2a)') &
            device_type, file_type, date%day, date%month, date%year, file_name, GEOCOM_DELIMITER

        pattern   = '%R1P,0,0:(?<grc>\d+),(?<nfiles>\d+)'
        responses = [ response_type('grc'), response_type('nfiles') ]

        call dm_geocom_prepare_request(request, string, pattern, responses)
    end subroutine dm_geocom_prepare_request_delete

    pure subroutine dm_geocom_prepare_request_do_measure(request, command, mode)
        !! Request of `TMC_DoMeasure` procedure.
        !!
        !! Creates request for trying a distance measurement. This command does
        !! not return any values.
        !!
        !! The argument `command` may be one of the following TMC measurement
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
        !! By default, argument `command` is set to `GEOCOM_TMC_DEF_DIST`, and
        !! argument `mode` to `GEOCOM_TMC_AUTO_INC`.
        !!
        !! If a distance measurement is performed in measurement program
        !! `GEOCOM_TMC_DEF_DIST`, the distance sensor will work with the set
        !! EDM mode.
        !!
        !! Responses:       grc
        !! Instruments:     TPS1200, TM30/TS30, TS16
        !! ASCII-Request:   `%R1Q,2008:<command>,<mode>`
        !! ASCII-Response:  `%R1P,0,0:<grc>`
        type(request_type), intent(out)          :: request !! Prepared request.
        integer,            intent(in), optional :: command !! TMC measurement mode (`TMC_MEASURE_PRG`).
        integer,            intent(in), optional :: mode    !! Inclination sensor measurement mode (`TMC_INCLINE_PRG`).

        character(len=REQUEST_REQUEST_LEN) :: string
        integer                            :: command_, mode_

        command_ = GEOCOM_TMC_DEF_DIST
        mode_    = GEOCOM_TMC_AUTO_INC

        if (present(command)) command_ = command
        if (present(mode))    mode_    = mode

        write (string, '("%R1Q,2008:", i1, ",", i1, a)') command_, mode_, GEOCOM_DELIMITER
        call dm_geocom_prepare_request(request, string, GEOCOM_GRC_PATTERN, GEOCOM_GRC_RESPONSES)
    end subroutine dm_geocom_prepare_request_do_measure

    pure subroutine dm_geocom_prepare_request_do_position(request, hz, v, pos_mode, atr_mode)
        !! Request of `AUT_MakePositioning` procedure.
        !!
        !! Creates request for for turning the telescope to a specified
        !! position.
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
        !! By default, `pos_mode` is `GEOCOM_AUT_NORMAL`, and `atr_mode` is
        !! `GEOCOM_AUT_POSITION`.
        !!
        !! Responses:       grc
        !! Instruments:     TPS1200, TM30/TS30, TS16
        !! ASCII-Request:   `%R1Q,9027:<hz>,<v>,<pos_mode>,<atr_mode>,0`
        !! ASCII-Response:  `%R1P,0,0:<grc>`
        type(request_type), intent(out)          :: request  !! Prepared request.
        real(kind=r8),      intent(in)           :: hz       !! Horizontal angle [rad].
        real(kind=r8),      intent(in)           :: v        !! Vertical angle [rad].
        integer,            intent(in), optional :: pos_mode !! Position mode (`AUT_POSMODE`).
        integer,            intent(in), optional :: atr_mode !! ATR mode (`AUT_ATRMODE`).

        character(len=REQUEST_REQUEST_LEN) :: string
        integer                            :: atr_mode_, pos_mode_

        pos_mode_ = GEOCOM_AUT_NORMAL
        atr_mode_ = GEOCOM_AUT_POSITION

        if (present(pos_mode)) pos_mode_ = pos_mode
        if (present(atr_mode)) atr_mode_ = atr_mode

        write (string, '("%R1Q,9027:", 2(f12.10, ","), 2(i1, ","), "0", a)') &
            hz, v, pos_mode_, atr_mode_, GEOCOM_DELIMITER

        call dm_geocom_prepare_request(request, string, GEOCOM_GRC_PATTERN, GEOCOM_GRC_RESPONSES)
    end subroutine dm_geocom_prepare_request_do_position

    ! pure subroutine dm_geocom_prepare_request_download(block_number, block)
    ! pure subroutine dm_geocom_prepare_request_fine_adjust(hz, v)
    ! pure subroutine dm_geocom_prepare_request_get_angle1(angle, mode)
    ! pure subroutine dm_geocom_prepare_request_get_angle5(only_angle, mode)
    ! pure subroutine dm_geocom_prepare_request_get_angular_correction(incline, stand_axis, collimation, tilt_axis)
    ! pure subroutine dm_geocom_prepare_request_get_atmospheric_correction(atmos)
    ! pure subroutine dm_geocom_prepare_request_get_atmospheric_ppm(ppm)
    ! pure subroutine dm_geocom_prepare_request_get_atr_error(error)
    ! pure subroutine dm_geocom_prepare_request_get_atr_setting(setting)
    ! pure subroutine dm_geocom_prepare_request_get_binary_available(available)
    ! pure subroutine dm_geocom_prepare_request_get_config(auto_power, timeout)
    ! pure subroutine dm_geocom_prepare_request_get_coordinate(wait_time, coordinate, mode)
    ! pure subroutine dm_geocom_prepare_request_get_date_time(dt)
    ! pure subroutine dm_geocom_prepare_request_get_date_time_centi(year, month, day, hour, minute, second, centisecond)
    ! pure subroutine dm_geocom_prepare_request_get_device_config(device)

    pure subroutine dm_geocom_prepare_request_get_double_precision(request)
        !! Request of `COM_GetDoublePrecision` procedure.
        !!
        !! Creates request for getting the double precision setting – the
        !! number of digits to the right of the decimal point – when double
        !! floating-point values are transmitted.
        !!
        !! Responses:       grc, ndigits
        !! Instruments:     TPS1200, TM30/TS30, TS16
        !! ASCII-Request:   `%R1Q,108:`
        !! ASCII-Response:  `%R1P,0,0:<grc>,<ndigits>`
        type(request_type), intent(out) :: request !! Prepared request.

        character(len=REQUEST_PATTERN_LEN) :: pattern
        character(len=REQUEST_REQUEST_LEN) :: string
        type(response_type)                :: responses(2)

        string    = '%R1Q,108:' // GEOCOM_DELIMITER
        pattern   = '%R1P,0,0:(?<grc>\d+),(?<ndigits>\d+)'
        responses = [ response_type('grc'), response_type('ndigits') ]

        call dm_geocom_prepare_request(request, string, pattern, responses)
    end subroutine dm_geocom_prepare_request_get_double_precision

    ! pure subroutine dm_geocom_prepare_request_get_edm_mode(mode)
    ! pure subroutine dm_geocom_prepare_request_get_egl_intensity(intensity)
    ! pure subroutine dm_geocom_prepare_request_get_face(face)
    ! pure subroutine dm_geocom_prepare_request_get_fine_adjust_mode(mode)
    ! pure subroutine dm_geocom_prepare_request_get_full_measurement(wait_time, hz, v, accuracy, cross_incl, length_incl, accuracy_incl, slope_dist, dist_time, mode)
    ! pure subroutine dm_geocom_prepare_request_get_geometric_ppm(automatic, scale_factor, offset, height, individual)
    ! pure subroutine dm_geocom_prepare_request_get_height(height)
    ! pure subroutine dm_geocom_prepare_request_get_image_config(mem_type, parameters)
    ! pure subroutine dm_geocom_prepare_request_get_incline_correction(mode)
    ! pure subroutine dm_geocom_prepare_request_get_incline_error(error)
    ! pure subroutine dm_geocom_prepare_request_get_instrument_name(name)
    ! pure subroutine dm_geocom_prepare_request_get_instrument_number(serial)
    ! pure subroutine dm_geocom_prepare_request_get_internal_temperature(temperature)
    ! pure subroutine dm_geocom_prepare_request_get_lock_status(status)
    ! pure subroutine dm_geocom_prepare_request_get_measurement_program(prog)
    ! pure subroutine dm_geocom_prepare_request_get_power(capacity, active, suggest)
    ! pure subroutine dm_geocom_prepare_request_get_prism_definition(type, definition)
    ! pure subroutine dm_geocom_prepare_request_get_prism_type(type)
    ! pure subroutine dm_geocom_prepare_request_get_prism_type2(type, name)
    ! pure subroutine dm_geocom_prepare_request_get_quick_distance(only_angle, slope_dist)
    ! pure subroutine dm_geocom_prepare_request_get_reduced_atr_fov(mode)
    ! pure subroutine dm_geocom_prepare_request_get_reflectorless_class(class)
    ! pure subroutine dm_geocom_prepare_request_get_refractive_correction(coefficient)
    ! pure subroutine dm_geocom_prepare_request_get_refractive_method(method)
    ! pure subroutine dm_geocom_prepare_request_get_search_area(area)
    ! pure subroutine dm_geocom_prepare_request_get_set_laser_pointer(mode)
    ! pure subroutine dm_geocom_prepare_request_get_signal(signal)
    ! pure subroutine dm_geocom_prepare_request_get_simple_coordinates(wait_time, e, n, h, prog)
    ! pure subroutine dm_geocom_prepare_request_get_simple_meta(wait_time, only_angle, slope_dist, mode)
    ! pure subroutine dm_geocom_prepare_request_get_slope_distance_correction(ppm_corr, prism_corr)
    ! pure subroutine dm_geocom_prepare_request_get_software_version(release, version, subversion)
    ! pure subroutine dm_geocom_prepare_request_get_software_version2(release, version, subversion)
    ! pure subroutine dm_geocom_prepare_request_get_station(station)
    ! pure subroutine dm_geocom_prepare_request_get_target_type(type)
    ! pure subroutine dm_geocom_prepare_request_get_timeout(hz, v)
    ! pure subroutine dm_geocom_prepare_request_get_tolerance(hz, v)
    ! pure subroutine dm_geocom_prepare_request_get_user_atr_mode(mode)
    ! pure subroutine dm_geocom_prepare_request_get_user_local_mode(mode)
    ! pure subroutine dm_geocom_prepare_request_get_user_prism_definition(name, add_const, type, creator)
    ! pure subroutine dm_geocom_prepare_request_get_user_spiral(hz, v)
    ! pure subroutine dm_geocom_prepare_request_list(next, last, dir_info)
    ! pure subroutine dm_geocom_prepare_request_lock_in()
    ! pure subroutine dm_geocom_prepare_request_measure_distance_angle(mode, hz, v, slope_dist)
    ! pure subroutine dm_geocom_prepare_request_null_proc()
    ! pure subroutine dm_geocom_prepare_request_ps_enable_range(enable)
    ! pure subroutine dm_geocom_prepare_request_ps_search_next(direction, swing)
    ! pure subroutine dm_geocom_prepare_request_ps_search_window()
    ! pure subroutine dm_geocom_prepare_request_ps_set_range(min_dist, max_dist)
    ! pure subroutine dm_geocom_prepare_request_search(hz, v)
    ! pure subroutine dm_geocom_prepare_request_search_target()
    ! pure subroutine dm_geocom_prepare_request_set_angle_correction(incline, stand_axis, collimation, tilt_axis)
    ! pure subroutine dm_geocom_prepare_request_set_atmospheric_correction(atmos)
    ! pure subroutine dm_geocom_prepare_request_set_atmospheric_ppm(ppm)
    ! pure subroutine dm_geocom_prepare_request_set_atr_setting(setting)
    ! pure subroutine dm_geocom_prepare_request_set_binary_available(available)
    ! pure subroutine dm_geocom_prepare_request_set_config(auto_power, timeout)
    ! pure subroutine dm_geocom_prepare_request_set_date_time(dt)
    ! pure subroutine dm_geocom_prepare_request_set_double_precision(ndigits)
    ! pure subroutine dm_geocom_prepare_request_set_edm_mode(mode)
    ! pure subroutine dm_geocom_prepare_request_set_egl_intensity(intensity)
    ! pure subroutine dm_geocom_prepare_request_set_fine_adjust_mode(mode)
    ! pure subroutine dm_geocom_prepare_request_set_geometric_ppm(automatic, scale_factor, offset, height, individual)
    ! pure subroutine dm_geocom_prepare_request_set_height(height)
    ! pure subroutine dm_geocom_prepare_request_set_image_config(mem_type, parameters)
    ! pure subroutine dm_geocom_prepare_request_set_incline_correction(mode)
    ! pure subroutine dm_geocom_prepare_request_set_measurement_program(prog)
    ! pure subroutine dm_geocom_prepare_request_set_offset(slope_dist, height, mode)
    ! pure subroutine dm_geocom_prepare_request_set_orientation(hz)
    ! pure subroutine dm_geocom_prepare_request_set_prism_correction(prism_corr)
    ! pure subroutine dm_geocom_prepare_request_set_prism_type(type)
    ! pure subroutine dm_geocom_prepare_request_set_prism_type2(type, name)
    ! pure subroutine dm_geocom_prepare_request_set_reduced_atr_fov(mode)
    ! pure subroutine dm_geocom_prepare_request_set_refractive_correction(coefficient)
    ! pure subroutine dm_geocom_prepare_request_set_refractive_method(method)
    ! pure subroutine dm_geocom_prepare_request_set_search_area(area)
    ! pure subroutine dm_geocom_prepare_request_set_station(station)
    ! pure subroutine dm_geocom_prepare_request_set_target_type(type)
    ! pure subroutine dm_geocom_prepare_request_set_timeout(hz, v)
    ! pure subroutine dm_geocom_prepare_request_set_tolerance(hz, v)
    ! pure subroutine dm_geocom_prepare_request_set_user_atr_mode(mode)
    ! pure subroutine dm_geocom_prepare_request_set_user_local_mode(mode)
    ! pure subroutine dm_geocom_prepare_request_set_user_prism_definition(name, add_const, type, creator)
    ! pure subroutine dm_geocom_prepare_request_set_user_spiral(hz, v)
    ! pure subroutine dm_geocom_prepare_request_set_velocity(omega)
    ! pure subroutine dm_geocom_prepare_request_setup_download(device_type, file_type, file_name, block_size, nblocks)
    ! pure subroutine dm_geocom_prepare_request_setup_list(device_type, file_type, search_path)
    ! pure subroutine dm_geocom_prepare_request_start_controller(mode)
    ! pure subroutine dm_geocom_prepare_request_stop_controller(mode)
    ! pure subroutine dm_geocom_prepare_request_switch_off(mode)
    ! pure subroutine dm_geocom_prepare_request_switch_on(mode)
    ! pure subroutine dm_geocom_prepare_request_take_image(mem_type, n)
end module dm_geocom_api
