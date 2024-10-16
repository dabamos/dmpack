! Author:  Philipp Engel
! Licence: ISC
module dm_geocom_error
    !! GeoCOM return codes and return code messages. GeoCOM return codes have
    !! the prefix `GRC_`.
    !!
    !! The GeoCOM return codes are compatible to at least the following sensors:
    !!
    !! * Leica TPS1100
    !! * Leica TPS1200
    !! * Leica TM30/TS30
    !! * Leica Viva TS16 (FlexLine)
    !!
    !! Obvious spelling errors in the return code descriptions have been
    !! corrected. However, some descriptions are rather silly (Heerbrugg
    !! English?).
    implicit none (type, external)
    private

    ! **************************************************************************
    ! GRC - GEOCOM RETURN CODES.
    ! **************************************************************************
    integer, parameter, public :: GRC_OK                         = 0     !! Function successfully completed.
    integer, parameter, public :: GRC_UNDEFINED                  = 1     !! Unknown error, result unspecified.
    integer, parameter, public :: GRC_IVPARAM                    = 2     !! Invalid parameter detected. Result unspecified.
    integer, parameter, public :: GRC_IVRESULT                   = 3     !! Invalid result.
    integer, parameter, public :: GRC_FATAL                      = 4     !! Fatal error.
    integer, parameter, public :: GRC_NOT_IMPL                   = 5     !! Not implemented.
    integer, parameter, public :: GRC_TIME_OUT                   = 6     !! Function execution timed out. Result unspecified.
    integer, parameter, public :: GRC_SET_INCOMPL                = 7     !! Parameter setup for subsystem is incomplete.
    integer, parameter, public :: GRC_ABORT                      = 8     !! Function execution has been aborted.
    integer, parameter, public :: GRC_NOMEMORY                   = 9     !! Fatal error (not enough memory).
    integer, parameter, public :: GRC_NOTINIT                    = 10    !! Fatal error (subsystem not initialised).
    integer, parameter, public :: GRC_SHUT_DOWN                  = 12    !! Subsystem is down.
    integer, parameter, public :: GRC_SYSBUSY                    = 13    !! System busy/already in use of another process.
    integer, parameter, public :: GRC_HWFAILURE                  = 14    !! Fatal error (hardware failure).
    integer, parameter, public :: GRC_ABORT_APPL                 = 15    !! Execution of application has been aborted.
    integer, parameter, public :: GRC_LOW_POWER                  = 16    !! Operation aborted (insufficient power supply level).
    integer, parameter, public :: GRC_IVVERSION                  = 17    !! Invalid version of file.
    integer, parameter, public :: GRC_BAT_EMPTY                  = 18    !! Battery empty, about 1 minute remaining.
    integer, parameter, public :: GRC_NO_EVENT                   = 20    !! No event pending.
    integer, parameter, public :: GRC_OUT_OF_TEMP                = 21    !! Out of temperature range.
    integer, parameter, public :: GRC_INSTRUMENT_TILT            = 22    !! Instrument tilting out of range.
    integer, parameter, public :: GRC_COM_SETTING                = 23    !! Communication error.
    integer, parameter, public :: GRC_NO_ACTION                  = 24    !! GRC_TYPE Input (do no action)
    integer, parameter, public :: GRC_SLEEP_MODE                 = 25    !! Instrument went into sleep mode.
    integer, parameter, public :: GRC_NOTOK                      = 26    !! Function not successfully completed.
    integer, parameter, public :: GRC_NA                         = 27    !! Not available (licence key not available).
    integer, parameter, public :: GRC_OVERFLOW                   = 28    !! Overflow error.
    integer, parameter, public :: GRC_STOPPED                    = 29    !! System or subsystem has been stopped.

    integer, parameter, public :: GRC_ANG                        = 256   !! ANG error.
    integer, parameter, public :: GRC_ANG_ERROR                  = 257   !! Angles and inclinations not valid.
    integer, parameter, public :: GRC_ANG_INCL_ERROR             = 258   !! Inclinations not valid.
    integer, parameter, public :: GRC_ANG_BAD_ACC                = 259   !! Value accuracies not reached.
    integer, parameter, public :: GRC_ANG_BAD_ANGLE_ACC          = 260   !! Angle accuracies not reached.
    integer, parameter, public :: GRC_ANG_BAD_INCLIN_ACC         = 261   !! Inclination accuracies not reached.
    integer, parameter, public :: GRC_ANG_WRITE_PROTECTED        = 266   !! No write access allowed.
    integer, parameter, public :: GRC_ANG_OUT_OF_RANGE           = 267   !! Value out of range.
    integer, parameter, public :: GRC_ANG_IR_OCCURED             = 268   !! Function aborted due to interrupt.
    integer, parameter, public :: GRC_ANG_HZ_MOVED               = 269   !! Hz moved during incline measurement.
    integer, parameter, public :: GRC_ANG_OS_ERROR               = 270   !! Troubles with operation system.
    integer, parameter, public :: GRC_ANG_DATA_ERROR             = 271   !! Overflow at parameter values.
    integer, parameter, public :: GRC_ANG_PEAK_CNT_UFL           = 272   !! Not enough peaks.
    integer, parameter, public :: GRC_ANG_TIME_OUT               = 273   !! Reading timeout.
    integer, parameter, public :: GRC_ANG_TOO_MANY_EXPOS         = 274   !! Too many exposures wanted.
    integer, parameter, public :: GRC_ANG_PIX_CTRL_ERR           = 275   !! Picture height out of range.
    integer, parameter, public :: GRC_ANG_MAX_POS_SKIP           = 276   !! Positive exposure dynamic overflow.
    integer, parameter, public :: GRC_ANG_MAX_NEG_SKIP           = 277   !! Negative exposure dynamic overflow.
    integer, parameter, public :: GRC_ANG_EXP_LIMIT              = 278   !! Exposure time overflow.
    integer, parameter, public :: GRC_ANG_UNDER_EXPOSURE         = 279   !! Picture under-exposured.
    integer, parameter, public :: GRC_ANG_OVER_EXPOSURE          = 280   !! Picture over-exposured.
    integer, parameter, public :: GRC_ANG_TMANY_PEAKS            = 300   !! Too many peaks detected.
    integer, parameter, public :: GRC_ANG_TLESS_PEAKS            = 301   !! Not enough peaks detected.
    integer, parameter, public :: GRC_ANG_PEAK_TOO_SLIM          = 302   !! Peak too slim.
    integer, parameter, public :: GRC_ANG_PEAK_TOO_WIDE          = 303   !! Peak to wide.
    integer, parameter, public :: GRC_ANG_BAD_PEAKDIFF           = 304   !! Bad peak difference.
    integer, parameter, public :: GRC_ANG_UNDER_EXP_PICT         = 305   !! Too low peak amplitude.
    integer, parameter, public :: GRC_ANG_PEAKS_INHOMOGEN        = 306   !! Inhomogeneous peak amplitudes.
    integer, parameter, public :: GRC_ANG_NO_DECOD_POSS          = 307   !! No peak decoding possible.
    integer, parameter, public :: GRC_ANG_UNSTABLE_DECOD         = 308   !! Peak decoding not stable.
    integer, parameter, public :: GRC_ANG_TLESS_FPEAKS           = 309   !! Not enough valid fine-peaks.
    integer, parameter, public :: GRC_ANG_INCL_OLD_PLANE         = 316   !! Inclination plane out of time range.
    integer, parameter, public :: GRC_ANG_INCL_NO_PLANE          = 317   !! Inclination no plane available.
    integer, parameter, public :: GRC_ANG_FAST_ANG_ERR           = 326   !! Errors in 5 kHz and or 2.5 kHz angle.
    integer, parameter, public :: GRC_ANG_FAST_ANG_ERR_5         = 327   !! Errors in 5 kHz angle.
    integer, parameter, public :: GRC_ANG_FAST_ANG_ERR_25        = 328   !! Errors in 2.5 kHz angle.
    integer, parameter, public :: GRC_ANG_TRANS_ERR              = 329   !! LVDS transfer error detected.
    integer, parameter, public :: GRC_ANG_TRANS_ERR_5            = 330   !! LVDS transfer error detected in 5 kHz mode.
    integer, parameter, public :: GRC_ANG_TRANS_ERR_25           = 331   !! LVDS transfer error detected in 2.5 kHz mode.

    integer, parameter, public :: GRC_ATA_NOT_READY              = 512   !! ATR system is not ready.
    integer, parameter, public :: GRC_ATA_NO_RESULT              = 513   !! Result is not available yet.
    integer, parameter, public :: GRC_ATA_SEVERAL_TARGETS        = 514   !! Several targets detected.
    integer, parameter, public :: GRC_ATA_BIG_SPOT               = 515   !! Spot is too big for analyse.
    integer, parameter, public :: GRC_ATA_BACKGROUND             = 516   !! Background is too bright.
    integer, parameter, public :: GRC_ATA_NO_TARGETS             = 517   !! No targets detected.
    integer, parameter, public :: GRC_ATA_NOT_ACCURAT            = 518   !! Accuracy worse than asked for.
    integer, parameter, public :: GRC_ATA_SPOT_ON_EDGE           = 519   !! Spot is on the edge of the sensing area.
    integer, parameter, public :: GRC_ATA_BLOOMING               = 522   !! Blooming or spot on edge detected.
    integer, parameter, public :: GRC_ATA_NOT_BUSY               = 523   !! ATR is not in a continuous mode.
    integer, parameter, public :: GRC_ATA_STRANGE_LIGHT          = 524   !! Not the spot of the own target illuminator.
    integer, parameter, public :: GRC_ATA_V24_FAIL               = 525   !! Communication error to sensor (ATR).
    integer, parameter, public :: GRC_ATA_DECODE_ERROR           = 526   !! Received Arguments cannot be decoded.
    integer, parameter, public :: GRC_ATA_HZ_FAIL                = 527   !! No spot detected in Hz direction.
    integer, parameter, public :: GRC_ATA_V_FAIL                 = 528   !! No spot detected in V direction.
    integer, parameter, public :: GRC_ATA_HZ_STRANGE_L           = 529   !! Strange light in Hz direction.
    integer, parameter, public :: GRC_ATA_V_STRANGE_L            = 530   !! Strange light in V direction.
    integer, parameter, public :: GRC_ATA_SLDR_TRANSFER_PENDING  = 531   !! On multiple ATA_SLDR_OpenTransfer.
    integer, parameter, public :: GRC_ATA_SLDR_TRANSFER_ILLEGAL  = 532   !! No ATA_SLDR_OpenTransfer happened.
    integer, parameter, public :: GRC_ATA_SLDR_DATA_ERROR        = 533   !! Unexpected data format received.
    integer, parameter, public :: GRC_ATA_SLDR_CHK_SUM_ERROR     = 534   !! Checksum error in transmitted data.
    integer, parameter, public :: GRC_ATA_SLDR_ADDRESS_ERROR     = 535   !! Address out of valid range.
    integer, parameter, public :: GRC_ATA_SLDR_INV_LOADFILE      = 536   !! Firmware file has invalid format.
    integer, parameter, public :: GRC_ATA_SLDR_UNSUPPORTED       = 537   !! Current (loaded) firmware does not support upload.
    integer, parameter, public :: GRC_ATA_PS_NOT_READY           = 538   !! PowerSearch system is not ready.
    integer, parameter, public :: GRC_ATA_ATR_SYSTEM_ERR         = 539   !! ATR system error.

    integer, parameter, public :: GRC_EDM                        = 768   !! EDM error.
    integer, parameter, public :: GRC_EDM_SYSTEM_ERR             = 769   !! Fatal EDM sensor error.
    integer, parameter, public :: GRC_EDM_INVALID_COMMAND        = 770   !! Invalid command or unknown command.
    integer, parameter, public :: GRC_EDM_BOOM_ERR               = 771   !! Boomerang error.
    integer, parameter, public :: GRC_EDM_SIGN_LOW_ERR           = 772   !! Received signal too low, prism too far away, or natural barrier, bad environment, etc.
    integer, parameter, public :: GRC_EDM_DIL_ERR                = 773   !! Obsolete.
    integer, parameter, public :: GRC_EDM_SIGN_HIGH_ERR          = 774   !! Received signal to strong, prism to near, strange light effect.
    integer, parameter, public :: GRC_EDM_TIMEOUT                = 775   !! Timeout, measuring time exceeded (signal too weak, beam interrupted).
    integer, parameter, public :: GRC_EDM_FLUKT_ERR              = 776   !! Too much turbulences or distractions.
    integer, parameter, public :: GRC_EDM_FMOT_ERR               = 777   !! Filter motor defective.
    integer, parameter, public :: GRC_EDM_DEV_NOT_INSTALLED      = 778   !! Device like EGL, DL is not installed.
    integer, parameter, public :: GRC_EDM_NOT_FOUND              = 779   !! Search result invalid.
    integer, parameter, public :: GRC_EDM_ERROR_RECEIVED         = 780   !! Communication ok, but an error reported from the EDM sensor.
    integer, parameter, public :: GRC_EDM_MISSING_SRVPWD         = 781   !! No service password is set.
    integer, parameter, public :: GRC_EDM_INVALID_ANSWER         = 782   !! Communication ok, but an unexpected answer received.
    integer, parameter, public :: GRC_EDM_SEND_ERR               = 783   !! Data send error, sending buffer is full.
    integer, parameter, public :: GRC_EDM_RECEIVE_ERR            = 784   !! Data receive error, like parity buffer overflow.
    integer, parameter, public :: GRC_EDM_INTERNAL_ERR           = 785   !! Internal EDM subsystem error.
    integer, parameter, public :: GRC_EDM_BUSY                   = 786   !! Sensor is working already, abort current measuring first.
    integer, parameter, public :: GRC_EDM_NO_MEASACTIVITY        = 787   !! No measurement activity started.
    integer, parameter, public :: GRC_EDM_CHKSUM_ERR             = 788   !! Calculated checksum, resp. received data wrong.
    integer, parameter, public :: GRC_EDM_INIT_OR_STOP_ERR       = 789   !! During start up or shut down phase an error occured.
    integer, parameter, public :: GRC_EDM_SRL_NOT_AVAILABLE      = 790   !! Red laser not available on this sensor HW.
    integer, parameter, public :: GRC_EDM_MEAS_ABORTED           = 791   !! Measurement will be aborted (will be used for the laser security).
    integer, parameter, public :: GRC_EDM_SLDR_TRANSFER_PENDING  = 798   !! Multiple OpenTransfer calls.
    integer, parameter, public :: GRC_EDM_SLDR_TRANSFER_ILLEGAL  = 799   !! No open transfer happened.
    integer, parameter, public :: GRC_EDM_SLDR_DATA_ERROR        = 800   !! Unexpected data format received.
    integer, parameter, public :: GRC_EDM_SLDR_CHK_SUM_ERROR     = 801   !! Checksum error in transmitted data.
    integer, parameter, public :: GRC_EDM_SLDR_ADDR_ERROR        = 802   !! Address out of valid range.
    integer, parameter, public :: GRC_EDM_SLDR_INV_LOADFILE      = 803   !! Firmware file has invalid format.
    integer, parameter, public :: GRC_EDM_SLDR_UNSUPPORTED       = 804   !! Current (loaded) firmware doesn't support upload.
    integer, parameter, public :: GRC_EDM_UNKNOW_ERR             = 808   !! Undocumented error from the EDM sensor, should not occur.
    integer, parameter, public :: GRC_EDM_DISTRANGE_ERR          = 818   !! Out of distance range (too small or large).
    integer, parameter, public :: GRC_EDM_SIGNTONOISE_ERR        = 819   !! Signal to noise ratio too small.
    integer, parameter, public :: GRC_EDM_NOISEHIGH_ERR          = 820   !! Noise too high.
    integer, parameter, public :: GRC_EDM_PWD_NOTSET             = 821   !! Password is not set.
    integer, parameter, public :: GRC_EDM_ACTION_NO_MORE_VALID   = 822   !! Elapsed time between prepare and start fast measurement for ATR too long.
    integer, parameter, public :: GRC_EDM_MULTRG_ERR             = 823   !! Possibly more than one target (also a sensor error).
    integer, parameter, public :: GRC_EDM_MISSING_EE_CONSTS      = 824   !! EEPROM constantss are missing.
    integer, parameter, public :: GRC_EDM_NOPRECISE              = 825   !! No precise measurement possible.
    integer, parameter, public :: GRC_EDM_MEAS_DIST_NOT_ALLOWED  = 826   !! Measured distance is too big (not allowed).

    integer, parameter, public :: GRC_GMF                        = 1024  !! GMF error.
    integer, parameter, public :: GRC_GMF_WRONG_AREA_DEF         = 1025  !! Wrong area definition.
    integer, parameter, public :: GRC_GMF_IDENTICAL_PTS          = 1026  !! Identical points.
    integer, parameter, public :: GRC_GMF_PTS_IN_LINE            = 1027  !! Points on one line
    integer, parameter, public :: GRC_GMF_OUT_OF_RANGE           = 1028  !! Out of range.
    integer, parameter, public :: GRC_GMF_PLAUSIBILITY_ERR       = 1029  !! Plausibility error.
    integer, parameter, public :: GRC_GMF_TOO_FEW_OBSERVATIONS   = 1030  !! Too few observations to calculate the average.
    integer, parameter, public :: GRC_GMF_NO_SOLUTION            = 1031  !! No solution.
    integer, parameter, public :: GRC_GMF_ONE_SOLUTION           = 1032  !! Only one solution.
    integer, parameter, public :: GRC_GMF_TWO_SOLUTIONS          = 1033  !! Second solution.
    integer, parameter, public :: GRC_GMF_ANGLE_SMALLER_15GON    = 1034  !! Intersection angle < 15 gon.
    integer, parameter, public :: GRC_GMF_INVALID_TRIANGLE_TYPE  = 1035  !! Invalid triangle.
    integer, parameter, public :: GRC_GMF_INVALID_ANGLE_SYSTEM   = 1036  !! Invalid angle unit.
    integer, parameter, public :: GRC_GMF_INVALID_DIST_SYSTEM    = 1037  !! Invalid distance unit.
    integer, parameter, public :: GRC_GMF_INVALID_V_SYSTEM       = 1038  !! Invalid vertical angle.
    integer, parameter, public :: GRC_GMF_INVALID_TEMP_SYSTEM    = 1039  !! Invalid temperature system.
    integer, parameter, public :: GRC_GMF_INVALID_PRES_SYSTEM    = 1040  !! Invalid pressure unit.
    integer, parameter, public :: GRC_GMF_RADIUS_NOT_POSSIBLE    = 1041  !! Invalid radius.
    integer, parameter, public :: GRC_GMF_NO_PROVISIONAL_VALUES  = 1042  !! Insufficient data (GM2).
    integer, parameter, public :: GRC_GMF_SINGULAR_MATRIX        = 1043  !! Bad data (GM2).
    integer, parameter, public :: GRC_GMF_TOO_MANY_ITERATIONS    = 1044  !! Bad data distr (GM2).
    integer, parameter, public :: GRC_GMF_IDENTICAL_TIE_POINTS   = 1045  !! Same tie points (GM2).
    integer, parameter, public :: GRC_GMF_SETUP_EQUALS_TIE_POINT = 1046  !! Station and tie point same (GM2).

    integer, parameter, public :: GRC_TMC                        = 1280  !! TMC error.
    integer, parameter, public :: GRC_TMC_NO_FULL_CORRECTION     = 1283  !! Measurement without full correction.
    integer, parameter, public :: GRC_TMC_ACCURACY_GUARANTEE     = 1284  !! Accuracy can not be guaranteed.
    integer, parameter, public :: GRC_TMC_ANGLE_OK               = 1285  !! Only angle measurement valid.
    integer, parameter, public :: GRC_TMC_ANGLE_NOT_FULL_CORR    = 1288  !! Only angle measurement valid but without full correction.
    integer, parameter, public :: GRC_TMC_ANGLE_NO_ACC_GUARANTY  = 1289  !! Only angle measurement valid but accuracy can not be guaranteed.
    integer, parameter, public :: GRC_TMC_ANGLE_ERROR            = 1290  !! No angle measurement.
    integer, parameter, public :: GRC_TMC_DIST_PPM               = 1291  !! Wrong setting of PPM or MM on EDM.
    integer, parameter, public :: GRC_TMC_DIST_ERROR             = 1292  !! Distance measurement not done (no aim).
    integer, parameter, public :: GRC_TMC_BUSY                   = 1293  !! System is busy (no measurement done).
    integer, parameter, public :: GRC_TMC_SIGNAL_ERROR           = 1294  !! No signal on EDM (only in signal mode).

    integer, parameter, public :: GRC_MOT_UNREADY                = 1792  !! Motorisation is not ready.
    integer, parameter, public :: GRC_MOT_BUSY                   = 1793  !! Motorisation is handling another task.
    integer, parameter, public :: GRC_MOT_NOT_OCONST             = 1794  !! Motorisation is not in velocity mode.
    integer, parameter, public :: GRC_MOT_NOT_CONFIG             = 1795  !! Motorisation is in the wrong mode or busy.
    integer, parameter, public :: GRC_MOT_NOT_POSIT              = 1796  !! Motorisation is not in posit mode.
    integer, parameter, public :: GRC_MOT_NOT_SERVICE            = 1797  !! Motorisation is not in service mode.
    integer, parameter, public :: GRC_MOT_NOT_BUSY               = 1798  !! Motorisation is handling no task.
    integer, parameter, public :: GRC_MOT_NOT_LOCK               = 1799  !! Motorisation is not in tracking mode.
    integer, parameter, public :: GRC_MOT_NOT_SPIRAL             = 1800  !! Motorisation is not in spiral mode.
    integer, parameter, public :: GRC_MOT_V_ENCODER              = 1801  !! Vertical encoder/motor error.
    integer, parameter, public :: GRC_MOT_HZ_ENCODER             = 1802  !! Horizontal encoder/motor error.
    integer, parameter, public :: GRC_MOT_HZ_V_ENCODER           = 1803  !! Horizontal and vertical encoder/motor error.

    integer, parameter, public :: GRC_BMM                        = 2304  !! BMM error.
    integer, parameter, public :: GRC_BMM_XFER_PENDING           = 2305  !! Loading process already opened.
    integer, parameter, public :: GRC_BMM_NO_XFER_OPEN           = 2306  !! Transfer not opened.
    integer, parameter, public :: GRC_BMM_UNKNOWN_CHARSET        = 2307  !! Unknown character set.
    integer, parameter, public :: GRC_BMM_NOT_INSTALLED          = 2308  !! Display module not present.
    integer, parameter, public :: GRC_BMM_ALREADY_EXIST          = 2309  !! Character set already exists.
    integer, parameter, public :: GRC_BMM_CANT_DELETE            = 2310  !! Character set cannot be deleted.
    integer, parameter, public :: GRC_BMM_MEM_ERROR              = 2311  !! Memory cannot be allocated.
    integer, parameter, public :: GRC_BMM_CHARSET_USED           = 2312  !! Character set still used.
    integer, parameter, public :: GRC_BMM_CHARSET_SAVED          = 2313  !! Charset cannot be deleted or is protected.
    integer, parameter, public :: GRC_BMM_INVALID_ADR            = 2314  !! Attempt to copy a character block outside the allocated memory.
    integer, parameter, public :: GRC_BMM_CANCELANDADR_ERROR     = 2315  !! Error during release of allocated memory.
    integer, parameter, public :: GRC_BMM_INVALID_SIZE           = 2316  !! Number of bytes specified in header does not match the bytes read.
    integer, parameter, public :: GRC_BMM_CANCELANDINVSIZE_ERROR = 2317  !! Allocated memory could not be released.
    integer, parameter, public :: GRC_BMM_ALL_GROUP_OCC          = 2318  !! Max. number of character sets already loaded.
    integer, parameter, public :: GRC_BMM_CANT_DEL_LAYERS        = 2319  !! Layer cannot be deleted.
    integer, parameter, public :: GRC_BMM_UNKNOWN_LAYER          = 2320  !! Required layer does not exist.
    integer, parameter, public :: GRC_BMM_INVALID_LAYERLEN       = 2321  !! Layer length exceeds maximum.

    integer, parameter, public :: GRC_COM_ERO                    = 3072  !! Initiate Extended Runtime Operation (ERO).
    integer, parameter, public :: GRC_COM_CANT_ENCODE            = 3073  !! Cannot encode arguments in client.
    integer, parameter, public :: GRC_COM_CANT_DECODE            = 3074  !! Cannot decode results in client.
    integer, parameter, public :: GRC_COM_CANT_SEND              = 3075  !! Hardware error while sending.
    integer, parameter, public :: GRC_COM_CANT_RECV              = 3076  !! Hardware error while receiving.
    integer, parameter, public :: GRC_COM_TIMEDOUT               = 3077  !! Request timed out.
    integer, parameter, public :: GRC_COM_WRONG_FORMAT           = 3078  !! Packet format error.
    integer, parameter, public :: GRC_COM_VER_MISMATCH           = 3079  !! Version mismatch between client and server.
    integer, parameter, public :: GRC_COM_CANT_DECODE_REQ        = 3080  !! Cannot decode arguments in server.
    integer, parameter, public :: GRC_COM_PROC_UNAVAIL           = 3081  !! Unknown RPC, procedure ID invalid.
    integer, parameter, public :: GRC_COM_CANT_ENCODE_REP        = 3082  !! Cannot encode results in server.
    integer, parameter, public :: GRC_COM_SYSTEM_ERR             = 3083  !! Unspecified generic system error.
    integer, parameter, public :: GRC_COM_FAILED                 = 3085  !! Unspecified error.
    integer, parameter, public :: GRC_COM_NO_BINARY              = 3086  !! Binary protocol not available.
    integer, parameter, public :: GRC_COM_INTR                   = 3087  !! Call interrupted.
    integer, parameter, public :: GRC_COM_REQUIRES_8DBITS        = 3090  !! Protocol needs 8 bit encoded characters.
    integer, parameter, public :: GRC_COM_TR_ID_MISMATCH         = 3093  !! TRANSACTIONS ID mismatch error.
    integer, parameter, public :: GRC_COM_NOT_GEOCOM             = 3094  !! Protocol not recognisable.
    integer, parameter, public :: GRC_COM_UNKNOWN_PORT           = 3095  !! Invalid port address.
    integer, parameter, public :: GRC_COM_ERO_END                = 3099  !! ERO is terminating.
    integer, parameter, public :: GRC_COM_OVERRUN                = 3100  !! Internal error (data buffer overflow).
    integer, parameter, public :: GRC_COM_SRVR_RX_CHECKSUM_ERRR  = 3101  !! Invalid checksum on server side received.
    integer, parameter, public :: GRC_COM_CLNT_RX_CHECKSUM_ERRR  = 3102  !! Invalid checksum on client side received.
    integer, parameter, public :: GRC_COM_PORT_NOT_AVAILABLE     = 3103  !! Port not available.
    integer, parameter, public :: GRC_COM_PORT_NOT_OPEN          = 3104  !! Port not opened.
    integer, parameter, public :: GRC_COM_NO_PARTNER             = 3105  !! Unable to find TPS.
    integer, parameter, public :: GRC_COM_ERO_NOT_STARTED        = 3106  !! Extended Runtime Operation could not be started.
    integer, parameter, public :: GRC_COM_CONS_REQ               = 3107  !! Att to send cons reqs.
    integer, parameter, public :: GRC_COM_SRVR_IS_SLEEPING       = 3108  !! TPS has gone to sleep (wait and try again).
    integer, parameter, public :: GRC_COM_SRVR_IS_OFF            = 3109  !! TPS has shut down (wait and try again).
    integer, parameter, public :: GRC_COM_NO_CHECKSUM            = 3110  !! No checksum in ASCII protocol available.

    integer, parameter, public :: GRC_AUT_TIMEOUT                = 8704  !! Position not reached.
    integer, parameter, public :: GRC_AUT_DETENT_ERROR           = 8705  !! Positioning not possible due to mounted EDM.
    integer, parameter, public :: GRC_AUT_ANGLE_ERROR            = 8706  !! Angle measurement error.
    integer, parameter, public :: GRC_AUT_MOTOR_ERROR            = 8707  !! Motorisation error.
    integer, parameter, public :: GRC_AUT_INCACC                 = 8708  !! Position not exactly reached.
    integer, parameter, public :: GRC_AUT_DEV_ERROR              = 8709  !! Deviation measurement error.
    integer, parameter, public :: GRC_AUT_NO_TARGET              = 8710  !! No target detected.
    integer, parameter, public :: GRC_AUT_MULTIPLE_TARGETS       = 8711  !! Multiple targets detected.
    integer, parameter, public :: GRC_AUT_BAD_ENVIRONMENT        = 8712  !! Bad environment conditions.
    integer, parameter, public :: GRC_AUT_DETECTOR_ERROR         = 8713  !! Error in target acquisition.
    integer, parameter, public :: GRC_AUT_NOT_ENABLED            = 8714  !! Target acquisition not enabled.
    integer, parameter, public :: GRC_AUT_CALACC                 = 8715  !! ATR calibration failed.
    integer, parameter, public :: GRC_AUT_ACCURACY               = 8716  !! Target position not exactly reached.
    integer, parameter, public :: GRC_AUT_DIST_STARTED           = 8717  !! Distance measurement has been started.
    integer, parameter, public :: GRC_AUT_SUPPLY_TOO_HIGH        = 8718  !! External supply voltage is too high.
    integer, parameter, public :: GRC_AUT_SUPPLY_TOO_LOW         = 8719  !! Internal or external supply voltage is too low.
    integer, parameter, public :: GRC_AUT_NO_WORKING_AREA        = 8720  !! Working area not set.
    integer, parameter, public :: GRC_AUT_ARRAY_FULL             = 8721  !! Power search data array is filled.
    integer, parameter, public :: GRC_AUT_NO_DATA                = 8722  !! No data available.

    integer, parameter, public :: GRC_KDM_NOT_AVAILABLE          = 12544 !! KDM device is not available.

    integer, parameter, public :: GRC_FTR_FILEACCESS             = 13056 !! File access error.
    integer, parameter, public :: GRC_FTR_WRONGFILEBLOCKNUMBER   = 13057 !! Block number was not the expected one.
    integer, parameter, public :: GRC_FTR_NOTENOUGHSPACE         = 13058 !! Not enough space on device to proceed uploading.
    integer, parameter, public :: GRC_FTR_INVALIDINPUT           = 13059 !! Rename of file failed.
    integer, parameter, public :: GRC_FTR_MISSINGSETUP           = 13060 !! Invalid parameter as input.

    ! Public procedures.
    public :: dm_geocom_error_message
    public :: dm_geocom_is_error
    public :: dm_geocom_is_ok
contains
    ! **************************************************************************
    ! PUBLIC PROCEDURES.
    ! **************************************************************************
    pure function dm_geocom_error_message(grc) result(message)
        !! Returns message associated with given GeoCOM (error) code.
        use :: dm_util, only: dm_itoa

        integer, intent(in)           :: grc     !! GeoCOM return code.
        character(len=:), allocatable :: message !! GeoCOM return code message.

        select case (grc)
            ! TPS
            case (GRC_OK);                         message = 'function successfully completed'
            case (GRC_UNDEFINED);                  message = 'unknown error, result unspecified'
            case (GRC_IVPARAM);                    message = 'invalid parameter detected, result unspecified'
            case (GRC_IVRESULT);                   message = 'invalid result'
            case (GRC_FATAL);                      message = 'fatal error'
            case (GRC_NOT_IMPL);                   message = 'not implemented'
            case (GRC_TIME_OUT);                   message = 'function execution timed out, result unspecified'
            case (GRC_SET_INCOMPL);                message = 'parameter setup for subsystem is incomplete'
            case (GRC_ABORT);                      message = 'function execution has been aborted'
            case (GRC_NOMEMORY);                   message = 'fatal error (not enough memory)'
            case (GRC_NOTINIT);                    message = 'fatal error (subsystem not initialised)'
            case (GRC_SHUT_DOWN);                  message = 'subsystem is down'
            case (GRC_SYSBUSY);                    message = 'system busy/already in use of another process'
            case (GRC_HWFAILURE);                  message = 'fatal error (hardware failure)'
            case (GRC_ABORT_APPL);                 message = 'execution of application has been aborted'
            case (GRC_LOW_POWER);                  message = 'operation aborted (insufficient power supply level)'
            case (GRC_IVVERSION);                  message = 'invalid version of file'
            case (GRC_BAT_EMPTY);                  message = 'battery empty, about 1 min remaining'
            case (GRC_NO_EVENT);                   message = 'no event pending'
            case (GRC_OUT_OF_TEMP);                message = 'out of temperature range'
            case (GRC_INSTRUMENT_TILT);            message = 'instrument tilting out of range'
            case (GRC_COM_SETTING);                message = 'communication error'
            case (GRC_NO_ACTION);                  message = 'GRC_TYPE input (do no action)'
            case (GRC_SLEEP_MODE);                 message = 'instrument went into sleep mode'
            case (GRC_NOTOK);                      message = 'function not successfully completed'
            case (GRC_NA);                         message = 'not available'
            case (GRC_OVERFLOW);                   message = 'overflow error'
            case (GRC_STOPPED);                    message = 'system or subsystem has been stopped'
            ! ANG
            case (GRC_ANG);                        message = 'ANG error'
            case (GRC_ANG_ERROR);                  message = 'angles and inclinations not valid'
            case (GRC_ANG_INCL_ERROR);             message = 'inclinations not valid'
            case (GRC_ANG_BAD_ACC);                message = 'value accuracies not reached'
            case (GRC_ANG_BAD_ANGLE_ACC);          message = 'angle accuracies not reached'
            case (GRC_ANG_BAD_INCLIN_ACC);         message = 'inclination accuracies not reached'
            case (GRC_ANG_WRITE_PROTECTED);        message = 'no write access allowed'
            case (GRC_ANG_OUT_OF_RANGE);           message = 'value out of range'
            case (GRC_ANG_IR_OCCURED);             message = 'function aborted due to interrupt'
            case (GRC_ANG_HZ_MOVED);               message = 'Hz moved during incline measurement'
            case (GRC_ANG_OS_ERROR);               message = 'troubles with operation system'
            case (GRC_ANG_DATA_ERROR);             message = 'overflow at parameter values'
            case (GRC_ANG_PEAK_CNT_UFL);           message = 'not enough peaks'
            case (GRC_ANG_TIME_OUT);               message = 'reading timeout'
            case (GRC_ANG_TOO_MANY_EXPOS);         message = 'too many exposures wanted'
            case (GRC_ANG_PIX_CTRL_ERR);           message = 'picture height out of range'
            case (GRC_ANG_MAX_POS_SKIP);           message = 'positive exposure dynamic overflow'
            case (GRC_ANG_MAX_NEG_SKIP);           message = 'negative exposure dynamic overflow'
            case (GRC_ANG_EXP_LIMIT);              message = 'exposure time overflow'
            case (GRC_ANG_UNDER_EXPOSURE);         message = 'picture under-exposured'
            case (GRC_ANG_OVER_EXPOSURE);          message = 'picture over-exposured'
            case (GRC_ANG_TMANY_PEAKS);            message = 'too many peaks detected'
            case (GRC_ANG_TLESS_PEAKS);            message = 'not enough peaks detected'
            case (GRC_ANG_PEAK_TOO_SLIM);          message = 'peak too slim'
            case (GRC_ANG_PEAK_TOO_WIDE);          message = 'peak too wide'
            case (GRC_ANG_BAD_PEAKDIFF);           message = 'bad peak difference'
            case (GRC_ANG_UNDER_EXP_PICT);         message = 'peak amplitude too low'
            case (GRC_ANG_PEAKS_INHOMOGEN);        message = 'inhomogeneous peak amplitudes'
            case (GRC_ANG_NO_DECOD_POSS);          message = 'no peak decoding possible'
            case (GRC_ANG_UNSTABLE_DECOD);         message = 'peak decoding not stable'
            case (GRC_ANG_TLESS_FPEAKS);           message = 'not enough valid fine-peaks'
            case (GRC_ANG_INCL_OLD_PLANE);         message = 'inclination plane out of time range'
            case (GRC_ANG_INCL_NO_PLANE);          message = 'inclination plane not available'
            case (GRC_ANG_FAST_ANG_ERR);           message = 'errors in 5 kHz and or 2.5 kHz angle'
            case (GRC_ANG_FAST_ANG_ERR_5);         message = 'errors in 5 kHz angle'
            case (GRC_ANG_FAST_ANG_ERR_25);        message = 'errors in 2.5 kHz angle'
            case (GRC_ANG_TRANS_ERR);              message = 'LVDS transfer error detected'
            case (GRC_ANG_TRANS_ERR_5);            message = 'LVDS transfer error detected in 5 kHz mode'
            case (GRC_ANG_TRANS_ERR_25);           message = 'LVDS transfer error detected in 2.5 kHz mode'
            ! ATA
            case (GRC_ATA_NOT_READY);              message = 'ATR system is not ready'
            case (GRC_ATA_NO_RESULT);              message = 'result is not available yet'
            case (GRC_ATA_SEVERAL_TARGETS);        message = 'several targets detected'
            case (GRC_ATA_BIG_SPOT);               message = 'spot is too big for analyse'
            case (GRC_ATA_BACKGROUND);             message = 'background is too bright'
            case (GRC_ATA_NO_TARGETS);             message = 'no targets detected'
            case (GRC_ATA_NOT_ACCURAT);            message = 'accuracy worse than asked for'
            case (GRC_ATA_SPOT_ON_EDGE);           message = 'spot is on the edge of the sensing area'
            case (GRC_ATA_BLOOMING);               message = 'blooming or spot on edge detected'
            case (GRC_ATA_NOT_BUSY);               message = 'ATR is not in a continuous mode'
            case (GRC_ATA_STRANGE_LIGHT);          message = 'not the spot of the own target illuminator'
            case (GRC_ATA_V24_FAIL);               message = 'communication error to sensor (ATR)'
            case (GRC_ATA_DECODE_ERROR);           message = 'received Arguments cannot be decoded'
            case (GRC_ATA_HZ_FAIL);                message = 'no spot detected in Hz direction'
            case (GRC_ATA_V_FAIL);                 message = 'no spot detected in V direction'
            case (GRC_ATA_HZ_STRANGE_L);           message = 'strange light in Hz direction'
            case (GRC_ATA_V_STRANGE_L);            message = 'strange light in V direction'
            case (GRC_ATA_SLDR_TRANSFER_PENDING);  message = 'on multiple ATA_SLDR_OpenTransfer'
            case (GRC_ATA_SLDR_TRANSFER_ILLEGAL);  message = 'no ATA_SLDR_OpenTransfer happened'
            case (GRC_ATA_SLDR_DATA_ERROR);        message = 'unexpected data format received'
            case (GRC_ATA_SLDR_CHK_SUM_ERROR);     message = 'checksum error in transmitted data'
            case (GRC_ATA_SLDR_ADDRESS_ERROR);     message = 'address out of valid range'
            case (GRC_ATA_SLDR_INV_LOADFILE);      message = 'firmware file has invalid format'
            case (GRC_ATA_SLDR_UNSUPPORTED);       message = 'currently loaded firmware does not support upload'
            case (GRC_ATA_PS_NOT_READY);           message = 'PowerSearch system is not ready'
            case (GRC_ATA_ATR_SYSTEM_ERR);         message = 'ATR system error'
            ! EDM
            case (GRC_EDM);                        message = 'EDM error'
            case (GRC_EDM_SYSTEM_ERR);             message = 'fatal EDM sensor error'
            case (GRC_EDM_INVALID_COMMAND);        message = 'invalid command or unknown command'
            case (GRC_EDM_BOOM_ERR);               message = 'boomerang error'
            case (GRC_EDM_SIGN_LOW_ERR);           message = 'received signal too low, prism too far away, or natural barrier, bad environment'
            case (GRC_EDM_DIL_ERR);                message = 'obsolete'
            case (GRC_EDM_SIGN_HIGH_ERR);          message = 'received signal to strong, prism to near, strange light effect'
            case (GRC_EDM_TIMEOUT);                message = 'timeout, measuring time exceeded (signal too weak, beam interrupted)'
            case (GRC_EDM_FLUKT_ERR);              message = 'too much turbulences or distractions'
            case (GRC_EDM_FMOT_ERR);               message = 'filter motor defective'
            case (GRC_EDM_DEV_NOT_INSTALLED);      message = 'device like EGL, DL is not installed'
            case (GRC_EDM_NOT_FOUND);              message = 'search result invalid'
            case (GRC_EDM_ERROR_RECEIVED);         message = 'communication ok, but error reported from the EDM sensor'
            case (GRC_EDM_MISSING_SRVPWD);         message = 'no service password is set'
            case (GRC_EDM_INVALID_ANSWER);         message = 'communication ok, but unexpected answer received'
            case (GRC_EDM_SEND_ERR);               message = 'data send error, sending buffer is full'
            case (GRC_EDM_RECEIVE_ERR);            message = 'data receive error, like parity buffer overflow'
            case (GRC_EDM_INTERNAL_ERR);           message = 'internal EDM subsystem error'
            case (GRC_EDM_BUSY);                   message = 'sensor is working already, abort current measuring first'
            case (GRC_EDM_NO_MEASACTIVITY);        message = 'no measurement activity started'
            case (GRC_EDM_CHKSUM_ERR);             message = 'calculated checksum, resp. received data wrong'
            case (GRC_EDM_INIT_OR_STOP_ERR);       message = 'during start up or shut down phase an error occured'
            case (GRC_EDM_SRL_NOT_AVAILABLE);      message = 'red laser not available on this sensor hardware'
            case (GRC_EDM_MEAS_ABORTED);           message = 'measurement will be aborted for laser security'
            case (GRC_EDM_SLDR_TRANSFER_PENDING);  message = 'multiple open transfer calls'
            case (GRC_EDM_SLDR_TRANSFER_ILLEGAL);  message = 'no open transfer happened'
            case (GRC_EDM_SLDR_DATA_ERROR);        message = 'unexpected data format received'
            case (GRC_EDM_SLDR_CHK_SUM_ERROR);     message = 'checksum error in transmitted data'
            case (GRC_EDM_SLDR_ADDR_ERROR);        message = 'address out of valid range'
            case (GRC_EDM_SLDR_INV_LOADFILE);      message = 'firmware file has invalid format'
            case (GRC_EDM_SLDR_UNSUPPORTED);       message = 'currently loaded firmware does not support upload'
            case (GRC_EDM_UNKNOW_ERR);             message = 'undocumented error from the EDM sensor'
            case (GRC_EDM_DISTRANGE_ERR);          message = 'out of distance range (too short or too long)'
            case (GRC_EDM_SIGNTONOISE_ERR);        message = 'signal to noise ratio too small'
            case (GRC_EDM_NOISEHIGH_ERR);          message = 'noise too high'
            case (GRC_EDM_PWD_NOTSET);             message = 'password is not set'
            case (GRC_EDM_ACTION_NO_MORE_VALID);   message = 'elapsed time between prepare and start of fast measurement for ATR too long'
            case (GRC_EDM_MULTRG_ERR);             message = 'possibly more than one target (also a sensor error)'
            case (GRC_EDM_MISSING_EE_CONSTS);      message = 'EEPROM constants are missing'
            case (GRC_EDM_NOPRECISE);              message = 'no precise measurement possible'
            case (GRC_EDM_MEAS_DIST_NOT_ALLOWED);  message = 'measured distance is too long (not allowed)'
            ! GMF
            case (GRC_GMF);                        message = 'GMF error'
            case (GRC_GMF_WRONG_AREA_DEF);         message = 'wrong area definition'
            case (GRC_GMF_IDENTICAL_PTS);          message = 'identical points'
            case (GRC_GMF_PTS_IN_LINE);            message = 'points on one line'
            case (GRC_GMF_OUT_OF_RANGE);           message = 'out of range'
            case (GRC_GMF_PLAUSIBILITY_ERR);       message = 'plausibility error'
            case (GRC_GMF_TOO_FEW_OBSERVATIONS);   message = 'too few observations to calculate the average'
            case (GRC_GMF_NO_SOLUTION);            message = 'no solution'
            case (GRC_GMF_ONE_SOLUTION);           message = 'only one solution'
            case (GRC_GMF_TWO_SOLUTIONS);          message = 'second solution'
            case (GRC_GMF_ANGLE_SMALLER_15GON);    message = 'intersection angle < 15 gon'
            case (GRC_GMF_INVALID_TRIANGLE_TYPE);  message = 'invalid triangle'
            case (GRC_GMF_INVALID_ANGLE_SYSTEM);   message = 'invalid angle unit'
            case (GRC_GMF_INVALID_DIST_SYSTEM);    message = 'invalid distance unit'
            case (GRC_GMF_INVALID_V_SYSTEM);       message = 'invalid vertical angle'
            case (GRC_GMF_INVALID_TEMP_SYSTEM);    message = 'invalid temperature system'
            case (GRC_GMF_INVALID_PRES_SYSTEM);    message = 'invalid pressure unit'
            case (GRC_GMF_RADIUS_NOT_POSSIBLE);    message = 'invalid radius'
            case (GRC_GMF_NO_PROVISIONAL_VALUES);  message = 'insufficient data (GM2)'
            case (GRC_GMF_SINGULAR_MATRIX);        message = 'bad data (GM2)'
            case (GRC_GMF_TOO_MANY_ITERATIONS);    message = 'bad data distr (GM2)'
            case (GRC_GMF_IDENTICAL_TIE_POINTS);   message = 'same tie points (GM2)'
            case (GRC_GMF_SETUP_EQUALS_TIE_POINT); message = 'station and tie point same (GM2)'
            ! TMC
            case (GRC_TMC);                        message = 'TMC error'
            case (GRC_TMC_NO_FULL_CORRECTION);     message = 'measurement without full correction'
            case (GRC_TMC_ACCURACY_GUARANTEE);     message = 'accuracy can not be guaranteed'
            case (GRC_TMC_ANGLE_OK);               message = 'only angle measurement valid'
            case (GRC_TMC_ANGLE_NOT_FULL_CORR);    message = 'only angle measurement valid but without full correction'
            case (GRC_TMC_ANGLE_NO_ACC_GUARANTY);  message = 'only angle measurement valid but accuracy can not be guaranteed'
            case (GRC_TMC_ANGLE_ERROR);            message = 'no angle measurement'
            case (GRC_TMC_DIST_PPM);               message = 'wrong setting of ppm or mm on EDM'
            case (GRC_TMC_DIST_ERROR);             message = 'distance measurement not done (no aim)'
            case (GRC_TMC_BUSY);                   message = 'system is busy (no measurement done)'
            case (GRC_TMC_SIGNAL_ERROR);           message = 'no signal on EDM (only in signal mode)'
            ! MOT
            case (GRC_MOT_UNREADY);                message = 'motorisation is not ready'
            case (GRC_MOT_BUSY);                   message = 'motorisation is handling another task'
            case (GRC_MOT_NOT_OCONST);             message = 'motorisation is not in velocity mode'
            case (GRC_MOT_NOT_CONFIG);             message = 'motorisation is in the wrong mode or busy'
            case (GRC_MOT_NOT_POSIT);              message = 'motorisation is not in posit mode'
            case (GRC_MOT_NOT_SERVICE);            message = 'motorisation is not in service mode'
            case (GRC_MOT_NOT_BUSY);               message = 'motorisation is handling no task'
            case (GRC_MOT_NOT_LOCK);               message = 'motorisation is not in tracking mode'
            case (GRC_MOT_NOT_SPIRAL);             message = 'motorisation is not in spiral mode'
            case (GRC_MOT_V_ENCODER);              message = 'vertical encoder/motor error'
            case (GRC_MOT_HZ_ENCODER);             message = 'horizontal encoder/motor error'
            case (GRC_MOT_HZ_V_ENCODER);           message = 'horizontal and vertical encoder/motor error'
            ! BMM
            case (GRC_BMM);                        message = 'BMM error'
            case (GRC_BMM_XFER_PENDING);           message = 'loading process already opened'
            case (GRC_BMM_NO_XFER_OPEN);           message = 'transfer not opened'
            case (GRC_BMM_UNKNOWN_CHARSET);        message = 'unknown character set'
            case (GRC_BMM_NOT_INSTALLED);          message = 'display module not present'
            case (GRC_BMM_ALREADY_EXIST);          message = 'character set already exists'
            case (GRC_BMM_CANT_DELETE);            message = 'character set cannot be deleted'
            case (GRC_BMM_MEM_ERROR);              message = 'memory cannot be allocated'
            case (GRC_BMM_CHARSET_USED);           message = 'character set still used'
            case (GRC_BMM_CHARSET_SAVED);          message = 'charset cannot be deleted or is protected'
            case (GRC_BMM_INVALID_ADR);            message = 'attempt to copy a character block outside the allocated memory'
            case (GRC_BMM_CANCELANDADR_ERROR);     message = 'error during release of allocated memory'
            case (GRC_BMM_INVALID_SIZE);           message = 'number of bytes specified in header does not match the bytes read'
            case (GRC_BMM_CANCELANDINVSIZE_ERROR); message = 'allocated memory could not be released'
            case (GRC_BMM_ALL_GROUP_OCC);          message = 'max. number of character sets already loaded'
            case (GRC_BMM_CANT_DEL_LAYERS);        message = 'layer cannot be deleted'
            case (GRC_BMM_UNKNOWN_LAYER);          message = 'required layer does not exist'
            case (GRC_BMM_INVALID_LAYERLEN);       message = 'layer length exceeds maximum'
            ! COM
            case (GRC_COM_ERO);                    message = 'initiate Extended Runtime Operation (ERO)'
            case (GRC_COM_CANT_ENCODE);            message = 'cannot encode arguments in client'
            case (GRC_COM_CANT_DECODE);            message = 'cannot decode results in client'
            case (GRC_COM_CANT_SEND);              message = 'hardware error while sending'
            case (GRC_COM_CANT_RECV);              message = 'hardware error while receiving'
            case (GRC_COM_TIMEDOUT);               message = 'request timed out'
            case (GRC_COM_WRONG_FORMAT);           message = 'packet format error'
            case (GRC_COM_VER_MISMATCH);           message = 'version mismatch between client and server'
            case (GRC_COM_CANT_DECODE_REQ);        message = 'cannot decode arguments in server'
            case (GRC_COM_PROC_UNAVAIL);           message = 'unknown RPC, procedure ID invalid'
            case (GRC_COM_CANT_ENCODE_REP);        message = 'cannot encode results in server'
            case (GRC_COM_SYSTEM_ERR);             message = 'unspecified generic system error'
            case (GRC_COM_FAILED);                 message = 'unspecified error'
            case (GRC_COM_NO_BINARY);              message = 'binary protocol not available'
            case (GRC_COM_INTR);                   message = 'call interrupted'
            case (GRC_COM_REQUIRES_8DBITS);        message = 'protocol needs 8 bit encoded characters'
            case (GRC_COM_TR_ID_MISMATCH);         message = 'transaction id mismatch error'
            case (GRC_COM_NOT_GEOCOM);             message = 'protocol not recognisable'
            case (GRC_COM_UNKNOWN_PORT);           message = 'invalid port address'
            case (GRC_COM_ERO_END);                message = 'ERO is terminating'
            case (GRC_COM_OVERRUN);                message = 'internal error (data buffer overflow)'
            case (GRC_COM_SRVR_RX_CHECKSUM_ERRR);  message = 'invalid checksum on server side received'
            case (GRC_COM_CLNT_RX_CHECKSUM_ERRR);  message = 'invalid checksum on client side received'
            case (GRC_COM_PORT_NOT_AVAILABLE);     message = 'port not available'
            case (GRC_COM_PORT_NOT_OPEN);          message = 'port not opened'
            case (GRC_COM_NO_PARTNER);             message = 'unable to find TPS'
            case (GRC_COM_ERO_NOT_STARTED);        message = 'Extended Runtime Operation (ERO) could not be started'
            case (GRC_COM_CONS_REQ);               message = 'att to send cons reqs'
            case (GRC_COM_SRVR_IS_SLEEPING);       message = 'TPS has gone to sleep (wait and try again)'
            case (GRC_COM_SRVR_IS_OFF);            message = 'TPS has shut down (wait and try again)'
            case (GRC_COM_NO_CHECKSUM);            message = 'no checksum in ASCII protocol available'
            ! AUT
            case (GRC_AUT_TIMEOUT);                message = 'position not reached'
            case (GRC_AUT_DETENT_ERROR);           message = 'positioning not possible due to mounted EDM'
            case (GRC_AUT_ANGLE_ERROR);            message = 'angle measurement error'
            case (GRC_AUT_MOTOR_ERROR);            message = 'motorisation error'
            case (GRC_AUT_INCACC);                 message = 'position not exactly reached'
            case (GRC_AUT_DEV_ERROR);              message = 'deviation measurement error'
            case (GRC_AUT_NO_TARGET);              message = 'no target detected'
            case (GRC_AUT_MULTIPLE_TARGETS);       message = 'multiple targets detected'
            case (GRC_AUT_BAD_ENVIRONMENT);        message = 'bad environment conditions'
            case (GRC_AUT_DETECTOR_ERROR);         message = 'error in target acquisition'
            case (GRC_AUT_NOT_ENABLED);            message = 'target acquisition not enabled'
            case (GRC_AUT_CALACC);                 message = 'ATR calibration failed'
            case (GRC_AUT_ACCURACY);               message = 'target position not exactly reached'
            case (GRC_AUT_DIST_STARTED);           message = 'distance measurement has been started'
            case (GRC_AUT_SUPPLY_TOO_HIGH);        message = 'external supply voltage is too high'
            case (GRC_AUT_SUPPLY_TOO_LOW);         message = 'internal or external supply voltage is too low'
            case (GRC_AUT_NO_WORKING_AREA);        message = 'working area not set'
            case (GRC_AUT_ARRAY_FULL);             message = 'power search data array is filled'
            case (GRC_AUT_NO_DATA);                message = 'no data available'
            ! KDM
            case (GRC_KDM_NOT_AVAILABLE);          message = 'KDM device is not available'
            ! FTR
            case (GRC_FTR_FILEACCESS);             message = 'file access error'
            case (GRC_FTR_WRONGFILEBLOCKNUMBER);   message = 'block number was not the expected one'
            case (GRC_FTR_NOTENOUGHSPACE);         message = 'not enough space on device to proceed uploading'
            case (GRC_FTR_INVALIDINPUT);           message = 'rename of file failed'
            case (GRC_FTR_MISSINGSETUP);           message = 'invalid parameter as input'
            ! unknown
            case default;                          message = 'unknown GeoCOM code (' // dm_itoa(grc) // ')'
        end select
    end function dm_geocom_error_message

    pure elemental logical function dm_geocom_is_error(grc) result(is_error)
        !! Returns `.true.` if given GeoCOM code is an error.
        integer, intent(in) :: grc !! GeoCOM code.

        is_error = (grc /= GRC_OK)
    end function dm_geocom_is_error

    pure elemental logical function dm_geocom_is_ok(grc) result(is_ok)
        !! Returns `.true.` if given GeoCOM code is not an error.
        integer, intent(in) :: grc !! GeoCOM code.

        is_ok = (grc == GRC_OK)
    end function dm_geocom_is_ok
end module dm_geocom_error
