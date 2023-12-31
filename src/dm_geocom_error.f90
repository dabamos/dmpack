! Author:  Philipp Engel
! Licence: ISC
module dm_geocom_error
    !! GeoCOM return codes and return code messages. GeoCOM return codes have
    !! the prefix `GRC_`.
    !!
    !! The GeoCOM return codes are compatible to at least the following sensors:
    !!
    !! * Leica TPS1100
    !! * Leica TPS1200+
    !! * Leica TM30/TS30
    !! * Leica Viva TS16 (FlexLine)
    !!
    !! Some obvious spelling errors in the return code descriptions have been
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
    integer, parameter, public :: GRC_NOT_IMPL                   = 5     !! Not implemented yet.
    integer, parameter, public :: GRC_TIME_OUT                   = 6     !! Function execution timed out. Result unspecified.
    integer, parameter, public :: GRC_SET_INCOMPL                = 7     !! Parameter setup for subsystem is incomplete.
    integer, parameter, public :: GRC_ABORT                      = 8     !! Function execution has been aborted.
    integer, parameter, public :: GRC_NOMEMORY                   = 9     !! Fatal error (not enough memory).
    integer, parameter, public :: GRC_NOTINIT                    = 10    !! Fatal error (subsystem not initialized).
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
    integer, parameter, public :: GRC_SLEEP_MODE                 = 25    !! Instrument run into the sleep mode.
    integer, parameter, public :: GRC_NOTOK                      = 26    !! Function not successfully completed.
    integer, parameter, public :: GRC_NA                         = 27    !! Not available.
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
    integer, parameter, public :: GRC_ANG_PEAK_CNT_UFL           = 272   !! Too less peaks.
    integer, parameter, public :: GRC_ANG_TIME_OUT               = 273   !! Reading timeout.
    integer, parameter, public :: GRC_ANG_TOO_MANY_EXPOS         = 274   !! Too many exposures wanted.
    integer, parameter, public :: GRC_ANG_PIX_CTRL_ERR           = 275   !! Picture height out of range.
    integer, parameter, public :: GRC_ANG_MAX_POS_SKIP           = 276   !! Positive exposure dynamic overflow.
    integer, parameter, public :: GRC_ANG_MAX_NEG_SKIP           = 277   !! Negative exposure dynamic overflow.
    integer, parameter, public :: GRC_ANG_EXP_LIMIT              = 278   !! Exposure time overflow.
    integer, parameter, public :: GRC_ANG_UNDER_EXPOSURE         = 279   !! Picture under-exposured.
    integer, parameter, public :: GRC_ANG_OVER_EXPOSURE          = 280   !! Picture over-exposured.
    integer, parameter, public :: GRC_ANG_TMANY_PEAKS            = 300   !! Too many peaks detected.
    integer, parameter, public :: GRC_ANG_TLESS_PEAKS            = 301   !! Too less peaks detected.
    integer, parameter, public :: GRC_ANG_PEAK_TOO_SLIM          = 302   !! Peak too slim.
    integer, parameter, public :: GRC_ANG_PEAK_TOO_WIDE          = 303   !! Peak to wide.
    integer, parameter, public :: GRC_ANG_BAD_PEAKDIFF           = 304   !! Bad peak difference.
    integer, parameter, public :: GRC_ANG_UNDER_EXP_PICT         = 305   !! Too less peak amplitude.
    integer, parameter, public :: GRC_ANG_PEAKS_INHOMOGEN        = 306   !! Inhomogeneous peak amplitudes.
    integer, parameter, public :: GRC_ANG_NO_DECOD_POSS          = 307   !! No peak decoding possible.
    integer, parameter, public :: GRC_ANG_UNSTABLE_DECOD         = 308   !! Peak decoding not stable.
    integer, parameter, public :: GRC_ANG_TLESS_FPEAKS           = 309   !! Too less valid fine-peaks.
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
    integer, parameter, public :: GRC_ATA_PS_NOT_READY           = 538   !! PS-System is not ready.
    integer, parameter, public :: GRC_ATA_ATR_SYSTEM_ERR         = 539   !! ATR system error.

    integer, parameter, public :: GRC_EDM                        = 768   !! EDM error.
    integer, parameter, public :: GRC_EDM_SYSTEM_ERR             = 769   !! Fatal EDM sensor error.
    integer, parameter, public :: GRC_EDM_INVALID_COMMAND        = 770   !! Invalid command or unknown command.
    integer, parameter, public :: GRC_EDM_BOOM_ERR               = 771   !! Boomerang error.
    integer, parameter, public :: GRC_EDM_SIGN_LOW_ERR           = 772   !! Received signal to low, prism to far away, or natural barrier, bad environment, etc.
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
    integer, parameter, public :: GRC_EDM_NOISEHIGH_ERR          = 820   !! Noise to high.
    integer, parameter, public :: GRC_EDM_PWD_NOTSET             = 821   !! Password is not set.
    integer, parameter, public :: GRC_EDM_ACTION_NO_MORE_VALID   = 822   !! Elapsed time between prepare and start fast measurement for ATR too long.
    integer, parameter, public :: GRC_EDM_MULTRG_ERR             = 823   !! Possibly more than one target (also a sensor error).
    integer, parameter, public :: GRC_EDM_MISSING_EE_CONSTS      = 824   !! EEPROM consts are missing.
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

    integer, parameter, public :: GRC_MOT_UNREADY                = 1792  !! Motorization is not ready.
    integer, parameter, public :: GRC_MOT_BUSY                   = 1793  !! Motorization is handling another task.
    integer, parameter, public :: GRC_MOT_NOT_OCONST             = 1794  !! Motorization is not in velocity mode.
    integer, parameter, public :: GRC_MOT_NOT_CONFIG             = 1795  !! Motorization is in the wrong mode or busy.
    integer, parameter, public :: GRC_MOT_NOT_POSIT              = 1796  !! Motorization is not in posit mode.
    integer, parameter, public :: GRC_MOT_NOT_SERVICE            = 1797  !! Motorization is not in service mode.
    integer, parameter, public :: GRC_MOT_NOT_BUSY               = 1798  !! Motorization is handling no task.
    integer, parameter, public :: GRC_MOT_NOT_LOCK               = 1799  !! Motorization is not in tracking mode.
    integer, parameter, public :: GRC_MOT_NOT_SPIRAL             = 1800  !! Motorization is not in spiral mode.
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
    integer, parameter, public :: GRC_COM_NOT_GEOCOM             = 3094  !! Protocol not recognizable.
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
    integer, parameter, public :: GRC_AUT_DIST_STARTED           = 8717  !! Dist. measurement has been started.
    integer, parameter, public :: GRC_AUT_SUPPLY_TOO_HIGH        = 8718  !! External supply voltage is too high.
    integer, parameter, public :: GRC_AUT_SUPPLY_TOO_LOW         = 8719  !! Int. or ext. supply voltage is too low.
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
contains
    ! **************************************************************************
    ! PUBLIC PROCEDURES.
    ! **************************************************************************
    pure function dm_geocom_error_message(code) result(str)
        !! Returns message associated with given GeoCOM (error) code.
        integer, intent(in)           :: code !! GeoCOM code.
        character(len=:), allocatable :: str  !! GeoCOM code message.

        select case (code)
            ! TPS
            case (GRC_OK)
                str = 'function successfully completed'
            case (GRC_UNDEFINED)
                str = 'unknown error, result unspecified'
            case (GRC_IVPARAM)
                str = 'invalid parameter detected, result unspecified'
            case (GRC_IVRESULT)
                str = 'invalid result'
            case (GRC_FATAL)
                str = 'fatal error'
            case (GRC_NOT_IMPL)
                str = 'not implemented yet'
            case (GRC_TIME_OUT)
                str = 'function execution timed out, result unspecified'
            case (GRC_SET_INCOMPL)
                str = 'parameter setup for subsystem is incomplete'
            case (GRC_ABORT)
                str = 'function execution has been aborted'
            case (GRC_NOMEMORY)
                str = 'fatal error (not enough memory)'
            case (GRC_NOTINIT)
                str = 'fatal error (subsystem not initialized)'
            case (GRC_SHUT_DOWN)
                str = 'subsystem is down'
            case (GRC_SYSBUSY)
                str = 'system busy/already in use of another process'
            case (GRC_HWFAILURE)
                str = 'fatal error (hardware failure)'
            case (GRC_ABORT_APPL)
                str = 'execution of application has been aborted'
            case (GRC_LOW_POWER)
                str = 'operation aborted (insufficient power supply level)'
            case (GRC_IVVERSION)
                str = 'invalid version of file'
            case (GRC_BAT_EMPTY)
                str = 'battery empty, about 1 min remaining'
            case (GRC_NO_EVENT)
                str = 'no event pending'
            case (GRC_OUT_OF_TEMP)
                str = 'out of temperature range'
            case (GRC_INSTRUMENT_TILT)
                str = 'instrument tilting out of range'
            case (GRC_COM_SETTING)
                str = 'communication error'
            case (GRC_NO_ACTION)
                str = 'GRC_TYPE input (do no action)'
            case (GRC_SLEEP_MODE)
                str = 'instrument run into the sleep mode'
            case (GRC_NOTOK)
                str = 'function not successfully completed'
            case (GRC_NA)
                str = 'not available'
            case (GRC_OVERFLOW)
                str = 'overflow error'
            case (GRC_STOPPED)
                str = 'system or subsystem has been stopped'

            ! ANG
            case (GRC_ANG)
                str = 'ANG error'
            case (GRC_ANG_ERROR)
                str = 'angles and inclinations not valid'
            case (GRC_ANG_INCL_ERROR)
                str = 'inclinations not valid'
            case (GRC_ANG_BAD_ACC)
                str = 'value accuracies not reached'
            case (GRC_ANG_BAD_ANGLE_ACC)
                str = 'angle accuracies not reached'
            case (GRC_ANG_BAD_INCLIN_ACC)
                str = 'inclination accuracies not reached'
            case (GRC_ANG_WRITE_PROTECTED)
                str = 'no write access allowed'
            case (GRC_ANG_OUT_OF_RANGE)
                str = 'value out of range'
            case (GRC_ANG_IR_OCCURED)
                str = 'function aborted due to interrupt'
            case (GRC_ANG_HZ_MOVED)
                str = 'Hz moved during incline measurement'
            case (GRC_ANG_OS_ERROR)
                str = 'troubles with operation system'
            case (GRC_ANG_DATA_ERROR)
                str = 'overflow at parameter values'
            case (GRC_ANG_PEAK_CNT_UFL)
                str = 'too less peaks'
            case (GRC_ANG_TIME_OUT)
                str = 'reading timeout'
            case (GRC_ANG_TOO_MANY_EXPOS)
                str = 'too many exposures wanted'
            case (GRC_ANG_PIX_CTRL_ERR)
                str = 'picture height out of range'
            case (GRC_ANG_MAX_POS_SKIP)
                str = 'positive exposure dynamic overflow'
            case (GRC_ANG_MAX_NEG_SKIP)
                str = 'negative exposure dynamic overflow'
            case (GRC_ANG_EXP_LIMIT)
                str = 'exposure time overflow'
            case (GRC_ANG_UNDER_EXPOSURE)
                str = 'picture under-exposured'
            case (GRC_ANG_OVER_EXPOSURE)
                str = 'picture over-exposured'
            case (GRC_ANG_TMANY_PEAKS)
                str = 'too many peaks detected'
            case (GRC_ANG_TLESS_PEAKS)
                str = 'too less peaks detected'
            case (GRC_ANG_PEAK_TOO_SLIM)
                str = 'peak too slim'
            case (GRC_ANG_PEAK_TOO_WIDE)
                str = 'peak to wide'
            case (GRC_ANG_BAD_PEAKDIFF)
                str = 'bad peak difference'
            case (GRC_ANG_UNDER_EXP_PICT)
                str = 'too less peak amplitude'
            case (GRC_ANG_PEAKS_INHOMOGEN)
                str = 'inhomogeneous peak amplitudes'
            case (GRC_ANG_NO_DECOD_POSS)
                str = 'no peak decoding possible'
            case (GRC_ANG_UNSTABLE_DECOD)
                str = 'peak decoding not stable'
            case (GRC_ANG_TLESS_FPEAKS)
                str = 'too less valid fine-peaks'
            case (GRC_ANG_INCL_OLD_PLANE)
                str = 'inclination plane out of time range'
            case (GRC_ANG_INCL_NO_PLANE)
                str = 'inclination no plane available'
            case (GRC_ANG_FAST_ANG_ERR)
                str = 'errors in 5 kHz and or 2.5 kHz angle'
            case (GRC_ANG_FAST_ANG_ERR_5)
                str = 'errors in 5 kHz angle'
            case (GRC_ANG_FAST_ANG_ERR_25)
                str = 'errors in 2.5 kHz angle'
            case (GRC_ANG_TRANS_ERR)
                str = 'LVDS transfer error detected'
            case (GRC_ANG_TRANS_ERR_5)
                str = 'LVDS transfer error detected in 5 kHz mode'
            case (GRC_ANG_TRANS_ERR_25)
                str = 'LVDS transfer error detected in 2.5 kHz mode'

            ! ATA
            case (GRC_ATA_NOT_READY)
                str = 'ATR system is not ready'
            case (GRC_ATA_NO_RESULT)
                str = 'result is not available yet'
            case (GRC_ATA_SEVERAL_TARGETS)
                str = 'several targets detected'
            case (GRC_ATA_BIG_SPOT)
                str = 'spot is too big for analyse'
            case (GRC_ATA_BACKGROUND)
                str = 'background is too bright'
            case (GRC_ATA_NO_TARGETS)
                str = 'no targets detected'
            case (GRC_ATA_NOT_ACCURAT)
                str = 'accuracy worse than asked for'
            case (GRC_ATA_SPOT_ON_EDGE)
                str = 'spot is on the edge of the sensing area'
            case (GRC_ATA_BLOOMING)
                str = 'blooming or spot on edge detected'
            case (GRC_ATA_NOT_BUSY)
                str = 'ATR is not in a continuous mode'
            case (GRC_ATA_STRANGE_LIGHT)
                str = 'not the spot of the own target illuminator'
            case (GRC_ATA_V24_FAIL)
                str = 'communication error to sensor (ATR)'
            case (GRC_ATA_DECODE_ERROR)
                str = 'received Arguments cannot be decoded'
            case (GRC_ATA_HZ_FAIL)
                str = 'no spot detected in Hz direction'
            case (GRC_ATA_V_FAIL)
                str = 'no spot detected in V direction'
            case (GRC_ATA_HZ_STRANGE_L)
                str = 'strange light in Hz direction'
            case (GRC_ATA_V_STRANGE_L)
                str = 'strange light in V direction'
            case (GRC_ATA_SLDR_TRANSFER_PENDING)
                str = 'on multiple ATA_SLDR_OpenTransfer'
            case (GRC_ATA_SLDR_TRANSFER_ILLEGAL)
                str = 'no ATA_SLDR_OpenTransfer happened'
            case (GRC_ATA_SLDR_DATA_ERROR)
                str = 'unexpected data format received'
            case (GRC_ATA_SLDR_CHK_SUM_ERROR)
                str = 'checksum error in transmitted data'
            case (GRC_ATA_SLDR_ADDRESS_ERROR)
                str = 'address out of valid range'
            case (GRC_ATA_SLDR_INV_LOADFILE)
                str = 'firmware file has invalid format'
            case (GRC_ATA_SLDR_UNSUPPORTED)
                str = 'current (loaded) firmware does not support upload'
            case (GRC_ATA_PS_NOT_READY)
                str = 'PS-System is not ready'
            case (GRC_ATA_ATR_SYSTEM_ERR)
                str = 'ATR system error'

            ! EDM
            case (GRC_EDM)
                str = 'EDM error'
            case (GRC_EDM_SYSTEM_ERR)
                str = 'fatal EDM sensor error'
            case (GRC_EDM_INVALID_COMMAND)
                str = 'invalid command or unknown command'
            case (GRC_EDM_BOOM_ERR)
                str = 'boomerang error'
            case (GRC_EDM_SIGN_LOW_ERR)
                str = 'received signal to low, prism to far away, or natural barrier, bad environment'
            case (GRC_EDM_DIL_ERR)
                str = 'obsolete'
            case (GRC_EDM_SIGN_HIGH_ERR)
                str = 'received signal to strong, prism to near, strange light effect'
            case (GRC_EDM_TIMEOUT)
                str = 'timeout, measuring time exceeded (signal too weak, beam interrupted)'
            case (GRC_EDM_FLUKT_ERR)
                str = 'too much turbulences or distractions'
            case (GRC_EDM_FMOT_ERR)
                str = 'filter motor defective'
            case (GRC_EDM_DEV_NOT_INSTALLED)
                str = 'device like EGL, DL is not installed'
            case (GRC_EDM_NOT_FOUND)
                str = 'search result invalid'
            case (GRC_EDM_ERROR_RECEIVED)
                str = 'Communication ok, but an error reported from the EDM sensor'
            case (GRC_EDM_MISSING_SRVPWD)
                str = 'no service password is set'
            case (GRC_EDM_INVALID_ANSWER)
                str = 'communication ok, but an unexpected answer received'
            case (GRC_EDM_SEND_ERR)
                str = 'data send error, sending buffer is full'
            case (GRC_EDM_RECEIVE_ERR)
                str = 'data receive error, like parity buffer overflow'
            case (GRC_EDM_INTERNAL_ERR)
                str = 'internal EDM subsystem error'
            case (GRC_EDM_BUSY)
                str = 'sensor is working already, abort current measuring first'
            case (GRC_EDM_NO_MEASACTIVITY)
                str = 'no measurement activity started'
            case (GRC_EDM_CHKSUM_ERR)
                str = 'calculated checksum, resp. received data wrong'
            case (GRC_EDM_INIT_OR_STOP_ERR)
                str = 'during start up or shut down phase an error occured'
            case (GRC_EDM_SRL_NOT_AVAILABLE)
                str = 'red laser not available on this sensor HW'
            case (GRC_EDM_MEAS_ABORTED)
                str = 'measurement will be aborted (will be used for the laser security)'
            case (GRC_EDM_SLDR_TRANSFER_PENDING)
                str = 'multiple OpenTransfer calls'
            case (GRC_EDM_SLDR_TRANSFER_ILLEGAL)
                str = 'no open transfer happened'
            case (GRC_EDM_SLDR_DATA_ERROR)
                str = 'unexpected data format received'
            case (GRC_EDM_SLDR_CHK_SUM_ERROR)
                str = 'checksum error in transmitted data'
            case (GRC_EDM_SLDR_ADDR_ERROR)
                str = 'address out of valid range'
            case (GRC_EDM_SLDR_INV_LOADFILE)
                str = 'firmware file has invalid format'
            case (GRC_EDM_SLDR_UNSUPPORTED)
                str = 'current (loaded) firmware does not support upload'
            case (GRC_EDM_UNKNOW_ERR)
                str = 'undocumented error from the EDM sensor'
            case (GRC_EDM_DISTRANGE_ERR)
                str = 'out of distance range (too small or large)'
            case (GRC_EDM_SIGNTONOISE_ERR)
                str = 'signal to noise ratio too small'
            case (GRC_EDM_NOISEHIGH_ERR)
                str = 'noise to high'
            case (GRC_EDM_PWD_NOTSET)
                str = 'password is not set'
            case (GRC_EDM_ACTION_NO_MORE_VALID)
                str = 'elapsed time between prepare and start fast measurement for ATR too long'
            case (GRC_EDM_MULTRG_ERR)
                str = 'possibly more than one target (also a sensor error)'
            case (GRC_EDM_MISSING_EE_CONSTS)
                str = 'EEPROM consts are missing'
            case (GRC_EDM_NOPRECISE)
                str = 'no precise measurement possible'
            case (GRC_EDM_MEAS_DIST_NOT_ALLOWED)
                str = 'measured distance is too big (not allowed)'

            ! GMF
            case (GRC_GMF)
                str = 'GMF error'
            case (GRC_GMF_WRONG_AREA_DEF)
                str = 'wrong area definition'
            case (GRC_GMF_IDENTICAL_PTS)
                str = 'identical points'
            case (GRC_GMF_PTS_IN_LINE)
                str = 'points on one line'
            case (GRC_GMF_OUT_OF_RANGE)
                str = 'out of range'
            case (GRC_GMF_PLAUSIBILITY_ERR)
                str = 'plausibility error'
            case (GRC_GMF_TOO_FEW_OBSERVATIONS)
                str = 'too few observations to calculate the average'
            case (GRC_GMF_NO_SOLUTION)
                str = 'no solution'
            case (GRC_GMF_ONE_SOLUTION)
                str = 'only one solution'
            case (GRC_GMF_TWO_SOLUTIONS)
                str = 'second solution'
            case (GRC_GMF_ANGLE_SMALLER_15GON)
                str = 'intersection angle < 15 gon'
            case (GRC_GMF_INVALID_TRIANGLE_TYPE)
                str = 'invalid triangle'
            case (GRC_GMF_INVALID_ANGLE_SYSTEM)
                str = 'invalid angle unit'
            case (GRC_GMF_INVALID_DIST_SYSTEM)
                str = 'invalid distance unit'
            case (GRC_GMF_INVALID_V_SYSTEM)
                str = 'invalid vertical angle'
            case (GRC_GMF_INVALID_TEMP_SYSTEM)
                str = 'invalid temperature system'
            case (GRC_GMF_INVALID_PRES_SYSTEM)
                str = 'invalid pressure unit'
            case (GRC_GMF_RADIUS_NOT_POSSIBLE)
                str = 'invalid radius'
            case (GRC_GMF_NO_PROVISIONAL_VALUES)
                str = 'insufficient data (GM2)'
            case (GRC_GMF_SINGULAR_MATRIX)
                str = 'bad data (GM2)'
            case (GRC_GMF_TOO_MANY_ITERATIONS)
                str = 'bad data distr (GM2)'
            case (GRC_GMF_IDENTICAL_TIE_POINTS)
                str = 'same tie points (GM2)'
            case (GRC_GMF_SETUP_EQUALS_TIE_POINT)
                str = 'station and tie point same (GM2)'

            ! TMC
            case (GRC_TMC)
                str = 'TMC error'
            case (GRC_TMC_NO_FULL_CORRECTION)
                str = 'measurement without full correction'
            case (GRC_TMC_ACCURACY_GUARANTEE)
                str = 'accuracy can not be guaranteed'
            case (GRC_TMC_ANGLE_OK)
                str = 'only angle measurement valid'
            case (GRC_TMC_ANGLE_NOT_FULL_CORR)
                str = 'only angle measurement valid but without full correction'
            case (GRC_TMC_ANGLE_NO_ACC_GUARANTY)
                str = 'only angle measurement valid but accuracy can not be guaranteed'
            case (GRC_TMC_ANGLE_ERROR)
                str = 'no angle measurement'
            case (GRC_TMC_DIST_PPM)
                str = 'wrong setting of ppm or mm on EDM'
            case (GRC_TMC_DIST_ERROR)
                str = 'distance measurement not done (no aim)'
            case (GRC_TMC_BUSY)
                str = 'system is busy (no measurement done)'
            case (GRC_TMC_SIGNAL_ERROR)
                str = 'no signal on EDM (only in signal mode)'

            ! MOT
            case (GRC_MOT_UNREADY)
                str = 'motorization is not ready'
            case (GRC_MOT_BUSY)
                str = 'motorization is handling another task'
            case (GRC_MOT_NOT_OCONST)
                str = 'motorization is not in velocity mode'
            case (GRC_MOT_NOT_CONFIG)
                str = 'motorization is in the wrong mode or busy'
            case (GRC_MOT_NOT_POSIT)
                str = 'motorization is not in posit mode'
            case (GRC_MOT_NOT_SERVICE)
                str = 'motorization is not in service mode'
            case (GRC_MOT_NOT_BUSY)
                str = 'motorization is handling no task'
            case (GRC_MOT_NOT_LOCK)
                str = 'motorization is not in tracking mode'
            case (GRC_MOT_NOT_SPIRAL)
                str = 'motorization is not in spiral mode'
            case (GRC_MOT_V_ENCODER)
                str = 'vertical encoder/motor error'
            case (GRC_MOT_HZ_ENCODER)
                str = 'horizontal encoder/motor error'
            case (GRC_MOT_HZ_V_ENCODER)
                str = 'horizontal and vertical encoder/motor error'

            ! BMM
            case (GRC_BMM)
                str = 'BMM error'
            case (GRC_BMM_XFER_PENDING)
                str = 'loading process already opened'
            case (GRC_BMM_NO_XFER_OPEN)
                str = 'transfer not opened'
            case (GRC_BMM_UNKNOWN_CHARSET)
                str = 'unknown character set'
            case (GRC_BMM_NOT_INSTALLED)
                str = 'display module not present'
            case (GRC_BMM_ALREADY_EXIST)
                str = 'character set already exists'
            case (GRC_BMM_CANT_DELETE)
                str = 'character set cannot be deleted'
            case (GRC_BMM_MEM_ERROR)
                str = 'memory cannot be allocated'
            case (GRC_BMM_CHARSET_USED)
                str = 'character set still used'
            case (GRC_BMM_CHARSET_SAVED)
                str = 'charset cannot be deleted or is protected'
            case (GRC_BMM_INVALID_ADR)
                str = 'attempt to copy a character block outside the allocated memory'
            case (GRC_BMM_CANCELANDADR_ERROR)
                str = 'error during release of allocated memory'
            case (GRC_BMM_INVALID_SIZE)
                str = 'number of bytes specified in header does not match the bytes read'
            case (GRC_BMM_CANCELANDINVSIZE_ERROR)
                str = 'allocated memory could not be released'
            case (GRC_BMM_ALL_GROUP_OCC)
                str = 'max. number of character sets already loaded'
            case (GRC_BMM_CANT_DEL_LAYERS)
                str = 'layer cannot be deleted'
            case (GRC_BMM_UNKNOWN_LAYER)
                str = 'required layer does not exist'
            case (GRC_BMM_INVALID_LAYERLEN)
                str = 'layer length exceeds maximum'

            ! COM
            case (GRC_COM_ERO)
                str = 'initiate Extended Runtime Operation (ERO)'
            case (GRC_COM_CANT_ENCODE)
                str = 'cannot encode arguments in client'
            case (GRC_COM_CANT_DECODE)
                str = 'cannot decode results in client'
            case (GRC_COM_CANT_SEND)
                str = 'hardware error while sending'
            case (GRC_COM_CANT_RECV)
                str = 'hardware error while receiving'
            case (GRC_COM_TIMEDOUT)
                str = 'request timed out'
            case (GRC_COM_WRONG_FORMAT)
                str = 'packet format error'
            case (GRC_COM_VER_MISMATCH)
                str = 'version mismatch between client and server'
            case (GRC_COM_CANT_DECODE_REQ)
                str = 'cannot decode arguments in server'
            case (GRC_COM_PROC_UNAVAIL)
                str = 'unknown RPC, procedure ID invalid'
            case (GRC_COM_CANT_ENCODE_REP)
                str = 'cannot encode results in server'
            case (GRC_COM_SYSTEM_ERR)
                str = 'unspecified generic system error'
            case (GRC_COM_FAILED)
                str = 'unspecified error'
            case (GRC_COM_NO_BINARY)
                str = 'binary protocol not available'
            case (GRC_COM_INTR)
                str = 'call interrupted'
            case (GRC_COM_REQUIRES_8DBITS)
                str = 'protocol needs 8 bit encoded characters'
            case (GRC_COM_TR_ID_MISMATCH)
                str = 'TRANSACTIONS ID mismatch error'
            case (GRC_COM_NOT_GEOCOM)
                str = 'protocol not recognizable'
            case (GRC_COM_UNKNOWN_PORT)
                str = 'invalid port address'
            case (GRC_COM_ERO_END)
                str = 'ERO is terminating'
            case (GRC_COM_OVERRUN)
                str = 'internal error (data buffer overflow)'
            case (GRC_COM_SRVR_RX_CHECKSUM_ERRR)
                str = 'invalid checksum on server side received'
            case (GRC_COM_CLNT_RX_CHECKSUM_ERRR)
                str = 'invalid checksum on client side received'
            case (GRC_COM_PORT_NOT_AVAILABLE)
                str = 'port not available'
            case (GRC_COM_PORT_NOT_OPEN)
                str = 'port not opened'
            case (GRC_COM_NO_PARTNER)
                str = 'unable to find TPS'
            case (GRC_COM_ERO_NOT_STARTED)
                str = 'extended Runtime Operation could not be started'
            case (GRC_COM_CONS_REQ)
                str = 'att to send cons reqs'
            case (GRC_COM_SRVR_IS_SLEEPING)
                str = 'TPS has gone to sleep (wait and try again)'
            case (GRC_COM_SRVR_IS_OFF)
                str = 'TPS has shut down (wait and try again)'
            case (GRC_COM_NO_CHECKSUM)
                str = 'no checksum in ASCII protocol available'

            ! AUT
            case (GRC_AUT_TIMEOUT)
                str = 'position not reached'
            case (GRC_AUT_DETENT_ERROR)
                str = 'positioning not possible due to mounted EDM'
            case (GRC_AUT_ANGLE_ERROR)
                str = 'angle measurement error'
            case (GRC_AUT_MOTOR_ERROR)
                str = 'motorisation error'
            case (GRC_AUT_INCACC)
                str = 'position not exactly reached'
            case (GRC_AUT_DEV_ERROR)
                str = 'deviation measurement error'
            case (GRC_AUT_NO_TARGET)
                str = 'no target detected'
            case (GRC_AUT_MULTIPLE_TARGETS)
                str = 'multiple targets detected'
            case (GRC_AUT_BAD_ENVIRONMENT)
                str = 'bad environment conditions'
            case (GRC_AUT_DETECTOR_ERROR)
                str = 'error in target acquisition'
            case (GRC_AUT_NOT_ENABLED)
                str = 'target acquisition not enabled'
            case (GRC_AUT_CALACC)
                str = 'ATR calibration failed'
            case (GRC_AUT_ACCURACY)
                str = 'target position not exactly reached'
            case (GRC_AUT_DIST_STARTED)
                str = 'dist. measurement has been started'
            case (GRC_AUT_SUPPLY_TOO_HIGH)
                str = 'external supply voltage is too high'
            case (GRC_AUT_SUPPLY_TOO_LOW)
                str = 'int. or ext. supply voltage is too low'
            case (GRC_AUT_NO_WORKING_AREA)
                str = 'working area not set'
            case (GRC_AUT_ARRAY_FULL)
                str = 'power search data array is filled'
            case (GRC_AUT_NO_DATA)
                str = 'no data available'

            ! KDM
            case (GRC_KDM_NOT_AVAILABLE)
                str = 'KDM device is not available'

            ! FTR
            case (GRC_FTR_FILEACCESS)
                str = 'file access error'
            case (GRC_FTR_WRONGFILEBLOCKNUMBER)
                str = 'block number was not the expected one'
            case (GRC_FTR_NOTENOUGHSPACE)
                str = 'not enough space on device to proceed uploading'
            case (GRC_FTR_INVALIDINPUT)
                str = 'rename of file failed'
            case (GRC_FTR_MISSINGSETUP)
                str = 'invalid parameter as input'

            case default
                str = 'unknown GeoCOM code'
        end select
    end function dm_geocom_error_message
end module dm_geocom_error
