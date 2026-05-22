! Author:  Philipp Engel
! Licence: ISC
module dm_gantner
    !! Fortran 2018 interface bindings to the eGateHighSpeedPort API of the
    !! Ganter Instruments GInsData library. This module has to be linked against
    !! the shared library `libGInsUtility.so` (x86-64 only).
    !!
    !! Character strings passed to the interfaces in this module have to be
    !! null-terminated with `dm_f_c_string()` first.
    !!
    !! ## Examples
    !!
    !! Connecting to a Gantner Q.station and reading date and time of the RTC:
    !!
    !! ``` fortran
    !! character(*), parameter :: HOST        = '10.10.10.2' !! IP address of the Gantner Q.station.
    !! integer,      parameter :: SAMPLE_RATE = 100          !! Sample rate in Hz.
    !! integer,      parameter :: TIMEOUT     = 5            !! Connection timeout in seconds.
    !!
    !! integer      :: client, connection
    !! integer      :: stat
    !! integer(u8)  :: month, day, hour, minute, second
    !! integer(u16) :: year, msecond
    !!
    !! client     = -1
    !! connection = -1
    !!
    !! io_block: block
    !!     ! Initialise connection to Gantner Q.station.
    !!     stat = dm_gantner_init(host        = dm_f_c_string(HOST), &
    !!                            timeout     = TIMEOUT,             &
    !!                            mode        = GANTNER_HSP_ONLINE,  &
    !!                            sample_rate = SAMPLE_RATE,         &
    !!                            client      = client,              &
    !!                            connection  = connection)
    !!     if (stat /= GANTNER_HSP_OK) exit io_block
    !!
    !!     ! Read date and time of device.
    !!     stat = dm_gantner_get_rtc(connection, year, month, day, hour, minute, second, msecond)
    !!     if (stat /= GANTNER_HSP_OK) exit io_block
    !!
    !!     print '("Date: ", i0.4, 2("-", i0.2))', year, month, day
    !!     print '("Time: ", 3(i0.2, ":"), i0.3)', hour, minute, second, msecond
    !! end block io_block
    !!
    !! if (stat /= GANTNER_HSP_OK) print '("Error ", i0)', stat
    !!
    !! ! Disconnect.
    !! stat = dm_gantner_close(connection, client)
    !! if (stat /= GANTNER_HSP_OK) print '("Error: failed to close connection (", i0, ")")', stat
    !! ```
    !!
    !! Reading device information from the Gantner Q.station:
    !!
    !! ``` fortran
    !! character(80) :: buffer
    !! integer       :: i, n
    !! real(r8)      :: f
    !!
    !! buffer = ' '
    !!
    !! stat = dm_gantner_get_device_info(connection, GANTNER_DEVICE_LOCATION,    0, f, buffer)
    !! print '("Controller Location.....: ", a)', trim(buffer); buffer = ' '
    !! stat = dm_gantner_get_device_info(connection, GANTNER_DEVICE_ADDRESS,     0, f, buffer)
    !! print '("Controller Address......: ", a)', trim(buffer); buffer = ' '
    !! stat = dm_gantner_get_device_info(connection, GANTNER_DEVICE_TYPENAME,    0, f, buffer)
    !! print '("Controller Type.........: ", a)', trim(buffer); buffer = ' '
    !! stat = dm_gantner_get_device_info(connection, GANTNER_DEVICE_VERSION,     0, f, buffer)
    !! print '("Controller Version......: ", a)', trim(buffer); buffer = ' '
    !! stat = dm_gantner_get_device_info(connection, GANTNER_DEVICE_TYPECODE,    0, f, buffer)
    !! print '("Controller Type Code....: ", a)', trim(buffer); buffer = ' '
    !! stat = dm_gantner_get_device_info(connection, GANTNER_DEVICE_SERIALNR,    0, f, buffer)
    !! print '("Controller S/N..........: ", a)', trim(buffer); buffer = ' '
    !! stat = dm_gantner_get_device_info(connection, GANTNER_DEVICE_SAMPLERATE,  0, f, buffer)
    !! print '("Controller Sample Rate..: ", i0)', floor(f)
    !! stat = dm_gantner_get_device_info(connection, GANTNER_DEVICE_MODULECOUNT, 0, f, buffer)
    !! print '("Controller Module Count.: ", i0)', floor(f)
    !! print *
    !!
    !! n = floor(f)
    !!
    !! do i = 0, n - 1
    !!     print '(20("="), " Module ", i2, " ", 20("="))', i
    !!     stat = dm_gantner_get_device_info(connection, GANTNER_MODULE_LOCATION,  i, f, buffer)
    !!     print '("Location................: ", a)', trim(buffer); buffer = ' '
    !!     stat = dm_gantner_get_device_info(connection, GANTNER_MODULE_TYPECODE,  i, f, buffer)
    !!     print '("Type Code...............: ", a)', trim(buffer); buffer = ' '
    !!     stat = dm_gantner_get_device_info(connection, GANTNER_MODULE_ADDRESS,   i, f, buffer)
    !!     print '("Address.................: ", i0)', floor(f)
    !!     stat = dm_gantner_get_device_info(connection, GANTNER_MODULE_UARTINDEX, i, f, buffer)
    !!     print '("UART Index..............: ", i0)', floor(f)
    !!     stat = dm_gantner_get_device_info(connection, GANTNER_MODULE_VARCOUNT,  i, f, buffer)
    !!     print '("Variable Count..........: ", i0)', floor(f)
    !! end do
    !!
    !! stat = dm_gantner_get_device_info(connection, GANTNER_DEVICE_CHANNELCOUNT, 0, f, buffer)
    !! print '("Controller Channel Count: ", i0)', floor(f)
    !! print *
    !!
    !! n = floor(f)
    !!
    !! do i = 0, n - 1
    !!     print '(19("="), " Channel ", i2, " ", 19("="))', i
    !!     stat = dm_gantner_get_device_info(connection, GANTNER_CHINFO_NAME, i, f, buffer)
    !!     print '("Name....................: ", a)', trim(buffer); buffer = ' '
    !!     stat = dm_gantner_get_device_info(connection, GANTNER_CHINFO_UNIT, i, f, buffer)
    !!     print '("Unit....................: ", a)', trim(buffer); buffer = ' '
    !!     stat = dm_gantner_get_device_info(connection, GANTNER_CHINFO_DADI, i, f, buffer)
    !!     print '("Data Direction..........: ", a)', trim(buffer); buffer = ' '
    !!     stat = dm_gantner_get_device_info(connection, GANTNER_CHINFO_VART, i, f, buffer)
    !!     print '("Variable Type...........: ", a)', trim(buffer); buffer = ' '
    !!     stat = dm_gantner_get_device_info(connection, GANTNER_CHINFO_FORM, i, f, buffer)
    !!     print '("Format..................: ", a)', trim(buffer); buffer = ' '
    !!     stat = dm_gantner_get_device_info(connection, GANTNER_CHINFO_TYPE, i, f, buffer)
    !!     print '("Type....................: ", a)', trim(buffer); buffer = ' '
    !!     stat = dm_gantner_get_device_info(connection, GANTNER_CHINFO_DTYI, i, f, buffer)
    !!     print '("Data Type Index.........: ", i0)', floor(f)
    !!     stat = dm_gantner_get_device_info(connection, GANTNER_CHINFO_INDI, i, f, buffer)
    !!     print '("Input Index.............: ", i0)', floor(f)
    !!     stat = dm_gantner_get_device_info(connection, GANTNER_CHINFO_INDO, i, f, buffer)
    !!     print '("Output Index............: ", i0)', floor(f)
    !!     stat = dm_gantner_get_device_info(connection, GANTNER_CHINFO_INDX, i, f, buffer)
    !!     print '("Total Index.............: ", i0)', floor(f)
    !!     stat = dm_gantner_get_device_info(connection, GANTNER_CHINFO_PREC, i, f, buffer)
    !!     print '("Precision...............: ", i0)', floor(f)
    !!     stat = dm_gantner_get_device_info(connection, GANTNER_CHINFO_FLEN, i, f, buffer)
    !!     print '("Field Length............: ", i0)', floor(f)
    !!     stat = dm_gantner_get_device_info(connection, GANTNER_CHINFO_RMIN, i, f, buffer)
    !!     print '("Range Minimum...........: ", i0)', floor(f)
    !!     stat = dm_gantner_get_device_info(connection, GANTNER_CHINFO_RMAX, i, f, buffer)
    !!     print '("Range Maximum...........: ", i0)', floor(f)
    !!
    !!     stat = dm_gantner_read_online_single(connection, i, f)
    !!     print '("Value...................: ", g0.12)', f
    !! end do
    !! ```
    !!
    !! Reading a single value from channel 22:
    !!
    !! ``` fortran
    !! integer  :: channel
    !! real(r8) :: value
    !!
    !! channel = 22
    !! stat = dm_gantner_read_online_single(connection, channel, value)
    !! ```
    !!
    !! Reading 8 of 32 channels at once:
    !!
    !! ``` fortran
    !! integer  :: from, to
    !! real(r8) :: values(0:31)
    !!
    !! from = 0
    !! to   = 7
    !! stat = dm_gantner_read_online_frame_to_double_array(connection, values, size(values), from, to)
    !! ```
    use :: dm_c
    implicit none (type, external)
    private

    ! General return codes
    integer(c_int), parameter, public :: GANTNER_HSP_OK                = 0
    integer(c_int), parameter, public :: GANTNER_HSP_ERROR             = 1
    integer(c_int), parameter, public :: GANTNER_HSP_CONNECTION_ERROR  = 2
    integer(c_int), parameter, public :: GANTNER_HSP_INIT_ERROR        = 3
    integer(c_int), parameter, public :: GANTNER_HSP_LIMIT_ERROR       = 4
    integer(c_int), parameter, public :: GANTNER_HSP_SYNC_CONF_ERROR   = 5
    integer(c_int), parameter, public :: GANTNER_HSP_MULTYUSED_ERROR   = 6
    integer(c_int), parameter, public :: GANTNER_HSP_INDEX_ERROR       = 7
    integer(c_int), parameter, public :: GANTNER_HSP_FILE_ERROR        = 8
    integer(c_int), parameter, public :: GANTNER_HSP_NOT_READY         = 9
    integer(c_int), parameter, public :: GANTNER_HSP_EXLIB_MISSING     = 10
    integer(c_int), parameter, public :: GANTNER_HSP_NOT_CONNECTED     = 11
    integer(c_int), parameter, public :: GANTNER_HSP_NO_FILE           = 12
    integer(c_int), parameter, public :: GANTNER_HSP_CORE_ERROR        = 13
    integer(c_int), parameter, public :: GANTNER_HSP_POINTER_INVALID   = 14
    integer(c_int), parameter, public :: GANTNER_HSP_NOT_IMPLEMENTED   = 15
    integer(c_int), parameter, public :: GANTNER_HSP_INVALID_TIMESTAMP = 16
    integer(c_int), parameter, public :: GANTNER_HSP_COMPLETE          = 17

    ! ChannelInfo IDs
    integer(c_int), parameter, public :: GANTNER_CHINFO_NAME     = 0  ! string: channel name
    integer(c_int), parameter, public :: GANTNER_CHINFO_UNIT     = 1  ! string: unit (°C, m, kg, ...)
    integer(c_int), parameter, public :: GANTNER_CHINFO_DADI     = 2  ! string: data direction (Input, Output, Empty, ...)
    integer(c_int), parameter, public :: GANTNER_CHINFO_FORM     = 3  ! string: e.g. %8.3
    integer(c_int), parameter, public :: GANTNER_CHINFO_TYPE     = 4  ! string: FLOAT, DOUBLE, ...
    integer(c_int), parameter, public :: GANTNER_CHINFO_INDI     = 5  ! integer: input access index
    integer(c_int), parameter, public :: GANTNER_CHINFO_INDO     = 6  ! integer: output access index
    integer(c_int), parameter, public :: GANTNER_CHINFO_INDX     = 7  ! integer: total access index
    integer(c_int), parameter, public :: GANTNER_CHINFO_PREC     = 8  ! integer: precision
    integer(c_int), parameter, public :: GANTNER_CHINFO_FLEN     = 9  ! integer: field length
    integer(c_int), parameter, public :: GANTNER_CHINFO_RMIN     = 30 ! integer: range min
    integer(c_int), parameter, public :: GANTNER_CHINFO_RMAX     = 31 ! integer: range max
    integer(c_int), parameter, public :: GANTNER_CHINFO_MIND     = 32 ! integer: module index
    integer(c_int), parameter, public :: GANTNER_CHINFO_DTYI     = 34 ! integer: data type index
    integer(c_int), parameter, public :: GANTNER_CHINFO_INOFFSET = 35 ! integer: input byte offset
    integer(c_int), parameter, public :: GANTNER_CHINFO_VART     = 33 ! integer: variable type (AIN, AOU, ...)
    integer(c_int), parameter, public :: GANTNER_CHINFO_UUID     = 36 ! integer: unique configuration id of variable

    ! DeviceInfo IDs
    integer(c_int), parameter, public :: GANTNER_DEVICE_LOCATION        = 10                  ! string
    integer(c_int), parameter, public :: GANTNER_DEVICE_ADDRESS         = 11                  ! string
    integer(c_int), parameter, public :: GANTNER_DEVICE_TYPE            = 12                  ! string
    integer(c_int), parameter, public :: GANTNER_DEVICE_TYPENAME        = GANTNER_DEVICE_TYPE ! string
    integer(c_int), parameter, public :: GANTNER_DEVICE_VERSION         = 13                  ! string
    integer(c_int), parameter, public :: GANTNER_DEVICE_TYPECODE        = 14                  ! string
    integer(c_int), parameter, public :: GANTNER_DEVICE_SERIALNR        = 15                  ! string
    integer(c_int), parameter, public :: GANTNER_DEVICE_SAMPLERATE      = 16                  ! integer
    integer(c_int), parameter, public :: GANTNER_DEVICE_MODULECOUNT     = 17                  ! integer
    integer(c_int), parameter, public :: GANTNER_DEVICE_CHANNELCOUNT    = 18                  ! integer
    integer(c_int), parameter, public :: GANTNER_DEVICE_MID             = 50                  ! integer
    integer(c_int), parameter, public :: GANTNER_DEVICE_BUFFERCOUNT     = 51                  ! integer
    integer(c_int), parameter, public :: GANTNER_DEVICE_LOGGERCOUNT     = 52                  ! integer
    integer(c_int), parameter, public :: GANTNER_DEVICE_TSTYPE          = 53                  ! integer
    integer(c_int), parameter, public :: GANTNER_DEVICE_DATAFRAMEWIDTH  = 54                  ! integer
    integer(c_int), parameter, public :: GANTNER_DEVICE_ENDIANNESS      = 55                  ! integer
    integer(c_int), parameter, public :: GANTNER_DEVICE_FRAMELENGTH_IN  = 56                  ! integer
    integer(c_int), parameter, public :: GANTNER_DEVICE_FRAMELENGTH_OUT = 57                  ! integer
    integer(c_int), parameter, public :: GANTNER_DEVICE_UUID            = 58                  ! string: unique configuration id of device
    integer(c_int), parameter, public :: GANTNER_DEVICE_SRC_UUID        = 59                  ! string: unique configuration id of the data source

    ! SlaveModuleInfo IDs
    integer(c_int), parameter, public :: GANTNER_MODULE_TYPE      = 19 ! string
    integer(c_int), parameter, public :: GANTNER_MODULE_TYPECODE  = 20 ! string
    integer(c_int), parameter, public :: GANTNER_MODULE_LOCATION  = 21 ! string
    integer(c_int), parameter, public :: GANTNER_MODULE_UUID      = 28 ! string
    integer(c_int), parameter, public :: GANTNER_MODULE_UARTINDEX = 22 ! integer
    integer(c_int), parameter, public :: GANTNER_MODULE_ADDRESS   = 23 ! integer
    integer(c_int), parameter, public :: GANTNER_MODULE_VARCOUNT  = 24 ! integer

    ! StorageInfo IDs
    integer(c_int), parameter, public :: GANTNER_STORE_FILECOUNT  = 25
    integer(c_int), parameter, public :: GANTNER_STORE_SECONDS    = 26

    ! Buffer IDs
    integer(c_int), parameter, public :: GANTNER_BUFFER_MAXFRAMES = 27

    ! Data direction IDs
    integer(c_int), parameter, public :: GANTNER_DADI_INPUT = 0 ! input
    integer(c_int), parameter, public :: GANTNER_DADI_OUTPT = 1 ! output
    integer(c_int), parameter, public :: GANTNER_DADI_INOUT = 2 ! input/output
    integer(c_int), parameter, public :: GANTNER_DADI_EMPTY = 3 ! empty
    integer(c_int), parameter, public :: GANTNER_DADI_STATS = 4 ! statistic channels

    ! Connection types
    integer(c_int), parameter, public :: GANTNER_HSP_ONLINE         = 1
    integer(c_int), parameter, public :: GANTNER_HSP_BUFFER         = 2
    integer(c_int), parameter, public :: GANTNER_HSP_ECONLOGGER     = 3
    integer(c_int), parameter, public :: GANTNER_HSP_ARCHIVES       = 4
    integer(c_int), parameter, public :: GANTNER_HSP_FILES          = 5
    integer(c_int), parameter, public :: GANTNER_HSP_DIAG           = 7
    integer(c_int), parameter, public :: GANTNER_HSP_DIRECT         = GANTNER_HSP_DIAG
    integer(c_int), parameter, public :: GANTNER_HSP_POSTPROCBUFFER = 8
    integer(c_int), parameter, public :: GANTNER_DLL_CONTROL        = 9
    integer(c_int), parameter, public :: GANTNER_HSP_BUFFER0        = 100
    integer(c_int), parameter, public :: GANTNER_HSP_BUFFER1        = 101

    ! Statistic info types
    integer(c_int), parameter, public :: GANTNER_STAT_CONNECTED  = 0
    integer(c_int), parameter, public :: GANTNER_STAT_STACKSIZE  = 1
    integer(c_int), parameter, public :: GANTNER_STAT_DECODETIME = 2

    ! Diagnostic types
    integer(c_int), parameter, public :: GANTNER_DIAG_CONTROLLER = 0
    integer(c_int), parameter, public :: GANTNER_DIAG_INTERFACE  = 1
    integer(c_int), parameter, public :: GANTNER_DIAG_TRANSPORT  = 2
    integer(c_int), parameter, public :: GANTNER_DIAG_VARIABLE   = 3
    integer(c_int), parameter, public :: GANTNER_DIAG_ITEMCOUNT  = 4

    ! Data storage types
    integer(c_int), parameter, public :: GANTNER_STOR_MDF = 0
    integer(c_int), parameter, public :: GANTNER_STOR_CSV = 1

    ! Timestamp types
    integer(c_int), parameter, public :: GANTNER_TSTYPE_NO           = 0
    integer(c_int), parameter, public :: GANTNER_TSTYPE_COUNTER      = 1
    integer(c_int), parameter, public :: GANTNER_TSTYPE_TIMEOLE2     = 2
    integer(c_int), parameter, public :: GANTNER_TSTYPE_DCSYSTEMTIME = 3

    ! File types
    integer(c_int), parameter, public :: GANTNER_FILE_DIR_ALL             = 0
    integer(c_int), parameter, public :: GANTNER_FILE_FLASHAPPLICATION    = 1
    integer(c_int), parameter, public :: GANTNER_FILE_FLASHDATA           = 2
    integer(c_int), parameter, public :: GANTNER_FILE_USBDATA             = 3
    integer(c_int), parameter, public :: GANTNER_FILE_VIRTUALSTATE        = 4
    integer(c_int), parameter, public :: GANTNER_FILE_VIRTUALONLINEBUFFER = 5
    integer(c_int), parameter, public :: GANTNER_FILE_VIRTUALCIRCLEBUFFER = 6
    integer(c_int), parameter, public :: GANTNER_FILE_VIRTUALARCHIVE      = 7
    integer(c_int), parameter, public :: GANTNER_FILE_VIRTUALLOGGER       = 8
    integer(c_int), parameter, public :: GANTNER_FILE_IDENTIFY_BY_PATH    = 10

    ! File locations
    integer(c_int), parameter, public :: GANTNER_LOC_LOCALE     = 0
    integer(c_int), parameter, public :: GANTNER_LOC_CONTROLLER = 1

    ! Data types
    integer(c_int), parameter, public :: GANTNER_DATY_NO      = 0
    integer(c_int), parameter, public :: GANTNER_DATY_BOOL    = 1
    integer(c_int), parameter, public :: GANTNER_DATY_SINT8   = 2
    integer(c_int), parameter, public :: GANTNER_DATY_USINT8  = 3
    integer(c_int), parameter, public :: GANTNER_DATY_SINT16  = 4
    integer(c_int), parameter, public :: GANTNER_DAYT_USINT16 = 5
    integer(c_int), parameter, public :: GANTNER_DATY_SINT32  = 6
    integer(c_int), parameter, public :: GANTNER_DATY_USINT32 = 7
    integer(c_int), parameter, public :: GANTNER_DATY_FLOAT   = 8
    integer(c_int), parameter, public :: GANTNER_DATY_SET8    = 9
    integer(c_int), parameter, public :: GANTNER_DATY_SET16   = 10
    integer(c_int), parameter, public :: GANTNER_DATY_SET32   = 11
    integer(c_int), parameter, public :: GANTNER_DATY_DOUBLE  = 12
    integer(c_int), parameter, public :: GANTNER_DATY_SINT64  = 13
    integer(c_int), parameter, public :: GANTNER_DATY_USINT64 = 14
    integer(c_int), parameter, public :: GANTNER_DATY_SET64   = 15

    ! Variable kind IDs
    integer(c_int), parameter, public :: GANTNER_VARKIND_EMPTY              = 0
    integer(c_int), parameter, public :: GANTNER_VARKIND_ANALOGOUTPUT       = 1
    integer(c_int), parameter, public :: GANTNER_VARKIND_DANALOGINPUT       = 2
    integer(c_int), parameter, public :: GANTNER_VARKIND_DIGITALOUTPUT      = 3
    integer(c_int), parameter, public :: GANTNER_VARKIND_DIGITALINPUT       = 4
    integer(c_int), parameter, public :: GANTNER_VARKIND_ARITHMETIC         = 5
    integer(c_int), parameter, public :: GANTNER_VARKIND_SETPOINT           = 6
    integer(c_int), parameter, public :: GANTNER_VARKIND_ALARM              = 7
    integer(c_int), parameter, public :: GANTNER_VARKIND_PIDCONTROLLER      = 8
    integer(c_int), parameter, public :: GANTNER_VARKIND_SIGNALCONDITIONING = 9
    integer(c_int), parameter, public :: GANTNER_VARKIND_REMOTE             = 10
    integer(c_int), parameter, public :: GANTNER_VARKIND_REFERENCE          = 11

    ! Callback types
    integer(c_int), parameter, public :: GANTNER_CALL_CONTROL = 0
    integer(c_int), parameter, public :: GANTNER_CALL_ERROR   = 1
    integer(c_int), parameter, public :: GANTNER_CALL_DIAG    = 2
    integer(c_int), parameter, public :: GANTNER_CALL_DSPDATA = 3
    integer(c_int), parameter, public :: GANTNER_CALL_FREADY  = 4
    integer(c_int), parameter, public :: GANTNER_CALL_DEBUG   = 5

    ! Remote control types
    integer(c_int), parameter, public :: GANTNER_REMOTE_START = 0
    integer(c_int), parameter, public :: GANTNER_REMOTE_STOP  = 1
    integer(c_int), parameter, public :: GANTNER_REMOTE_END   = 2

    public :: dm_gantner_close
    public :: dm_gantner_diagnostic
    public :: dm_gantner_get_channel_info
    public :: dm_gantner_get_channel_info_int
    public :: dm_gantner_get_channel_info_string
    public :: dm_gantner_get_device_info
    public :: dm_gantner_get_file_count
    public :: dm_gantner_get_file_info
    public :: dm_gantner_get_number_of_channels
    public :: dm_gantner_get_receive_timeout
    public :: dm_gantner_get_rtc
    public :: dm_gantner_get_sample_rate
    public :: dm_gantner_init
    public :: dm_gantner_init_buffer
    public :: dm_gantner_read_buffer_next_frame
    public :: dm_gantner_read_online_frame
    public :: dm_gantner_read_online_frame_to_double_array
    public :: dm_gantner_read_online_single
    public :: dm_gantner_set_back_time
    public :: dm_gantner_set_receive_timeout
    public :: dm_gantner_set_rtc
    public :: dm_gantner_set_sample_rate
    public :: dm_gantner_sleep

    interface
        ! int32_t _CD_eGateHighSpeedPort_Close(int32_t connectionInstance, int32_t clientInstance)
        function dm_gantner_close(connection, client) bind(c, name='_CD_eGateHighSpeedPort_Close')
            import :: c_int32_t
            implicit none
            integer(c_int32_t),  intent(in), value :: connection
            integer(c_int32_t),  intent(in), value :: client
            integer(c_int32_t)                     :: dm_gantner_close
        end function dm_gantner_close

        ! int32_t _CD_eGateHighSpeedPort_Diagnostic(int32_t ConnectionInstance, uint32_t diagLevel, uint32_t index, uint32_t *cycleCount, uint32_t *errorCount)
        function dm_gantner_diagnostic(connection, level, index, cycle_count, error_count) bind(c, name='CALLINGCONVENTION_CD')
            import :: c_int32_t, c_uint32_t
            implicit none
            integer(c_int32_t),  intent(in), value :: connection
            integer(c_uint32_t), intent(in), value :: level
            integer(c_uint32_t), intent(in), value :: index
            integer(c_uint32_t), intent(out)       :: cycle_count
            integer(c_uint32_t), intent(out)       :: error_count
            integer(c_int32_t)                     :: dm_gantner_diagnostic
        end function dm_gantner_diagnostic

        ! int32_t _CD_eGateHighSpeedPort_Init(const char *hostName, int32_t timeOutSec, int32_t mode, int32_t sampleRateHz, int32_t *clientInstance, int32_t *connectionInstance)
        function dm_gantner_init(host, timeout, mode, sample_rate, client, connection) bind(c, name='_CD_eGateHighSpeedPort_Init')
            !! Initialises the Ethernet HighSpeedPort connection to a
            !! Gantner Instruments controller.
            import :: c_char, c_int32_t
            implicit none
            character(c_char),  intent(in)        :: host            !! IP address of controller
            integer(c_int32_t), intent(in), value :: timeout         !! connection timeout in seconds
            integer(c_int32_t), intent(in), value :: mode            !! connection type (GANTNER_HSP_ONLINE, GANTNER_HSP_BUFFER, ...)
            integer(c_int32_t), intent(in), value :: sample_rate     !! sample rate in Hz
            integer(c_int32_t), intent(inout)     :: client          !! client instance
            integer(c_int32_t), intent(inout)     :: connection      !! connection instance
            integer(c_int32_t)                    :: dm_gantner_init !! status code
        end function dm_gantner_init

        ! int32_t _CD_eGateHighSpeedPort_InitBuffer(int32_t connectionInstance, int32_t bufferIndex, int32_t autoRun)
        function dm_gantner_init_buffer(connection, buffer_index, auto_run) bind(c, name='_CD_eGateHighSpeedPort_InitBuffer')
            import :: c_int32_t
            implicit none
            integer(c_int32_t), intent(in), value :: connection
            integer(c_int32_t), intent(in), value :: buffer_index
            integer(c_int32_t), intent(in), value :: auto_run
            integer(c_int32_t)                    :: dm_gantner_init_buffer
        end function dm_gantner_init_buffer

        ! int32_t _CD_eGateHighSpeedPort_GetChannelInfo(int32_t ConnectionInstance, char *ChannelName, char *ChannelID, int32_t *IndexTotal, int32_t *IndexIn, int32_t *IndexOut)
        function dm_gantner_get_channel_info(connection, channel_name, channel_id, index_total, index_in, index_out) &
                bind(c, name='_CD_eGateHighSpeedPort_GetChannelInfo')
            !! Read essential channel information depending on the type
            !! provided. The first parameter that is specified will be used to
            !! find the variable. All other information will be filled with the
            !! corresponding information.
            import :: c_char, c_int32_t
            implicit none
            integer(c_int32_t), intent(in), value :: connection
            character(c_char),  intent(inout)     :: channel_name
            character(c_char),  intent(inout)     :: channel_id
            integer(c_int32_t), intent(out)       :: index_total
            integer(c_int32_t), intent(out)       :: index_in
            integer(c_int32_t), intent(out)       :: index_out
            integer(c_int32_t)                    :: dm_gantner_get_channel_info
        end function dm_gantner_get_channel_info

        ! int32_t _CD_eGateHighSpeedPort_GetChannelInfo_Int(int32_t connectionInstance, uint32_t typeID, uint32_t directionID, uint32_t channelIndex, int32_t *channelInfo)
        function dm_gantner_get_channel_info_int(connection, type_id, direction_id, channel_index, channel_info) &
                bind(c, name='_CD_eGateHighSpeedPort_GetChannelInfo_Int')
            !! The argument `channel_info` must be large enough to hold the data (wanky API).
            import :: c_int32_t, c_uint32_t
            implicit none
            integer(c_int32_t),  intent(in), value :: connection
            integer(c_uint32_t), intent(in), value :: type_id
            integer(c_uint32_t), intent(in), value :: direction_id
            integer(c_uint32_t), intent(in), value :: channel_index
            integer(c_int32_t),  intent(out)       :: channel_info
            integer(c_int32_t)                     :: dm_gantner_get_channel_info_int
        end function dm_gantner_get_channel_info_int

        ! int32_t _CD_eGateHighSpeedPort_GetChannelInfo_String(int32_t connectionInstance, uint32_t typeID, uint32_t directionID, uint32_t channelIndex, char *channelInfo)
        function dm_gantner_get_channel_info_string(connection, type_id, direction_id, channel_index, channel_info) &
                bind(c, name='_CD_eGateHighSpeedPort_GetChannelInfo_String')
            !! The argument `channel_info` must be large enough to hold the data (wanky API).
            import :: c_char, c_int32_t, c_uint32_t
            implicit none
            integer(c_int32_t),  intent(in), value :: connection
            integer(c_uint32_t), intent(in), value :: type_id
            integer(c_uint32_t), intent(in), value :: direction_id
            integer(c_uint32_t), intent(in), value :: channel_index
            character(c_char),   intent(inout)     :: channel_info
            integer(c_int32_t)                     :: dm_gantner_get_channel_info_string
        end function dm_gantner_get_channel_info_string

        ! int32_t _CD_eGateHighSpeedPort_GetDeviceInfo(int32_t connectionInstance, int32_t typeID, int32_t Index, double *info, char *infoString)
        function dm_gantner_get_device_info(connection, type_id, index, info, info_string) &
                bind(c, name='_CD_eGateHighSpeedPort_GetDeviceInfo')
            !! The argument `info_string` must be large enough to hold the data (wanky API).
            import :: c_char, c_double, c_int32_t
            implicit none
            integer(c_int32_t), intent(in), value :: connection
            integer(c_int32_t), intent(in), value :: type_id
            integer(c_int32_t), intent(in), value :: index
            real(c_double),     intent(out)       :: info
            character(c_char),  intent(inout)     :: info_string
            integer(c_int32_t)                    :: dm_gantner_get_device_info
        end function dm_gantner_get_device_info

        ! int32_t _CD_eGateHighSpeedPort_GetFileCount(int32_t connectionInstance, uint32_t fileTypeID, const char *filePath, uint32_t *fileCount)
        function dm_gantner_get_file_count(connection, file_type_id, file_path, file_count) &
                bind(c, name='_CD_eGateHighSpeedPort_GetFileCount')
            import :: c_char, c_int32_t, c_uint32_t
            implicit none
            integer(c_int32_t),  intent(in), value :: connection
            integer(c_uint32_t), intent(in), value :: file_type_id
            character(c_char),   intent(in)        :: file_path
            integer(c_uint32_t), intent(out)       :: file_count
            integer(c_int32_t)                     :: dm_gantner_get_file_count
        end function dm_gantner_get_file_count

        ! int32_t _CD_eGateHighSpeedPort_GetFileInfo(int32_t connectionInstance, uint32_t fileIndex, char *fileName, uint32_t fileNameLen, char *fileIdent, uint32_t fileIdentLen, uint32_t *size, double *oleTime)
        function dm_gantner_get_file_info(connection, file_index, file_name, file_name_len, file_ident, file_ident_len, &
                file_size, ole_time) bind(c, name='_CD_eGateHighSpeedPort_GetFileInfo')
            import :: c_char, c_double, c_int32_t, c_uint32_t
            implicit none
            integer(c_int32_t),  intent(in), value :: connection
            integer(c_int32_t),  intent(in), value :: file_index
            character(c_char),   intent(inout)     :: file_name
            integer(c_int32_t),  intent(in)        :: file_name_len
            character(c_char),   intent(inout)     :: file_ident
            integer(c_int32_t),  intent(in)        :: file_ident_len
            integer(c_uint32_t), intent(out)       :: file_size
            real(c_double),      intent(out)       :: ole_time
            integer(c_int32_t)                     :: dm_gantner_get_file_info
        end function dm_gantner_get_file_info

        ! int32_t _CD_eGateHighSpeedPort_GetNumberOfChannels(int32_t connectionInstance, uint32_t directionID, uint32_t *channelCount)
        function dm_gantner_get_number_of_channels(connection, direction_id, channel_count) &
                bind(c, name='_CD_eGateHighSpeedPort_GetNumberOfChannels')
            import :: c_int32_t
            implicit none
            integer(c_int32_t), intent(in), value :: connection
            integer(c_int32_t), intent(in), value :: direction_id
            integer(c_int32_t), intent(out)       :: channel_count
            integer(c_int32_t)                    :: dm_gantner_get_number_of_channels
        end function dm_gantner_get_number_of_channels

        ! int32_t _CD_eGateHighSpeedPort_GetReceiveTimeout(int32_t connectionInstance, int32_t *timeOut)
        function dm_gantner_get_receive_timeout(connection, timeout) bind(c, name='_CD_eGateHighSpeedPort_GetReceiveTimeout')
            import :: c_int32_t
            implicit none
            integer(c_int32_t), intent(in), value :: connection
            integer(c_int32_t), intent(out)       :: timeout
            integer(c_int32_t)                    :: dm_gantner_get_receive_timeout
        end function dm_gantner_get_receive_timeout

        ! int32_t _CD_eGateHighSpeedPort_GetRTC(int32_t connectionInstance, uint16_t *year, uint8_t *month, uint8_t *day, uint8_t *hour, uint8_t *minute, uint8_t *second, uint16_t *millisecond)
        function dm_gantner_get_rtc(connection, year, month, day, hour, minute, second, msecond) &
                bind(c, name='_CD_eGateHighSpeedPort_GetRTC')
            !! Reads the current time of the device.
            import :: c_int32_t, c_uint8_t, c_uint16_t
            implicit none
            integer(c_int32_t),  intent(in), value :: connection
            integer(c_uint16_t), intent(out)       :: year
            integer(c_uint8_t),  intent(out)       :: month
            integer(c_uint8_t),  intent(out)       :: day
            integer(c_uint8_t),  intent(out)       :: hour
            integer(c_uint8_t),  intent(out)       :: minute
            integer(c_uint8_t),  intent(out)       :: second
            integer(c_uint16_t), intent(out)       :: msecond
            integer(c_int32_t)                     :: dm_gantner_get_rtc
        end function dm_gantner_get_rtc

        ! int32_t _CD_eGateHighSpeedPort_GetSampleRate(int32_t connectionInstance, int32_t *sampleRateHz)
        function dm_gantner_get_sample_rate(connection, sample_rate) bind(c, name='_CD_eGateHighSpeedPort_GetSampleRate')
            import :: c_int32_t
            implicit none
            integer(c_int32_t), intent(in), value :: connection  !! connection instance
            integer(c_int32_t), intent(out)       :: sample_rate !! sample rate in Hz
            integer(c_int32_t)                    :: dm_gantner_get_sample_rate
        end function dm_gantner_get_sample_rate

        ! int32_t _CD_eGateHighSpeedPort_ReadBuffer_NextFrame(int32_t connectionInstance, int32_t clientInstance)
        function dm_gantner_read_buffer_next_frame(connection, client) bind(c, name='_CD_eGateHighSpeedPort_ReadBuffer_NextFrame')
            import :: c_int32_t
            implicit none
            integer(c_int32_t), intent(in), value :: connection
            integer(c_int32_t), intent(in), value :: client
            integer(c_int32_t)                    :: dm_gantner_read_buffer_next_frame
        end function dm_gantner_read_buffer_next_frame

        ! int32_t _CD_eGateHighSpeedPort_ReadOnline_Frame(int32_t connectionInstance)
        function dm_gantner_read_online_frame(connection) bind(c, name='_CD_eGateHighSpeedPort_ReadOnline_Frame')
            import :: c_int32_t
            implicit none
            integer(c_int32_t), intent(in), value :: connection
            integer(c_int32_t)                    :: dm_gantner_read_online_frame
        end function dm_gantner_read_online_frame

        ! int32_t _CD_eGateHighSpeedPort_ReadOnline_FrameToDoubleArray(int32_t connectionInstance, double *valueArray, uint32_t arrayLength, int32_t startIndex, int32_t channelCount)
        function dm_gantner_read_online_frame_to_double_array(connection, value_array, array_len, start_index, channel_count) &
                bind(c, name='_CD_eGateHighSpeedPort_ReadOnline_FrameToDoubleArray')
            import :: c_double, c_int32_t
            implicit none
            integer(c_int32_t), intent(in), value :: connection
            real(c_double),     intent(inout)     :: value_array(*)
            integer(c_int32_t), intent(in), value :: array_len
            integer(c_int32_t), intent(in), value :: start_index
            integer(c_int32_t), intent(in), value :: channel_count
            integer(c_int32_t)                    :: dm_gantner_read_online_frame_to_double_array
        end function dm_gantner_read_online_frame_to_double_array

        ! int32_t _CD_eGateHighSpeedPort_ReadOnline_Single(int32_t connectionInstance, int32_t channelIndex, double *value)
        function dm_gantner_read_online_single(connection, channel_index, value) &
                bind(c, name='_CD_eGateHighSpeedPort_ReadOnline_Single')
            import :: c_double, c_int32_t
            implicit none
            integer(c_int32_t), intent(in), value :: connection
            integer(c_int32_t), intent(in), value :: channel_index
            real(c_double),     intent(out)       :: value
            integer(c_int32_t)                    :: dm_gantner_read_online_single
        end function dm_gantner_read_online_single

        ! int32_t _CD_eGateHighSpeedPort_SetBackTime(int32_t connectionInstance, double backTimeSec)
        function dm_gantner_set_back_time(connection, back_time) bind(c, name='_CD_eGateHighSpeedPort_SetBackTime')
            import :: c_double, c_int32_t
            implicit none
            integer(c_int32_t), intent(in), value :: connection
            real(c_double),     intent(in), value :: back_time
            integer(c_int32_t)                    :: dm_gantner_set_back_time
        end function dm_gantner_set_back_time

        ! int32_t _CD_eGateHighSpeedPort_SetReceiveTimeout(int32_t connectionInstance, int32_t timeOut)
        function dm_gantner_set_receive_timeout(connection, timeout) bind(c, name='_CD_eGateHighSpeedPort_SetReceiveTimeout')
            import :: c_int32_t
            implicit none
            integer(c_int32_t), intent(in), value :: connection
            integer(c_int32_t), intent(in), value :: timeout
            integer(c_int32_t)                    :: dm_gantner_set_receive_timeout
        end function dm_gantner_set_receive_timeout

        ! int32_t _CD_eGateHighSpeedPort_SetRTC(int32_t connectionInstance, uint16_t year, uint8_t month, uint8_t day, uint8_t hour, uint8_t minute, uint8_t second, uint16_t millisecond)
        function dm_gantner_set_rtc(connection, year, month, day, hour, minute, second, msecond) &
                bind(c, name='_CD_eGateHighSpeedPort_SetRTC')
            import :: c_int32_t, c_uint8_t, c_uint16_t
            implicit none
            integer(c_int32_t),  intent(in), value :: connection
            integer(c_uint16_t), intent(in), value :: year
            integer(c_uint8_t),  intent(in), value :: month
            integer(c_uint8_t),  intent(in), value :: day
            integer(c_uint8_t),  intent(in), value :: hour
            integer(c_uint8_t),  intent(in), value :: minute
            integer(c_uint8_t),  intent(in), value :: second
            integer(c_uint16_t), intent(in), value :: msecond
            integer(c_int32_t)                     :: dm_gantner_set_rtc
        end function dm_gantner_set_rtc

        ! int32_t _CD_eGateHighSpeedPort_SetSampleRate(int32_t connectionInstance, int32_t sampleRateHz)
        function dm_gantner_set_sample_rate(connection, sample_rate) bind(c, name='_CD_eGateHighSpeedPort_SetSampleRate')
            import :: c_int32_t
            implicit none
            integer(c_int32_t), intent(in), value :: connection
            integer(c_int32_t), intent(in), value :: sample_rate
            integer(c_int32_t)                    :: dm_gantner_set_sample_rate
        end function dm_gantner_set_sample_rate

        ! int32_t _CD_eGateHighSpeedPort_SleepMS(int32_t time_msec)
        function dm_gantner_sleep(msec) bind(c, name='_CD_eGateHighSpeedPort_SleepMS')
            import :: c_int32_t
            implicit none
            integer(c_int32_t), intent(in), value :: msec
            integer(c_int32_t)                    :: dm_gantner_sleep
        end function dm_gantner_sleep
    end interface
end module dm_gantner
