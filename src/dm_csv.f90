! Author:  Philipp Engel
! Licence: ISC
module dm_csv
    !! Contains subroutines to convert various derived types to CSV format.
    use :: dm_ascii
    use :: dm_error
    use :: dm_kind
    use :: dm_string
    use :: dm_util
    implicit none (type, external)
    private

    character, parameter, public :: CSV_SEPARATOR  = ','  !! Default CSV field separator.
    integer,   parameter, public :: CSV_BUFFER_LEN = 8192 !! CSV line buffer length.

    character(len=*), parameter :: FMT_REAL = '1pg0.12'

    interface dm_csv_from
        !! Generic derived type to CSV serialisation function.
        module procedure :: csv_from_beat
        module procedure :: csv_from_beats
        module procedure :: csv_from_data_point
        module procedure :: csv_from_data_points
        module procedure :: csv_from_log
        module procedure :: csv_from_logs
        module procedure :: csv_from_node
        module procedure :: csv_from_nodes
        module procedure :: csv_from_observ
        module procedure :: csv_from_observ_view
        module procedure :: csv_from_observ_views
        module procedure :: csv_from_observs
        module procedure :: csv_from_sensor
        module procedure :: csv_from_sensors
        module procedure :: csv_from_target
        module procedure :: csv_from_targets
    end interface dm_csv_from

    interface dm_csv_read
        !! Generic derived type from CSV reader.
        module procedure :: csv_read_log
        module procedure :: csv_read_node
        module procedure :: csv_read_observ
        module procedure :: csv_read_sensor
        module procedure :: csv_read_target
    end interface dm_csv_read

    interface dm_csv_write
        !! Generic derived type to CSV writer.
        module procedure :: csv_write_beat
        module procedure :: csv_write_beats
        module procedure :: csv_write_data_point
        module procedure :: csv_write_data_points
        module procedure :: csv_write_log
        module procedure :: csv_write_logs
        module procedure :: csv_write_node
        module procedure :: csv_write_nodes
        module procedure :: csv_write_observ
        module procedure :: csv_write_observs
        module procedure :: csv_write_sensor
        module procedure :: csv_write_sensors
        module procedure :: csv_write_target
        module procedure :: csv_write_targets
    end interface dm_csv_write

    interface csv_next
        !! Generic CSV record reader.
        module procedure :: csv_next_int32
        module procedure :: csv_next_int64
        module procedure :: csv_next_real32
        module procedure :: csv_next_real64
        module procedure :: csv_next_string
    end interface csv_next

    ! Public procedures.
    public :: dm_csv_from
    public :: dm_csv_read
    public :: dm_csv_write

    public :: dm_csv_header_beat
    public :: dm_csv_header_data_point
    public :: dm_csv_header_log
    public :: dm_csv_header_node
    public :: dm_csv_header_observ
    public :: dm_csv_header_observ_view
    public :: dm_csv_header_sensor
    public :: dm_csv_header_target

    ! Private procedures.
    private :: csv_from_beat
    private :: csv_from_beats
    private :: csv_from_data_point
    private :: csv_from_data_points
    private :: csv_from_log
    private :: csv_from_logs
    private :: csv_from_node
    private :: csv_from_nodes
    private :: csv_from_observ
    private :: csv_from_observ_view
    private :: csv_from_observ_views
    private :: csv_from_observs
    private :: csv_from_sensor
    private :: csv_from_sensors
    private :: csv_from_target
    private :: csv_from_targets

    private :: csv_next
    private :: csv_next_int32
    private :: csv_next_int64
    private :: csv_next_real32
    private :: csv_next_real64
    private :: csv_next_string
    private :: csv_parse
    private :: csv_unquote

    private :: csv_read_log
    private :: csv_read_node
    private :: csv_read_observ
    private :: csv_read_sensor
    private :: csv_read_target

    private :: csv_write_beat
    private :: csv_write_beats
    private :: csv_write_data_point
    private :: csv_write_data_points
    private :: csv_write_log
    private :: csv_write_logs
    private :: csv_write_node
    private :: csv_write_nodes
    private :: csv_write_observ
    private :: csv_write_observs
    private :: csv_write_sensor
    private :: csv_write_sensors
    private :: csv_write_target
    private :: csv_write_targets
contains
    ! **************************************************************************
    ! PUBLIC PROCEDURES.
    ! **************************************************************************
    function dm_csv_header_beat(separator) result(header)
        !! Returns header string of CSV representation of the beat type as
        !! allocatable string.
        character, intent(in), optional :: separator !! CSV separator.
        character(len=:), allocatable   :: header    !! CSV header string.

        character :: s

        s = dm_present(separator, CSV_SEPARATOR)

        header = '#node_id'  // s // &
                 'address'   // s // &
                 'version'   // s // &
                 'time_sent' // s // &
                 'time_recv' // s // &
                 'error'     // s // &
                 'interval'  // s // &
                 'uptime'
    end function dm_csv_header_beat

    function dm_csv_header_data_point(separator) result(header)
        !! Returns header string of CSV representation of the data point type as
        !! allocatable string.
        character, intent(in), optional :: separator !! CSV separator.
        character(len=:), allocatable   :: header    !! CSV header string.

        character :: s

        s = dm_present(separator, CSV_SEPARATOR)

        header = '#x' // s // 'y'
    end function dm_csv_header_data_point

    function dm_csv_header_log(separator) result(header)
        !! Returns header string of CSV representation of the log type as
        !! allocatable string.
        character, intent(in), optional :: separator !! CSV separator.
        character(len=:), allocatable   :: header    !! CSV header string.

        character :: s

        s = dm_present(separator, CSV_SEPARATOR)

        header = '#id'       // s // &
                 'level'     // s // &
                 'error'     // s // &
                 'timestamp' // s // &
                 'node_id'   // s // &
                 'sensor_id' // s // &
                 'target_id' // s // &
                 'observ_id' // s // &
                 'message'
    end function dm_csv_header_log

    function dm_csv_header_node(separator) result(header)
        !! Returns header string of CSV representation of the node type as
        !! allocatable string.
        character, intent(in), optional :: separator !! CSV separator.
        character(len=:), allocatable   :: header    !! CSV header string.

        character :: s

        s = dm_present(separator, CSV_SEPARATOR)

        header = '#id'  // s // &
                 'name' // s // &
                 'meta' // s // &
                 'x'    // s // &
                 'y'    // s // &
                 'z'    // s // &
                 'lon'  // s // &
                 'lat'  // s // &
                 'alt'
    end function dm_csv_header_node

    function dm_csv_header_observ(separator) result(header)
        !! Returns CSV header string of CSV representation of the observation
        !! type as allocatable string.
        use :: dm_observ
        use :: dm_request

        character, intent(in), optional :: separator !! CSV separator.
        character(len=:), allocatable   :: header    !! CSV header string.

        character                     :: s
        character(len=:), allocatable :: ai, aj
        integer                       :: i, j

        s = dm_present(separator, CSV_SEPARATOR)

        header = '#id'        // s // &
                 'node_id'    // s // &
                 'sensor_id'  // s // &
                 'target_id'  // s // &
                 'name'       // s // &
                 'timestamp'  // s // &
                 'source'     // s // &
                 'device'     // s // &
                 'priority'   // s // &
                 'error'      // s // &
                 'next'       // s // &
                 'nreceivers' // s // &
                 'nrequests'

        do i = 1, OBSERV_MAX_NRECEIVERS
            header = header // s // 'receivers(' // dm_itoa(i) // ')'
        end do

        do i = 1, OBSERV_MAX_NREQUESTS
            ai = dm_itoa(i)
            header = header // s // &
                     'requests(' // ai // ').name'       // s // &
                     'requests(' // ai // ').timestamp'  // s // &
                     'requests(' // ai // ').request'    // s // &
                     'requests(' // ai // ').response'   // s // &
                     'requests(' // ai // ').delimiter'  // s // &
                     'requests(' // ai // ').pattern'    // s // &
                     'requests(' // ai // ').delay'      // s // &
                     'requests(' // ai // ').error'      // s // &
                     'requests(' // ai // ').mode'       // s // &
                     'requests(' // ai // ').retries'    // s // &
                     'requests(' // ai // ').state'      // s // &
                     'requests(' // ai // ').timeout'    // s // &
                     'requests(' // ai // ').nresponses'

            do j = 1, REQUEST_MAX_NRESPONSES
                aj = dm_itoa(j)
                header = header // s // &
                         'requests(' // ai // ').responses(' // aj // ').name'  // s // &
                         'requests(' // ai // ').responses(' // aj // ').unit'  // s // &
                         'requests(' // ai // ').responses(' // aj // ').type'  // s // &
                         'requests(' // ai // ').responses(' // aj // ').error' // s // &
                         'requests(' // ai // ').responses(' // aj // ').value'
            end do
        end do
    end function dm_csv_header_observ

    function dm_csv_header_observ_view(separator) result(header)
        !! Returns CSV header string of CSV representation of the observation
        !! view type as allocatable string.
        character, intent(in), optional :: separator !! CSV separator.
        character(len=:), allocatable   :: header    !! CSV header string.

        character :: s

        s = dm_present(separator, CSV_SEPARATOR)

        header = '#node_id'          // s // &
                 'sensor_id'         // s // &
                 'target_id'         // s // &
                 'observ_id'         // s // &
                 'observ_name'       // s // &
                 'observ_error'      // s // &
                 'request_name'      // s // &
                 'request_timestamp' // s // &
                 'request_error'     // s // &
                 'response_name'     // s // &
                 'response_unit'     // s // &
                 'response_type'     // s // &
                 'response_error'    // s // &
                 'response_value'
    end function dm_csv_header_observ_view

    function dm_csv_header_sensor(separator) result(header)
        !! Returns header string of CSV representation of the sensor type as
        !! allocatable string.
        character, intent(in), optional :: separator !! CSV separator.
        character(len=:), allocatable   :: header    !! CSV header string.

        character :: s

        s = dm_present(separator, CSV_SEPARATOR)

        header = '#id'     // s // &
                 'node_id' // s // &
                 'type'    // s // &
                 'name'    // s // &
                 'sn'      // s // &
                 'meta'    // s // &
                 'x'       // s // &
                 'y'       // s // &
                 'z'       // s // &
                 'lon'     // s // &
                 'lat'     // s // &
                 'alt'
    end function dm_csv_header_sensor

    function dm_csv_header_target(separator) result(header)
        !! Returns header string of CSV representation of the target type as
        !! allocatable string.
        character, intent(in), optional :: separator !! CSV separator.
        character(len=:), allocatable   :: header    !! CSV header string.

        character :: s

        s = dm_present(separator, CSV_SEPARATOR)

        header = '#id'   // s // &
                 'name'  // s // &
                 'meta'  // s // &
                 'state' // s // &
                 'x'     // s // &
                 'y'     // s // &
                 'z'     // s // &
                 'lon'   // s // &
                 'lat'   // s // &
                 'alt'
    end function dm_csv_header_target

    ! **************************************************************************
    ! PRIVATE PROCEDURES.
    ! **************************************************************************
    function csv_from_beat(beat, separator) result(csv)
        !! Returns allocatable string of beat in CSV format.
        use :: dm_beat

        type(beat_type), intent(inout)        :: beat      !! Beat type.
        character,       intent(in), optional :: separator !! CSV separator.
        character(len=:), allocatable         :: csv       !! Allocatable CSV string.

        character :: s

        s = dm_present(separator, CSV_SEPARATOR)

        csv = trim(beat%node_id)     // s // &
              trim(beat%client)      // s // &
              trim(beat%address)     // s // &
              trim(beat%time_sent)   // s // &
              trim(beat%time_recv)   // s // &
              dm_itoa(beat%error)    // s // &
              dm_itoa(beat%interval) // s // &
              dm_itoa(beat%uptime)
    end function csv_from_beat

    function csv_from_beats(beats, header, separator) result(csv)
        !! Returns allocatable string of beats in CSV format.
        use :: dm_beat

        type(beat_type), intent(inout)        :: beats(:)  !! Beat array.
        logical,         intent(in), optional :: header    !! CSV header flag.
        character,       intent(in), optional :: separator !! CSV separator.
        character(len=:), allocatable         :: csv       !! Allocatable CSV string.

        character :: s
        integer   :: i

        s = dm_present(separator, CSV_SEPARATOR)

        if (dm_present(header, .false.)) then
            csv = dm_csv_header_beat(s) // ASCII_LF
        else
            csv = ''
        end if

        do i = 1, size(beats)
            csv = csv // dm_csv_from(beats(i), s) // ASCII_LF
        end do
    end function csv_from_beats

    function csv_from_data_point(dp, separator) result(csv)
        ! Returns allocatable string of data point in CSV format.
        use :: dm_dp

        type(dp_type), intent(inout)        :: dp        !! Data point type.
        character,     intent(in), optional :: separator !! CSV separator.
        character(len=:), allocatable       :: csv       !! Allocatable CSV string.

        character :: s

        s = dm_present(separator, CSV_SEPARATOR)

        csv = trim(dp%x) // s // dm_ftoa(dp%y)
    end function csv_from_data_point

    function csv_from_data_points(data_points, header, separator) result(csv)
        !! Returns allocatable string of data points in CSV format.
        use :: dm_dp

        type(dp_type), intent(inout)        :: data_points(:) !! Data point array.
        logical,       intent(in), optional :: header         !! CSV header flag.
        character,     intent(in), optional :: separator      !! CSV separator.
        character(len=:), allocatable       :: csv            !! Allocatable CSV string.

        character :: s
        integer   :: i

        s = dm_present(separator, CSV_SEPARATOR)

        if (dm_present(header, .false.)) then
            csv = dm_csv_header_data_point(s) // ASCII_LF
        else
            csv = ''
        end if

        do i = 1, size(data_points)
            csv = csv // dm_csv_from(data_points(i), s) // ASCII_LF
        end do
    end function csv_from_data_points

    function csv_from_log(log, separator) result(csv)
        !! Returns allocatable string of log in CSV format: id, level, error,
        !! timestamp, node_id, sensor_id, target_id, observ_id, message.
        use :: dm_log

        type(log_type), intent(inout)        :: log       !! Log data.
        character,      intent(in), optional :: separator !! CSV field separator.
        character(len=:), allocatable        :: csv       !! Allocatable CSV string.

        character :: s

        s = dm_present(separator, CSV_SEPARATOR)

        csv = trim(log%id)        // s // &
              dm_itoa(log%level)  // s // &
              dm_itoa(log%error)  // s // &
              trim(log%timestamp) // s // &
              trim(log%node_id)   // s // &
              trim(log%sensor_id) // s // &
              trim(log%target_id) // s // &
              trim(log%observ_id) // s // &
              '"' // trim(log%message) // '"'
    end function csv_from_log

    function csv_from_logs(logs, header, separator) result(csv)
        !! Returns allocatable string of logs in CSV format.
        use :: dm_log

        type(log_type),   intent(inout)        :: logs(:)   !! Array of log data.
        logical,          intent(in), optional :: header    !! CSV header flag.
        character,        intent(in), optional :: separator !! CSV separator.
        character(len=:), allocatable          :: csv       !! Allocatable CSV string.

        character :: s
        integer   :: i

        s = dm_present(separator, CSV_SEPARATOR)

        if (dm_present(header, .false.)) then
            csv = dm_csv_header_log(s) // ASCII_LF
        else
            csv = ''
        end if

        do i = 1, size(logs)
            csv = csv // dm_csv_from(logs(i), s) // ASCII_LF
        end do
    end function csv_from_logs

    function csv_from_node(node, separator) result(csv)
        !! Returns allocatable string of node in CSV format.
        use :: dm_node

        type(node_type), intent(inout)        :: node      !! Node type.
        character,       intent(in), optional :: separator !! CSV separator.
        character(len=:), allocatable         :: csv       !! Allocatable CSV string.

        character :: s

        s = dm_present(separator, CSV_SEPARATOR)

        csv = trim(node%id)                 // s // &
              trim(node%name)               // s // &
              '"' // trim(node%meta) // '"' // s // &
              dm_ftoa(node%x)               // s // &
              dm_ftoa(node%y)               // s // &
              dm_ftoa(node%z)               // s // &
              dm_ftoa(node%lon)             // s // &
              dm_ftoa(node%lat)             // s // &
              dm_ftoa(node%alt)
    end function csv_from_node

    function csv_from_nodes(nodes, header, separator) result(csv)
        !! Returns allocatable string of nodes in CSV format.
        use :: dm_node

        type(node_type),  intent(inout)        :: nodes(:)  !! Nodes array.
        logical,          intent(in), optional :: header    !! CSV header flag.
        character,        intent(in), optional :: separator !! CSV separator.
        character(len=:), allocatable          :: csv       !! Allocatable CSV string.

        character :: s
        integer   :: i

        s = dm_present(separator, CSV_SEPARATOR)

        if (dm_present(header, .false.)) then
            csv = dm_csv_header_node(s) // ASCII_LF
        else
            csv = ''
        end if

        do i = 1, size(nodes)
            csv = csv // dm_csv_from(nodes(i), s) // ASCII_LF
        end do
    end function csv_from_nodes

    function csv_from_observ(observ, separator) result(csv)
        !! Returns allocatable string of observation in CSV format.
        use :: dm_observ
        use :: dm_request
        use :: dm_response

        type(observ_type), intent(inout)        :: observ    !! Observation data.
        character,         intent(in), optional :: separator !! CSV separator.
        character(len=:), allocatable           :: csv       !! Allocatable CSV string.

        character :: s
        integer   :: i, j

        s = dm_present(separator, CSV_SEPARATOR)

        csv = trim(observ%id)            // s // &
              trim(observ%node_id)       // s // &
              trim(observ%sensor_id)     // s // &
              trim(observ%target_id)     // s // &
              trim(observ%name)          // s // &
              trim(observ%timestamp)     // s // &
              trim(observ%source)        // s // &
              trim(observ%device)        // s // &
              dm_itoa(observ%priority)   // s // &
              dm_itoa(observ%error)      // s // &
              dm_itoa(observ%next)       // s // &
              dm_itoa(observ%nreceivers) // s // &
              dm_itoa(observ%nrequests)

        do i = 1, OBSERV_MAX_NRECEIVERS
            csv = csv // s // trim(observ%receivers(i))
        end do


        do i = 1, OBSERV_MAX_NREQUESTS
            if (i > observ%nrequests) then
                csv = csv // repeat(s, 11 + (REQUEST_MAX_NRESPONSES * 4))
                cycle
            end if

            csv = csv // s // trim(observ%requests(i)%name)                    // s // &
                              trim(observ%requests(i)%timestamp)               // s // &
                              '"' // trim(observ%requests(i)%request)   // '"' // s // &
                              '"' // trim(observ%requests(i)%response)  // '"' // s // &
                              '"' // trim(observ%requests(i)%delimiter) // '"' // s // &
                              '"' // trim(observ%requests(i)%pattern)   // '"' // s // &
                              dm_itoa(observ%requests(i)%delay)                // s // &
                              dm_itoa(observ%requests(i)%error)                // s // &
                              dm_itoa(observ%requests(i)%mode)                 // s // &
                              dm_itoa(observ%requests(i)%retries)              // s // &
                              dm_itoa(observ%requests(i)%state)                // s // &
                              dm_itoa(observ%requests(i)%timeout)              // s // &
                              dm_itoa(observ%requests(i)%nresponses)

            do j = 1, REQUEST_MAX_NRESPONSES
                if (j > observ%requests(i)%nresponses) then
                    csv = csv // repeat(s, 4)
                    cycle
                end if

                csv = csv // s // trim(observ%requests(i)%responses(j)%name)     // s // &
                                  trim(observ%requests(i)%responses(j)%unit)     // s // &
                                  dm_itoa(observ%requests(i)%responses(j)%type)  // s // &
                                  dm_itoa(observ%requests(i)%responses(j)%error) // s // &
                                  dm_ftoa(observ%requests(i)%responses(j)%value)
            end do
        end do
    end function csv_from_observ

    function csv_from_observ_view(view, separator) result(csv)
        !! Returns allocatable string of observation view (stub observation without
        !! receivers, requests, responses) in CSV format.
        use :: dm_observ

        type(observ_view_type), intent(inout)        :: view      !! Observation view type.
        character,              intent(in), optional :: separator !! CSV separator.
        character(len=:), allocatable                :: csv       !! Allocatable CSV string.

        character :: s

        s = dm_present(separator, CSV_SEPARATOR)

        csv = trim(view%node_id)           // s // &
              trim(view%sensor_id)         // s // &
              trim(view%target_id)         // s // &
              trim(view%observ_id)         // s // &
              trim(view%observ_name)       // s // &
              dm_itoa(view%observ_error)   // s // &
              trim(view%request_name)      // s // &
              trim(view%request_timestamp) // s // &
              dm_itoa(view%request_error)  // s // &
              trim(view%response_name)     // s // &
              trim(view%response_unit)     // s // &
              dm_itoa(view%response_type)  // s // &
              dm_itoa(view%response_error) // s // &
              dm_ftoa(view%response_value)
    end function csv_from_observ_view

    function csv_from_observ_views(views, header, separator) result(csv)
        !! Returns allocatable string of observation views in CSV format.
        use :: dm_observ

        type(observ_view_type), intent(inout)        :: views(:)   !! Array of observation views.
        logical,                intent(in), optional :: header     !! CSV header flag.
        character,              intent(in), optional :: separator  !! CSV separator.
        character(len=:), allocatable                :: csv        !! Allocatable CSV string.

        character :: s
        integer   :: i

        s = dm_present(separator, CSV_SEPARATOR)

        if (dm_present(header, .false.)) then
            csv = dm_csv_header_observ_view(s) // ASCII_LF
        else
            csv = ''
        end if

        do i = 1, size(views)
            csv = csv // dm_csv_from(views(i), s) // ASCII_LF
        end do
    end function csv_from_observ_views

    function csv_from_observs(observs, header, separator) result(csv)
        !! Returns allocatable string of observations in CSV format.
        use :: dm_observ

        type(observ_type), intent(inout)        :: observs(:) !! Array of observations.
        logical,           intent(in), optional :: header     !! CSV header flag.
        character,         intent(in), optional :: separator  !! CSV separator.
        character(len=:), allocatable           :: csv        !! Allocatable CSV string.

        character :: s
        integer   :: i

        s = dm_present(separator, CSV_SEPARATOR)

        if (dm_present(header, .false.)) then
            csv = dm_csv_header_observ(s) // ASCII_LF
        else
            csv = ''
        end if

        do i = 1, size(observs)
            csv = csv // dm_csv_from(observs(i), s) // ASCII_LF
        end do
    end function csv_from_observs

    function csv_from_sensor(sensor, separator) result(csv)
        !! Returns allocatable string of sensor in CSV format.
        use :: dm_sensor

        type(sensor_type), intent(inout)        :: sensor    !! Sensor type.
        character,         intent(in), optional :: separator !! CSV separator.
        character(len=:), allocatable           :: csv       !! Allocatable CSV string.

        character :: s

        s = dm_present(separator, CSV_SEPARATOR)

        csv = trim(sensor%id)                 // s // &
              trim(sensor%node_id)            // s // &
              dm_itoa(sensor%type)            // s // &
              trim(sensor%name)               // s // &
              trim(sensor%sn)                 // s // &
              '"' // trim(sensor%meta) // '"' // s // &
              dm_ftoa(sensor%x)               // s // &
              dm_ftoa(sensor%y)               // s // &
              dm_ftoa(sensor%z)               // s // &
              dm_ftoa(sensor%lon)             // s // &
              dm_ftoa(sensor%lat)             // s // &
              dm_ftoa(sensor%alt)
    end function csv_from_sensor

    function csv_from_sensors(sensors, header, separator) result(csv)
        !! Returns allocatable string of sensors in CSV format.
        use :: dm_sensor

        type(sensor_type), intent(inout)        :: sensors(:) !! Sensors array.
        logical,           intent(in), optional :: header     !! CSV header flag.
        character,         intent(in), optional :: separator  !! CSV separator.
        character(len=:), allocatable           :: csv        !! Allocatable CSV string.

        character :: s
        integer   :: i

        s = dm_present(separator, CSV_SEPARATOR)

        if (dm_present(header, .false.)) then
            csv = dm_csv_header_sensor(s) // ASCII_LF
        else
            csv = ''
        end if

        do i = 1, size(sensors)
            csv = csv // dm_csv_from(sensors(i), s) // ASCII_LF
        end do
    end function csv_from_sensors

    function csv_from_target(target, separator) result(csv)
        !! Returns allocatable string of target in CSV format.
        use :: dm_target

        type(target_type), intent(inout)        :: target    !! Target type.
        character,         intent(in), optional :: separator !! CSV separator.
        character(len=:), allocatable           :: csv       !! Allocatable CSV string.

        character :: s

        s = dm_present(separator, CSV_SEPARATOR)

        csv = trim(target%id)                 // s // &
              trim(target%name)               // s // &
              '"' // trim(target%meta) // '"' // s // &
              dm_itoa(target%state)           // s // &
              dm_ftoa(target%x)               // s // &
              dm_ftoa(target%y)               // s // &
              dm_ftoa(target%z)               // s // &
              dm_ftoa(target%lon)             // s // &
              dm_ftoa(target%lat)             // s // &
              dm_ftoa(target%alt)
    end function csv_from_target

    function csv_from_targets(targets, header, separator) result(csv)
        !! Returns allocatable string of targets in CSV format.
        use :: dm_target

        type(target_type), intent(inout)        :: targets(:) !! Targets array.
        logical,           intent(in), optional :: header     !! CSV header flag.
        character,         intent(in), optional :: separator  !! CSV separator.
        character(len=:), allocatable           :: csv        !! Allocatable CSV string.

        character :: s
        integer   :: i

        s = dm_present(separator, CSV_SEPARATOR)

        if (dm_present(header, .false.)) then
            csv = dm_csv_header_target(s) // ASCII_LF
        else
            csv = ''
        end if

        do i = 1, size(targets)
            csv = csv // dm_csv_from(targets(i), s) // ASCII_LF
        end do
    end function csv_from_targets

    integer function csv_next_int32(input, output, separator, limit, pos, quote) result(rc)
        !! Reads next 4-byte integer until separator.
        character(len=*), intent(inout)        :: input     !! Input string to parse.
        integer(kind=i4), intent(out)          :: output    !! Output integer.
        character,        intent(in)           :: separator !! CSV field separator.
        integer,          intent(in)           :: limit     !! Total length of input string.
        integer,          intent(inout)        :: pos       !! Position of last separator on input/output.
        character,        intent(in), optional :: quote     !! Quote character, enables unquoting.

        character :: q
        integer   :: old

        q   = dm_present(quote, ASCII_NUL)
        old = pos
        rc  = csv_parse(input, separator, limit, pos)
        if (dm_is_error(rc)) return
        call dm_string_to(input(old + 1:pos - 1), output, error=rc)
    end function csv_next_int32

    integer function csv_next_int64(input, output, separator, limit, pos, quote) result(rc)
        !! Reads next 8-byte integer until separator.
        character(len=*), intent(inout)        :: input     !! Input string to parse.
        integer(kind=i8), intent(out)          :: output    !! Output integer.
        character,        intent(in)           :: separator !! CSV field separator.
        integer,          intent(in)           :: limit     !! Total length of input string.
        integer,          intent(inout)        :: pos       !! Position of last separator on input/output.
        character,        intent(in), optional :: quote     !! Quote character, enables unquoting.

        character :: q
        integer   :: old

        q   = dm_present(quote, ASCII_NUL)
        old = pos
        rc  = csv_parse(input, separator, limit, pos, q)
        if (dm_is_error(rc)) return
        call dm_string_to(input(old + 1:pos - 1), output, error=rc)
    end function csv_next_int64

    integer function csv_next_real32(input, output, separator, limit, pos, quote) result(rc)
        !! Reads next 4-byte real until separator.
        character(len=*), intent(inout)        :: input     !! Input string to parse.
        real(kind=r4),    intent(out)          :: output    !! Output real.
        character,        intent(in)           :: separator !! CSV field separator.
        integer,          intent(in)           :: limit     !! Total length of input string.
        integer,          intent(inout)        :: pos       !! Position of last separator on input/output.
        character,        intent(in), optional :: quote     !! Quote character, enables unquoting.

        character :: q
        integer   :: old

        q   = dm_present(quote, ASCII_NUL)
        old = pos
        rc  = csv_parse(input, separator, limit, pos, q)
        if (dm_is_error(rc)) return
        call dm_string_to(input(old + 1:pos - 1), output, error=rc)
    end function csv_next_real32

    integer function csv_next_real64(input, output, separator, limit, pos, quote) result(rc)
        !! Reads next 8-byte real until separator.
        character(len=*), intent(inout)        :: input     !! Input string to parse.
        real(kind=r8),    intent(out)          :: output    !! Output real.
        character,        intent(in)           :: separator !! CSV field separator.
        integer,          intent(in)           :: limit     !! Total length of input string.
        integer,          intent(inout)        :: pos       !! Position of last separator on input/output.
        character,        intent(in), optional :: quote     !! Quote character, enables unquoting.

        character :: q
        integer   :: old

        q   = dm_present(quote, ASCII_NUL)
        old = pos
        rc  = csv_parse(input, separator, limit, pos, q)
        if (dm_is_error(rc)) return
        call dm_string_to(input(old + 1:pos - 1), output, error=rc)
    end function csv_next_real64

    integer function csv_next_string(input, output, separator, limit, pos, quote) result(rc)
        !! Reads next character string until separator.
        character(len=*), intent(inout)        :: input     !! Input string to parse.
        character(len=*), intent(inout)        :: output    !! Output string (must be large enough to hold field value).
        character,        intent(in)           :: separator !! CSV field separator.
        integer,          intent(in)           :: limit     !! Total length of input string.
        integer,          intent(inout)        :: pos       !! Position of last separator on input/output.
        character,        intent(in), optional :: quote     !! Quote character, enables unquoting.

        integer :: old
        logical :: quoted

        quoted = .false.
        if (present(quote)) quoted = (quote /= ASCII_NUL)

        old = pos

        if (quoted) then
            rc = csv_parse(input, separator, limit, pos, quote)
        else
            rc = csv_parse(input, separator, limit, pos)
        end if

        if (dm_is_error(rc)) return
        output = input(old + 1:pos - 1)
        if (quoted) call csv_unquote(output, quote)
    end function csv_next_string

    integer function csv_parse(string, separator, limit, pos, quote) result(rc)
        !! Returns position of next separator character in `pos`. If no
        !! separator was found, `pos` is set to `limit`.
        character(len=*), intent(inout)        :: string    !! Input string.
        character,        intent(in)           :: separator !! Separator character.
        integer,          intent(in)           :: limit     !! Length of complete string.
        integer,          intent(inout)        :: pos       !! Position of last/next separator.
        character,        intent(in), optional :: quote     !! Quote character.

        character :: a
        integer   :: i
        logical   :: q, f

        rc = E_BOUNDS
        if (pos < 0 .or. pos > limit) return

        a = ASCII_NUL
        f = .false.

        if (present(quote)) then
            a = quote
            if (a /= ASCII_NUL) q = .true.
        end if

        do i = pos + 1, limit + 1
            pos = i
            if (i > limit) exit
            if (q .and. string(i:i) == a) f = .not. f
            if (.not. f .and. string(i:i) == separator) exit
        end do

        rc = E_NONE
    end function csv_parse

    integer function csv_read_log(log, unit, separator, quote) result(rc)
        !! Reads log from file or standard input. If no separator character is
        !! passed, the default one will be used (comma). If a quote character
        !! is given, separators within quoted strings will be ignored.
        use :: dm_log

        type(log_type), intent(out)          :: log       !! Log type.
        integer,        intent(in), optional :: unit      !! File unit.
        character,      intent(in), optional :: separator !! CSV separator.
        character,      intent(in), optional :: quote     !! CSV quote character.

        character(len=CSV_BUFFER_LEN) :: buffer

        character :: q, s
        integer   :: n, p
        integer   :: unit_, stat

        unit_ = dm_present(unit, stdin)
        s     = dm_present(separator, CSV_SEPARATOR)
        q     = dm_present(quote, ASCII_NUL)

        rc = E_READ
        read (unit_, '(a)', iostat=stat) buffer

        rc = E_EOF
        if (is_iostat_end(stat)) return

        rc = E_EOR
        if (is_iostat_eor(stat)) return

        n = len_trim(buffer)
        if (n == 0) return
        if (buffer(1:1) == '#') return

        p = 0 ! Cursor in buffer string.

        rc = csv_next(buffer, log%id,        s, n, p, q); if (rc /= E_NONE) return
        rc = csv_next(buffer, log%level,     s, n, p, q); if (rc /= E_NONE) return
        rc = csv_next(buffer, log%error,     s, n, p, q); if (rc /= E_NONE) return
        rc = csv_next(buffer, log%timestamp, s, n, p, q); if (rc /= E_NONE) return
        rc = csv_next(buffer, log%node_id,   s, n, p, q); if (rc /= E_NONE) return
        rc = csv_next(buffer, log%sensor_id, s, n, p, q); if (rc /= E_NONE) return
        rc = csv_next(buffer, log%target_id, s, n, p, q); if (rc /= E_NONE) return
        rc = csv_next(buffer, log%observ_id, s, n, p, q); if (rc /= E_NONE) return
        rc = csv_next(buffer, log%source,    s, n, p, q); if (rc /= E_NONE) return
        rc = csv_next(buffer, log%message,   s, n, p, q); if (rc /= E_NONE) return

        rc = E_NONE
    end function csv_read_log

    integer function csv_read_node(node, unit, separator, quote) result(rc)
        !! Reads node from file or standard input. If no separator character is
        !! passed, the default one will be used (comma). If a quote character
        !! is given, separators within quoted strings will be ignored.
        use :: dm_node

        type(node_type), intent(out)          :: node      !! Node type.
        integer,         intent(in), optional :: unit      !! File unit.
        character,       intent(in), optional :: separator !! CSV separator.
        character,       intent(in), optional :: quote     !! CSV quote character.

        character(len=CSV_BUFFER_LEN) :: buffer

        character :: q, s
        integer   :: n, p
        integer   :: unit_, stat

        unit_ = dm_present(unit, stdin)
        s     = dm_present(separator, CSV_SEPARATOR)
        q     = dm_present(quote, ASCII_NUL)

        rc = E_READ
        read (unit_, '(a)', iostat=stat) buffer

        rc = E_EOF
        if (is_iostat_end(stat)) return

        rc = E_EOR
        if (is_iostat_eor(stat)) return

        n = len_trim(buffer)
        if (n == 0) return
        if (buffer(1:1) == '#') return

        p = 0 ! Cursor in buffer string.

        rc = csv_next(buffer, node%id,   s, n, p, q); if (rc /= E_NONE) return
        rc = csv_next(buffer, node%name, s, n, p, q); if (rc /= E_NONE) return
        rc = csv_next(buffer, node%meta, s, n, p, q); if (rc /= E_NONE) return
        rc = csv_next(buffer, node%x,    s, n, p, q); if (rc /= E_NONE) return
        rc = csv_next(buffer, node%y,    s, n, p, q); if (rc /= E_NONE) return
        rc = csv_next(buffer, node%z,    s, n, p, q); if (rc /= E_NONE) return
        rc = csv_next(buffer, node%lon,  s, n, p, q); if (rc /= E_NONE) return
        rc = csv_next(buffer, node%lat,  s, n, p, q); if (rc /= E_NONE) return
        rc = csv_next(buffer, node%alt,  s, n, p, q); if (rc /= E_NONE) return

        rc = E_NONE
    end function csv_read_node

    integer function csv_read_observ(observ, unit, separator, quote) result(rc)
        !! Reads observation from file or standard input. If no separator
        !! character is passed, the default one will be used (comma). If a
        !! quote character is given, separators within quoted strings will be
        !! ignored.
        use :: dm_observ
        use :: dm_request
        use :: dm_response

        type(observ_type), intent(out)          :: observ    !! Observation type.
        integer,           intent(in), optional :: unit      !! File unit.
        character,         intent(in), optional :: separator !! CSV separator.
        character,         intent(in), optional :: quote     !! CSV quote character.

        character(len=CSV_BUFFER_LEN) :: buffer

        character :: q, s
        integer   :: i, j, n, p
        integer   :: unit_, stat

        unit_ = dm_present(unit, stdin)
        s     = dm_present(separator, CSV_SEPARATOR)
        q     = dm_present(quote, ASCII_NUL)

        read (unit_, '(a)', iostat=stat) buffer

        rc = E_EOF
        if (is_iostat_end(stat)) return

        rc = E_EOR
        if (is_iostat_eor(stat)) return

        n = len_trim(buffer)
        if (n == 0) return
        if (buffer(1:1) == '#') return

        p = 0 ! Cursor in buffer string.

        rc = csv_next(buffer, observ%id,         s, n, p, q); if (rc /= E_NONE) return
        rc = csv_next(buffer, observ%node_id,    s, n, p, q); if (rc /= E_NONE) return
        rc = csv_next(buffer, observ%sensor_id,  s, n, p, q); if (rc /= E_NONE) return
        rc = csv_next(buffer, observ%target_id,  s, n, p, q); if (rc /= E_NONE) return
        rc = csv_next(buffer, observ%name,       s, n, p, q); if (rc /= E_NONE) return
        rc = csv_next(buffer, observ%timestamp,  s, n, p, q); if (rc /= E_NONE) return
        rc = csv_next(buffer, observ%source,     s, n, p, q); if (rc /= E_NONE) return
        rc = csv_next(buffer, observ%device,     s, n, p, q); if (rc /= E_NONE) return
        rc = csv_next(buffer, observ%priority,   s, n, p, q); if (rc /= E_NONE) return
        rc = csv_next(buffer, observ%error,      s, n, p, q); if (rc /= E_NONE) return
        rc = csv_next(buffer, observ%next,       s, n, p, q); if (rc /= E_NONE) return
        rc = csv_next(buffer, observ%nreceivers, s, n, p, q); if (rc /= E_NONE) return
        rc = csv_next(buffer, observ%nrequests,  s, n, p, q); if (rc /= E_NONE) return

        do i = 1, OBSERV_MAX_NRECEIVERS
            rc = csv_next(buffer, observ%receivers(i), s, n, p, q)
            if (rc /= E_NONE) return
        end do

        do i = 1, OBSERV_MAX_NREQUESTS
            rc = csv_next(buffer, observ%requests(i)%name,       s, n, p, q); if (rc /= E_NONE) return
            rc = csv_next(buffer, observ%requests(i)%timestamp,  s, n, p, q); if (rc /= E_NONE) return
            rc = csv_next(buffer, observ%requests(i)%request,    s, n, p, q); if (rc /= E_NONE) return
            rc = csv_next(buffer, observ%requests(i)%response,   s, n, p, q); if (rc /= E_NONE) return
            rc = csv_next(buffer, observ%requests(i)%delimiter,  s, n, p, q); if (rc /= E_NONE) return
            rc = csv_next(buffer, observ%requests(i)%pattern,    s, n, p, q); if (rc /= E_NONE) return
            rc = csv_next(buffer, observ%requests(i)%delay,      s, n, p, q); if (rc /= E_NONE) return
            rc = csv_next(buffer, observ%requests(i)%error,      s, n, p, q); if (rc /= E_NONE) return
            rc = csv_next(buffer, observ%requests(i)%mode,       s, n, p, q); if (rc /= E_NONE) return
            rc = csv_next(buffer, observ%requests(i)%retries,    s, n, p, q); if (rc /= E_NONE) return
            rc = csv_next(buffer, observ%requests(i)%state,      s, n, p, q); if (rc /= E_NONE) return
            rc = csv_next(buffer, observ%requests(i)%timeout,    s, n, p, q); if (rc /= E_NONE) return
            rc = csv_next(buffer, observ%requests(i)%nresponses, s, n, p, q); if (rc /= E_NONE) return

            do j = 1, REQUEST_MAX_NRESPONSES
                rc = csv_next(buffer, observ%requests(i)%responses(j)%name,  s, n, p, q); if (rc /= E_NONE) return
                rc = csv_next(buffer, observ%requests(i)%responses(j)%unit,  s, n, p, q); if (rc /= E_NONE) return
                rc = csv_next(buffer, observ%requests(i)%responses(j)%type,  s, n, p, q); if (rc /= E_NONE) return
                rc = csv_next(buffer, observ%requests(i)%responses(j)%error, s, n, p, q); if (rc /= E_NONE) return
                rc = csv_next(buffer, observ%requests(i)%responses(j)%value, s, n, p, q); if (rc /= E_NONE) return
            end do
        end do

        rc = E_NONE
    end function csv_read_observ

    integer function csv_read_sensor(sensor, unit, separator, quote) result(rc)
        !! Reads sensor from file or standard input. If no separator character is
        !! passed, the default one will be used (comma). If a quote character
        !! is given, separators within quoted strings will be ignored.
        use :: dm_sensor

        type(sensor_type), intent(out)          :: sensor    !! Sensor type.
        integer,           intent(in), optional :: unit      !! File unit.
        character,         intent(in), optional :: separator !! CSV separator.
        character,         intent(in), optional :: quote     !! CSV quote character.

        character(len=CSV_BUFFER_LEN) :: buffer

        character :: q, s
        integer   :: n, p
        integer   :: unit_, stat

        unit_ = dm_present(unit, stdin)
        s     = dm_present(separator, CSV_SEPARATOR)
        q     = dm_present(quote, ASCII_NUL)

        rc = E_READ
        read (unit_, '(a)', iostat=stat) buffer

        rc = E_EOF
        if (is_iostat_end(stat)) return

        rc = E_EOR
        if (is_iostat_eor(stat)) return

        n = len_trim(buffer)
        if (n == 0) return
        if (buffer(1:1) == '#') return

        p = 0 ! Cursor in buffer string.

        rc = csv_next(buffer, sensor%id,      s, n, p, q); if (rc /= E_NONE) return
        rc = csv_next(buffer, sensor%node_id, s, n, p, q); if (rc /= E_NONE) return
        rc = csv_next(buffer, sensor%type,    s, n, p, q); if (rc /= E_NONE) return
        rc = csv_next(buffer, sensor%name,    s, n, p, q); if (rc /= E_NONE) return
        rc = csv_next(buffer, sensor%sn,      s, n, p, q); if (rc /= E_NONE) return
        rc = csv_next(buffer, sensor%meta,    s, n, p, q); if (rc /= E_NONE) return
        rc = csv_next(buffer, sensor%x,       s, n, p, q); if (rc /= E_NONE) return
        rc = csv_next(buffer, sensor%y,       s, n, p, q); if (rc /= E_NONE) return
        rc = csv_next(buffer, sensor%z,       s, n, p, q); if (rc /= E_NONE) return
        rc = csv_next(buffer, sensor%lon,     s, n, p, q); if (rc /= E_NONE) return
        rc = csv_next(buffer, sensor%lat,     s, n, p, q); if (rc /= E_NONE) return
        rc = csv_next(buffer, sensor%alt,     s, n, p, q); if (rc /= E_NONE) return

        rc = E_NONE
    end function csv_read_sensor

    integer function csv_read_target(target, unit, separator, quote) result(rc)
        !! Reads target from file or standard input. If no separator character is
        !! passed, the default one will be used (comma). If a quote character
        !! is given, separators within quoted strings will be ignored.
        use :: dm_target

        type(target_type), intent(out)          :: target    !! Target type.
        integer,           intent(in), optional :: unit      !! File unit.
        character,         intent(in), optional :: separator !! CSV separator.
        character,         intent(in), optional :: quote     !! CSV quote character.

        character(len=CSV_BUFFER_LEN) :: buffer

        character :: q, s
        integer   :: n, p
        integer   :: unit_, stat

        unit_ = dm_present(unit, stdin)
        s     = dm_present(separator, CSV_SEPARATOR)
        q     = dm_present(quote, ASCII_NUL)

        rc = E_READ
        read (unit_, '(a)', iostat=stat) buffer

        rc = E_EOF
        if (is_iostat_end(stat)) return

        rc = E_EOR
        if (is_iostat_eor(stat)) return

        n = len_trim(buffer)
        if (n == 0) return
        if (buffer(1:1) == '#') return

        p = 0 ! Cursor in buffer string.

        rc = csv_next(buffer, target%id,    s, n, p, q); if (rc /= E_NONE) return
        rc = csv_next(buffer, target%name,  s, n, p, q); if (rc /= E_NONE) return
        rc = csv_next(buffer, target%meta,  s, n, p, q); if (rc /= E_NONE) return
        rc = csv_next(buffer, target%state, s, n, p, q); if (rc /= E_NONE) return
        rc = csv_next(buffer, target%x,     s, n, p, q); if (rc /= E_NONE) return
        rc = csv_next(buffer, target%y,     s, n, p, q); if (rc /= E_NONE) return
        rc = csv_next(buffer, target%z,     s, n, p, q); if (rc /= E_NONE) return
        rc = csv_next(buffer, target%lon,   s, n, p, q); if (rc /= E_NONE) return
        rc = csv_next(buffer, target%lat,   s, n, p, q); if (rc /= E_NONE) return
        rc = csv_next(buffer, target%alt,   s, n, p, q); if (rc /= E_NONE) return

        rc = E_NONE
    end function csv_read_target

    integer function csv_write_beat(beat, unit, header, separator) result(rc)
        !! Writes beat to file or standard output.
        use :: dm_beat

        type(beat_type), intent(inout)        :: beat      !! Beat type.
        integer,         intent(in), optional :: unit      !! File unit.
        logical,         intent(in), optional :: header    !! CSV header flag.
        character,       intent(in), optional :: separator !! CSV separator.

        character :: s
        integer   :: unit_, stat

        rc = E_WRITE

        unit_ = dm_present(unit, stdout)
        s     = dm_present(separator, CSV_SEPARATOR)

        if (dm_present(header, .false.)) then
            write (unit_, '(a)', iostat=stat) dm_csv_header_beat(s)
            if (stat /= 0) return
        end if

        write (unit_, '(8a, 3(i0, a), i0)', iostat=stat) &
            trim(beat%node_id),   s, &
            trim(beat%client),    s, &
            trim(beat%address),   s, &
            trim(beat%time_sent), s, &
            trim(beat%time_recv), s, &
            beat%error,           s, &
            beat%interval,        s, &
            beat%uptime
        if (stat /= 0) return

        rc = E_NONE
    end function csv_write_beat

    integer function csv_write_beats(beats, unit, header, separator) result(rc)
        !! Writes beats to file or standard output.
        use :: dm_beat

        type(beat_type), intent(inout)        :: beats(:)  !! Beat array.
        integer,         intent(in), optional :: unit      !! File unit.
        logical,         intent(in), optional :: header    !! CSV header flag.
        character,       intent(in), optional :: separator !! CSV separator.

        character :: s
        integer   :: i, unit_
        logical   :: header_

        rc = E_WRITE

        unit_   = dm_present(unit, stdout)
        header_ = dm_present(header, .false.)
        s       = dm_present(separator, CSV_SEPARATOR)

        do i = 1, size(beats)
            rc = dm_csv_write(beats(i), unit_, header_, s)
            if (dm_is_error(rc)) exit
            header_ = .false.
        end do
    end function csv_write_beats

    integer function csv_write_data_point(data_point, unit, header, separator) result(rc)
        !! Writes data point to file or standard output.
        use :: dm_dp

        type(dp_type), intent(inout)        :: data_point !! Data point type.
        integer,       intent(in), optional :: unit       !! File unit.
        logical,       intent(in), optional :: header     !! CSV header flag.
        character,     intent(in), optional :: separator  !! CSV separator.

        character :: s
        integer   :: unit_, stat

        rc = E_WRITE

        unit_ = dm_present(unit, stdout)
        s     = dm_present(separator, CSV_SEPARATOR)

        if (dm_present(header, .false.)) then
            write (unit_, '(a)', iostat=stat) dm_csv_header_data_point(s)
            if (stat /= 0) return
        end if

        write (unit_, '(a29, a1, ' // FMT_REAL // ')', iostat=stat) data_point%x, s, data_point%y
        if (stat /= 0) return

        rc = E_NONE
    end function csv_write_data_point

    integer function csv_write_data_points(data_points, unit, header, separator) result(rc)
        !! Writes data points to file or standard output.
        use :: dm_dp

        type(dp_type), intent(inout)        :: data_points(:) !! Data point array.
        integer,       intent(in), optional :: unit           !! File unit.
        logical,       intent(in), optional :: header         !! CSV header flag.
        character,     intent(in), optional :: separator      !! CSV separator.

        character :: s
        integer   :: i, unit_
        logical   :: header_

        rc = E_WRITE

        unit_   = dm_present(unit, stdout)
        header_ = dm_present(header, .false.)
        s       = dm_present(separator, CSV_SEPARATOR)

        do i = 1, size(data_points)
            rc = dm_csv_write(data_points(i), unit_, header_, s)
            if (dm_is_error(rc)) exit
            header_ = .false.
        end do
    end function csv_write_data_points

    integer function csv_write_log(log, unit, header, separator) result(rc)
        !! Writes log to file or standard output.
        use :: dm_log

        type(log_type), intent(inout)        :: log       !! Log type.
        integer,        intent(in), optional :: unit      !! File unit.
        logical,        intent(in), optional :: header    !! CSV header flag.
        character,      intent(in), optional :: separator !! CSV field separator.

        character :: s
        integer   :: unit_, stat

        rc = E_WRITE

        unit_ = dm_present(unit, stdout)
        s     = dm_present(separator, CSV_SEPARATOR)

        if (dm_present(header, .false.)) then
            write (unit_, '(a)', iostat=stat) dm_csv_header_log(s)
            if (stat /= 0) return
        end if

        write (unit_, '(2a, 2(i0, a), 15a)', iostat=stat) &
            trim(log%id),        s, &
            log%level,           s, &
            log%error,           s, &
            trim(log%timestamp), s, &
            trim(log%node_id),   s, &
            trim(log%sensor_id), s, &
            trim(log%target_id), s, &
            trim(log%observ_id), s, &
            trim(log%source),    s, &
            '"', trim(log%message), '"'
        if (stat /= 0) return

        rc = E_NONE
    end function csv_write_log

    integer function csv_write_logs(logs, unit, header, separator) result(rc)
        !! Writes logs to file or standard output.
        use :: dm_log

        type(log_type), intent(inout)        :: logs(:)   !! Log array.
        integer,        intent(in), optional :: unit      !! File unit.
        logical,        intent(in), optional :: header    !! CSV header flag.
        character,      intent(in), optional :: separator !! CSV field separator.

        character :: s
        integer   :: i, unit_
        logical   :: header_

        rc = E_WRITE

        unit_   = dm_present(unit, stdout)
        header_ = dm_present(header, .false.)
        s       = dm_present(separator, CSV_SEPARATOR)

        do i = 1, size(logs)
            rc = dm_csv_write(logs(i), unit_, header_, s)
            if (dm_is_error(rc)) exit
            header_ = .false.
        end do
    end function csv_write_logs

    integer function csv_write_node(node, unit, header, separator) result(rc)
        !! Writes node to file or standard output.
        use :: dm_node

        type(node_type), intent(inout)        :: node      !! Node type.
        integer,         intent(in), optional :: unit      !! File unit.
        logical,         intent(in), optional :: header    !! CSV header flag.
        character,       intent(in), optional :: separator !! CSV separator.

        character :: s
        integer   :: unit_, stat

        rc = E_WRITE

        unit_ = dm_present(unit, stdout)
        s     = dm_present(separator, CSV_SEPARATOR)

        if (dm_present(header, .false.)) then
            write (unit_, '(a)', iostat=stat) dm_csv_header_node(s)
            if (stat /= 0) return
        end if

        write (unit_, '(7a, 6(a, ' // FMT_REAL // '))', iostat=stat) &
            trim(node%id),             s, &
            trim(node%name),           s, &
            '"', trim(node%meta), '"', s, &
            node%x,                    s, &
            node%y,                    s, &
            node%z,                    s, &
            node%lon,                  s, &
            node%lat,                  s, &
            node%alt
        if (stat /= 0) return

        rc = E_NONE
    end function csv_write_node

    integer function csv_write_nodes(nodes, unit, header, separator) result(rc)
        !! Writes nodes to file or standard output.
        use :: dm_node

        type(node_type), intent(inout)        :: nodes(:)  !! Node array.
        integer,         intent(in), optional :: unit      !! File unit.
        logical,         intent(in), optional :: header    !! CSV header flag.
        character,       intent(in), optional :: separator !! CSV separator.

        character :: s
        integer   :: i, unit_
        logical   :: header_

        rc = E_WRITE

        unit_   = dm_present(unit, stdout)
        header_ = dm_present(header, .false.)
        s       = dm_present(separator, CSV_SEPARATOR)

        do i = 1, size(nodes)
            rc = dm_csv_write(nodes(i), unit_, header_, s)
            if (dm_is_error(rc)) exit
            header_ = .false.
        end do
    end function csv_write_nodes

    integer function csv_write_observ(observ, unit, header, separator) result(rc)
        !! Writes observation to file or standard output.
        use :: dm_observ
        use :: dm_request
        use :: dm_response

        type(observ_type), intent(inout)        :: observ    !! Observation type.
        integer,           intent(in), optional :: unit      !! File unit.
        logical,           intent(in), optional :: header    !! CSV header flag.
        character,         intent(in), optional :: separator !! CSV separator.

        character :: s
        integer   :: i, j, unit_, stat

        rc = E_WRITE

        unit_ = dm_present(unit, stdout)
        s     = dm_present(separator, CSV_SEPARATOR)

        if (dm_present(header, .false.)) then
            write (unit_, '(a)', iostat=stat) dm_csv_header_observ(s)
            if (stat /= 0) return
        end if

        write (unit_, '(16a, 4(i0, a), i0)', advance='no', iostat=stat) &
              trim(observ%id),        s, &
              trim(observ%node_id),   s, &
              trim(observ%sensor_id), s, &
              trim(observ%target_id), s, &
              trim(observ%name),      s, &
              trim(observ%timestamp), s, &
              trim(observ%source),    s, &
              trim(observ%device),    s, &
              observ%priority,        s, &
              observ%error,           s, &
              observ%next,            s, &
              observ%nreceivers,      s, &
              observ%nrequests
        if (stat /= 0) return

        do i = 1, OBSERV_MAX_NRECEIVERS
            write (unit_, '(2a)', advance='no', iostat=stat) s, trim(observ%receivers(i))
            if (stat /= 0) return
        end do

        do i = 1, OBSERV_MAX_NREQUESTS
            if (i > observ%nrequests) then
                write (unit_, '(a)', advance='no', iostat=stat) &
                    repeat(s, 11 + (REQUEST_MAX_NRESPONSES * 4))
                if (stat /= 0) return
                cycle
            end if

            write (unit_, '(a)', advance='no', iostat=stat) s
            if (stat /= 0) return

            write (unit_, '(20a, 6(i0, a), i0)', advance='no', iostat=stat) &
                trim(observ%requests(i)%name),                s, &
                trim(observ%requests(i)%timestamp),           s, &
                '"', trim(observ%requests(i)%request),   '"', s, &
                '"', trim(observ%requests(i)%response),  '"', s, &
                '"', trim(observ%requests(i)%delimiter), '"', s, &
                '"', trim(observ%requests(i)%pattern),   '"', s, &
                observ%requests(i)%delay,                     s, &
                observ%requests(i)%error,                     s, &
                observ%requests(i)%mode,                      s, &
                observ%requests(i)%retries,                   s, &
                observ%requests(i)%state,                     s, &
                observ%requests(i)%timeout,                   s, &
                observ%requests(i)%nresponses
            if (stat /= 0) return

            do j = 1, REQUEST_MAX_NRESPONSES
                if (j > observ%requests(i)%nresponses) then
                    write (unit_, '(a)', advance='no', iostat=stat) repeat(s, 4)
                    if (stat /= 0) return
                    cycle
                end if

                write (unit_, '(a)', advance='no', iostat=stat) s
                if (stat /= 0) return

                write (unit_, '(4a, 2(i0, a), ' // FMT_REAL // ')', advance='no', iostat=stat) &
                    trim(observ%requests(i)%responses(j)%name), s, &
                    trim(observ%requests(i)%responses(j)%unit), s, &
                    observ%requests(i)%responses(j)%type,       s, &
                    observ%requests(i)%responses(j)%error,      s, &
                    observ%requests(i)%responses(j)%value
                if (stat /= 0) return
            end do
        end do

        write (unit_, *)

        rc = E_NONE
    end function csv_write_observ

    integer function csv_write_observs(observs, unit, header, separator) result(rc)
        !! Writes observations to file or standard output.
        use :: dm_observ

        type(observ_type), intent(inout)        :: observs(:) !! Observation array.
        integer,           intent(in), optional :: unit       !! File unit.
        logical,           intent(in), optional :: header     !! CSV header flag.
        character,         intent(in), optional :: separator  !! CSV separator.

        character :: s
        integer   :: i, unit_
        logical   :: header_

        rc = E_WRITE

        unit_   = dm_present(unit, stdout)
        header_ = dm_present(header, .false.)
        s       = dm_present(separator, CSV_SEPARATOR)

        do i = 1, size(observs)
            rc = dm_csv_write(observs(i), unit_, header_, s)
            if (dm_is_error(rc)) exit
            header_ = .false.
        end do
    end function csv_write_observs

    integer function csv_write_sensor(sensor, unit, header, separator) result(rc)
        !! Write sensor to file or standard output.
        use :: dm_sensor

        type(sensor_type), intent(inout)        :: sensor    !! Sensor type.
        integer,           intent(in), optional :: unit      !! File unit.
        logical,           intent(in), optional :: header    !! CSV header flag.
        character,         intent(in), optional :: separator !! CSV separator.

        character :: s
        integer   :: unit_, stat

        rc = E_WRITE

        unit_ = dm_present(unit, stdout)
        s     = dm_present(separator, CSV_SEPARATOR)

        if (dm_present(header, .false.)) then
            write (unit_, '(a)', iostat=stat) dm_csv_header_sensor(s)
            if (stat /= 0) return
        end if

        write (unit_, '(4a, i0, 8a, 6(a, ' // FMT_REAL // '))', iostat=stat) &
            trim(sensor%id),             s, &
            trim(sensor%node_id),        s, &
            sensor%type,                 s, &
            trim(sensor%name),           s, &
            trim(sensor%sn),             s, &
            '"', trim(sensor%meta), '"', s, &
            sensor%x,                    s, &
            sensor%y,                    s, &
            sensor%z,                    s, &
            sensor%lon,                  s, &
            sensor%lat,                  s, &
            sensor%alt
        if (stat /= 0) return

        rc = E_NONE
    end function csv_write_sensor

    integer function csv_write_sensors(sensors, unit, header, separator) result(rc)
        !! Writes sensors to file or standard output.
        use :: dm_sensor

        type(sensor_type), intent(inout)        :: sensors(:) !! Sensor array.
        integer,           intent(in), optional :: unit       !! File unit.
        logical,           intent(in), optional :: header     !! CSV header flag.
        character,         intent(in), optional :: separator  !! CSV separator.

        character :: s
        integer   :: i, unit_
        logical   :: header_

        rc = E_WRITE

        unit_   = dm_present(unit, stdout)
        header_ = dm_present(header, .false.)
        s       = dm_present(separator, CSV_SEPARATOR)

        do i = 1, size(sensors)
            rc = dm_csv_write(sensors(i), unit_, header_, s)
            if (dm_is_error(rc)) exit
            header_ = .false.
        end do
    end function csv_write_sensors

    integer function csv_write_target(target, unit, header, separator) result(rc)
        !! Writes target to file or standard output.
        use :: dm_target

        type(target_type), intent(inout)        :: target    !! Target type.
        integer,           intent(in), optional :: unit      !! File unit.
        logical,           intent(in), optional :: header    !! CSV header flag.
        character,         intent(in), optional :: separator !! CSV separator.

        character :: s
        integer   :: unit_, stat

        rc = E_WRITE

        unit_ = dm_present(unit, stdout)
        s     = dm_present(separator, CSV_SEPARATOR)

        if (dm_present(header, .false.)) then
            write (unit_, '(a)', iostat=stat) dm_csv_header_target(s)
            if (stat /= 0) return
        end if

        write (unit_, '(8a, i0, 6(a, ' // FMT_REAL // '))', iostat=stat) &
            trim(target%id),             s, &
            trim(target%name),           s, &
            '"', trim(target%meta), '"', s, &
            target%state,                s, &
            target%x,                    s, &
            target%y,                    s, &
            target%z,                    s, &
            target%lon,                  s, &
            target%lat,                  s, &
            target%alt
        if (stat /= 0) return

        rc = E_NONE
    end function csv_write_target

    integer function csv_write_targets(targets, unit, header, separator) result(rc)
        !! Writes targets to file or standard output.
        use :: dm_target

        type(target_type), intent(inout)        :: targets(:) !! Target array.
        integer,           intent(in), optional :: unit       !! File unit.
        logical,           intent(in), optional :: header     !! CSV header flag.
        character,         intent(in), optional :: separator  !! CSV separator.

        character :: s
        integer   :: i, unit_
        logical   :: header_

        rc = E_WRITE

        unit_   = dm_present(unit, stdout)
        header_ = dm_present(header, .false.)
        s       = dm_present(separator, CSV_SEPARATOR)

        do i = 1, size(targets)
            rc = dm_csv_write(targets(i), unit_, header_, s)
            if (dm_is_error(rc)) exit
            header_ = .false.
        end do
    end function csv_write_targets

    subroutine csv_unquote(string, quote)
        !! Removes given quote character at start and end from string.
        character(len=*), intent(inout) :: string !! String to unquote on input, unquoted string on output.
        character,        intent(in)    :: quote  !! Quote character.

        integer :: i

        string = adjustl(string)
        if (string(1:1) /= quote) return
        i = index(string, quote, back=.true.)
        if (i == 1) return
        if (i == 2) then
            string = ''
            return
        end if
        string = string(2:i - 1)
    end subroutine csv_unquote
end module dm_csv
