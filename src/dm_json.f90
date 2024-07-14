! Author:  Philipp Engel
! Licence: ISC
module dm_json
    !! Contains subroutines to convert derived types to JSON format.
    use :: dm_dp
    use :: dm_error
    use :: dm_kind
    use :: dm_util
    implicit none (type, external)
    private

    interface dm_json_from
        !! Generic derived type to JSON converter.
        module procedure :: json_from_beat
        module procedure :: json_from_beats
        module procedure :: json_from_data_point
        module procedure :: json_from_data_points
        module procedure :: json_from_log
        module procedure :: json_from_logs
        module procedure :: json_from_node
        module procedure :: json_from_nodes
        module procedure :: json_from_observ
        module procedure :: json_from_observs
        module procedure :: json_from_sensor
        module procedure :: json_from_sensors
        module procedure :: json_from_target
        module procedure :: json_from_targets
    end interface dm_json_from

    interface dm_json_write
        !! Generic derived type to JSON writer.
        module procedure :: json_write_beat
        module procedure :: json_write_beats
        module procedure :: json_write_data_point
        module procedure :: json_write_data_points
        module procedure :: json_write_log
        module procedure :: json_write_logs
        module procedure :: json_write_node
        module procedure :: json_write_nodes
        module procedure :: json_write_observ
        module procedure :: json_write_observs
        module procedure :: json_write_sensor
        module procedure :: json_write_sensors
        module procedure :: json_write_target
        module procedure :: json_write_targets
    end interface dm_json_write

    public :: dm_json_from
    public :: dm_json_write

    private :: json_escape

    private :: json_from_beat
    private :: json_from_beats
    private :: json_from_log
    private :: json_from_logs
    private :: json_from_node
    private :: json_from_nodes
    private :: json_from_observ
    private :: json_from_observs
    private :: json_from_sensor
    private :: json_from_sensors
    private :: json_from_target
    private :: json_from_targets

    private :: json_write_beat
    private :: json_write_beats
    private :: json_write_data_point
    private :: json_write_data_points
    private :: json_write_log
    private :: json_write_logs
    private :: json_write_node
    private :: json_write_nodes
    private :: json_write_observ
    private :: json_write_observs
    private :: json_write_sensor
    private :: json_write_sensors
    private :: json_write_target
    private :: json_write_targets
contains
    ! ******************************************************************
    ! PRIVATE PROCEDURES.
    ! ******************************************************************
    pure function json_escape(str) result(esc)
        !! Escapes passed character string by replacing each occurance of `\`
        !! with `\\`.
        character(len=*), intent(in)  :: str !! String to escape.
        character(len=:), allocatable :: esc !! Escaped string.
        integer                       :: i

        esc = ''

        do i = 1, len_trim(str)
            if (str(i:i) == '\') then
                esc = esc // '\\'
                cycle
            end if
            esc = esc // str(i:i)
        end do
    end function json_escape

    function json_from_beat(beat) result(json)
        !! Returns beat in JSON format.
        use :: dm_beat, only: beat_type

        type(beat_type), intent(inout) :: beat !! Beat type.
        character(len=:), allocatable  :: json !! Alloctable JSON string.

        json = '{"node_id":"'  // trim(beat%node_id)     // '",' // &
               '"address":"'   // trim(beat%address)     // '",' // &
               '"client":"'    // trim(beat%client)      // '",' // &
               '"time_sent":"' // trim(beat%time_sent)   // '",' // &
               '"time_recv":"' // trim(beat%time_recv)   // '",' // &
               '"error":'      // dm_itoa(beat%error)    // ','  // &
               '"interval":'   // dm_itoa(beat%interval) // ','  // &
               '"uptime":'     // dm_itoa(beat%uptime)   // '}'
    end function json_from_beat

    function json_from_beats(beats) result(json)
        !! Returns array of beats in JSON format.
        use :: dm_beat, only: beat_type

        type(beat_type), intent(inout) :: beats(:) !! Array of beat types.
        character(len=:), allocatable  :: json     !! Allocatable JSON string.

        integer :: i, n

        n = size(beats)

        if (n == 0) then
            json = '[]'
            return
        end if

        json = '['

        do i = 1, n
            if (i < n) then
                json = json // dm_json_from(beats(i)) // ','
            else
                json = json // dm_json_from(beats(i))
            end if
        end do

        json = json // ']'
    end function json_from_beats

    function json_from_data_point(data_point) result(json)
        !! Returns data point in JSON format.
        use :: dm_dp, only: dp_type

        type(dp_type), intent(inout)  :: data_point !! Data point type.
        character(len=:), allocatable :: json       !! Alloctable JSON string.

        json = '{"x":"' // trim(data_point%x)    // '",' // &
               '"y":'   // dm_ftoa(data_point%y) // '}'
    end function json_from_data_point

    function json_from_data_points(data_points) result(json)
        !! Returns array of data points in JSON format.
        use :: dm_dp, only: dp_type

        type(dp_type), intent(inout)  :: data_points(:) !! Data points array.
        character(len=:), allocatable :: json           !! Allocatable JSON string.

        integer :: i, n

        n = size(data_points)

        if (n == 0) then
            json = '[]'
            return
        end if

        json = '['

        do i = 1, n
            if (i < n) then
                json = json // dm_json_from(data_points(i)) // ','
            else
                json = json // dm_json_from(data_points(i))
            end if
        end do

        json = json // ']'
    end function json_from_data_points

    function json_from_log(log) result(json)
        !! Returns log in JSON format.
        use :: dm_log, only: log_type

        type(log_type), intent(inout) :: log  !! Log type.
        character(len=:), allocatable :: json !! Alloctable JSON string.

        json = '{"id":"'       // trim(log%id)        // '",' // &
               '"level":'      // dm_itoa(log%level)  // ','  // &
               '"error":'      // dm_itoa(log%error)  // ','  // &
               '"timestamp":"' // trim(log%timestamp) // '",' // &
               '"node_id":"'   // trim(log%node_id)   // '",' // &
               '"sensor_id":"' // trim(log%sensor_id) // '",' // &
               '"target_id":"' // trim(log%target_id) // '",' // &
               '"observ_id":"' // trim(log%observ_id) // '",' // &
               '"message":"'   // trim(log%message)   // '"}'
    end function json_from_log

    function json_from_logs(logs) result(json)
        !! Returns array of logs in JSON format.
        use :: dm_log, only: log_type

        type(log_type), intent(inout) :: logs(:) !! Array of log types.
        character(len=:), allocatable :: json    !! Allocatable JSON string.

        integer :: i, n

        n = size(logs)

        if (n == 0) then
            json = '[]'
            return
        end if

        json = '['

        do i = 1, n
            if (i < n) then
                json = json // dm_json_from(logs(i)) // ','
            else
                json = json // dm_json_from(logs(i))
            end if
        end do

        json = json // ']'
    end function json_from_logs

    function json_from_node(node) result(json)
        !! Returns node in JSON format.
        use :: dm_node, only: node_type

        type(node_type), intent(inout) :: node !! Node type.
        character(len=:), allocatable  :: json !! Alloctable JSON string.

        json = '{"id":"'  // trim(node%id)   // '",' // &
               '"name":"' // trim(node%name) // '",' // &
               '"meta":"' // trim(node%meta) // '",' // &
               '"x":'     // dm_ftoa(node%x) // ','  // &
               '"y":'     // dm_ftoa(node%y) // ','  // &
               '"z":'     // dm_ftoa(node%z) // '}'
    end function json_from_node

    function json_from_nodes(nodes) result(json)
        !! Returns array of nodes in JSON format.
        use :: dm_node, only: node_type

        type(node_type), intent(inout) :: nodes(:) !! Array of node types.
        character(len=:), allocatable  :: json     !! Allocatable JSON string.

        integer :: i, n

        n = size(nodes)

        if (n == 0) then
            json = '[]'
            return
        end if

        json = '['

        do i = 1, n
            if (i < n) then
                json = json // dm_json_from(nodes(i)) // ','
            else
                json = json // dm_json_from(nodes(i))
            end if
        end do

        json = json // ']'
    end function json_from_nodes

    function json_from_observ(observ) result(json)
        !! JSON encoding of given observation type (for poor people).
        use :: dm_observ, only: observ_type

        type(observ_type), intent(inout) :: observ !! Observation data.
        character(len=:), allocatable    :: json   !! Alloctable JSON string.

        character(len=:), allocatable :: receivers
        character(len=:), allocatable :: requests
        integer                       :: i, j

        ! Receivers.
        receivers = '['

        do i = 1, observ%nreceivers
            receivers = receivers // '"' // trim(observ%receivers(i)) // '"'
            if (i < observ%nreceivers) receivers = receivers // ','
        end do

        receivers = receivers // ']'

        ! Requests and responses,
        requests = '['

        do i = 1, observ%nrequests
            requests = requests // &
            '{"name":"'      // trim(observ%requests(i)%name)             // '",' // &
            '"timestamp":"'  // trim(observ%requests(i)%timestamp)        // '",' // &
            '"request":"'    // json_escape(observ%requests(i)%request)   // '",' // &
            '"response":"'   // json_escape(observ%requests(i)%response)  // '",' // &
            '"delimiter":"'  // json_escape(observ%requests(i)%delimiter) // '",' // &
            '"pattern":"'    // json_escape(observ%requests(i)%pattern)   // '",' // &
            '"delay":'       // dm_itoa(observ%requests(i)%delay)         // ','  // &
            '"error":'       // dm_itoa(observ%requests(i)%error)         // ','  // &
            '"mode":'        // dm_itoa(observ%requests(i)%mode)          // ','  // &
            '"retries":'     // dm_itoa(observ%requests(i)%retries)       // ','  // &
            '"state":'       // dm_itoa(observ%requests(i)%state)         // ','  // &
            '"timeout":'     // dm_itoa(observ%requests(i)%timeout)       // ','  // &
            '"nresponses":'  // dm_itoa(observ%requests(i)%nresponses)    // ','  // &
            '"responses":['

            do j = 1, observ%requests(i)%nresponses
                requests = requests // &
                '{"name":"' // trim(observ%requests(i)%responses(j)%name)     // '",' // &
                '"unit":"'  // trim(observ%requests(i)%responses(j)%unit)     // '",' // &
                '"type":'   // dm_itoa(observ%requests(i)%responses(j)%type)  // ','  // &
                '"error":'  // dm_itoa(observ%requests(i)%responses(j)%error) // ','  // &
                '"value":'  // dm_ftoa(observ%requests(i)%responses(j)%value) // '}'

                if (j < observ%requests(i)%nresponses) requests = requests // ','
            end do

            requests = requests // ']' // '}'
            if (i < observ%nrequests) requests = requests // ','
        end do

        requests = requests // ']'

        ! Complete JSON.
        json = '{"id":"'       // trim(observ%id)            // '",' // &
               '"node_id":"'   // trim(observ%node_id)       // '",' // &
               '"sensor_id":"' // trim(observ%sensor_id)     // '",' // &
               '"target_id":"' // trim(observ%target_id)     // '",' // &
               '"name":"'      // trim(observ%name)          // '",' // &
               '"timestamp":"' // trim(observ%timestamp)     // '",' // &
               '"source":"'    // trim(observ%source)        // '",' // &
               '"device":"'    // json_escape(observ%device) // '",' // &
               '"priority":'   // dm_itoa(observ%priority)   // ','  // &
               '"error":'      // dm_itoa(observ%error)      // ','  // &
               '"next":'       // dm_itoa(observ%next)       // ','  // &
               '"nreceivers":' // dm_itoa(observ%nreceivers) // ','  // &
               '"nrequests":'  // dm_itoa(observ%nrequests)  // ','  // &
               '"receivers":'  // receivers                  // ','  // &
               '"requests":'   // requests                   // '}'
    end function json_from_observ

    function json_from_observs(observs) result(json)
        !! Returns array of observations in JSON format.
        use :: dm_observ, only: observ_type

        type(observ_type), intent(inout) :: observs(:) !! Array of observations.
        character(len=:), allocatable    :: json       !! Allocatable JSON string.

        integer :: i, n

        n = size(observs)

        if (n == 0) then
            json = '[]'
            return
        end if

        json = '['

        do i = 1, n
            if (i < n) then
                json = json // dm_json_from(observs(i)) // ','
            else
                json = json // dm_json_from(observs(i))
            end if
        end do

        json = json // ']'
    end function json_from_observs

    function json_from_sensor(sensor) result(json)
        !! Returns sensor in JSON format.
        use :: dm_sensor, only: sensor_type

        type(sensor_type), intent(inout) :: sensor !! Sensor type.
        character(len=:), allocatable    :: json   !! Alloctable JSON string.

        json = '{"id":"'     // trim(sensor%id)      // '",' // &
               '"node_id":"' // trim(sensor%node_id) // '",' // &
               '"type":'     // dm_itoa(sensor%type) // ','  // &
               '"name":"'    // trim(sensor%name)    // '",' // &
               '"sn":"'      // trim(sensor%sn)      // '",' // &
               '"meta":"'    // trim(sensor%meta)    // '",' // &
               '"x":'        // dm_ftoa(sensor%x)    // ','  // &
               '"y":'        // dm_ftoa(sensor%y)    // ','  // &
               '"z":'        // dm_ftoa(sensor%z)    // '}'
    end function json_from_sensor

    function json_from_sensors(sensors) result(json)
        !! Returns array of sensors in JSON format.
        use :: dm_sensor, only: sensor_type

        type(sensor_type), intent(inout) :: sensors(:) !! Array of sensors.
        character(len=:), allocatable    :: json       !! Allocatable JSON string.

        integer :: i, n

        n = size(sensors)

        if (n == 0) then
            json = '[]'
            return
        end if

        json = '['

        do i = 1, n
            if (i < n) then
                json = json // dm_json_from(sensors(i)) // ','
            else
                json = json // dm_json_from(sensors(i))
            end if
        end do

        json = json // ']'
    end function json_from_sensors

    function json_from_target(target) result(json)
        !! Returns target in JSON format.
        use :: dm_target, only: target_type

        type(target_type), intent(inout) :: target !! Sensor type.
        character(len=:), allocatable    :: json   !! Alloctable JSON string.

        json = '{"id":"'  // trim(target%id)       // '",' // &
               '"name":"' // trim(target%name)     // '",' // &
               '"meta":"' // trim(target%meta)     // '",' // &
               '"state":' // dm_itoa(target%state) // ','  // &
               '"x":'     // dm_ftoa(target%x)     // ','  // &
               '"y":'     // dm_ftoa(target%y)     // ','  // &
               '"z":'     // dm_ftoa(target%z)     // '}'
    end function json_from_target

    function json_from_targets(targets) result(json)
        !! Returns array of targets in JSON format.
        use :: dm_target, only: target_type

        type(target_type), intent(inout) :: targets(:) !! Array of targets.
        character(len=:), allocatable    :: json       !! Allocatable JSON string.

        integer :: i, n

        n = size(targets)

        if (n == 0) then
            json = '[]'
            return
        end if

        json = '['

        do i = 1, n
            if (i < n) then
                json = json // dm_json_from(targets(i)) // ','
            else
                json = json // dm_json_from(targets(i))
            end if
        end do

        json = json // ']'
    end function json_from_targets

    integer function json_write_beat(beat, unit) result(rc)
        !! Writes beat to file or standard output.
        use :: dm_beat, only: beat_type

        type(beat_type), intent(inout)        :: beat !! Beat type.
        integer,         intent(in), optional :: unit !! File unit.

        integer :: stat, unit_

        rc = E_WRITE
        unit_ = stdout
        if (present(unit)) unit_ = unit
        write (unit_, '(a)', iostat=stat) dm_json_from(beat)
        if (stat /= 0) return
        rc = E_NONE
    end function json_write_beat

    integer function json_write_beats(beats, unit) result(rc)
        !! Writes beats to file or standard output.
        use :: dm_beat, only: beat_type

        type(beat_type), intent(inout)        :: beats(:) !! Beat array.
        integer,         intent(in), optional :: unit     !! File unit.

        integer :: i, n, stat, unit_

        rc = E_WRITE
        unit_ = stdout
        if (present(unit)) unit_ = unit
        n = size(beats)

        if (n == 0) then
            write (unit_, '("[]")', iostat=stat)
            if (stat /= 0) return
            rc = E_NONE
            return
        end if

        write (unit_, '("[")', advance='no', iostat=stat)
        if (stat /= 0) return

        do i = 1, n
            write (unit_, '(a)', advance='no', iostat=stat) dm_json_from(beats(i))
            if (i < n) write (unit_, '(",")', advance='no', iostat=stat)
            if (stat /= 0) return
        end do

        write (unit_, '("]")', iostat=stat)
        if (stat /= 0) return
        rc = E_NONE
    end function json_write_beats

    integer function json_write_data_point(data_point, unit) result(rc)
        !! Writes data point to file or standard output.
        use :: dm_dp, only: dp_type

        type(dp_type), intent(inout)        :: data_point !! Data point type.
        integer,       intent(in), optional :: unit       !! File unit.

        integer :: stat, unit_

        rc = E_WRITE
        unit_ = stdout
        if (present(unit)) unit_ = unit
        write (unit_, '(a)', iostat=stat) dm_json_from(data_point)
        if (stat /= 0) return
        rc = E_NONE
    end function json_write_data_point

    integer function json_write_data_points(data_points, unit) result(rc)
        !! Writes data_points to file or standard output.
        use :: dm_dp, only: dp_type

        type(dp_type), intent(inout)        :: data_points(:) !! Data point array.
        integer,       intent(in), optional :: unit       !! File unit.

        integer :: i, n, stat, unit_

        rc = E_WRITE
        unit_ = stdout
        if (present(unit)) unit_ = unit
        n = size(data_points)

        if (n == 0) then
            write (unit_, '("[]")', iostat=stat)
            if (stat /= 0) return
            rc = E_NONE
            return
        end if

        write (unit_, '("[")', advance='no', iostat=stat)
        if (stat /= 0) return

        do i = 1, n
            write (unit_, '(a)', advance='no', iostat=stat) dm_json_from(data_points(i))
            if (i < n) write (unit_, '(",")', advance='no', iostat=stat)
            if (stat /= 0) return
        end do

        write (unit_, '("]")', iostat=stat)
        if (stat /= 0) return
        rc = E_NONE
    end function json_write_data_points

    integer function json_write_log(log, unit) result(rc)
        !! Writes log to file or standard output.
        use :: dm_log, only: log_type

        type(log_type), intent(inout)        :: log  !! Log type.
        integer,        intent(in), optional :: unit !! File unit.

        integer :: stat, unit_

        rc = E_WRITE
        unit_ = stdout
        if (present(unit)) unit_ = unit
        write (unit_, '(a)', iostat=stat) dm_json_from(log)
        if (stat /= 0) return
        rc = E_NONE
    end function json_write_log

    integer function json_write_logs(logs, unit) result(rc)
        !! Writes logs to file or standard output.
        use :: dm_log, only: log_type

        type(log_type), intent(inout)        :: logs(:) !! Log array.
        integer,        intent(in), optional :: unit    !! File unit.

        integer :: i, n, stat, unit_

        rc = E_WRITE
        unit_ = stdout
        if (present(unit)) unit_ = unit
        n = size(logs)

        if (n == 0) then
            write (unit_, '("[]")', iostat=stat)
            if (stat /= 0) return
            rc = E_NONE
            return
        end if

        write (unit_, '("[")', advance='no', iostat=stat)
        if (stat /= 0) return

        do i = 1, n
            write (unit_, '(a)', advance='no', iostat=stat) dm_json_from(logs(i))
            if (i < n) write (unit_, '(",")', advance='no', iostat=stat)
            if (stat /= 0) return
        end do

        write (unit_, '("]")', iostat=stat)
        if (stat /= 0) return
        rc = E_NONE
    end function json_write_logs

    integer function json_write_node(node, unit) result(rc)
        !! Writes node to file or standard output.
        use :: dm_node, only: node_type

        type(node_type), intent(inout)        :: node !! Node type.
        integer,         intent(in), optional :: unit !! File unit.

        integer :: stat, unit_

        rc = E_WRITE
        unit_ = stdout
        if (present(unit)) unit_ = unit
        write (unit_, '(a)', iostat=stat) dm_json_from(node)
        if (stat /= 0) return
        rc = E_NONE
    end function json_write_node

    integer function json_write_nodes(nodes, unit) result(rc)
        !! Writes nodes to file or standard output.
        use :: dm_node, only: node_type

        type(node_type), intent(inout)        :: nodes(:) !! Node array.
        integer,         intent(in), optional :: unit     !! File unit.

        integer :: i, n, stat, unit_

        rc = E_WRITE
        unit_ = stdout
        if (present(unit)) unit_ = unit
        n = size(nodes)

        if (n == 0) then
            write (unit_, '("[]")', iostat=stat)
            if (stat /= 0) return
            rc = E_NONE
            return
        end if

        write (unit_, '("[")', advance='no', iostat=stat)
        if (stat /= 0) return

        do i = 1, n
            write (unit_, '(a)', advance='no', iostat=stat) dm_json_from(nodes(i))
            if (i < n) write (unit_, '(",")', advance='no', iostat=stat)
            if (stat /= 0) return
        end do

        write (unit_, '("]")', iostat=stat)
        if (stat /= 0) return
        rc = E_NONE
    end function json_write_nodes

    integer function json_write_observ(observ, unit) result(rc)
        !! Writes observation to file or standard output.
        use :: dm_observ, only: observ_type

        type(observ_type), intent(inout)        :: observ !! Observation type.
        integer,           intent(in), optional :: unit   !! File unit.

        integer :: stat, unit_

        rc = E_WRITE
        unit_ = stdout
        if (present(unit)) unit_ = unit
        write (unit_, '(a)', iostat=stat) dm_json_from(observ)
        if (stat /= 0) return
        rc = E_NONE
    end function json_write_observ

    integer function json_write_observs(observs, unit) result(rc)
        !! Writes observations to file or standard output.
        use :: dm_observ, only: observ_type

        type(observ_type), intent(inout)        :: observs(:) !! Observation array.
        integer,           intent(in), optional :: unit       !! File unit.

        integer :: i, n, stat, unit_

        rc = E_WRITE
        unit_ = stdout
        if (present(unit)) unit_ = unit
        n = size(observs)

        if (n == 0) then
            write (unit_, '("[]")', iostat=stat)
            if (stat /= 0) return
            rc = E_NONE
            return
        end if

        write (unit_, '("[")', advance='no', iostat=stat)
        if (stat /= 0) return

        do i = 1, n
            write (unit_, '(a)', advance='no', iostat=stat) dm_json_from(observs(i))
            if (i < n) write (unit_, '(",")', advance='no', iostat=stat)
            if (stat /= 0) return
        end do

        write (unit_, '("]")', iostat=stat)
        if (stat /= 0) return
        rc = E_NONE
    end function json_write_observs

    integer function json_write_sensor(sensor, unit) result(rc)
        !! Writes sensor to file or standard output.
        use :: dm_sensor, only: sensor_type

        type(sensor_type), intent(inout)        :: sensor !! Sensor type.
        integer,           intent(in), optional :: unit   !! File unit.

        integer :: stat, unit_

        rc = E_WRITE
        unit_ = stdout
        if (present(unit)) unit_ = unit
        write (unit_, '(a)', iostat=stat) dm_json_from(sensor)
        if (stat /= 0) return
        rc = E_NONE
    end function json_write_sensor

    integer function json_write_sensors(sensors, unit) result(rc)
        !! Writes sensors to file or standard output.
        use :: dm_sensor, only: sensor_type

        type(sensor_type), intent(inout)        :: sensors(:) !! Sensor array.
        integer,           intent(in), optional :: unit       !! File unit.

        integer :: i, n, stat, unit_

        rc = E_WRITE
        unit_ = stdout
        if (present(unit)) unit_ = unit
        n = size(sensors)

        if (n == 0) then
            write (unit_, '("[]")', iostat=stat)
            if (stat /= 0) return
            rc = E_NONE
            return
        end if

        write (unit_, '("[")', advance='no', iostat=stat)
        if (stat /= 0) return

        do i = 1, n
            write (unit_, '(a)', advance='no', iostat=stat) dm_json_from(sensors(i))
            if (i < n) write (unit_, '(",")', advance='no', iostat=stat)
            if (stat /= 0) return
        end do

        write (unit_, '("]")', iostat=stat)
        if (stat /= 0) return
        rc = E_NONE
    end function json_write_sensors

    integer function json_write_target(target, unit) result(rc)
        !! Writes target to file or standard output.
        use :: dm_target, only: target_type

        type(target_type), intent(inout)        :: target !! Target type.
        integer,           intent(in), optional :: unit   !! File unit.

        integer :: stat, unit_

        rc = E_WRITE
        unit_ = stdout
        if (present(unit)) unit_ = unit
        write (unit_, '(a)', iostat=stat) dm_json_from(target)
        if (stat /= 0) return
        rc = E_NONE
    end function json_write_target

    integer function json_write_targets(targets, unit) result(rc)
        !! Writes targets to file or standard output.
        use :: dm_target, only: target_type

        type(target_type), intent(inout)        :: targets(:) !! Target array.
        integer,           intent(in), optional :: unit       !! File unit.

        integer :: i, n, stat, unit_

        rc = E_WRITE
        unit_ = stdout
        if (present(unit)) unit_ = unit
        n = size(targets)

        if (n == 0) then
            write (unit_, '("[]")', iostat=stat)
            if (stat /= 0) return
            rc = E_NONE
            return
        end if

        write (unit_, '("[")', advance='no', iostat=stat)
        if (stat /= 0) return

        do i = 1, n
            write (unit_, '(a)', advance='no', iostat=stat) dm_json_from(targets(i))
            if (i < n) write (unit_, '(",")', advance='no', iostat=stat)
            if (stat /= 0) return
        end do

        write (unit_, '("]")', iostat=stat)
        if (stat /= 0) return
        rc = E_NONE
    end function json_write_targets
end module dm_json
