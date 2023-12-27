! Author:  Philipp Engel
! Licence: ISC
module dm_jsonl
    !! Contains subroutines to convert derived types to
    !! [JSON Lines](https://jsonlines.org/) or
    !! [Newline Delimited JSON](http://ndjson.org/) format.
    use :: dm_ascii, only: NL => ASCII_LF
    use :: dm_error
    use :: dm_kind
    use :: dm_json
    use :: dm_util
    implicit none (type, external)
    private

    interface dm_jsonl_from
        !! Generic derived type to JSON Lines converter.
        module procedure :: jsonl_from_beats
        module procedure :: jsonl_from_data_points
        module procedure :: jsonl_from_logs
        module procedure :: jsonl_from_nodes
        module procedure :: jsonl_from_observs
        module procedure :: jsonl_from_sensors
        module procedure :: jsonl_from_targets
    end interface

    interface dm_jsonl_write
        !! Generic derived type to JSON Lines writer.
        module procedure :: jsonl_write_beats
        module procedure :: jsonl_write_data_points
        module procedure :: jsonl_write_logs
        module procedure :: jsonl_write_nodes
        module procedure :: jsonl_write_observs
        module procedure :: jsonl_write_sensors
        module procedure :: jsonl_write_targets
    end interface

    public :: dm_jsonl_from
    public :: dm_jsonl_write

    private :: jsonl_from_beats
    private :: jsonl_from_logs
    private :: jsonl_from_nodes
    private :: jsonl_from_observs
    private :: jsonl_from_sensors
    private :: jsonl_from_targets

    private :: jsonl_write_beats
    private :: jsonl_write_data_points
    private :: jsonl_write_logs
    private :: jsonl_write_nodes
    private :: jsonl_write_observs
    private :: jsonl_write_sensors
    private :: jsonl_write_targets
contains
    ! ******************************************************************
    ! PRIVATE PROCEDURES.
    ! ******************************************************************
    function jsonl_from_beats(beats) result(jsonl)
        !! Returns array of beats in JSON Lines format.
        use :: dm_beat
        type(beat_type), intent(inout) :: beats(:) !! Array of beat types.
        character(len=:), allocatable  :: jsonl    !! Allocatable JSON Lines string.

        integer :: i, n

        n = size(beats)

        if (n == 0) then
            jsonl = ''
            return
        end if

        do i = 1, n
            if (i < n) then
                jsonl = jsonl // dm_json_from(beats(i)) // NL
            else
                jsonl = jsonl // dm_json_from(beats(i))
            end if
        end do
    end function jsonl_from_beats

    function jsonl_from_data_points(data_points) result(jsonl)
        !! Returns array of data points in JSON Lines format.
        use :: dm_dp
        type(dp_type), intent(inout)  :: data_points(:) !! Data points array.
        character(len=:), allocatable :: jsonl          !! Allocatable JSON Lines string.

        integer :: i, n

        n = size(data_points)

        if (n == 0) then
            jsonl = ''
            return
        end if

        do i = 1, n
            if (i < n) then
                jsonl = jsonl // dm_json_from(data_points(i)) // NL
            else
                jsonl = jsonl // dm_json_from(data_points(i))
            end if
        end do
    end function jsonl_from_data_points

    function jsonl_from_logs(logs) result(jsonl)
        !! Returns array of logs in JSON Lines format.
        use :: dm_log
        type(log_type), intent(inout) :: logs(:) !! Array of log types.
        character(len=:), allocatable :: jsonl   !! Allocatable JSON Lines string.

        integer :: i, n

        n = size(logs)

        if (n == 0) then
            jsonl = ''
            return
        end if

        do i = 1, n
            if (i < n) then
                jsonl = jsonl // dm_json_from(logs(i)) // NL
            else
                jsonl = jsonl // dm_json_from(logs(i))
            end if
        end do
    end function jsonl_from_logs

    function jsonl_from_nodes(nodes) result(jsonl)
        !! Returns array of nodes in JSON Lines format.
        use :: dm_node
        type(node_type), intent(inout) :: nodes(:) !! Array of node types.
        character(len=:), allocatable  :: jsonl    !! Allocatable JSON Lines string.

        integer :: i, n

        n = size(nodes)

        if (n == 0) then
            jsonl = ''
            return
        end if

        do i = 1, n
            if (i < n) then
                jsonl = jsonl // dm_json_from(nodes(i)) // NL
            else
                jsonl = jsonl // dm_json_from(nodes(i))
            end if
        end do
    end function jsonl_from_nodes

    function jsonl_from_observs(observs) result(jsonl)
        !! Returns array of observations in JSON Lines format.
        use :: dm_observ
        type(observ_type), intent(inout) :: observs(:) !! Array of observations.
        character(len=:), allocatable    :: jsonl      !! Allocatable JSON Lines string.

        integer :: i, n

        n = size(observs)

        if (n == 0) then
            jsonl = ''
            return
        end if

        do i = 1, n
            if (i < n) then
                jsonl = jsonl // dm_json_from(observs(i)) // NL
            else
                jsonl = jsonl // dm_json_from(observs(i))
            end if
        end do
    end function jsonl_from_observs

    function jsonl_from_sensors(sensors) result(jsonl)
        !! Returns array of sensors in JSON Lines format.
        use :: dm_sensor
        type(sensor_type), intent(inout) :: sensors(:) !! Array of sensors.
        character(len=:), allocatable    :: jsonl      !! Allocatable JSON Lines string.

        integer :: i, n

        n = size(sensors)

        if (n == 0) then
            jsonl = ''
            return
        end if

        do i = 1, n
            if (i < n) then
                jsonl = jsonl // dm_json_from(sensors(i)) // NL
            else
                jsonl = jsonl // dm_json_from(sensors(i))
            end if
        end do
    end function jsonl_from_sensors

    function jsonl_from_targets(targets) result(jsonl)
        !! Returns array of targets in JSON Lines format.
        use :: dm_target
        type(target_type), intent(inout) :: targets(:) !! Array of targets.
        character(len=:), allocatable    :: jsonl      !! Allocatable JSON Lines string.

        integer :: i, n

        n = size(targets)

        if (n == 0) then
            jsonl = ''
            return
        end if

        do i = 1, n
            if (i < n) then
                jsonl = jsonl // dm_json_from(targets(i)) // NL
            else
                jsonl = jsonl // dm_json_from(targets(i))
            end if
        end do
    end function jsonl_from_targets

    integer function jsonl_write_beats(beats, unit) result(rc)
        !! Writes beats to file or standard output.
        use :: dm_beat
        type(beat_type), intent(inout)        :: beats(:) !! Beat array.
        integer,         intent(in), optional :: unit     !! File unit.

        integer :: i, n, stat, unit_

        rc = E_NONE

        unit_ = stdout
        if (present(unit)) unit_ = unit

        n = size(beats)
        if (n == 0) return

        rc = E_WRITE

        do i = 1, n
            write (unit_, '(a)', iostat=stat) dm_json_from(beats(i))
            if (stat /= 0) return
        end do

        rc = E_NONE
    end function jsonl_write_beats

    integer function jsonl_write_data_points(data_points, unit) result(rc)
        !! Writes data_points to file or standard output.
        use :: dm_dp
        type(dp_type), intent(inout)        :: data_points(:) !! Data point array.
        integer,       intent(in), optional :: unit           !! File unit.

        integer :: i, n, stat, unit_

        rc = E_NONE

        unit_ = stdout
        if (present(unit)) unit_ = unit

        n = size(data_points)
        if (n == 0) return

        rc = E_WRITE

        do i = 1, n
            write (unit_, '(a)', iostat=stat) dm_json_from(data_points(i))
            if (stat /= 0) return
        end do

        rc = E_NONE
    end function jsonl_write_data_points

    integer function jsonl_write_logs(logs, unit) result(rc)
        !! Writes logs to file or standard output.
        use :: dm_log
        type(log_type), intent(inout)        :: logs(:) !! Log array.
        integer,        intent(in), optional :: unit    !! File unit.

        integer :: i, n, stat, unit_

        rc = E_NONE

        unit_ = stdout
        if (present(unit)) unit_ = unit

        n = size(logs)
        if (n == 0) return

        rc = E_WRITE

        do i = 1, n
            write (unit_, '(a)', iostat=stat) dm_json_from(logs(i))
            if (stat /= 0) return
        end do

        rc = E_NONE
    end function jsonl_write_logs

    integer function jsonl_write_nodes(nodes, unit) result(rc)
        !! Writes nodes to file or standard output.
        use :: dm_node
        type(node_type), intent(inout)        :: nodes(:) !! Node array.
        integer,         intent(in), optional :: unit     !! File unit.

        integer :: i, n, stat, unit_

        rc = E_NONE

        unit_ = stdout
        if (present(unit)) unit_ = unit

        n = size(nodes)
        if (n == 0) return

        rc = E_WRITE

        do i = 1, n
            write (unit_, '(a)', iostat=stat) dm_json_from(nodes(i))
            if (stat /= 0) return
        end do

        rc = E_NONE
    end function jsonl_write_nodes

    integer function jsonl_write_observs(observs, unit) result(rc)
        !! Writes observations to file or standard output.
        use :: dm_observ
        type(observ_type), intent(inout)        :: observs(:) !! Observation array.
        integer,           intent(in), optional :: unit       !! File unit.

        integer :: i, n, stat, unit_

        rc = E_NONE

        unit_ = stdout
        if (present(unit)) unit_ = unit

        n = size(observs)
        if (n == 0) return

        rc = E_WRITE

        do i = 1, n
            write (unit_, '(a)', iostat=stat) dm_json_from(observs(i))
            if (stat /= 0) return
        end do

        rc = E_NONE
    end function jsonl_write_observs

    integer function jsonl_write_sensors(sensors, unit) result(rc)
        !! Writes sensors to file or standard output.
        use :: dm_sensor
        type(sensor_type), intent(inout)        :: sensors(:) !! Sensor array.
        integer,           intent(in), optional :: unit       !! File unit.

        integer :: i, n, stat, unit_

        rc = E_NONE

        unit_ = stdout
        if (present(unit)) unit_ = unit

        n = size(sensors)
        if (n == 0) return

        rc = E_WRITE

        do i = 1, n
            write (unit_, '(a)', iostat=stat) dm_json_from(sensors(i))
            if (stat /= 0) return
        end do

        rc = E_NONE
    end function jsonl_write_sensors

    integer function jsonl_write_targets(targets, unit) result(rc)
        !! Writes targets to file or standard output.
        use :: dm_target
        type(target_type), intent(inout)        :: targets(:) !! Target array.
        integer,           intent(in), optional :: unit       !! File unit.

        integer :: i, n, stat, unit_

        rc = E_NONE

        unit_ = stdout
        if (present(unit)) unit_ = unit

        n = size(targets)
        if (n == 0) return

        rc = E_WRITE

        do i = 1, n
            write (unit_, '(a)', iostat=stat) dm_json_from(targets(i))
            if (stat /= 0) return
        end do

        rc = E_NONE
    end function jsonl_write_targets
end module dm_jsonl
