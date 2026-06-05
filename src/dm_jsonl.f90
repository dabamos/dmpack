! Author:  Philipp Engel
! Licence: ISC
module dm_jsonl
    !! Contains subroutines to convert derived types to [JSON Lines](https://jsonlines.org/)
    !! or [Newline Delimited JSON](http://ndjson.org/) format.
    use :: dm_ascii, only: NL => ASCII_LF
    use :: dm_error
    use :: dm_kind
    use :: dm_json
    use :: dm_util
    implicit none (type, external)
    private

    interface dm_jsonl_write
        !! Generic derived type to JSON Lines writer.
        module procedure :: jsonl_write_beats
        module procedure :: jsonl_write_dps
        module procedure :: jsonl_write_logs
        module procedure :: jsonl_write_nodes
        module procedure :: jsonl_write_observs
        module procedure :: jsonl_write_sensors
        module procedure :: jsonl_write_targets
    end interface dm_jsonl_write

    public :: dm_jsonl_write

    private :: jsonl_write_beats
    private :: jsonl_write_dps
    private :: jsonl_write_logs
    private :: jsonl_write_nodes
    private :: jsonl_write_observs
    private :: jsonl_write_sensors
    private :: jsonl_write_targets
contains
    ! **************************************************************************
    ! PRIVATE PROCEDURES
    ! **************************************************************************
    integer function jsonl_write_beats(beats, unit) result(rc)
        !! Writes beats to file or standard output.
        use :: dm_beat

        type(beat_type), intent(inout)        :: beats(:) !! Beat array.
        integer,         intent(in), optional :: unit     !! File unit.

        integer :: i, n, stat, unit_

        rc = E_NONE
        unit_ = dm_present(unit, stdout)

        n = size(beats)
        if (n == 0) return

        rc = E_WRITE

        do i = 1, n
            write (unit_, '(a)', iostat=stat) dm_json_from(beats(i))
            if (stat /= 0) return
        end do

        rc = E_NONE
    end function jsonl_write_beats

    integer function jsonl_write_dps(dps, unit) result(rc)
        !! Writes dps to file or standard output.
        use :: dm_dp

        type(dp_type), intent(inout)        :: dps(:) !! Data point array.
        integer,       intent(in), optional :: unit   !! File unit.

        integer :: i, n, stat, unit_

        rc = E_NONE
        unit_ = dm_present(unit, stdout)

        n = size(dps)
        if (n == 0) return

        rc = E_WRITE

        do i = 1, n
            write (unit_, '(a)', iostat=stat) dm_json_from(dps(i))
            if (stat /= 0) return
        end do

        rc = E_NONE
    end function jsonl_write_dps

    integer function jsonl_write_logs(logs, unit) result(rc)
        !! Writes logs to file or standard output.
        use :: dm_log

        type(log_type), intent(inout)        :: logs(:) !! Log array.
        integer,        intent(in), optional :: unit    !! File unit.

        integer :: i, n, stat, unit_

        rc = E_NONE
        unit_ = dm_present(unit, stdout)

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
        unit_ = dm_present(unit, stdout)

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
        unit_ = dm_present(unit, stdout)

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
        unit_ = dm_present(unit, stdout)

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
        unit_ = dm_present(unit, stdout)

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
