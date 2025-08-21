! Author:  Philipp Engel
! Licence: ISC
module dm_serial
    !! Module for serialising derived types to CSV, JSON/JSONL, and Namelist.
    use :: dm_ascii
    use :: dm_csv
    use :: dm_error
    use :: dm_format
    use :: dm_json
    use :: dm_kind
    use :: dm_nml
    implicit none (type, external)
    private

    abstract interface
        subroutine dm_serial_callback(string)
            !! Public callback routine to pass serialised type to.
            character(len=*), intent(in) :: string !! Serialised type string.
        end subroutine dm_serial_callback
    end interface

    interface dm_serial_out
        !! Generic serialisation function.
        module procedure :: serial_out_beat
        module procedure :: serial_out_log
        module procedure :: serial_out_node
        module procedure :: serial_out_observ
        module procedure :: serial_out_sensor
        module procedure :: serial_out_target
    end interface dm_serial_out

    public :: dm_serial_callback
    public :: dm_serial_iterate
    public :: dm_serial_out

    private :: serial_out_beat
    private :: serial_out_log
    private :: serial_out_node
    private :: serial_out_observ
    private :: serial_out_sensor
    private :: serial_out_target
contains
    ! **************************************************************************
    ! PUBLIC PROCEDURES.
    ! **************************************************************************
    integer function dm_serial_iterate(format, index, size, type, string, callback, unit, header, separator) result(rc)
        use :: dm_beat,   only: beat_type
        use :: dm_log,    only: log_type
        use :: dm_node,   only: node_type
        use :: dm_observ, only: observ_type
        use :: dm_sensor, only: sensor_type
        use :: dm_target, only: target_type
        use :: dm_util,   only: dm_present

        integer,                       intent(in)              :: format    !! Format enumerator (`FORMAT_*`).
        integer(kind=i8),              intent(in),    optional :: index     !! Array index.
        integer(kind=i8),              intent(in),    optional :: size      !! Array size.
        class(*),                      intent(inout), optional :: type      !! Derived type to serialise.
        character(len=:), allocatable, intent(out),   optional :: string    !! Output string.
        procedure(dm_serial_callback),                optional :: callback  !! Output callback.
        integer,                       intent(in),    optional :: unit      !! Output unit.
        logical,                       intent(in),    optional :: header    !! Add CSV header.
        character,                     intent(in),    optional :: separator !! CSV separator.

        character(len=:), allocatable :: output

        integer          :: stat
        integer(kind=i8) :: index_, size_
        logical          :: callback_, header_, unit_, string_, type_

        index_ = dm_present(index, 0_i8)
        size_  = dm_present(size,  0_i8)

        callback_ = merge(.true.,  .false., present(callback))
        header_   = merge(header,  .false., present(header) .and. (index_ <= 1))
        unit_     = merge(.true.,  .false., present(unit))
        string_   = merge(.true.,  .false., present(string))
        type_     = merge(.true.,  .false., present(type))

        if (type_ .and. size_ > 0) then
            select type (type)
                type is (beat_type);   rc = dm_serial_out(format, type, string=output, header=header_, separator=separator)
                type is (log_type);    rc = dm_serial_out(format, type, string=output, header=header_, separator=separator)
                type is (node_type);   rc = dm_serial_out(format, type, string=output, header=header_, separator=separator)
                type is (observ_type); rc = dm_serial_out(format, type, string=output, header=header_, separator=separator)
                type is (sensor_type); rc = dm_serial_out(format, type, string=output, header=header_, separator=separator)
                type is (target_type); rc = dm_serial_out(format, type, string=output, header=header_, separator=separator)
                class default;         rc = E_INVALID
            end select

            if (dm_is_error(rc)) then
                if (string_) string = ''
                return
            end if
        end if

        select case (format)
            case (FORMAT_CSV, FORMAT_JSONL)
                if (.not. type_ .or. index_ == 0 .or. size_ == 0) then
                    if (string_) string = ''
                    return
                end if

                if (string_) string = output

                if (callback_ .and. (index_ == size_ .or. size_ == 1)) then
                    call callback(output)
                else if (callback_) then
                    call callback(output // ASCII_LF)
                end if

                if (unit_) then
                    write (unit, '(a)', iostat=stat) output
                    if (stat /= 0) rc = E_WRITE
                end if

            case (FORMAT_JSON)
                if (.not. type_ .or. index_ == 0 .or. size_ == 0) then
                    if (string_)   string = '[]'
                    if (callback_) call callback('[]')
                    if (unit_)     write (unit, '("[]")', advance='no', iostat=stat)
                else if (size_ == 1) then
                    if (string_)   string = '[' // output // ']'
                    if (callback_) call callback('[' // output // ']')
                    if (unit_)     write (unit, '("[", a, "]")', advance='no', iostat=stat) output
                else if (index_ == 1) then
                    if (string_)   string = '[' // output
                    if (callback_) call callback('[' // output)
                    if (unit_)     write (unit, '("[", a)', advance='no', iostat=stat) output
                else if (index_ < size_) then
                    if (string_)   string = output // ','
                    if (callback_) call callback(output // ',')
                    if (unit_)     write (unit, '(a, ",")', advance='no', iostat=stat) output
                else
                    if (string_)   string = output // ']'
                    if (callback_) call callback(output // ']')
                    if (unit_)     write (unit, '(a, "]")', advance='no', iostat=stat) output
                end if

                if (unit_ .and. stat /= 0) rc = E_WRITE
        end select
    end function dm_serial_iterate

    ! **************************************************************************
    ! PRIVATE PROCEDURES.
    ! **************************************************************************
    integer function serial_out_beat(format, beat, string, callback, unit, header, separator, newline) result(rc)
        !! Serialises the passed beat type to CSV, JSON/JSONL, or Namelist,
        !! depending on argument `format`.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_INVALID` if format is invalid.
        !! * `E_WRITE` if writing to unit failed.
        !!
        use :: dm_beat

        integer,                       intent(in)            :: format    !! Output format enumerator (`FORMAT_*`).
        type(beat_type),               intent(inout)         :: beat      !! Beat type.
        character(len=:), allocatable, intent(out), optional :: string    !! Output string.
        procedure(dm_serial_callback),              optional :: callback  !! Output callback.
        integer,                       intent(in),  optional :: unit      !! Output unit.
        logical,                       intent(in),  optional :: header    !! Add CSV header.
        character,                     intent(in),  optional :: separator !! CSV separator.
        logical,                       intent(in),  optional :: newline   !! Write newline to output unit.

        character(len=NML_BEAT_LEN)   :: buffer
        character(len=:), allocatable :: output

        integer :: stat
        logical :: callback_, header_, newline_, unit_, string_

        rc = E_NONE

        callback_ = merge(.true.,  .false., present(callback))
        header_   = merge(header,  .false., present(header))
        newline_  = merge(newline, .true.,  present(newline))
        unit_     = merge(.true.,  .false., present(unit))
        string_   = merge(.true.,  .false., present(string))

        select case (format)
            case (FORMAT_CSV)
                if (header_) then
                    output = dm_csv_header_beat(separator) // ASCII_LF // dm_csv_from(beat, separator)
                else
                    output = dm_csv_from(beat, separator)
                end if

            case (FORMAT_JSON, FORMAT_JSONL)
                output = dm_json_from(beat)

            case (FORMAT_NML)
                rc = dm_nml_from(beat, buffer)
                output = trim(buffer)

            case default
                rc = E_INVALID
                return
        end select

        if (string_)   string = output
        if (callback_) call callback(output)

        if (.not. unit_) return

        if (newline_) then
            write (unit, '(a)', iostat=stat) output
        else
            write (unit, '(a)', advance='no', iostat=stat) output
        end if

        if (stat /= 0) rc = E_WRITE
    end function serial_out_beat

    integer function serial_out_log(format, log, string, callback, unit, header, separator, newline) result(rc)
        !! Serialises the passed log type to CSV, JSON/JSONL, or Namelist,
        !! depending on argument `format`.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_INVALID` if format is invalid.
        !! * `E_WRITE` if writing to unit failed.
        !!
        use :: dm_log

        integer,                       intent(in)            :: format    !! Output format enumerator (`FORMAT_*`).
        type(log_type),                intent(inout)         :: log       !! Log type.
        character(len=:), allocatable, intent(out), optional :: string    !! Output string.
        procedure(dm_serial_callback),              optional :: callback  !! Output callback.
        integer,                       intent(in),  optional :: unit      !! Output unit.
        logical,                       intent(in),  optional :: header    !! Add CSV header.
        character,                     intent(in),  optional :: separator !! CSV separator.
        logical,                       intent(in),  optional :: newline   !! Write newline to output unit.

        character(len=NML_LOG_LEN)    :: buffer
        character(len=:), allocatable :: output

        integer :: stat
        logical :: callback_, header_, newline_, unit_, string_

        rc = E_NONE

        callback_ = merge(.true.,  .false., present(callback))
        header_   = merge(header,  .false., present(header))
        newline_  = merge(newline, .true.,  present(newline))
        unit_     = merge(.true.,  .false., present(unit))
        string_   = merge(.true.,  .false., present(string))

        select case (format)
            case (FORMAT_CSV)
                if (header_) then
                    output = dm_csv_header_log(separator) // ASCII_LF // dm_csv_from(log, separator)
                else
                    output = dm_csv_from(log, separator)
                end if

            case (FORMAT_JSON, FORMAT_JSONL)
                output = dm_json_from(log)

            case (FORMAT_NML)
                rc = dm_nml_from(log, buffer)
                output = trim(buffer)

            case default
                rc = E_INVALID
                return
        end select

        if (string_)   string = output
        if (callback_) call callback(output)

        if (.not. unit_) return

        if (newline_) then
            write (unit, '(a)', iostat=stat) output
        else
            write (unit, '(a)', advance='no', iostat=stat) output
        end if

        if (stat /= 0) rc = E_WRITE
    end function serial_out_log

    integer function serial_out_node(format, node, string, callback, unit, header, separator, newline) result(rc)
        !! Serialises the passed node type to CSV, JSON/JSONL, or Namelist,
        !! depending on argument `format`.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_INVALID` if format is invalid.
        !! * `E_WRITE` if writing to unit failed.
        !!
        use :: dm_node

        integer,                       intent(in)            :: format    !! Output format enumerator (`FORMAT_*`).
        type(node_type),               intent(inout)         :: node      !! Node type.
        character(len=:), allocatable, intent(out), optional :: string    !! Output string.
        procedure(dm_serial_callback),              optional :: callback  !! Output callback.
        integer,                       intent(in),  optional :: unit      !! Output unit.
        logical,                       intent(in),  optional :: header    !! Add CSV header.
        character,                     intent(in),  optional :: separator !! CSV separator.
        logical,                       intent(in),  optional :: newline   !! Write newline to output unit.

        character(len=NML_NODE_LEN)   :: buffer
        character(len=:), allocatable :: output

        integer :: stat
        logical :: callback_, header_, newline_, unit_, string_

        rc = E_NONE

        callback_ = merge(.true.,  .false., present(callback))
        header_   = merge(header,  .false., present(header))
        newline_  = merge(newline, .true.,  present(newline))
        unit_     = merge(.true.,  .false., present(unit))
        string_   = merge(.true.,  .false., present(string))

        select case (format)
            case (FORMAT_CSV)
                if (header_) then
                    output = dm_csv_header_node(separator) // ASCII_LF // dm_csv_from(node, separator)
                else
                    output = dm_csv_from(node, separator)
                end if

            case (FORMAT_JSON, FORMAT_JSONL)
                output = dm_json_from(node)

            case (FORMAT_NML)
                rc = dm_nml_from(node, buffer)
                output = trim(buffer)

            case default
                rc = E_INVALID
                return
        end select

        if (string_)   string = output
        if (callback_) call callback(output)

        if (.not. unit_) return

        if (newline_) then
            write (unit, '(a)', iostat=stat) output
        else
            write (unit, '(a)', advance='no', iostat=stat) output
        end if

        if (stat /= 0) rc = E_WRITE
    end function serial_out_node

    integer function serial_out_observ(format, observ, string, callback, unit, header, separator, newline) result(rc)
        !! Serialises the passed observation type to CSV, JSON/JSONL, or
        !! Namelist, depending on argument `format`.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_INVALID` if format is invalid.
        !! * `E_WRITE` if writing to unit failed.
        !!
        use :: dm_observ

        integer,                       intent(in)            :: format    !! Output format enumerator (`FORMAT_*`).
        type(observ_type),             intent(inout)         :: observ    !! Observation type.
        character(len=:), allocatable, intent(out), optional :: string    !! Output string.
        procedure(dm_serial_callback),              optional :: callback  !! Output callback.
        integer,                       intent(in),  optional :: unit      !! Output unit.
        logical,                       intent(in),  optional :: header    !! Add CSV header.
        character,                     intent(in),  optional :: separator !! CSV separator.
        logical,                       intent(in),  optional :: newline   !! Write newline to output unit.

        character(len=NML_OBSERV_LEN) :: buffer
        character(len=:), allocatable :: output

        integer :: stat
        logical :: callback_, header_, newline_, unit_, string_

        rc = E_NONE

        callback_ = merge(.true.,  .false., present(callback))
        header_   = merge(header,  .false., present(header))
        newline_  = merge(newline, .true.,  present(newline))
        unit_     = merge(.true.,  .false., present(unit))
        string_   = merge(.true.,  .false., present(string))

        select case (format)
            case (FORMAT_CSV)
                if (header_) then
                    output = dm_csv_header_observ(separator) // ASCII_LF // dm_csv_from(observ, separator)
                else
                    output = dm_csv_from(observ, separator)
                end if

            case (FORMAT_JSON, FORMAT_JSONL)
                output = dm_json_from(observ)

            case (FORMAT_NML)
                rc = dm_nml_from(observ, buffer)
                output = trim(buffer)

            case default
                rc = E_INVALID
                return
        end select

        if (string_)   string = output
        if (callback_) call callback(output)

        if (.not. unit_) return

        if (newline_) then
            write (unit, '(a)', iostat=stat) output
        else
            write (unit, '(a)', advance='no', iostat=stat) output
        end if

        if (stat /= 0) rc = E_WRITE
    end function serial_out_observ

    integer function serial_out_sensor(format, sensor, string, callback, unit, header, separator, newline) result(rc)
        !! Serialises the passed sensor type to CSV, JSON/JSONL, or Namelist,
        !! depending on argument `format`.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_INVALID` if format is invalid.
        !! * `E_WRITE` if writing to unit failed.
        !!
        use :: dm_sensor

        integer,                       intent(in)            :: format    !! Output format enumerator (`FORMAT_*`).
        type(sensor_type),             intent(inout)         :: sensor    !! Sensor type.
        character(len=:), allocatable, intent(out), optional :: string    !! Output string.
        procedure(dm_serial_callback),              optional :: callback  !! Output callback.
        integer,                       intent(in),  optional :: unit      !! Output unit.
        logical,                       intent(in),  optional :: header    !! Add CSV header.
        character,                     intent(in),  optional :: separator !! CSV separator.
        logical,                       intent(in),  optional :: newline   !! Write newline to output unit.

        character(len=NML_SENSOR_LEN) :: buffer
        character(len=:), allocatable :: output

        integer :: stat
        logical :: callback_, header_, newline_, unit_, string_

        rc = E_NONE

        callback_ = merge(.true.,  .false., present(callback))
        header_   = merge(header,  .false., present(header))
        newline_  = merge(newline, .true.,  present(newline))
        unit_     = merge(.true.,  .false., present(unit))
        string_   = merge(.true.,  .false., present(string))

        select case (format)
            case (FORMAT_CSV)
                if (header_) then
                    output = dm_csv_header_sensor(separator) // ASCII_LF // dm_csv_from(sensor, separator)
                else
                    output = dm_csv_from(sensor, separator)
                end if

            case (FORMAT_JSON, FORMAT_JSONL)
                output = dm_json_from(sensor)

            case (FORMAT_NML)
                rc = dm_nml_from(sensor, buffer)
                output = trim(buffer)

            case default
                rc = E_INVALID
                return
        end select

        if (string_)   string = output
        if (callback_) call callback(output)

        if (.not. unit_) return

        if (newline_) then
            write (unit, '(a)', iostat=stat) output
        else
            write (unit, '(a)', advance='no', iostat=stat) output
        end if

        if (stat /= 0) rc = E_WRITE
    end function serial_out_sensor

    integer function serial_out_target(format, target, string, callback, unit, header, separator, newline) result(rc)
        !! Serialises the passed target type to CSV, JSON/JSONL, or Namelist,
        !! depending on argument `format`.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_INVALID` if format is invalid.
        !! * `E_WRITE` if writing to unit failed.
        !!
        use :: dm_target

        integer,                       intent(in)            :: format    !! Output format enumerator (`FORMAT_*`).
        type(target_type),             intent(inout)         :: target    !! Target type.
        character(len=:), allocatable, intent(out), optional :: string    !! Output string.
        procedure(dm_serial_callback),              optional :: callback  !! Output callback.
        integer,                       intent(in),  optional :: unit      !! Output unit.
        logical,                       intent(in),  optional :: header    !! Add CSV header.
        character,                     intent(in),  optional :: separator !! CSV separator.
        logical,                       intent(in),  optional :: newline   !! Write newline to output unit.

        character(len=NML_TARGET_LEN) :: buffer
        character(len=:), allocatable :: output

        integer :: stat
        logical :: callback_, header_, newline_, unit_, string_

        rc = E_NONE

        callback_ = merge(.true.,  .false., present(callback))
        header_   = merge(header,  .false., present(header))
        newline_  = merge(newline, .true.,  present(newline))
        unit_     = merge(.true.,  .false., present(unit))
        string_   = merge(.true.,  .false., present(string))

        select case (format)
            case (FORMAT_CSV)
                if (header_) then
                    output = dm_csv_header_target(separator) // ASCII_LF // dm_csv_from(target, separator)
                else
                    output = dm_csv_from(target, separator)
                end if

            case (FORMAT_JSON, FORMAT_JSONL)
                output = dm_json_from(target)

            case (FORMAT_NML)
                rc = dm_nml_from(target, buffer)
                output = trim(buffer)

            case default
                rc = E_INVALID
                return
        end select

        if (string_)   string = output
        if (callback_) call callback(output)

        if (.not. unit_) return

        if (newline_) then
            write (unit, '(a)', iostat=stat) output
        else
            write (unit, '(a)', advance='no', iostat=stat) output
        end if

        if (stat /= 0) rc = E_WRITE
    end function serial_out_target
end module dm_serial
