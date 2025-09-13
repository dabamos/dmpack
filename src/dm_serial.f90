! Author:  Philipp Engel
! Licence: ISC
module dm_serial
    !! Module for serialising beat, log, node, observation, sensor, and target
    !! derived types to CSV, JSON, JSONL, and Namelist.
    !!
    !! The following serialisation formats are valid:
    !!
    !! * `FORMAT_CSV`
    !! * `FORMAT_JSON`
    !! * `FORMAT_JSONL`
    !! * `FORMAT_NML`
    !!
    !! The example reads sensors from an observation database and writes them
    !! in JSON format to a scratch file:
    !!
    !! ```fortran
    !! integer            :: rc, unit
    !! type(db_type)      :: db
    !! type(db_stmt_type) :: db_stmt
    !! type(sensor_type)  :: sensor
    !! type(serial_class) :: serial
    !!
    !! rc = dm_db_open(db, '/var/dmpack/observ.sqlite')
    !! rc = dm_db_select_sensors(db, db_stmt, sensor)
    !!
    !! open (action='readwrite', form='formatted', newunit=unit, status='scratch')
    !! call serial%create(sensor, FORMAT_JSON, unit=unit, empty=(rc /= E_NONE))
    !!
    !! do while (rc == E_NONE)
    !!     call serial%next(sensor)
    !!     rc = dm_db_select_sensors(db, db_stmt, sensor)
    !! end do
    !!
    !! call serial%destroy()
    !! close (unit)
    !!
    !! call dm_db_finalize(db_stmt)
    !! call dm_db_close(db)
    !! ```
    use :: dm_ascii
    use :: dm_csv
    use :: dm_error
    use :: dm_format
    use :: dm_json
    use :: dm_kind
    use :: dm_nml
    use :: dm_type
    implicit none (type, external)
    private

    integer, parameter, public :: SERIAL_UNIT_NONE = -99999 !! Default file unit.

    abstract interface
        subroutine dm_serial_callback(string)
            !! Public callback routine to pass serialised type to.
            character(*), intent(in) :: string !! Serialised type string.
        end subroutine dm_serial_callback
    end interface

    type, public :: serial_class
        !! Serialisation class.
        private
        integer                                        :: format    = FORMAT_NONE      !! Output format (`FORMAT_*`).
        integer                                        :: unit      = SERIAL_UNIT_NONE !! Optional output unit.
        logical                                        :: empty     = .false.          !! No data to expect.
        logical                                        :: first     = .true.           !! First element flag.
        logical                                        :: header    = .false.          !! Output CSV header.
        logical                                        :: newline   = .false.          !! Add newline to callback argument (CSV, JSONL, NML).
        character                                      :: separator = CSV_SEPARATOR    !! CSV separator.
        procedure(dm_serial_callback), pointer, nopass :: callback  => null()          !! Optional output callback.
    contains
        private
        ! Private methods.
        procedure         :: next_beat   => serial_next_beat
        procedure         :: next_log    => serial_next_log
        procedure         :: next_node   => serial_next_node
        procedure         :: next_observ => serial_next_observ
        procedure         :: next_sensor => serial_next_sensor
        procedure         :: next_target => serial_next_target
        procedure         :: out         => serial_out
        ! Public methods.
        procedure, public :: create      => serial_create
        procedure, public :: destroy     => serial_destroy
        generic,   public :: next        => next_beat,   &
                                            next_log,    &
                                            next_node,   &
                                            next_observ, &
                                            next_sensor, &
                                            next_target
    end type serial_class

    public :: dm_serial_callback

    private :: serial_create
    private :: serial_destroy
    private :: serial_next_beat
    private :: serial_next_log
    private :: serial_next_node
    private :: serial_next_observ
    private :: serial_next_sensor
    private :: serial_next_target
    private :: serial_out
contains
    ! **************************************************************************
    ! PRIVATE PROCEDURES.
    ! **************************************************************************
    subroutine serial_create(this, type, format, callback, unit, empty, header, newline, separator, error)
        !! Constructor of serialisation class. Argument `type` must be one of:
        !!
        !! * `beat_type`
        !! * `log_type`
        !! * `node_type`
        !! * `observ_type`
        !! * `sensor_type`
        !! * `target_type`
        !!
        !! If argument `newline` is passed and `.true.`, a newline character is
        !! appended to output in format CSV, JSONL, or NML passed to the
        !! callback routine `callback`. The argument `error` is set to `E_INVALID`
        !! if one of the arguments is invalid.
        use :: dm_beat,   only: beat_type
        use :: dm_log,    only: log_type
        use :: dm_node,   only: node_type
        use :: dm_observ, only: observ_type
        use :: dm_sensor, only: sensor_type
        use :: dm_target, only: target_type

        class(serial_class), intent(out)           :: this      !! Serial object to create.
        class(*),            intent(inout)         :: type      !! Type to serialise.
        integer,             intent(in)            :: format    !! Format enumerator (`FORMAT_*`).
        procedure(dm_serial_callback),    optional :: callback  !! Output callback.
        integer,             intent(in),  optional :: unit      !! Output unit.
        logical,             intent(in),  optional :: empty     !! No content to expect.
        logical,             intent(in),  optional :: header    !! Add CSV header.
        logical,             intent(in),  optional :: newline   !! Add newline to callback argument.
        character,           intent(in),  optional :: separator !! CSV separator.
        integer,             intent(out), optional :: error     !! Error code.

        logical :: valid
        integer :: type_

        if (present(error)) error = E_INVALID

        select type (type)
            type is (beat_type);   type_ = TYPE_BEAT
            type is (log_type);    type_ = TYPE_LOG
            type is (node_type);   type_ = TYPE_NODE
            type is (observ_type); type_ = TYPE_OBSERV
            type is (sensor_type); type_ = TYPE_SENSOR
            type is (target_type); type_ = TYPE_TARGET
            class default;         type_ = TYPE_NONE
        end select

        if (.not. dm_type_is_valid(type_)) return

        valid = (format == FORMAT_CSV .or. format == FORMAT_JSON .or. format == FORMAT_JSONL .or. format == FORMAT_NML)
        if (.not. valid) return
        this%format = format

        if (present(callback))  this%callback  => callback
        if (present(unit))      this%unit      = unit
        if (present(empty))     this%empty     = empty
        if (present(header))    this%header    = header
        if (present(separator)) this%separator = separator

        if (present(newline)) then
            this%newline = (newline .and. (this%format == FORMAT_CSV .or. this%format == FORMAT_JSONL .or. this%format == FORMAT_NML))
        end if

        select case (this%format)
            case (FORMAT_CSV)
                if (this%header) then
                    select case (type_)
                        case (TYPE_BEAT);   call this%out(dm_csv_header_beat  (this%separator), error)
                        case (TYPE_LOG);    call this%out(dm_csv_header_log   (this%separator), error)
                        case (TYPE_NODE);   call this%out(dm_csv_header_node  (this%separator), error)
                        case (TYPE_OBSERV); call this%out(dm_csv_header_observ(this%separator), error)
                        case (TYPE_SENSOR); call this%out(dm_csv_header_sensor(this%separator), error)
                        case (TYPE_TARGET); call this%out(dm_csv_header_target(this%separator), error)
                    end select
                end if

            case (FORMAT_JSON)
                if (this%empty) then
                    call this%out('[]', error)
                else
                    call this%out('[', error)
                end if
        end select
    end subroutine serial_create

    subroutine serial_destroy(this, error)
        !! Destroys serialisation object and outputs last bytes.
        class(serial_class), intent(inout)         :: this  !! Serial object.
        integer,             intent(out), optional :: error !! Error code.

        if (present(error)) error = E_NONE
        if (this%format == FORMAT_JSON .and. .not. this%empty) call this%out(']', error)

        this%callback => null()
        this%unit     = SERIAL_UNIT_NONE
    end subroutine serial_destroy

    subroutine serial_next_beat(this, beat, error)
        !! Serialises the passed beat type to CSV, JSON, JSONL, or Namelist,
        !! depending on the format configured.
        !!
        !! On error, the subroutine sets argument `error` to:
        !!
        !! * `E_EMPTY` if the serial object has been declared as empty.
        !! * `E_WRITE` if writing to unit failed.
        !!
        use :: dm_beat

        class(serial_class), intent(inout)         :: this  !! Serial object.
        type(beat_type),     intent(inout)         :: beat  !! Beat type.
        integer,             intent(out), optional :: error !! Error code.

        character(NML_BEAT_LEN) :: buffer
        integer                 :: rc

        if (present(error)) error = E_EMPTY
        if (this%empty) return

        select case (this%format)
            case (FORMAT_CSV)
                call this%out(dm_csv_from(beat, this%separator), error)

            case (FORMAT_JSON)
                if (this%first) then
                    call this%out(dm_json_from(beat), error)
                else
                    call this%out(',' // dm_json_from(beat), error)
                end if

            case (FORMAT_JSONL)
                call this%out(dm_json_from(beat), error)

            case (FORMAT_NML)
                rc = dm_nml_from(beat, buffer)
                call this%out(trim(buffer), error)
        end select

        this%first = .false.
    end subroutine serial_next_beat

    subroutine serial_next_log(this, log, error)
        !! Serialises the passed log type to CSV, JSON, JSONL, or Namelist,
        !! depending on the format configured.
        !!
        !! On error, the subroutine sets argument `error` to:
        !!
        !! * `E_EMPTY` if the serial object has been declared as empty.
        !! * `E_WRITE` if writing to unit failed.
        !!
        use :: dm_log

        class(serial_class), intent(inout)         :: this  !! Serial object.
        type(log_type),      intent(inout)         :: log   !! Log type.
        integer,             intent(out), optional :: error !! Error code.

        character(NML_LOG_LEN) :: buffer
        integer                :: rc

        if (present(error)) error = E_EMPTY
        if (this%empty) return

        select case (this%format)
            case (FORMAT_CSV)
                call this%out(dm_csv_from(log, this%separator), error)

            case (FORMAT_JSON)
                if (this%first) then
                    call this%out(dm_json_from(log), error)
                else
                    call this%out(',' // dm_json_from(log), error)
                end if

            case (FORMAT_JSONL)
                call this%out(dm_json_from(log), error)

            case (FORMAT_NML)
                rc = dm_nml_from(log, buffer)
                call this%out(trim(buffer), error)
        end select

        this%first = .false.
    end subroutine serial_next_log

    subroutine serial_next_node(this, node, error)
        !! Serialises the passed node type to CSV, JSON, JSONL, or Namelist,
        !! depending on the format configured.
        !!
        !! On error, the subroutine sets argument `error` to:
        !!
        !! * `E_EMPTY` if the serial object has been declared as empty.
        !! * `E_WRITE` if writing to unit failed.
        !!
        use :: dm_node

        class(serial_class), intent(inout)         :: this  !! Serial object.
        type(node_type),     intent(inout)         :: node  !! Node type.
        integer,             intent(out), optional :: error !! Error code.

        character(NML_NODE_LEN) :: buffer
        integer                 :: rc

        if (present(error)) error = E_EMPTY
        if (this%empty) return

        select case (this%format)
            case (FORMAT_CSV)
                call this%out(dm_csv_from(node, this%separator), error)

            case (FORMAT_JSON)
                if (this%first) then
                    call this%out(dm_json_from(node), error)
                else
                    call this%out(',' // dm_json_from(node), error)
                end if

            case (FORMAT_JSONL)
                call this%out(dm_json_from(node), error)

            case (FORMAT_NML)
                rc = dm_nml_from(node, buffer)
                call this%out(trim(buffer), error)
        end select

        this%first = .false.
    end subroutine serial_next_node

    subroutine serial_next_observ(this, observ, error)
        !! Serialises the passed observation type to CSV, JSON, JSONL, or
        !! Namelist, depending on the format configured.
        !!
        !! On error, the subroutine sets argument `error` to:
        !!
        !! * `E_EMPTY` if the serial object has been declared as empty.
        !! * `E_WRITE` if writing to unit failed.
        !!
        use :: dm_observ

        class(serial_class), intent(inout)         :: this   !! Serial object.
        type(observ_type),   intent(inout)         :: observ !! Observation type.
        integer,             intent(out), optional :: error  !! Error code.

        character(NML_OBSERV_LEN) :: buffer
        integer                   :: rc

        if (present(error)) error = E_EMPTY
        if (this%empty) return

        select case (this%format)
            case (FORMAT_CSV)
                call this%out(dm_csv_from(observ, this%separator), error)

            case (FORMAT_JSON)
                if (this%first) then
                    call this%out(dm_json_from(observ), error)
                else
                    call this%out(',' // dm_json_from(observ), error)
                end if

            case (FORMAT_JSONL)
                call this%out(dm_json_from(observ), error)

            case (FORMAT_NML)
                rc = dm_nml_from(observ, buffer)
                call this%out(trim(buffer), error)
        end select

        this%first = .false.
    end subroutine serial_next_observ

    subroutine serial_next_sensor(this, sensor, error)
        !! Serialises the passed sensor type to CSV, JSON, JSONL, or Namelist,
        !! depending on the format configured.
        !!
        !! On error, the subroutine sets argument `error` to:
        !!
        !! * `E_EMPTY` if the serial object has been declared as empty.
        !! * `E_WRITE` if writing to unit failed.
        !!
        use :: dm_sensor

        class(serial_class), intent(inout)         :: this   !! Serial object.
        type(sensor_type),   intent(inout)         :: sensor !! Sensor type.
        integer,             intent(out), optional :: error  !! Error code.

        character(NML_SENSOR_LEN) :: buffer
        integer                   :: rc

        if (present(error)) error = E_EMPTY
        if (this%empty) return

        select case (this%format)
            case (FORMAT_CSV)
                call this%out(dm_csv_from(sensor, this%separator), error)

            case (FORMAT_JSON)
                if (this%first) then
                    call this%out(dm_json_from(sensor), error)
                else
                    call this%out(',' // dm_json_from(sensor), error)
                end if

            case (FORMAT_JSONL)
                call this%out(dm_json_from(sensor), error)

            case (FORMAT_NML)
                rc = dm_nml_from(sensor, buffer)
                call this%out(trim(buffer), error)
        end select

        this%first = .false.
    end subroutine serial_next_sensor

    subroutine serial_next_target(this, target, error)
        !! Serialises the passed target type to CSV, JSON, JSONL, or Namelist,
        !! depending on the format configured.
        !!
        !! On error, the subroutine sets argument `error` to:
        !!
        !! * `E_EMPTY` if the serial object has been declared as empty.
        !! * `E_WRITE` if writing to unit failed.
        !!
        use :: dm_target

        class(serial_class), intent(inout)         :: this   !! Serial object.
        type(target_type),   intent(inout)         :: target !! Target type.
        integer,             intent(out), optional :: error  !! Error code.

        character(NML_TARGET_LEN) :: buffer
        integer                   :: rc

        if (present(error)) error = E_EMPTY
        if (this%empty) return

        select case (this%format)
            case (FORMAT_CSV)
                call this%out(dm_csv_from(target, this%separator), error)

            case (FORMAT_JSON)
                if (this%first) then
                    call this%out(dm_json_from(target), error)
                else
                    call this%out(',' // dm_json_from(target), error)
                end if

            case (FORMAT_JSONL)
                call this%out(dm_json_from(target), error)

            case (FORMAT_NML)
                rc = dm_nml_from(target, buffer)
                call this%out(trim(buffer), error)
        end select

        this%first = .false.
    end subroutine serial_next_target

    subroutine serial_out(this, string, error)
        !! Writes string to callback and unit, if configured. On error,
        !! argument `error` is set to `E_WRITE` if writing to the file unit
        !! failed. The string passed to the callback is appended with a newline
        !! character in CSV, JSONL, or NML format.
        use :: dm_ascii, only: ASCII_LF

        class(serial_class), intent(inout)         :: this   !! Serial object.
        character(*),        intent(in)            :: string !! Output string.
        integer,             intent(out), optional :: error  !! Error code.

        integer :: stat

        if (present(error)) error = E_NONE

        if (associated(this%callback)) then
            if (this%newline) then
                call this%callback(string // ASCII_LF)
            else
                call this%callback(string)
            end if
        end if

        if (this%unit /= SERIAL_UNIT_NONE) then
            if (this%format == FORMAT_JSON) then
                write (this%unit, '(a)', advance='no', iostat=stat) string
            else
                write (this%unit, '(a)', iostat=stat) string
            end if
            if (present(error) .and. stat /= 0) error = E_WRITE
        end if
    end subroutine serial_out
end module dm_serial
