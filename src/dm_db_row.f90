! Author:  Philipp Engel
! Licence: ISC
module dm_db_row
    !! Database table row access functions.
    use :: dm_db
    use :: dm_error
    use :: dm_util
    implicit none (type, external)
    private

    interface dm_db_row_next
        !! Private generic table row access function.
        module procedure :: db_row_next_allocatable
        module procedure :: db_row_next_character
        module procedure :: db_row_next_beat
        module procedure :: db_row_next_data_point
        module procedure :: db_row_next_image
        module procedure :: db_row_next_log
        module procedure :: db_row_next_node
        module procedure :: db_row_next_observ
        module procedure :: db_row_next_observ_view
        module procedure :: db_row_next_sensor
        module procedure :: db_row_next_string
        module procedure :: db_row_next_sync
        module procedure :: db_row_next_target
        module procedure :: db_row_next_transfer
    end interface dm_db_row_next

    ! Public procedures.
    public :: dm_db_row_next

    ! Private procedures.
    private :: db_row_next_allocatable
    private :: db_row_next_character
    private :: db_row_next_data_point
    private :: db_row_next_image
    private :: db_row_next_log
    private :: db_row_next_node
    private :: db_row_next_observ
    private :: db_row_next_observ_view
    private :: db_row_next_sensor
    private :: db_row_next_string
    private :: db_row_next_target
    private :: db_row_next_transfer
contains
    ! **************************************************************************
    ! PRIVATE FUNCTIONS.
    ! **************************************************************************
    integer function db_row_next_allocatable(db_stmt, string, validate) result(rc)
        !! Reads string from table row and returns it as allocatable character
        !! string. Column types are validated by default. Returns `E_DB_TYPE`
        !! if the validation failed.
        type(db_stmt_type),            intent(inout)        :: db_stmt  !! Database statement type.
        character(len=:), allocatable, intent(out)          :: string   !! Allocatable character string.
        logical,                       intent(in), optional :: validate !! Validate column types.

        if (dm_present(validate, .true.)) then
            rc = E_DB_TYPE
            if (.not. dm_db_column_is_text(db_stmt, 0)) then
                string = ''
                return
            end if
        end if

        call dm_db_column(db_stmt, 0, string)

        rc = E_NONE
    end function db_row_next_allocatable

    integer function db_row_next_character(db_stmt, string, nbyte, validate) result(rc)
        !! Reads string from table row. The passed argument `str` must be
        !! allocated! Column types are validated by default. Returns
        !! `E_DB_TYPE` if the validation failed.
        type(db_stmt_type), intent(inout)        :: db_stmt  !! Database statement type.
        character(len=*),   intent(inout)        :: string   !! Character string.
        integer,            intent(out)          :: nbyte    !! Size of string in bytes.
        logical,            intent(in), optional :: validate !! Validate column types.

        nbyte  = 0

        if (dm_present(validate, .true.)) then
            rc = E_DB_TYPE
            if (.not. dm_db_column_is_text(db_stmt, 0)) then
                string = ''
                return
            end if
        end if

        call dm_db_column(db_stmt, 0, string, nbyte)

        rc = E_NONE
    end function db_row_next_character

    integer function db_row_next_beat(db_stmt, beat, validate) result(rc)
        !! Reads beat data from table row. Column types are validated by
        !! default. Returns `E_DB_TYPE` if the validation failed.
        use :: dm_beat

        type(db_stmt_type), intent(inout)        :: db_stmt  !! Database statement type.
        type(beat_type),    intent(inout)        :: beat     !! Beat type.
        logical,            intent(in), optional :: validate !! Validate column types.

        integer :: n

        if (dm_present(validate, .true.)) then
            rc = E_DB_TYPE
            if (.not. dm_db_column_is_text   (db_stmt, 0)) return
            if (.not. dm_db_column_is_text   (db_stmt, 1)) return
            if (.not. dm_db_column_is_text   (db_stmt, 2)) return
            if (.not. dm_db_column_is_text   (db_stmt, 3)) return
            if (.not. dm_db_column_is_text   (db_stmt, 4)) return
            if (.not. dm_db_column_is_integer(db_stmt, 5)) return
            if (.not. dm_db_column_is_integer(db_stmt, 6)) return
            if (.not. dm_db_column_is_integer(db_stmt, 7)) return
        end if

        call dm_db_column(db_stmt, 0, beat%node_id,   n)
        call dm_db_column(db_stmt, 1, beat%address,   n)
        call dm_db_column(db_stmt, 2, beat%client,    n)
        call dm_db_column(db_stmt, 3, beat%time_sent, n)
        call dm_db_column(db_stmt, 4, beat%time_recv, n)
        call dm_db_column(db_stmt, 5, beat%error)
        call dm_db_column(db_stmt, 6, beat%interval)
        call dm_db_column(db_stmt, 7, beat%uptime)

        rc = E_NONE
    end function db_row_next_beat

    integer function db_row_next_data_point(db_stmt, dp, validate) result(rc)
        !! Reads observation data from table row. Column types are validated by
        !! default. Returns `E_DB_TYPE` if the validation failed.
        use :: dm_dp

        type(db_stmt_type), intent(inout)        :: db_stmt  !! Database statement type.
        type(dp_type),      intent(inout)        :: dp       !! Data point type.
        logical,            intent(in), optional :: validate !! Validate column types.

        integer :: n

        if (dm_present(validate, .true.)) then
            rc = E_DB_TYPE
            if (.not. dm_db_column_is_text (db_stmt, 0)) return
            if (.not. dm_db_column_is_float(db_stmt, 1)) return
        end if

        call dm_db_column(db_stmt, 0, dp%x, n)
        call dm_db_column(db_stmt, 1, dp%y)

        rc = E_NONE
    end function db_row_next_data_point

    integer function db_row_next_image(db_stmt, image, validate) result(rc)
        !! Reads image data from table row. Column types are validated by
        !! default. Returns `E_DB_TYPE` if the validation failed.
        use :: dm_image

        type(db_stmt_type), intent(inout)        :: db_stmt  !! Database statement type.
        type(image_type),   intent(inout)        :: image    !! Image type.
        logical,            intent(in), optional :: validate !! Validate column types.

        integer :: n

        if (dm_present(validate, .true.)) then
            rc = E_DB_TYPE
            if (.not. dm_db_column_is_text   (db_stmt, 0)) return
            if (.not. dm_db_column_is_text   (db_stmt, 1)) return
            if (.not. dm_db_column_is_text   (db_stmt, 2)) return
            if (.not. dm_db_column_is_text   (db_stmt, 3)) return
            if (.not. dm_db_column_is_text   (db_stmt, 4)) return
            if (.not. dm_db_column_is_text   (db_stmt, 5)) return
            if (.not. dm_db_column_is_integer(db_stmt, 6)) return
            if (.not. dm_db_column_is_integer(db_stmt, 7)) return
            if (.not. dm_db_column_is_integer(db_stmt, 8)) return
        end if

        call dm_db_column(db_stmt, 0, image%id,        n)
        call dm_db_column(db_stmt, 1, image%node_id,   n)
        call dm_db_column(db_stmt, 2, image%sensor_id, n)
        call dm_db_column(db_stmt, 3, image%target_id, n)
        call dm_db_column(db_stmt, 4, image%timestamp, n)
        call dm_db_column(db_stmt, 5, image%mime,      n)
        call dm_db_column(db_stmt, 6, image%width)
        call dm_db_column(db_stmt, 7, image%height)
        call dm_db_column(db_stmt, 8, image%size)

        rc = E_NONE
    end function db_row_next_image

    integer function db_row_next_log(db_stmt, log, validate) result(rc)
        !! Reads log data from table row. Column types are validated by
        !! default. Returns `E_DB_TYPE` if the validation failed.
        use :: dm_log

        type(db_stmt_type), intent(inout)        :: db_stmt  !! Database statement type.
        type(log_type),     intent(inout)        :: log      !! Log type.
        logical,            intent(in), optional :: validate !! Validate column types.

        integer :: n

        if (dm_present(validate, .true.)) then
            rc = E_DB_TYPE
            if (.not. dm_db_column_is_text   (db_stmt, 0)) return
            if (.not. dm_db_column_is_integer(db_stmt, 1)) return
            if (.not. dm_db_column_is_integer(db_stmt, 2)) return
            if (.not. dm_db_column_is_text   (db_stmt, 3)) return
            if (.not. dm_db_column_is_text   (db_stmt, 4)) return
            if (.not. dm_db_column_is_text   (db_stmt, 5)) return
            if (.not. dm_db_column_is_text   (db_stmt, 6)) return
            if (.not. dm_db_column_is_text   (db_stmt, 7)) return
            if (.not. dm_db_column_is_text   (db_stmt, 8)) return
            if (.not. dm_db_column_is_text   (db_stmt, 9)) return
        end if

        call dm_db_column(db_stmt, 0, log%id,        n)
        call dm_db_column(db_stmt, 1, log%level)
        call dm_db_column(db_stmt, 2, log%error)
        call dm_db_column(db_stmt, 3, log%timestamp, n)
        call dm_db_column(db_stmt, 4, log%node_id,   n)
        call dm_db_column(db_stmt, 5, log%sensor_id, n)
        call dm_db_column(db_stmt, 6, log%target_id, n)
        call dm_db_column(db_stmt, 7, log%observ_id, n)
        call dm_db_column(db_stmt, 8, log%source,    n)
        call dm_db_column(db_stmt, 9, log%message,   n)

        rc = E_NONE
    end function db_row_next_log

    integer function db_row_next_node(db_stmt, node, validate) result(rc)
        !! Reads node data from table row. Column types are validated by
        !! default. Returns `E_DB_TYPE` if the validation failed.
        use :: dm_node

        type(db_stmt_type), intent(inout)        :: db_stmt  !! Database statement type.
        type(node_type),    intent(inout)        :: node     !! Node type.
        logical,            intent(in), optional :: validate !! Validate column types.

        integer :: n

        if (dm_present(validate, .true.)) then
            rc = E_DB_TYPE
            if (.not. dm_db_column_is_text (db_stmt, 0)) return
            if (.not. dm_db_column_is_text (db_stmt, 1)) return
            if (.not. dm_db_column_is_text (db_stmt, 2)) return
            if (.not. dm_db_column_is_float(db_stmt, 3)) return
            if (.not. dm_db_column_is_float(db_stmt, 4)) return
            if (.not. dm_db_column_is_float(db_stmt, 5)) return
            if (.not. dm_db_column_is_float(db_stmt, 6)) return
            if (.not. dm_db_column_is_float(db_stmt, 7)) return
            if (.not. dm_db_column_is_float(db_stmt, 8)) return
        end if

        call dm_db_column(db_stmt, 0, node%id,   n)
        call dm_db_column(db_stmt, 1, node%name, n)
        call dm_db_column(db_stmt, 2, node%meta, n)
        call dm_db_column(db_stmt, 3, node%x)
        call dm_db_column(db_stmt, 4, node%y)
        call dm_db_column(db_stmt, 5, node%z)
        call dm_db_column(db_stmt, 6, node%longitude)
        call dm_db_column(db_stmt, 7, node%latitude)
        call dm_db_column(db_stmt, 8, node%elevation)

        rc = E_NONE
    end function db_row_next_node

    integer function db_row_next_observ(db_stmt, observ, validate) result(rc)
        !! Reads observation data from table row. Column types are validated by
        !! default. Returns `E_DB_TYPE` if the validation failed.
        use :: dm_observ

        type(db_stmt_type), intent(inout)        :: db_stmt  !! Database statement type.
        type(observ_type),  intent(inout)        :: observ   !! Observation type.
        logical,            intent(in), optional :: validate !! Validate column types.

        integer :: n

        if (dm_present(validate, .true.)) then
            rc = E_DB_TYPE
            if (.not. dm_db_column_is_text   (db_stmt,  0)) return
            if (.not. dm_db_column_is_text   (db_stmt,  1)) return
            if (.not. dm_db_column_is_text   (db_stmt,  2)) return
            if (.not. dm_db_column_is_text   (db_stmt,  3)) return
            if (.not. dm_db_column_is_text   (db_stmt,  4)) return
            if (.not. dm_db_column_is_text   (db_stmt,  5)) return
            if (.not. dm_db_column_is_text   (db_stmt,  6)) return
            if (.not. dm_db_column_is_text   (db_stmt,  7)) return
            if (.not. dm_db_column_is_integer(db_stmt,  8)) return
            if (.not. dm_db_column_is_integer(db_stmt,  9)) return
            if (.not. dm_db_column_is_integer(db_stmt, 10)) return
            if (.not. dm_db_column_is_integer(db_stmt, 11)) return
            if (.not. dm_db_column_is_integer(db_stmt, 12)) return
        end if

        call dm_db_column(db_stmt,  0, observ%id,        n)
        call dm_db_column(db_stmt,  1, observ%node_id,   n)
        call dm_db_column(db_stmt,  2, observ%sensor_id, n)
        call dm_db_column(db_stmt,  3, observ%target_id, n)
        call dm_db_column(db_stmt,  4, observ%name,      n)
        call dm_db_column(db_stmt,  5, observ%timestamp, n)
        call dm_db_column(db_stmt,  6, observ%source,    n)
        call dm_db_column(db_stmt,  7, observ%device,    n)
        call dm_db_column(db_stmt,  8, observ%priority)
        call dm_db_column(db_stmt,  9, observ%error)
        call dm_db_column(db_stmt, 10, observ%next)
        call dm_db_column(db_stmt, 11, observ%nreceivers)
        call dm_db_column(db_stmt, 12, observ%nrequests)

        rc = E_NONE
    end function db_row_next_observ

    integer function db_row_next_observ_view(db_stmt, view, validate) result(rc)
        !! Reads observation data from table row. Column types are validated by
        !! default. Returns `E_DB_TYPE` if the validation failed.
        use :: dm_observ

        type(db_stmt_type),     intent(inout)        :: db_stmt  !! Database statement type.
        type(observ_view_type), intent(inout)        :: view     !! Observation view type.
        logical,                intent(in), optional :: validate !! Validate column types.

        integer :: n

        if (dm_present(validate, .true.)) then
            rc = E_DB_TYPE
            if (.not. dm_db_column_is_text   (db_stmt,  0)) return
            if (.not. dm_db_column_is_text   (db_stmt,  1)) return
            if (.not. dm_db_column_is_text   (db_stmt,  2)) return
            if (.not. dm_db_column_is_text   (db_stmt,  3)) return
            if (.not. dm_db_column_is_text   (db_stmt,  4)) return
            if (.not. dm_db_column_is_integer(db_stmt,  5)) return
            if (.not. dm_db_column_is_text   (db_stmt,  6)) return
            if (.not. dm_db_column_is_text   (db_stmt,  7)) return
            if (.not. dm_db_column_is_integer(db_stmt,  8)) return
            if (.not. dm_db_column_is_text   (db_stmt,  9)) return
            if (.not. dm_db_column_is_text   (db_stmt, 10)) return
            if (.not. dm_db_column_is_integer(db_stmt, 11)) return
            if (.not. dm_db_column_is_integer(db_stmt, 12)) return
            if (.not. dm_db_column_is_float  (db_stmt, 13)) return
        end if

        call dm_db_column(db_stmt,  0, view%observ_id,         n)
        call dm_db_column(db_stmt,  1, view%node_id,           n)
        call dm_db_column(db_stmt,  2, view%sensor_id,         n)
        call dm_db_column(db_stmt,  3, view%target_id,         n)
        call dm_db_column(db_stmt,  4, view%observ_name,       n)
        call dm_db_column(db_stmt,  5, view%observ_error)
        call dm_db_column(db_stmt,  6, view%request_name,      n)
        call dm_db_column(db_stmt,  7, view%request_timestamp, n)
        call dm_db_column(db_stmt,  8, view%request_error)
        call dm_db_column(db_stmt,  9, view%response_name,     n)
        call dm_db_column(db_stmt, 10, view%response_unit,     n)
        call dm_db_column(db_stmt, 11, view%response_type)
        call dm_db_column(db_stmt, 12, view%response_error)
        call dm_db_column(db_stmt, 13, view%response_value)

        rc = E_NONE
    end function db_row_next_observ_view

    integer function db_row_next_sensor(db_stmt, sensor, validate) result(rc)
        !! Reads sensor data from table row. Column types are validated by
        !! default. Returns `E_DB_TYPE` if the validation failed.
        use :: dm_sensor

        type(db_stmt_type), intent(inout)        :: db_stmt  !! Database statement type.
        type(sensor_type),  intent(inout)        :: sensor   !! Sensor type.
        logical,            intent(in), optional :: validate !! Validate column types.

        integer :: n

        if (dm_present(validate, .true.)) then
            rc = E_DB_TYPE
            if (.not. dm_db_column_is_text   (db_stmt,  0)) return
            if (.not. dm_db_column_is_text   (db_stmt,  1)) return
            if (.not. dm_db_column_is_integer(db_stmt,  2)) return
            if (.not. dm_db_column_is_text   (db_stmt,  3)) return
            if (.not. dm_db_column_is_text   (db_stmt,  4)) return
            if (.not. dm_db_column_is_text   (db_stmt,  5)) return
            if (.not. dm_db_column_is_float  (db_stmt,  6)) return
            if (.not. dm_db_column_is_float  (db_stmt,  7)) return
            if (.not. dm_db_column_is_float  (db_stmt,  8)) return
            if (.not. dm_db_column_is_float  (db_stmt,  9)) return
            if (.not. dm_db_column_is_float  (db_stmt, 10)) return
            if (.not. dm_db_column_is_float  (db_stmt, 11)) return
        end if

        call dm_db_column(db_stmt,  0, sensor%id,      n)
        call dm_db_column(db_stmt,  1, sensor%node_id, n)
        call dm_db_column(db_stmt,  2, sensor%type)
        call dm_db_column(db_stmt,  3, sensor%name,    n)
        call dm_db_column(db_stmt,  4, sensor%sn,      n)
        call dm_db_column(db_stmt,  5, sensor%meta,    n)
        call dm_db_column(db_stmt,  6, sensor%x)
        call dm_db_column(db_stmt,  7, sensor%y)
        call dm_db_column(db_stmt,  8, sensor%z)
        call dm_db_column(db_stmt,  9, sensor%longitude)
        call dm_db_column(db_stmt, 10, sensor%latitude)
        call dm_db_column(db_stmt, 11, sensor%elevation)

        rc = E_NONE
    end function db_row_next_sensor

    integer function db_row_next_string(db_stmt, string, validate) result(rc)
        !! Reads string from table row and returns it as derived type
        !! `string_type`. Column types are validated by default. Returns
        !! `E_DB_TYPE` if the validation failed.
        use :: dm_string, only: string_type

        type(db_stmt_type), intent(inout)        :: db_stmt  !! Database statement type.
        type(string_type),  intent(out)          :: string   !! String type.
        logical,            intent(in), optional :: validate !! Validate column types.

        if (dm_present(validate, .true.)) then
            rc = E_DB_TYPE
            if (.not. dm_db_column_is_text(db_stmt, 0)) then
                string%data = ''
                return
            end if
        end if

        call dm_db_column(db_stmt, 0, string%data)

        rc = E_NONE
    end function db_row_next_string

    integer function db_row_next_sync(db_stmt, sync) result(rc)
        !! Reads sync data from table row. Returns `E_DB_TYPE` on error.
        use :: dm_sync
        use :: dm_time, only: TIME_DEFAULT

        type(db_stmt_type), intent(inout) :: db_stmt !! Database statement type.
        type(sync_type),    intent(inout) :: sync    !! Sync type.

        integer :: n

        rc = E_DB_TYPE
        if (.not. dm_db_column_is_text(db_stmt, 0)) return

        call dm_db_column(db_stmt, 0, sync%id, n)

        if (dm_db_column_is_text(db_stmt, 1)) then
            call dm_db_column(db_stmt, 1, sync%timestamp, n)
        else
            sync%timestamp = TIME_DEFAULT
        end if

        if (dm_db_column_is_integer(db_stmt, 2)) then
            call dm_db_column(db_stmt, 2, sync%code)
        else
            sync%code = 0
        end if

        if (dm_db_column_is_integer(db_stmt, 3)) then
            call dm_db_column(db_stmt, 3, sync%attempts)
        else
            sync%attempts = 0
        end if

        rc = E_NONE
    end function db_row_next_sync

    integer function db_row_next_target(db_stmt, target, validate) result(rc)
        !! Reads target data from table row. Column types are validated by
        !! default. Returns `E_DB_TYPE` if the validation failed.
        use :: dm_target

        type(db_stmt_type), intent(inout)        :: db_stmt  !! Database statement type.
        type(target_type),  intent(inout)        :: target   !! Target type.
        logical,            intent(in), optional :: validate !! Validate column types.

        integer :: n

        if (dm_present(validate, .true.)) then
            rc = E_DB_TYPE
            if (.not. dm_db_column_is_text   (db_stmt, 0)) return
            if (.not. dm_db_column_is_text   (db_stmt, 1)) return
            if (.not. dm_db_column_is_text   (db_stmt, 2)) return
            if (.not. dm_db_column_is_integer(db_stmt, 3)) return
            if (.not. dm_db_column_is_float  (db_stmt, 4)) return
            if (.not. dm_db_column_is_float  (db_stmt, 5)) return
            if (.not. dm_db_column_is_float  (db_stmt, 6)) return
            if (.not. dm_db_column_is_float  (db_stmt, 7)) return
            if (.not. dm_db_column_is_float  (db_stmt, 8)) return
            if (.not. dm_db_column_is_float  (db_stmt, 9)) return
        end if

        call dm_db_column(db_stmt, 0, target%id,   n)
        call dm_db_column(db_stmt, 1, target%name, n)
        call dm_db_column(db_stmt, 2, target%meta, n)
        call dm_db_column(db_stmt, 3, target%state)
        call dm_db_column(db_stmt, 4, target%x)
        call dm_db_column(db_stmt, 5, target%y)
        call dm_db_column(db_stmt, 6, target%z)
        call dm_db_column(db_stmt, 7, target%longitude)
        call dm_db_column(db_stmt, 8, target%latitude)
        call dm_db_column(db_stmt, 9, target%elevation)

        rc = E_NONE
    end function db_row_next_target

    integer function db_row_next_transfer(db_stmt, transfer, validate) result(rc)
        !! Reads transfer data from table row. Column types are validated by
        !! default. Returns `E_DB_TYPE` if the validation failed.
        use :: dm_transfer

        type(db_stmt_type),  intent(inout)        :: db_stmt  !! Database statement type.
        type(transfer_type), intent(inout)        :: transfer !! Transfer type.
        logical,             intent(in), optional :: validate !! Validate column types.

        integer :: n

        if (dm_present(validate, .true.)) then
            rc = E_DB_TYPE
            if (.not. dm_db_column_is_text   (db_stmt, 0)) return
            if (.not. dm_db_column_is_text   (db_stmt, 1)) return
            if (.not. dm_db_column_is_text   (db_stmt, 2)) return
            if (.not. dm_db_column_is_text   (db_stmt, 3)) return
            if (.not. dm_db_column_is_text   (db_stmt, 4)) return
            if (.not. dm_db_column_is_integer(db_stmt, 5)) return
            if (.not. dm_db_column_is_integer(db_stmt, 6)) return
            if (.not. dm_db_column_is_integer(db_stmt, 7)) return
            if (.not. dm_db_column_is_integer(db_stmt, 8)) return
        end if

        call dm_db_column(db_stmt, 0, transfer%id,        n)
        call dm_db_column(db_stmt, 1, transfer%node_id,   n)
        call dm_db_column(db_stmt, 2, transfer%type_id,   n)
        call dm_db_column(db_stmt, 3, transfer%timestamp, n)
        call dm_db_column(db_stmt, 4, transfer%address,   n)
        call dm_db_column(db_stmt, 5, transfer%type)
        call dm_db_column(db_stmt, 6, transfer%state)
        call dm_db_column(db_stmt, 7, transfer%error)
        call dm_db_column(db_stmt, 8, transfer%size)

        rc = E_NONE
    end function db_row_next_transfer
end module dm_db_row
