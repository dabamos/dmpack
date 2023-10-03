! Author:  Philipp Engel
! Licence: ISC
module dm_sensor
    !! Sensor type declaration.
    use :: dm_id
    use :: dm_kind
    use :: dm_node
    use :: dm_string
    implicit none (type, external)
    private

    integer, parameter, public :: SENSOR_ID_LEN   = ID_LEN
    integer, parameter, public :: SENSOR_NAME_LEN = 32
    integer, parameter, public :: SENSOR_SN_LEN   = 32
    integer, parameter, public :: SENSOR_META_LEN = 32

    integer, parameter, public :: SENSOR_TYPE_NONE    = 0 !! Unknown sensor type.
    integer, parameter, public :: SENSOR_TYPE_VIRTUAL = 1 !! Virtual sensor.
    integer, parameter, public :: SENSOR_TYPE_FS      = 2 !! File system.
    integer, parameter, public :: SENSOR_TYPE_PROCESS = 3 !! Process or service.
    integer, parameter, public :: SENSOR_TYPE_METEO   = 4 !! Meteorological sensor.
    integer, parameter, public :: SENSOR_TYPE_RTS     = 5 !! Robotic total station.
    integer, parameter, public :: SENSOR_TYPE_GNSS    = 6 !! GNSS sensor.
    integer, parameter, public :: SENSOR_TYPE_LEVEL   = 7 !! Level sensor.
    integer, parameter, public :: SENSOR_TYPE_MEMS    = 8 !! MEMS sensor.
    integer, parameter, public :: SENSOR_NTYPES       = 9 !! Number of sensor types.

    integer, parameter, public :: SENSOR_TYPE_NAME_LEN = 7

    character(len=*), parameter, public :: SENSOR_TYPE_NAMES(0:SENSOR_NTYPES - 1) = [ &
        character(len=SENSOR_TYPE_NAME_LEN) :: &
        'none', 'virtual', 'fs', 'process', 'meteo', 'rts', 'gnss', 'level', 'mems' &
    ] !! Array of sensor type names.

    type, public :: sensor_type
        !! Sensor description.
        character(len=SENSOR_ID_LEN)   :: id       = ' '              !! Sensor id (-0-9A-Za-z).
        character(len=NODE_ID_LEN)     :: node_id  = ' '              !! Associated sensor node.
        integer                        :: type     = SENSOR_TYPE_NONE !! Sensor type.
        character(len=SENSOR_NAME_LEN) :: name     = ' '              !! Sensor name.
        character(len=SENSOR_SN_LEN)   :: sn       = ' '              !! Serial number (optional).
        character(len=SENSOR_META_LEN) :: meta     = ' '              !! Meta information (optional).
    end type sensor_type

    integer, parameter, public :: SENSOR_SIZE = storage_size(sensor_type(), kind=i8) / 8 !! Size of `sensor_type` in bytes.

    interface operator (==)
        !! Returns whether sensors are equal.
        module procedure :: dm_sensor_equals
    end interface

    public :: operator (==)

    public :: dm_sensor_equals
    public :: dm_sensor_type_from_name
    public :: dm_sensor_type_valid
    public :: dm_sensor_out
    public :: dm_sensor_valid
contains
    ! ******************************************************************
    ! PUBLIC PROCEDURES.
    ! ******************************************************************
    pure elemental logical function dm_sensor_equals(sensor1, sensor2) result(equals)
        !! Returns `.true.` if given sensors are equal.
        type(sensor_type), intent(in) :: sensor1 !! First sensor.
        type(sensor_type), intent(in) :: sensor2 !! Second sensor.

        equals = .false.
        if (sensor1%id      /= sensor2%id)      return
        if (sensor1%node_id /= sensor2%node_id) return
        if (sensor1%type    /= sensor2%type)    return
        if (sensor1%name    /= sensor2%name)    return
        if (sensor1%sn      /= sensor2%sn)      return
        if (sensor1%meta    /= sensor2%meta)    return
        equals = .true.
    end function dm_sensor_equals

    pure elemental integer function dm_sensor_type_from_name(name) result(type)
        !! Returns format enumerator from given name.
        character(len=*), intent(in) :: name !! Format name.

        character(len=SENSOR_TYPE_NAME_LEN) :: name_

        ! Normalised name.
        name_ = dm_lower(name)

        select case (name_)
            case (SENSOR_TYPE_NAMES(SENSOR_TYPE_VIRTUAL))
                type = SENSOR_TYPE_VIRTUAL
            case (SENSOR_TYPE_NAMES(SENSOR_TYPE_FS))
                type = SENSOR_TYPE_FS
            case (SENSOR_TYPE_NAMES(SENSOR_TYPE_PROCESS))
                type = SENSOR_TYPE_PROCESS
            case (SENSOR_TYPE_NAMES(SENSOR_TYPE_METEO))
                type = SENSOR_TYPE_METEO
            case (SENSOR_TYPE_NAMES(SENSOR_TYPE_GNSS))
                type = SENSOR_TYPE_GNSS
            case (SENSOR_TYPE_NAMES(SENSOR_TYPE_RTS))
                type = SENSOR_TYPE_RTS
            case (SENSOR_TYPE_NAMES(SENSOR_TYPE_LEVEL))
                type = SENSOR_TYPE_LEVEL
            case (SENSOR_TYPE_NAMES(SENSOR_TYPE_MEMS))
                type = SENSOR_TYPE_MEMS
            case default
                type = SENSOR_TYPE_NONE
        end select
    end function dm_sensor_type_from_name

    pure elemental logical function dm_sensor_type_valid(type) result(valid)
        !! Returns `.true.` if `type` is valid sensor type.
        integer, intent(in) :: type !! Sensor type.

        valid = .false.
        if (type < SENSOR_TYPE_NONE .or. type > SENSOR_NTYPES - 1) return
        valid = .true.
    end function dm_sensor_type_valid

    pure elemental logical function dm_sensor_valid(sensor) result(valid)
        !! Returns `.true.` if the attributes of the given sensor type are
        !! valid.
        type(sensor_type), intent(in) :: sensor !! Sensor type.

        valid = .false.
        if (.not. dm_sensor_type_valid(sensor%type)) return
        if (.not. dm_id_valid(sensor%id)) return
        if (.not. dm_id_valid(sensor%node_id)) return
        if (len_trim(sensor%name) == 0) return
        valid = .true.
    end function dm_sensor_valid

    subroutine dm_sensor_out(sensor, unit)
        !! Prints sensor to standard output or given file unit.
        type(sensor_type), intent(inout)        :: sensor !! Sensor type.
        integer,           intent(in), optional :: unit   !! File unit.

        integer :: unit_

        unit_ = stdout
        if (present(unit)) unit_ = unit

        write (unit_, '("sensor.id: ", a)')      trim(sensor%id)
        write (unit_, '("sensor.node_id: ", a)') trim(sensor%node_id)
        write (unit_, '("sensor.type: ")', advance='no')

        if (dm_sensor_type_valid(sensor%type)) then
            write (unit_, '(a)') trim(SENSOR_TYPE_NAMES(sensor%type))
        else
            write (unit_, '("invalid")')
        end if

        write (unit_, '("sensor.name: ", a)') trim(sensor%name)
        write (unit_, '("sensor.sn: ", a)')   trim(sensor%sn)
        write (unit_, '("sensor.meta: ", a)') trim(sensor%meta)
    end subroutine dm_sensor_out
end module dm_sensor
