! Author:  Philipp Engel
! Licence: ISC
module dm_sensor
    !! Sensor type declaration.
    use :: dm_id
    use :: dm_kind
    use :: dm_node
    implicit none (type, external)
    private

    integer, parameter, public :: SENSOR_ID_LEN   = ID_LEN !! Max. sensor id length.
    integer, parameter, public :: SENSOR_NAME_LEN = 32     !! Max. sensor name length.
    integer, parameter, public :: SENSOR_SN_LEN   = 32     !! Max. sensor serial number length.
    integer, parameter, public :: SENSOR_META_LEN = 32     !! Max. sensor meta description length.

    ! Sensor types.
    integer, parameter, public :: SENSOR_TYPE_NONE    = 0  !! Unknown sensor type (valid).
    integer, parameter, public :: SENSOR_TYPE_VIRTUAL = 1  !! Virtual sensor.
    integer, parameter, public :: SENSOR_TYPE_SYSTEM  = 2  !! Operating system.
    integer, parameter, public :: SENSOR_TYPE_FS      = 3  !! File system.
    integer, parameter, public :: SENSOR_TYPE_PROCESS = 4  !! Process or service.
    integer, parameter, public :: SENSOR_TYPE_NETWORK = 5  !! Network-based sensor (Ethernet, HTTP).
    integer, parameter, public :: SENSOR_TYPE_MULTI   = 6  !! Multi-sensor system.
    integer, parameter, public :: SENSOR_TYPE_METEO   = 7  !! Meteorological sensor.
    integer, parameter, public :: SENSOR_TYPE_RTS     = 8  !! Robotic total station.
    integer, parameter, public :: SENSOR_TYPE_GNSS    = 9  !! GNSS sensor.
    integer, parameter, public :: SENSOR_TYPE_LEVEL   = 10 !! Level sensor.
    integer, parameter, public :: SENSOR_TYPE_MEMS    = 11 !! MEMS sensor.
    integer, parameter, public :: SENSOR_TYPE_CAMERA  = 12 !! IP camera or webcam.
    integer, parameter, public :: SENSOR_TYPE_MPPT    = 13 !! Maximum Power Point Tracking (MPPT) solar controller.
    integer, parameter, public :: SENSOR_TYPE_SHUNT   = 14 !! Solar battery shunt.
    integer, parameter, public :: SENSOR_TYPE_LAST    = 14 !! Never use this.

    integer, parameter, public :: SENSOR_TYPE_NAME_LEN = 7 !! Max. length of sensor type name.

    character(len=*), parameter, public :: SENSOR_TYPE_NAMES(SENSOR_TYPE_NONE:SENSOR_TYPE_LAST) = [ &
        character(len=SENSOR_TYPE_NAME_LEN) :: &
        'none', 'virtual', 'system', 'fs', 'process', 'network', 'multi', 'meteo', 'rts', &
        'gnss', 'level', 'mems', 'camera', 'mppt', 'shunt' &
    ] !! Array of sensor type names.

    type, public :: sensor_type
        !! Sensor description.
        character(len=SENSOR_ID_LEN)   :: id        = ' '              !! Sensor id (`-0-9A-Z_a-z`).
        character(len=NODE_ID_LEN)     :: node_id   = ' '              !! Associated sensor node.
        integer                        :: type      = SENSOR_TYPE_NONE !! Sensor type.
        character(len=SENSOR_NAME_LEN) :: name      = ' '              !! Sensor name.
        character(len=SENSOR_SN_LEN)   :: sn        = ' '              !! Serial number (optional).
        character(len=SENSOR_META_LEN) :: meta      = ' '              !! Meta information (optional).
        real(kind=r8)                  :: x         = 0.0_r8           !! Sensor x or easting (optional).
        real(kind=r8)                  :: y         = 0.0_r8           !! Sensor y or northing (optional).
        real(kind=r8)                  :: z         = 0.0_r8           !! Sensor z or elevation (optional).
        real(kind=r8)                  :: longitude = 0.0_r8           !! Longitude in degrees (optional).
        real(kind=r8)                  :: latitude  = 0.0_r8           !! Latitude in degrees (optional).
        real(kind=r8)                  :: elevation = 0.0_r8           !! Elevation in metres (optional).
    end type sensor_type

    integer, parameter, public :: SENSOR_SIZE = storage_size(sensor_type()) / 8 !! Size of `sensor_type` in bytes.

    interface operator (==)
        !! Returns whether sensors are equal.
        module procedure :: dm_sensor_equals
    end interface

    public :: operator (==)

    public :: dm_sensor_equals
    public :: dm_sensor_is_valid
    public :: dm_sensor_out
    public :: dm_sensor_type_from_name
    public :: dm_sensor_type_is_valid
    public :: dm_sensor_type_to_name
contains
    ! **************************************************************************
    ! PUBLIC PROCEDURES.
    ! **************************************************************************
    pure elemental logical function dm_sensor_equals(sensor1, sensor2) result(equals)
        !! Returns `.true.` if given sensors are equal.
        use :: dm_util, only: dm_equals

        type(sensor_type), intent(in) :: sensor1 !! First sensor.
        type(sensor_type), intent(in) :: sensor2 !! Second sensor.

        equals = .false.

        if (sensor1%id      /= sensor2%id)      return
        if (sensor1%node_id /= sensor2%node_id) return
        if (sensor1%type    /= sensor2%type)    return
        if (sensor1%name    /= sensor2%name)    return
        if (sensor1%sn      /= sensor2%sn)      return
        if (sensor1%meta    /= sensor2%meta)    return

        if (.not. dm_equals(sensor1%x, sensor2%x)) return
        if (.not. dm_equals(sensor1%y, sensor2%y)) return
        if (.not. dm_equals(sensor1%z, sensor2%z)) return

        if (.not. dm_equals(sensor1%longitude, sensor2%longitude)) return
        if (.not. dm_equals(sensor1%latitude,  sensor2%latitude))  return
        if (.not. dm_equals(sensor1%elevation, sensor2%elevation)) return

        equals = .true.
    end function dm_sensor_equals

    pure elemental logical function dm_sensor_is_valid(sensor) result(valid)
        !! Returns `.true.` if the attributes of the given sensor type are
        !! valid.
        type(sensor_type), intent(in) :: sensor !! Sensor type.

        valid = .false.
        if (.not. dm_sensor_type_is_valid(sensor%type)) return
        if (.not. dm_id_is_valid(sensor%id)) return
        if (.not. dm_id_is_valid(sensor%node_id)) return
        if (len_trim(sensor%name) == 0) return
        valid = .true.
    end function dm_sensor_is_valid

    pure elemental integer function dm_sensor_type_from_name(name) result(type)
        !! Returns type enumerator from given name.
        use :: dm_string, only: dm_to_lower
        character(len=*), intent(in) :: name !! Sensor type name.

        character(len=SENSOR_TYPE_NAME_LEN) :: name_

        ! Normalise name.
        name_ = dm_to_lower(name)

        select case (name_)
            case (SENSOR_TYPE_NAMES(SENSOR_TYPE_VIRTUAL)); type = SENSOR_TYPE_VIRTUAL
            case (SENSOR_TYPE_NAMES(SENSOR_TYPE_SYSTEM));  type = SENSOR_TYPE_SYSTEM
            case (SENSOR_TYPE_NAMES(SENSOR_TYPE_FS));      type = SENSOR_TYPE_FS
            case (SENSOR_TYPE_NAMES(SENSOR_TYPE_PROCESS)); type = SENSOR_TYPE_PROCESS
            case (SENSOR_TYPE_NAMES(SENSOR_TYPE_NETWORK)); type = SENSOR_TYPE_NETWORK
            case (SENSOR_TYPE_NAMES(SENSOR_TYPE_MULTI));   type = SENSOR_TYPE_MULTI
            case (SENSOR_TYPE_NAMES(SENSOR_TYPE_METEO));   type = SENSOR_TYPE_METEO
            case (SENSOR_TYPE_NAMES(SENSOR_TYPE_GNSS));    type = SENSOR_TYPE_GNSS
            case (SENSOR_TYPE_NAMES(SENSOR_TYPE_RTS));     type = SENSOR_TYPE_RTS
            case (SENSOR_TYPE_NAMES(SENSOR_TYPE_LEVEL));   type = SENSOR_TYPE_LEVEL
            case (SENSOR_TYPE_NAMES(SENSOR_TYPE_MEMS));    type = SENSOR_TYPE_MEMS
            case (SENSOR_TYPE_NAMES(SENSOR_TYPE_CAMERA));  type = SENSOR_TYPE_CAMERA
            case default;                                  type = SENSOR_TYPE_NONE
        end select
    end function dm_sensor_type_from_name

    pure elemental logical function dm_sensor_type_is_valid(type) result(valid)
        !! Returns `.true.` if `type` is valid sensor type. The type
        !! `SENSOR_TYPE_NONE` is a valid type.
        integer, intent(in) :: type !! Sensor type.

        valid = (type >= SENSOR_TYPE_NONE .and. type <= SENSOR_TYPE_LAST)
    end function dm_sensor_type_is_valid

    pure function dm_sensor_type_to_name(type) result(name)
        !! Returns name of given type enumerator as allocatable string.
        integer, intent(in)           :: type !! Sensor type enumerator (`SENSOR_TYPE_*`).
        character(len=:), allocatable :: name !! Sensor type name.

        if (.not. dm_sensor_type_is_valid(type)) then
            name = 'invalid'
            return
        end if

        name = trim(SENSOR_TYPE_NAMES(type))
    end function dm_sensor_type_to_name

    subroutine dm_sensor_out(sensor, unit)
        !! Prints sensor to standard output or given file unit.
        use :: dm_util, only: dm_present

        type(sensor_type), intent(inout)        :: sensor !! Sensor type.
        integer,           intent(in), optional :: unit   !! File unit.

        integer :: unit_

        unit_ = dm_present(unit, stdout)

        write (unit_, '("sensor.id: ", a)')              trim(sensor%id)
        write (unit_, '("sensor.node_id: ", a)')         trim(sensor%node_id)
        write (unit_, '("sensor.type: ", i0)')           sensor%type
        write (unit_, '("sensor.name: ", a)')            trim(sensor%name)
        write (unit_, '("sensor.sn: ", a)')              trim(sensor%sn)
        write (unit_, '("sensor.meta: ", a)')            trim(sensor%meta)
        write (unit_, '("sensor.x: ", 1pg0.12)')         sensor%x
        write (unit_, '("sensor.y: ", 1pg0.12)')         sensor%y
        write (unit_, '("sensor.z: ", 1pg0.12)')         sensor%z
        write (unit_, '("sensor.longitude: ", 1pg0.12)') sensor%longitude
        write (unit_, '("sensor.latitude: ", 1pg0.12)')  sensor%latitude
        write (unit_, '("sensor.elevation: ", 1pg0.12)') sensor%elevation
    end subroutine dm_sensor_out
end module dm_sensor
