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
    integer, parameter, public :: SENSOR_TYPE_NONE       = 0  !! Unknown sensor type (valid).
    integer, parameter, public :: SENSOR_TYPE_VIRTUAL    = 1  !! Virtual sensor.
    integer, parameter, public :: SENSOR_TYPE_SYSTEM     = 2  !! Operating system.
    integer, parameter, public :: SENSOR_TYPE_FS         = 3  !! File system.
    integer, parameter, public :: SENSOR_TYPE_PROCESS    = 4  !! Process or service.
    integer, parameter, public :: SENSOR_TYPE_NETWORK    = 5  !! Network-based sensor (Ethernet, HTTP).
    integer, parameter, public :: SENSOR_TYPE_MULTI      = 6  !! Multi-sensor system.
    integer, parameter, public :: SENSOR_TYPE_RELAY      = 7  !! Relay.
    integer, parameter, public :: SENSOR_TYPE_RTD        = 8  !! Relay.
    integer, parameter, public :: SENSOR_TYPE_METEO      = 9  !! Meteorological sensor.
    integer, parameter, public :: SENSOR_TYPE_RTS        = 10 !! Robotic total station.
    integer, parameter, public :: SENSOR_TYPE_GNSS       = 11 !! GNSS sensor.
    integer, parameter, public :: SENSOR_TYPE_LEVEL      = 12 !! Level sensor.
    integer, parameter, public :: SENSOR_TYPE_MEMS       = 13 !! MEMS sensor.
    integer, parameter, public :: SENSOR_TYPE_TRANSDUCER = 14 !! Transducer.
    integer, parameter, public :: SENSOR_TYPE_CAMERA     = 15 !! IP camera or webcam.
    integer, parameter, public :: SENSOR_TYPE_MPPT       = 16 !! Maximum Power Point Tracking (MPPT) controller.
    integer, parameter, public :: SENSOR_TYPE_SHUNT      = 17 !! Battery shunt.
    integer, parameter, public :: SENSOR_TYPE_BATTERY    = 18 !! Battery.
    integer, parameter, public :: SENSOR_TYPE_LAST       = 18 !! Never use this.

    integer, parameter, public :: SENSOR_TYPE_NAME_LEN = 10 !! Max. length of sensor type name.

    character(len=*), parameter, public :: SENSOR_TYPE_NAMES(SENSOR_TYPE_NONE:SENSOR_TYPE_LAST) = [ &
        character(len=SENSOR_TYPE_NAME_LEN) :: &
        'none', 'virtual', 'system', 'fs', 'process', 'network', 'multi', 'relay', 'rtd', 'meteo', &
        'rts', 'gnss', 'level', 'mems', 'transducer', 'camera', 'mppt', 'shunt', 'battery' &
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

    integer, parameter, public :: SENSOR_TYPE_SIZE = storage_size(sensor_type()) / 8 !! Size of `sensor_type` in bytes.

    interface operator (==)
        !! Returns `.true.` if sensors are equal.
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
        use :: dm_util,   only: dm_equals

        type(sensor_type), intent(in) :: sensor1 !! First sensor.
        type(sensor_type), intent(in) :: sensor2 !! Second sensor.

        equals = (sensor1%id      == sensor2%id                   .and. &
                  sensor1%node_id == sensor2%node_id              .and. &
                  sensor1%type    == sensor2%type                 .and. &
                  sensor1%name    == sensor2%name                 .and. &
                  sensor1%sn      == sensor2%sn                   .and. &
                  sensor1%meta    == sensor2%meta                 .and. &
                  dm_equals(sensor1%x,         sensor2%x)         .and. &
                  dm_equals(sensor1%y,         sensor2%y)         .and. &
                  dm_equals(sensor1%z,         sensor2%z)         .and. &
                  dm_equals(sensor1%longitude, sensor2%longitude) .and. &
                  dm_equals(sensor1%latitude,  sensor2%latitude)  .and. &
                  dm_equals(sensor1%elevation, sensor2%elevation))
    end function dm_sensor_equals

    pure elemental logical function dm_sensor_is_valid(sensor) result(valid)
        !! Returns `.true.` if the attributes of the given sensor type are
        !! valid.
        use :: dm_string, only: dm_string_is_printable

        type(sensor_type), intent(in) :: sensor !! Sensor type.

        valid = (dm_sensor_type_is_valid(sensor%type) .and. &
                 dm_id_is_valid(sensor%id)            .and. &
                 dm_id_is_valid(sensor%node_id)       .and. &
                 len_trim(sensor%name) > 0            .and. &
                 dm_string_is_printable(sensor%name)  .and. &
                 dm_string_is_printable(sensor%sn)    .and. &
                 dm_string_is_printable(sensor%meta))
    end function dm_sensor_is_valid

    pure elemental integer function dm_sensor_type_from_name(name) result(type)
        !! Returns type enumerator from given name. This function is rather
        !! slow and should not be called inside a loop.
        use :: dm_string, only: dm_to_lower

        character(len=*), intent(in) :: name !! Sensor type name.

        character(len=SENSOR_TYPE_NAME_LEN) :: name_
        integer                             :: i

        name_ = dm_to_lower(name)
        type  = SENSOR_TYPE_NONE

        do i = 1, SENSOR_TYPE_LAST
            if (name_ /= SENSOR_TYPE_NAMES(i)) cycle
            type = i
            exit
        end do
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

        if (.not. dm_sensor_type_is_valid(type) .and. type /= SENSOR_TYPE_NONE) then
            name = 'invalid'
            return
        end if

        name = trim(SENSOR_TYPE_NAMES(type))
    end function dm_sensor_type_to_name

    subroutine dm_sensor_out(sensor, unit)
        !! Prints sensor to standard output or given file unit.
        use :: dm_util, only: dm_present

        character(len=*), parameter :: FMT_REAL = '1pg0.12'

        type(sensor_type), intent(inout)        :: sensor !! Sensor type.
        integer,           intent(in), optional :: unit   !! File unit.

        integer :: unit_

        unit_ = dm_present(unit, stdout)

        write (unit_, '("sensor.id: ", a)')      trim(sensor%id)
        write (unit_, '("sensor.node_id: ", a)') trim(sensor%node_id)
        write (unit_, '("sensor.type: ", i0)')   sensor%type
        write (unit_, '("sensor.name: ", a)')    trim(sensor%name)
        write (unit_, '("sensor.sn: ", a)')      trim(sensor%sn)
        write (unit_, '("sensor.meta: ", a)')    trim(sensor%meta)
        write (unit_, '("sensor.x: ", '         // FMT_REAL // ')') sensor%x
        write (unit_, '("sensor.y: ", '         // FMT_REAL // ')') sensor%y
        write (unit_, '("sensor.z: ", '         // FMT_REAL // ')') sensor%z
        write (unit_, '("sensor.longitude: ", ' // FMT_REAL // ')') sensor%longitude
        write (unit_, '("sensor.latitude: ", '  // FMT_REAL // ')') sensor%latitude
        write (unit_, '("sensor.elevation: ", ' // FMT_REAL // ')') sensor%elevation
    end subroutine dm_sensor_out
end module dm_sensor
