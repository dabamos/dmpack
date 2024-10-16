! Author:  Philipp Engel
! Licence: ISC
module dm_geojson
    !! Contains subroutines to convert derived types to
    !! [GeoJSON](https://geojson.org/) format (RFC 7946).
    use :: dm_ascii, only: NL => ASCII_LF
    use :: dm_error
    use :: dm_json
    use :: dm_kind
    use :: dm_type
    use :: dm_util
    implicit none (type, external)
    private

    interface dm_geojson_from
        !! Generic derived type to GeoJSON serialisation functions.
        module procedure :: geojson_from_node
        module procedure :: geojson_from_sensor
        module procedure :: geojson_from_target
    end interface dm_geojson_from

    interface dm_geojson_write
        !! Generic derived type to GeoJSON writer.
        module procedure :: geojson_write_node
        module procedure :: geojson_write_sensor
        module procedure :: geojson_write_target
    end interface dm_geojson_write

    public :: dm_geojson_feature_point
    public :: dm_geojson_from
    public :: dm_geojson_write

    private :: geojson_from_node
    private :: geojson_from_sensor
    private :: geojson_from_target
    private :: geojson_write_node
    private :: geojson_write_sensor
    private :: geojson_write_target
contains
    ! **************************************************************************
    ! PUBLIC PROCEDURES.
    ! **************************************************************************
    subroutine dm_geojson_feature_point(geojson, type, lon, lat, alt, data, comma)
        !! Returns GeoJSON feature point of given DMPACK type, longitude,
        !! latitude, altitude, and type data in JSON. The output string
        !! `geojson` is of the following form:
        !!
        !! ```json
        !! {
        !!   "type": "Feature",
        !!   "geometry": {
        !!     "type": "Point",
        !!     "coordinates": [
        !!       10.4541194000,
        !!       51.1642292000,
        !!       10.0000000000
        !!     ]
        !!   },
        !!   "properties": {
        !!     "type": "node",
        !!     "data": {
        !!       "id": "dummy-node",
        !!       "name": "Dummy Node",
        !!       "meta": "dummy description",
        !!       "x": 0.0,
        !!       "y": 0.0,
        !!       "z": 0.0,
        !!       "lon": 10.4541194000,
        !!       "lat": 51.1642292000,
        !!       "alt": 10.0000000000
        !!     }
        !!   }
        !! }
        !! ```
        !!
        !! The property _data_ is set to the passed JSON string `data`.
        character(len=:), allocatable, intent(out)          :: geojson !! Output GeoJSON string.
        integer,                       intent(in)           :: type    !! Point type (`TYPE_NODE`, `TYPE_SENSOR`, `TYPE_TARGET`).
        real(kind=r8),                 intent(in)           :: lon     !! Point longitude (decimal).
        real(kind=r8),                 intent(in)           :: lat     !! Point latitude (decimal).
        real(kind=r8),                 intent(in)           :: alt     !! Point altitude.
        character(len=*),              intent(in)           :: data    !! Point JSON data.
        logical,                       intent(in), optional :: comma   !! Append comma separator.

        integer :: type_
        logical :: comma_

        type_ = TYPE_NONE
        if (dm_type_is_valid(type)) type_ = type

        comma_ = .false.
        if (present(comma)) comma_ = comma

        geojson = '{"type":"Feature","geometry":{"type":"Point","coordinates":[' // &
                  dm_ftoa(lon) // ',' // dm_ftoa(lat) // ',' // dm_ftoa(alt) // &
                  ']},"properties":{"type":"' // trim(TYPE_NAMES(type_)) // '","data":' // &
                  trim(data) // '}}'
        if (comma_) geojson = geojson // ','
    end subroutine dm_geojson_feature_point

    ! **************************************************************************
    ! PRIVATE PROCEDURES.
    ! **************************************************************************
    function geojson_from_node(node, comma) result(geojson)
        !! Returns node as allocatable string in GeoJSON format.
        use :: dm_node, only: node_type

        type(node_type), intent(inout)        :: node    !! Node type.
        logical,         intent(in), optional :: comma   !! Append comma separator.
        character(len=:), allocatable         :: geojson !! GeoJSON string.

        call dm_geojson_feature_point(geojson, TYPE_NODE, node%lon, node%lat, node%alt, dm_json_from(node), comma)
    end function geojson_from_node

    function geojson_from_sensor(sensor, comma) result(geojson)
        !! Returns sensor as allocatable string in GeoJSON format.
        use :: dm_sensor, only: sensor_type

        type(sensor_type), intent(inout)        :: sensor  !! Sensor type.
        logical,           intent(in), optional :: comma   !! Append comma separator.
        character(len=:), allocatable           :: geojson !! GeoJSON string.

        call dm_geojson_feature_point(geojson, TYPE_SENSOR, sensor%lon, sensor%lat, sensor%alt, dm_json_from(sensor), comma)
    end function geojson_from_sensor

    function geojson_from_target(target, comma) result(geojson)
        !! Returns target as allocatable string in GeoJSON format.
        use :: dm_target, only: target_type

        type(target_type), intent(inout)        :: target  !! Target type.
        logical,           intent(in), optional :: comma   !! Append comma separator.
        character(len=:), allocatable           :: geojson !! GeoJSON string.

        call dm_geojson_feature_point(geojson, TYPE_TARGET, target%lon, target%lat, target%alt, dm_json_from(target), comma)
    end function geojson_from_target

    integer function geojson_write_node(node, unit, comma) result(rc)
        !! Writes node to file or standard output.
        use :: dm_node, only: node_type

        type(node_type), intent(inout)        :: node  !! Node type.
        integer,         intent(in), optional :: unit  !! File unit.
        logical,         intent(in), optional :: comma !! Append comma separator.

        integer :: stat, unit_

        rc = E_WRITE
        unit_ = stdout
        if (present(unit)) unit_ = unit
        write (unit_, '(a)', iostat=stat) dm_geojson_from(node, comma)
        if (stat /= 0) return
        rc = E_NONE
    end function geojson_write_node

    integer function geojson_write_sensor(sensor, unit, comma) result(rc)
        !! Writes sensor to file or standard output.
        use :: dm_sensor, only: sensor_type

        type(sensor_type), intent(inout)        :: sensor !! Sensor type.
        integer,           intent(in), optional :: unit   !! File unit.
        logical,           intent(in), optional :: comma  !! Append comma separator.

        integer :: stat, unit_

        rc = E_WRITE
        unit_ = stdout
        if (present(unit)) unit_ = unit
        write (unit_, '(a)', iostat=stat) dm_geojson_from(sensor, comma)
        if (stat /= 0) return
        rc = E_NONE
    end function geojson_write_sensor

    integer function geojson_write_target(target, unit, comma) result(rc)
        !! Writes target to file or standard output.
        use :: dm_target, only: target_type

        type(target_type), intent(inout)        :: target !! Target type.
        integer,           intent(in), optional :: unit   !! File unit.
        logical,           intent(in), optional :: comma  !! Append comma separator.

        integer :: stat, unit_

        rc = E_WRITE
        unit_ = stdout
        if (present(unit)) unit_ = unit
        write (unit_, '(a)', iostat=stat) dm_geojson_from(target, comma)
        if (stat /= 0) return
        rc = E_NONE
    end function geojson_write_target
end module dm_geojson
