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
        !! Generic derived type to GeoJSON converter.
        module procedure :: geojson_from_node
        module procedure :: geojson_from_sensor
        module procedure :: geojson_from_target
    end interface dm_geojson_from

    interface dm_geojson_write
        !! Generic derived type to GeoJSON writer.
        module procedure :: geojson_write_node
        module procedure :: geojson_write_nodes
    end interface dm_geojson_write

    public :: dm_geojson_feature_point
    public :: dm_geojson_from
    public :: dm_geojson_write

    private :: geojson_from_node
    private :: geojson_from_sensor
    private :: geojson_from_target

    private :: geojson_write_node
    private :: geojson_write_nodes
contains
    ! ******************************************************************
    ! PUBLIC PROCEDURES.
    ! ******************************************************************
    subroutine dm_geojson_feature_point(geojson, type, id, name, meta, x, y, z, longitude, latitude, altitude)
        !! Returns a GeoJSON string of the following form:
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
        !!     "id": "dummy-node",
        !!     "name": "Dummy Node",
        !!     "meta": "dummy description"
        !!     "x": 0.0,
        !!     "y": 0.0,
        !!     "z": 0.0,
        !!     "longitude": 10.4541194000,
        !!     "latitude": 51.1642292000,
        !!     "altitude": 10.0000000000
        !!   }
        !! }
        !! ```
        character(len=:), allocatable, intent(out) :: geojson   !! Output GeoJSON string.
        integer,                       intent(in)  :: type      !! Point type.
        character(len=*),              intent(in)  :: id        !! Point id.
        character(len=*),              intent(in)  :: name      !! Point name.
        character(len=*),              intent(in)  :: meta      !! Point meta data.
        real(kind=r8),                 intent(in)  :: x         !! Point x.
        real(kind=r8),                 intent(in)  :: y         !! Point y.
        real(kind=r8),                 intent(in)  :: z         !! Point z.
        real(kind=r8),                 intent(in)  :: longitude !! Point longitude.
        real(kind=r8),                 intent(in)  :: latitude  !! Point latitude.
        real(kind=r8),                 intent(in)  :: altitude  !! Point altitude.

        integer :: type_

        type_ = TYPE_NONE
        if (dm_type_valid(type)) type_ = type

        geojson = &
            '{"type":"Feature",' // &
            '"geometry":{"type":"Point",' // '"coordinates":[' // &
            dm_ftoa(longitude) // ','   // &
            dm_ftoa(latitude)  // ','   // &
            dm_ftoa(altitude)  // ']},' // &
            '"properties":{' // &
            '"type":"'     // trim(TYPE_NAMES(type_)) // '",' // &
            '"id":"'       // id                      // '",' // &
            '"name":"'     // name                    // '",' // &
            '"meta":"'     // dm_json_escape(meta)    // '",' // &
            '"x":'         // dm_ftoa(x)              // ','  // &
            '"y":'         // dm_ftoa(y)              // ','  // &
            '"z":'         // dm_ftoa(z)              // ','  // &
            '"longitude":' // dm_ftoa(longitude)      // ','  // &
            '"latitude":'  // dm_ftoa(latitude)       // ','  // &
            '"altitude":'  // dm_ftoa(altitude)       // '}}'
    end subroutine dm_geojson_feature_point

    ! ******************************************************************
    ! PRIVATE PROCEDURES.
    ! ******************************************************************
    function geojson_from_node(node) result(geojson)
        !! Returns node in GeoJSON format.
        use :: dm_node, only: node_type

        type(node_type), intent(inout) :: node    !! Node type.
        character(len=:), allocatable  :: geojson !! Alloctable GeoJSON string.

        call dm_geojson_feature_point(geojson   = geojson, &
                                      type      = TYPE_NODE, &
                                      id        = trim(node%id), &
                                      name      = trim(node%name), &
                                      meta      = trim(node%meta), &
                                      x         = node%x, &
                                      y         = node%y, &
                                      z         = node%z, &
                                      longitude = node%longitude, &
                                      latitude  = node%latitude, &
                                      altitude  = node%altitude)
    end function geojson_from_node

    function geojson_from_sensor(sensor) result(geojson)
        !! Returns sensor in GeoJSON format.
        use :: dm_sensor, only: sensor_type

        type(sensor_type), intent(inout) :: sensor  !! Sensor type.
        character(len=:), allocatable    :: geojson !! Alloctable GeoJSON string.

        call dm_geojson_feature_point(geojson   = geojson, &
                                      type      = TYPE_SENSOR, &
                                      id        = trim(sensor%id), &
                                      name      = trim(sensor%name), &
                                      meta      = trim(sensor%meta), &
                                      x         = sensor%x, &
                                      y         = sensor%y, &
                                      z         = sensor%z, &
                                      longitude = sensor%x, &
                                      latitude  = sensor%y, &
                                      altitude  = sensor%z)
    end function geojson_from_sensor

    function geojson_from_target(target) result(geojson)
        !! Returns target in GeoJSON format.
        use :: dm_target, only: target_type

        type(target_type), intent(inout) :: target  !! Target type.
        character(len=:), allocatable    :: geojson !! Alloctable GeoJSON string.

        call dm_geojson_feature_point(geojson   = geojson, &
                                      type      = TYPE_TARGET, &
                                      id        = trim(target%id), &
                                      name      = trim(target%name), &
                                      meta      = trim(target%meta), &
                                      x         = target%x, &
                                      y         = target%y, &
                                      z         = target%z, &
                                      longitude = target%x, &
                                      latitude  = target%y, &
                                      altitude  = target%z)
    end function geojson_from_target

    integer function geojson_write_node(node, unit) result(rc)
        !! Writes node to file or standard output.
        use :: dm_node, only: node_type

        type(node_type), intent(inout)        :: node !! Node type.
        integer,         intent(in), optional :: unit !! File unit.

        integer :: stat, unit_

        rc = E_WRITE
        unit_ = stdout
        if (present(unit)) unit_ = unit
        write (unit_, '(a)', iostat=stat) dm_geojson_from(node)
        if (stat /= 0) return
        rc = E_NONE
    end function geojson_write_node

    integer function geojson_write_nodes(nodes, unit) result(rc)
        !! Writes nodes array to file or standard output.
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
            write (unit_, '(a)', advance='no', iostat=stat) dm_geojson_from(nodes(i))
            if (stat /= 0) return
            if (i < n) write (unit_, '(",")', advance='no', iostat=stat)
            if (stat /= 0) return
        end do

        write (unit_, '("]")', iostat=stat)
        if (stat /= 0) return
        rc = E_NONE
    end function geojson_write_nodes
end module dm_geojson
