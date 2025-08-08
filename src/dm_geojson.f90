! Author:  Philipp Engel
! Licence: ISC
module dm_geojson
    !! Contains subroutines to convert derived types to
    !! [GeoJSON](https://geojson.org/) format (RFC 7946).
    use :: dm_ascii, only: NL => ASCII_LF
    use :: dm_error
    use :: dm_json
    use :: dm_kind
    use :: dm_string
    use :: dm_type
    use :: dm_util
    implicit none (type, external)
    private

    type, public :: geojson_feature_collection_type
        !! Opaque GeoJSON Feature Collection type.
        private
        integer                        :: index     = 0 !! Current collection size.
        integer                        :: nfeatures = 0 !! Max. collection size.
        type(string_type), allocatable :: features(:)
    end type geojson_feature_collection_type

    interface dm_geojson_feature_collection_add
        !! Generic derived type add to GeoJSON Feature Collection functions.
        module procedure :: geojson_feature_collection_add_node
        module procedure :: geojson_feature_collection_add_sensor
        module procedure :: geojson_feature_collection_add_target
    end interface dm_geojson_feature_collection_add

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

    public :: dm_geojson_feature_collection
    public :: dm_geojson_feature_collection_add
    public :: dm_geojson_feature_collection_create
    public :: dm_geojson_feature_collection_destroy
    public :: dm_geojson_feature_point
    public :: dm_geojson_from
    public :: dm_geojson_write

    private :: geojson_feature_collection_add
    private :: geojson_feature_collection_add_node
    private :: geojson_feature_collection_add_sensor
    private :: geojson_feature_collection_add_target
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
    function dm_geojson_feature_collection(collection) result(geojson)
        !! Returns GeoJSON Feature Collection as GeoJSON string.
        type(geojson_feature_collection_type), intent(inout) :: collection !! GeoJSON Feature Collection type.
        character(len=:), allocatable                        :: geojson   !! GeoJSON string.

        integer :: i

        geojson = '{"type":"FeatureCollection","features":['

        do i = 1, collection%index
            if (i < collection%index) then
                geojson = geojson // collection%features(i)%data // ','
            else
                geojson = geojson // collection%features(i)%data
            end if
        end do

        geojson = geojson // ']}'
    end function dm_geojson_feature_collection

    integer function dm_geojson_feature_collection_create(collection, max_size) result(rc)
        !! Creates new GeoJSON Feature Collection if given maximum size.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_ALLOC` if memory allocation failed.
        !! * `E_INVALID` if argument `max_size` is less than 1.
        !!
        type(geojson_feature_collection_type), intent(out) :: collection !! GeoJSON Feature Collection type.
        integer,                               intent(in)  :: max_size   !! Max. size of Feature Collection.

        integer :: stat

        rc = E_INVALID
        if (max_size <= 0) return

        rc = E_ALLOC
        allocate (collection%features(max_size), stat=stat)
        if (stat /= 0) return

        rc = E_NONE
        collection%nfeatures = max_size
    end function dm_geojson_feature_collection_create

    function dm_geojson_feature_point(type, longitude, latitude, elevation, json) result(geojson)
        !! Returns GeoJSON feature point of given DMPACK type, longitude,
        !! latitude, elevation, and type properties in JSON. The returned
        !! allocatable string is of the following form:
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
        !!     "properties": {
        !!       "id": "dummy-node",
        !!       "name": "Dummy Node",
        !!       "meta": "dummy description",
        !!       "x": 0.0,
        !!       "y": 0.0,
        !!       "z": 0.0,
        !!       "longitude": 10.4541194000,
        !!       "latitude": 51.1642292000,
        !!       "elevation": 10.0000000000
        !!     }
        !!   }
        !! }
        !! ```
        integer,          intent(in)  :: type      !! Point type (`TYPE_NODE`, `TYPE_SENSOR`, `TYPE_TARGET`).
        real(kind=r8),    intent(in)  :: longitude !! Point longitude (decimal).
        real(kind=r8),    intent(in)  :: latitude  !! Point latitude (decimal).
        real(kind=r8),    intent(in)  :: elevation !! Point elevation.
        character(len=*), intent(in)  :: json      !! Point JSON data.
        character(len=:), allocatable :: geojson   !! GeoJSON string.

        integer :: type_

        type_ = TYPE_NONE
        if (dm_type_is_valid(type)) type_ = type

        geojson = '{"type":"Feature","geometry":{"type":"Point","coordinates":[' // &
                  dm_ftoa(longitude) // ',' // dm_ftoa(latitude) // ',' // dm_ftoa(elevation) // &
                  ']},"properties":{"type":"' // trim(TYPE_NAMES(type_)) // '","properties":' // &
                  trim(json) // '}}'
    end function dm_geojson_feature_point

    subroutine dm_geojson_feature_collection_destroy(collection)
        !! Destroys GeoJSON Feature Collection.
        type(geojson_feature_collection_type), intent(inout) :: collection !! GeoJSON Feature Collection type.

        if (allocated(collection%features)) deallocate (collection%features)

        collection%index     = 0
        collection%nfeatures = 0
    end subroutine dm_geojson_feature_collection_destroy

    ! **************************************************************************
    ! PRIVATE PROCEDURES.
    ! **************************************************************************
    integer function geojson_feature_collection_add(collection, feature) result(rc)
        !! Adds GeoJSON Feature to Feature Collection.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_BOUNDS` if collection is full.
        !! * `E_INVALID` if collection has not been created.
        !!
        type(geojson_feature_collection_type), intent(inout) :: collection !! GeoJSON Feature Collection type.
        character(len=*),                      intent(in)    :: feature    !! GeoJSON Feature string.

        integer :: i

        rc = E_INVALID
        if (.not. allocated(collection%features)) return

        rc = E_BOUNDS
        i = collection%index + 1
        if (i > collection%nfeatures) return

        rc = E_NONE
        collection%index = i
        collection%features(i)%data = feature
    end function geojson_feature_collection_add

    integer function geojson_feature_collection_add_node(collection, node) result(rc)
        !! Adds node to GeoJSON Feature Collection.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_BOUNDS` if collection is full.
        !! * `E_INVALID` if collection has not been created.
        !!
        use :: dm_node, only: node_type

        type(geojson_feature_collection_type), intent(inout) :: collection !! GeoJSON Feature Collection type.
        type(node_type),                       intent(inout) :: node       !! Node type.

        rc = geojson_feature_collection_add(collection, dm_geojson_from(node))
    end function geojson_feature_collection_add_node

    integer function geojson_feature_collection_add_sensor(collection, sensor) result(rc)
        !! Adds sensor to GeoJSON Feature Collection.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_BOUNDS` if collection is full.
        !! * `E_INVALID` if collection has not been created.
        !!
        use :: dm_sensor, only: sensor_type

        type(geojson_feature_collection_type), intent(inout) :: collection !! GeoJSON Feature Collection type.
        type(sensor_type),                     intent(inout) :: sensor     !! Sensor type.

        rc = geojson_feature_collection_add(collection, dm_geojson_from(sensor))
    end function geojson_feature_collection_add_sensor

    integer function geojson_feature_collection_add_target(collection, target) result(rc)
        !! Adds target to GeoJSON Feature Collection.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_BOUNDS` if collection is full.
        !! * `E_INVALID` if collection has not been created.
        !!
        use :: dm_target, only: target_type

        type(geojson_feature_collection_type), intent(inout) :: collection !! GeoJSON Feature Collection type.
        type(target_type),                     intent(inout) :: target     !! Target type.

        rc = geojson_feature_collection_add(collection, dm_geojson_from(target))
    end function geojson_feature_collection_add_target

    function geojson_from_node(node) result(geojson)
        !! Returns node as allocatable string in GeoJSON format.
        use :: dm_node, only: node_type

        type(node_type), intent(inout) :: node    !! Node type.
        character(len=:), allocatable  :: geojson !! GeoJSON string.

        geojson = dm_geojson_feature_point(TYPE_NODE, node%longitude, node%latitude, node%elevation, dm_json_from(node))
    end function geojson_from_node

    function geojson_from_sensor(sensor) result(geojson)
        !! Returns sensor as allocatable string in GeoJSON format.
        use :: dm_sensor, only: sensor_type

        type(sensor_type), intent(inout) :: sensor  !! Sensor type.
        character(len=:), allocatable    :: geojson !! GeoJSON string.

        geojson = dm_geojson_feature_point(TYPE_SENSOR, sensor%longitude, sensor%latitude, sensor%elevation, dm_json_from(sensor))
    end function geojson_from_sensor

    function geojson_from_target(target) result(geojson)
        !! Returns target as allocatable string in GeoJSON format.
        use :: dm_target, only: target_type

        type(target_type), intent(inout) :: target  !! Target type.
        character(len=:), allocatable    :: geojson !! GeoJSON string.

        geojson = dm_geojson_feature_point(TYPE_TARGET, target%longitude, target%latitude, target%elevation, dm_json_from(target))
    end function geojson_from_target

    integer function geojson_write_node(node, unit) result(rc)
        !! Writes node to file or standard output.
        use :: dm_node, only: node_type

        type(node_type), intent(inout)        :: node !! Node type.
        integer,         intent(in), optional :: unit !! File unit.

        integer :: stat, unit_

        rc = E_WRITE
        unit_ = dm_present(unit, stdout)
        write (unit_, '(a)', iostat=stat) dm_geojson_from(node)
        if (stat /= 0) return
        rc = E_NONE
    end function geojson_write_node

    integer function geojson_write_sensor(sensor, unit) result(rc)
        !! Writes sensor to file or standard output.
        use :: dm_sensor, only: sensor_type

        type(sensor_type), intent(inout)        :: sensor !! Sensor type.
        integer,           intent(in), optional :: unit   !! File unit.

        integer :: stat, unit_

        rc = E_WRITE
        unit_ = dm_present(unit, stdout)
        write (unit_, '(a)', iostat=stat) dm_geojson_from(sensor)
        if (stat /= 0) return
        rc = E_NONE
    end function geojson_write_sensor

    integer function geojson_write_target(target, unit) result(rc)
        !! Writes target to file or standard output.
        use :: dm_target, only: target_type

        type(target_type), intent(inout)        :: target !! Target type.
        integer,           intent(in), optional :: unit   !! File unit.

        integer :: stat, unit_

        rc = E_WRITE
        unit_ = dm_present(unit, stdout)
        write (unit_, '(a)', iostat=stat) dm_geojson_from(target)
        if (stat /= 0) return
        rc = E_NONE
    end function geojson_write_target
end module dm_geojson
