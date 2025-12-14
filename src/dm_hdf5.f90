! Author:  Philipp Engel
! Licence: ISC
module dm_hdf5
    !! Abstraction layer around HDF5. Link this module against `-lhdf5` and
    !! `-lhdf5_fortran`.
    !!
    !! The module provides wrapper procedures to HDF5 in order to read and
    !! write derived types as 1-dimensional compound data sets. Only nodes,
    !! observations, sensors, and targets are supported.
    !!
    !! The following example writes and reads eight empty observations as group
    !! `timeseries` to and from file `sample.hdf5`:
    !!
    !! ```fortran
    !! integer, parameter :: NOBSERVS = 8
    !!
    !! integer                        :: rc
    !! type(hdf5_file_type)           :: file
    !! type(hdf5_group_type)          :: group
    !! type(observ_type), allocatable :: input(:), output(:)
    !!
    !! allocate (output(N))
    !!
    !! ! Initialise HDF5, create file and group.
    !! rc = dm_hdf5_init()
    !! rc = dm_hdf5_open(file, 'sample.hdf5', create=.true.)
    !! rc = dm_hdf5_open(file, group, 'timeseries', create=.true.)
    !!
    !! ! Write array to file, then read array back from file.
    !! rc = dm_hdf5_write(group, output)
    !! rc = dm_hdf5_read(group, input)
    !!
    !! ! Clean-up.
    !! rc = dm_hdf5_close(group)
    !! rc = dm_hdf5_close(file)
    !! rc = dm_hdf5_destroy()
    !! ```
    !!
    !! ## References
    !!
    !! * [HDF5 Reference Manual](https://docs.hdfgroup.org/hdf5/v1_14/_r_m.html)
    !! * [HDF5 Fortran Examples](https://github.com/HDFGroup/hdf5/tree/develop/fortran/examples)
    !!
    use, intrinsic :: iso_c_binding
    use :: hdf5
    use :: dm_error
    use :: dm_kind
    use :: dm_util
    implicit none (type, external)
    private

    ! HDF5 data set names of DMPACK derived types.
    character(*), parameter, public :: HDF5_DATASET_NODE   = 'node_type'   !! Default name of node data set.
    character(*), parameter, public :: HDF5_DATASET_OBSERV = 'observ_type' !! Default name of observation data set.
    character(*), parameter, public :: HDF5_DATASET_SENSOR = 'sensor_type' !! Default name of sensor data set.
    character(*), parameter, public :: HDF5_DATASET_TARGET = 'target_type' !! Default name of target data set.

    ! HDF5 access modes.
    integer, parameter, public :: HDF5_RDONLY = 0 !! Read-only access.
    integer, parameter, public :: HDF5_RDWR   = 1 !! Read/write access.

    ! HDF5 filters.
    integer, parameter, public :: HDF5_FILTER_DEFLATE    = 1 !! gzip or deflate compression (`H5Z_FILTER_DEFLATE`).
    integer, parameter, public :: HDF5_FILTER_SHUFFLE    = 2 !! Shuffle algorithm (`H5Z_FILTER_SHUFFLE`).
    integer, parameter, public :: HDF5_FILTER_FLETCHER32 = 3 !! Fletcher32 checksum (`H5Z_FILTER_FLETCHER32`).
    integer, parameter, public :: HDF5_FILTER_SZIP       = 4 !! SZIP compression (`H5Z_FILTER_SZIP`).

    type, private :: hdf5_id_type
        !! Opaque HDF5 id type.
        private
        integer(hid_t) :: id = -1 !! Identifier.
    end type hdf5_id_type

    type, extends(hdf5_id_type), public :: hdf5_file_type
        !! Opaque HDF5 file type.
        private
    end type hdf5_file_type

    type, extends(hdf5_id_type), public :: hdf5_group_type
        !! Opaque HDF5 group type.
        private
    end type hdf5_group_type

    interface dm_hdf5_close
        !! Generic HDF5 close function.
        module procedure :: hdf5_close_file
        module procedure :: hdf5_close_group
    end interface dm_hdf5_close

    interface dm_hdf5_open
        !! Generic HDF5 open function.
        module procedure :: hdf5_open_file
        module procedure :: hdf5_open_group
    end interface dm_hdf5_open

    interface dm_hdf5_read
        !! Generic HDF5 read function.
        module procedure :: hdf5_read_nodes
        module procedure :: hdf5_read_observs
        module procedure :: hdf5_read_sensors
        module procedure :: hdf5_read_targets
    end interface dm_hdf5_read

    interface dm_hdf5_write
        !! Generic HDF5 write function.
        module procedure :: hdf5_write_nodes
        module procedure :: hdf5_write_observs
        module procedure :: hdf5_write_targets
        module procedure :: hdf5_write_sensors
    end interface dm_hdf5_write

    ! Public procedures.
    public :: dm_hdf5_close
    public :: dm_hdf5_destroy
    public :: dm_hdf5_file_free
    public :: dm_hdf5_file_is_valid
    public :: dm_hdf5_file_path
    public :: dm_hdf5_has_filter
    public :: dm_hdf5_group_exists
    public :: dm_hdf5_init
    public :: dm_hdf5_open
    public :: dm_hdf5_read
    public :: dm_hdf5_version
    public :: dm_hdf5_version_number
    public :: dm_hdf5_write

    ! Private procedures.
    private :: hdf5_close_file
    private :: hdf5_close_group
    private :: hdf5_open_file
    private :: hdf5_open_group

    private :: hdf5_create_node
    private :: hdf5_create_observ
    private :: hdf5_create_sensor
    private :: hdf5_create_target

    private :: hdf5_read_nodes
    private :: hdf5_read_observs
    private :: hdf5_read_sensors
    private :: hdf5_read_targets

    private :: hdf5_write
    private :: hdf5_write_nodes
    private :: hdf5_write_observs
    private :: hdf5_write_sensors
    private :: hdf5_write_targets
contains
    ! **************************************************************************
    ! PUBLIC PROCEDURES.
    ! **************************************************************************
    integer function dm_hdf5_destroy() result(rc)
        !! Destroys HDF5 Fortran interface. Returns `E_HDF5` on error.
        integer :: stat

        rc = E_HDF5
        call h5close_f(stat)
        if (stat < 0) return
        rc = E_NONE
    end function dm_hdf5_destroy

    integer function dm_hdf5_file_free(file, free_space) result(rc)
        !! Returns amount of free space within a file in argument `free_space`.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_INVALID` if the passed HDF5 file is not opened.
        !! * `E_HDF5` if the HDF5 library call failed.
        !!
        type(hdf5_file_type), intent(inout) :: file       !! HDF5 file type.
        integer(i8),          intent(out)   :: free_space !! Free space in bytes.

        integer           :: stat
        integer(hssize_t) :: sz

        rc = E_INVALID
        free_space = 0_i8
        if (file%id < 0) return

        rc = E_HDF5
        call h5fget_freespace_f(file%id, sz, stat)
        if (stat < 0) return

        free_space = int(sz, i8)
        rc = E_NONE
    end function dm_hdf5_file_free

    logical function dm_hdf5_file_is_valid(path) result(valid)
        !! Returns `.true.` if file at given path is a valid HDF5 file.
        use :: dm_file, only: dm_file_exists

        character(*), intent(in) :: path !! File path.

        integer :: stat

        valid = .false.
        if (.not. dm_file_exists(path)) return
        call h5fis_hdf5_f(trim(path), valid, stat)
    end function dm_hdf5_file_is_valid

    integer function dm_hdf5_file_path(file, path, n) result(rc)
        !! Returns file path of given HDF5 file in `path`. The argument `path`
        !! must be large enough to hold the full path. The actual length is
        !! returned in optional argument `n`.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_INVALID` if the passed HDF5 file is not opened.
        !! * `E_HDF5` if the HDF5 library call failed.
        !!
        type(hdf5_file_type), intent(inout)         :: file !! HDF5 file type.
        character(*),         intent(inout)         :: path !! Path of HDF5 file.
        integer,              intent(out), optional :: n    !! Path length.

        integer         :: stat
        integer(size_t) :: sz

        rc = E_INVALID
        if (present(n)) n = 0
        if (file%id < 0) return

        rc = E_HDF5
        call h5fget_name_f(file%id, path, sz, stat)
        if (stat < 0) return

        rc = E_NONE
        if (present(n)) n = int(sz)
    end function dm_hdf5_file_path

    logical function dm_hdf5_has_filter(filter, error) result(available)
        !! Returns the status of the given filter. The following filters are
        !! supported:
        !!
        !! * `HDF5_FILTER_DEFLATE`    – gzip or deflate compression.
        !! * `HDF5_FILTER_SHUFFLE`    – Shuffle algorithm.
        !! * `HDF5_FILTER_FLETCHER32` – Fletcher32 checksum.
        !! * `HDF5_FILTER_SZIP`       – SZIP compression.
        !!
        !! Argument `error` is set to `E_INVALID` if the given filter is
        !! invalid, and to `E_HDF5` if the library call failed.
        integer, intent(in)            :: filter !! Filter enumerator.
        integer, intent(out), optional :: error  !! Error code.

        integer :: f, rc, stat

        available = .false.

        hdf5_block: block
            rc = E_INVALID
            select case (filter)
                case (HDF5_FILTER_DEFLATE);    f = H5Z_FILTER_DEFLATE_F
                case (HDF5_FILTER_SHUFFLE);    f = H5Z_FILTER_SHUFFLE_F
                case (HDF5_FILTER_FLETCHER32); f = H5Z_FILTER_FLETCHER32_F
                case (HDF5_FILTER_SZIP);       f = H5Z_FILTER_SZIP_F
                case default;                  exit hdf5_block
            end select

            rc = E_HDF5
            call h5zfilter_avail_f(f, available, stat)
            if (stat < 0) exit hdf5_block

            rc = E_NONE
        end block hdf5_block

        if (present(error)) error = rc
    end function dm_hdf5_has_filter

    logical function dm_hdf5_group_exists(id, name, error) result(exists)
        !! Returns `.true.` if group of given name `name` exists in file or
        !! group `id`. The function returns the following error codes in
        !! `error`:
        !!
        !! * `E_INVALID` if the given HDF5 id type (file, group) is invalid.
        !! * `E_HDF5` if the HDF5 library call failed.
        !!
        class(hdf5_id_type), intent(inout)         :: id    !! HDF5 file or group type.
        character(*),        intent(in)            :: name  !! Group name.
        integer,             intent(out), optional :: error !! Error code.

        integer :: rc, stat

        exists = .false.

        hdf5_block: block
            rc = E_INVALID
            if (id%id < 0) exit hdf5_block
            call h5lexists_f(id%id, trim(name), exists, stat)
            rc = E_HDF5
            if (stat < 0) exit hdf5_block
            rc = E_NONE
        end block hdf5_block

        if (present(error)) error = rc
    end function dm_hdf5_group_exists

    integer function dm_hdf5_init() result(rc)
        !! Initialises HDF5 Fortran interface. The function returns `E_HDF5` on
        !! error.
        integer :: stat

        rc = E_HDF5
        call h5open_f(stat)
        if (stat < 0) return
        rc = E_NONE
    end function dm_hdf5_init

    function dm_hdf5_version(name) result(version)
        !! Returns HDF5 library version as allocatable string.
        logical, intent(in), optional :: name    !! Add prefix `libhdf5/`.
        character(:), allocatable     :: version !! Version string.

        character(8) :: v
        integer      :: major, minor, release
        integer      :: rc

        rc = dm_hdf5_version_number(major, minor, release)
        write (v, '(2(i0, "."), i0)') major, minor, release

        if (dm_present(name, .false.)) then
            version = 'libhdf5/' // trim(v)
        else
            version = trim(v)
        end if
    end function dm_hdf5_version

    integer function dm_hdf5_version_number(major, minor, release) result(rc)
        !! Returns version numbers of HDF5 library. The function returns
        !! `E_HDF5` on error.
        use :: h5lib, only: h5get_libversion_f

        integer, intent(out), optional :: major   !! Major version of HDF5 library.
        integer, intent(out), optional :: minor   !! Minor version of HDF5 library.
        integer, intent(out), optional :: release !! Release version of HDF5 library.

        integer :: major_, minor_, release_
        integer :: stat

        rc = E_HDF5
        call h5get_libversion_f(major_, minor_, release_, stat)

        if (present(major))   major   = major_
        if (present(minor))   minor   = minor_
        if (present(release)) release = release_

        if (stat < 0) return
        rc = E_NONE
    end function dm_hdf5_version_number

    ! **************************************************************************
    ! PRIVATE PROCEDURES.
    ! **************************************************************************
    integer function hdf5_close_file(file) result(rc)
        !! Closes HDF5 file. Returns `E_INVALID` if the passed HDF5 file is not
        !! opened. Returns `E_HDF5` if closing the file failed.
        type(hdf5_file_type), intent(inout) :: file !! HDF5 file type.

        integer :: stat

        rc = E_INVALID
        if (file%id < 0) return

        rc = E_HDF5
        call h5fclose_f(file%id, stat)
        if (stat < 0) return

        file = hdf5_file_type()
        rc = E_NONE
    end function hdf5_close_file

    integer function hdf5_close_group(group) result(rc)
        !! Closes HDF5 group. Returns `E_INVALID` if the passed HDF5 group
        !! is not opened. Returns `E_HDF5` if closing the group failed.
        type(hdf5_group_type), intent(inout) :: group !! HDF5 group type.

        integer :: stat

        rc = E_INVALID
        if (group%id < 0) return

        rc = E_HDF5
        call h5gclose_f(group%id, stat)
        if (stat < 0) return

        group = hdf5_group_type()
        rc = E_NONE
    end function hdf5_close_group

    integer function hdf5_create_node(type_id) result(rc)
        !! Creates compound memory data type for derived type `node_type`.
        !! The function returns `E_HDF5` in error.
        use :: dm_node

        integer(hid_t), intent(out) :: type_id !! Data type id.

        integer                 :: stat
        integer(hid_t)          :: tid
        integer(hsize_t)        :: dims(1), offset
        type(node_type), target :: node

        rc = E_HDF5
        type_id = -1

        ! Create compound type.
        offset = int(NODE_TYPE_SIZE, hsize_t)
        call h5tcreate_f(H5T_COMPOUND_F, offset, type_id, stat);          if (stat < 0) return

        ! Node id.
        dims(1) = int(NODE_ID_LEN, hsize_t)
        offset  = h5offsetof(c_loc(node), c_loc(node%id))
        call h5tarray_create_f(H5T_NATIVE_CHARACTER, 1, dims, tid, stat); if (stat < 0) return
        call h5tinsert_f(type_id, 'id', offset, tid, stat);               if (stat < 0) return
        call h5tclose_f(tid, stat);                                       if (stat < 0) return

        ! Node name.
        dims(1) = int(NODE_NAME_LEN, hsize_t)
        offset  = h5offsetof(c_loc(node), c_loc(node%name))
        call h5tarray_create_f(H5T_NATIVE_CHARACTER, 1, dims, tid, stat); if (stat < 0) return
        call h5tinsert_f(type_id, 'name', offset, tid, stat);             if (stat < 0) return
        call h5tclose_f(tid, stat);                                       if (stat < 0) return

        ! Node meta.
        dims(1) = int(NODE_META_LEN, hsize_t)
        offset  = h5offsetof(c_loc(node), c_loc(node%meta))
        call h5tarray_create_f(H5T_NATIVE_CHARACTER, 1, dims, tid, stat); if (stat < 0) return
        call h5tinsert_f(type_id, 'meta', offset, tid, stat);             if (stat < 0) return
        call h5tclose_f(tid, stat);                                       if (stat < 0) return

        ! Node x.
        offset = h5offsetof(c_loc(node), c_loc(node%x))
        call h5tinsert_f(type_id, 'x', offset, h5kind_to_type(kind(node%x), H5_REAL_KIND), stat)
        if (stat < 0) return

        ! Node y.
        offset = h5offsetof(c_loc(node), c_loc(node%y))
        call h5tinsert_f(type_id, 'y', offset, h5kind_to_type(kind(node%y), H5_REAL_KIND), stat)
        if (stat < 0) return

        ! Node z.
        offset = h5offsetof(c_loc(node), c_loc(node%z))
        call h5tinsert_f(type_id, 'z', offset, h5kind_to_type(kind(node%z), H5_REAL_KIND), stat)
        if (stat < 0) return

        ! Node longitude.
        offset = h5offsetof(c_loc(node), c_loc(node%longitude))
        call h5tinsert_f(type_id, 'longitude', offset, h5kind_to_type(kind(node%longitude), H5_REAL_KIND), stat)
        if (stat < 0) return

        ! Node latitude.
        offset = h5offsetof(c_loc(node), c_loc(node%latitude))
        call h5tinsert_f(type_id, 'latitude', offset, h5kind_to_type(kind(node%latitude), H5_REAL_KIND), stat)
        if (stat < 0) return

        ! Node elevation.
        offset = h5offsetof(c_loc(node), c_loc(node%elevation))
        call h5tinsert_f(type_id, 'elevation', offset, h5kind_to_type(kind(node%elevation), H5_REAL_KIND), stat)
        if (stat < 0) return

        rc = E_NONE
    end function hdf5_create_node

    integer function hdf5_create_observ(type_id) result(rc)
        !! Creates compound memory data type for derived type `observ_type`.
        !! The function returns `E_HDF5` in error.
        use :: dm_observ
        use :: dm_node
        use :: dm_request
        use :: dm_response
        use :: dm_sensor
        use :: dm_target
        use :: dm_time

        integer(hid_t), intent(out) :: type_id !! Data type id.

        character(32)             :: name
        integer                   :: i, j, k, stat
        integer(hid_t)            :: tid
        integer(hsize_t)          :: dims(1), offset
        type(observ_type), target :: observ

        rc = E_HDF5
        type_id = -1

        ! Create compound type.
        offset = int(OBSERV_TYPE_SIZE, hsize_t)
        call h5tcreate_f(H5T_COMPOUND_F, offset, type_id, stat);          if (stat < 0) return

        ! Observation id.
        dims(1) = int(OBSERV_ID_LEN, hsize_t)
        offset  = h5offsetof(c_loc(observ), c_loc(observ%id))
        call h5tarray_create_f(H5T_NATIVE_CHARACTER, 1, dims, tid, stat); if (stat < 0) return
        call h5tinsert_f(type_id, 'id', offset, tid, stat);               if (stat < 0) return
        call h5tclose_f(tid, stat);                                       if (stat < 0) return

        ! Node id.
        dims(1) = int(NODE_ID_LEN, hsize_t)
        offset  = h5offsetof(c_loc(observ), c_loc(observ%node_id))
        call h5tarray_create_f(H5T_NATIVE_CHARACTER, 1, dims, tid, stat); if (stat < 0) return
        call h5tinsert_f(type_id, 'node_id', offset, tid, stat);          if (stat < 0) return
        call h5tclose_f(tid, stat);                                       if (stat < 0) return

        ! Sensor id.
        dims(1) = int(SENSOR_ID_LEN, hsize_t)
        offset  = h5offsetof(c_loc(observ), c_loc(observ%sensor_id))
        call h5tarray_create_f(H5T_NATIVE_CHARACTER, 1, dims, tid, stat); if (stat < 0) return
        call h5tinsert_f(type_id, 'sensor_id', offset, tid, stat);        if (stat < 0) return
        call h5tclose_f(tid, stat);                                       if (stat < 0) return

        ! Target id.
        dims(1) = int(TARGET_ID_LEN, hsize_t)
        offset  = h5offsetof(c_loc(observ), c_loc(observ%target_id))
        call h5tarray_create_f(H5T_NATIVE_CHARACTER, 1, dims, tid, stat); if (stat < 0) return
        call h5tinsert_f(type_id, 'target_id', offset, tid, stat);        if (stat < 0) return
        call h5tclose_f(tid, stat);                                       if (stat < 0) return

        ! Observation name.
        dims(1) = int(OBSERV_NAME_LEN, hsize_t)
        offset  = h5offsetof(c_loc(observ), c_loc(observ%name))
        call h5tarray_create_f(H5T_NATIVE_CHARACTER, 1, dims, tid, stat); if (stat < 0) return
        call h5tinsert_f(type_id, 'name', offset, tid, stat);             if (stat < 0) return
        call h5tclose_f(tid, stat);                                       if (stat < 0) return

        ! Observation timestamp.
        dims(1) = int(TIME_LEN, hsize_t)
        offset  = h5offsetof(c_loc(observ), c_loc(observ%timestamp))
        call h5tarray_create_f(H5T_NATIVE_CHARACTER, 1, dims, tid, stat); if (stat < 0) return
        call h5tinsert_f(type_id, 'timestamp', offset, tid, stat);        if (stat < 0) return
        call h5tclose_f(tid, stat);                                       if (stat < 0) return

        ! Observation source.
        dims(1) = int(OBSERV_SOURCE_LEN, hsize_t)
        offset  = h5offsetof(c_loc(observ), c_loc(observ%source))
        call h5tarray_create_f(H5T_NATIVE_CHARACTER, 1, dims, tid, stat); if (stat < 0) return
        call h5tinsert_f(type_id, 'source', offset, tid, stat);           if (stat < 0) return
        call h5tclose_f(tid, stat);                                       if (stat < 0) return

        ! Observation device.
        dims(1) = int(OBSERV_DEVICE_LEN, hsize_t)
        offset  = h5offsetof(c_loc(observ), c_loc(observ%device))
        call h5tarray_create_f(H5T_NATIVE_CHARACTER, 1, dims, tid, stat); if (stat < 0) return
        call h5tinsert_f(type_id, 'device', offset, tid, stat);           if (stat < 0) return
        call h5tclose_f(tid, stat);                                       if (stat < 0) return

        ! Observation priority.
        offset = h5offsetof(c_loc(observ), c_loc(observ%priority))
        call h5tinsert_f(type_id, 'priority', offset, h5kind_to_type(kind(observ%priority), H5_INTEGER_KIND), stat)
        if (stat < 0) return

        ! Observation error.
        offset = h5offsetof(c_loc(observ), c_loc(observ%error))
        call h5tinsert_f(type_id, 'error', offset, h5kind_to_type(kind(observ%error), H5_INTEGER_KIND), stat)
        if (stat < 0) return

        ! Observation next.
        offset = h5offsetof(c_loc(observ), c_loc(observ%next))
        call h5tinsert_f(type_id, 'next', offset, h5kind_to_type(kind(observ%next), H5_INTEGER_KIND), stat)
        if (stat < 0) return

        ! Observation #receivers.
        offset = h5offsetof(c_loc(observ), c_loc(observ%nreceivers))
        call h5tinsert_f(type_id, 'nreceivers', offset, h5kind_to_type(kind(observ%nreceivers), H5_INTEGER_KIND), stat)
        if (stat < 0) return

        ! Observation #requests.
        offset = h5offsetof(c_loc(observ), c_loc(observ%nrequests))
        call h5tinsert_f(type_id, 'nrequests', offset, h5kind_to_type(kind(observ%nrequests), H5_INTEGER_KIND), stat)
        if (stat < 0) return

        ! Observation receivers.
        do i = 1, OBSERV_MAX_NRECEIVERS
            write (name, '("receivers_", i0)') i
            dims(1) = int(OBSERV_RECEIVER_LEN, hsize_t)
            offset  = h5offsetof(c_loc(observ), c_loc(observ%receivers(i)))
            call h5tarray_create_f(H5T_NATIVE_CHARACTER, 1, dims, tid, stat); if (stat < 0) return
            call h5tinsert_f(type_id, trim(name), offset, tid, stat);         if (stat < 0) return
            call h5tclose_f(tid, stat);                                       if (stat < 0) return
        end do

        ! Observation requests.
        do i = 1, OBSERV_MAX_NREQUESTS
            write (name, '("requests_", i0, "_name")') i
            dims(1) = int(TIME_LEN, hsize_t)
            offset  = h5offsetof(c_loc(observ), c_loc(observ%requests(i)%name))
            call h5tarray_create_f(H5T_NATIVE_CHARACTER, 1, dims, tid, stat); if (stat < 0) return
            call h5tinsert_f(type_id, trim(name), offset, tid, stat);         if (stat < 0) return
            call h5tclose_f(tid, stat);                                       if (stat < 0) return

            write (name, '("requests_", i0, "_timestamp")') i
            dims(1) = int(TIME_LEN, hsize_t)
            offset  = h5offsetof(c_loc(observ), c_loc(observ%requests(i)%timestamp))
            call h5tarray_create_f(H5T_NATIVE_CHARACTER, 1, dims, tid, stat); if (stat < 0) return
            call h5tinsert_f(type_id, trim(name), offset, tid, stat);         if (stat < 0) return
            call h5tclose_f(tid, stat);                                       if (stat < 0) return

            write (name, '("requests_", i0, "_request")') i
            dims(1) = int(REQUEST_REQUEST_LEN, hsize_t)
            offset  = h5offsetof(c_loc(observ), c_loc(observ%requests(i)%request))
            call h5tarray_create_f(H5T_NATIVE_CHARACTER, 1, dims, tid, stat); if (stat < 0) return
            call h5tinsert_f(type_id, trim(name), offset, tid, stat);         if (stat < 0) return
            call h5tclose_f(tid, stat);                                       if (stat < 0) return

            write (name, '("requests_", i0, "_response")') i
            dims(1) = int(REQUEST_RESPONSE_LEN, hsize_t)
            offset  = h5offsetof(c_loc(observ), c_loc(observ%requests(i)%response))
            call h5tarray_create_f(H5T_NATIVE_CHARACTER, 1, dims, tid, stat); if (stat < 0) return
            call h5tinsert_f(type_id, trim(name), offset, tid, stat);         if (stat < 0) return
            call h5tclose_f(tid, stat);                                       if (stat < 0) return

            write (name, '("requests_", i0, "_delimiter")') i
            dims(1) = int(REQUEST_DELIMITER_LEN, hsize_t)
            offset  = h5offsetof(c_loc(observ), c_loc(observ%requests(i)%delimiter))
            call h5tarray_create_f(H5T_NATIVE_CHARACTER, 1, dims, tid, stat); if (stat < 0) return
            call h5tinsert_f(type_id, trim(name), offset, tid, stat);         if (stat < 0) return
            call h5tclose_f(tid, stat);                                       if (stat < 0) return

            write (name, '("requests_", i0, "_pattern")') i
            dims(1) = int(REQUEST_PATTERN_LEN, hsize_t)
            offset  = h5offsetof(c_loc(observ), c_loc(observ%requests(i)%pattern))
            call h5tarray_create_f(H5T_NATIVE_CHARACTER, 1, dims, tid, stat); if (stat < 0) return
            call h5tinsert_f(type_id, trim(name), offset, tid, stat);         if (stat < 0) return
            call h5tclose_f(tid, stat);                                       if (stat < 0) return

            write (name, '("requests_", i0, "_delay")') i
            offset = h5offsetof(c_loc(observ), c_loc(observ%requests(i)%delay))
            k = kind(observ%requests(i)%delay)
            call h5tinsert_f(type_id, trim(name), offset, h5kind_to_type(k, H5_INTEGER_KIND), stat)
            if (stat < 0) return

            write (name, '("requests_", i0, "_error")') i
            offset = h5offsetof(c_loc(observ), c_loc(observ%requests(i)%error))
            k = kind(observ%requests(i)%error)
            call h5tinsert_f(type_id, trim(name), offset, h5kind_to_type(k, H5_INTEGER_KIND), stat)
            if (stat < 0) return

            write (name, '("requests_", i0, "_mode")') i
            offset = h5offsetof(c_loc(observ), c_loc(observ%requests(i)%mode))
            k = kind(observ%requests(i)%mode)
            call h5tinsert_f(type_id, trim(name), offset, h5kind_to_type(k, H5_INTEGER_KIND), stat)
            if (stat < 0) return

            write (name, '("requests_", i0, "_retries")') i
            offset = h5offsetof(c_loc(observ), c_loc(observ%requests(i)%retries))
            k = kind(observ%requests(i)%retries)
            call h5tinsert_f(type_id, trim(name), offset, h5kind_to_type(k, H5_INTEGER_KIND), stat)
            if (stat < 0) return

            write (name, '("requests_", i0, "_state")') i
            offset = h5offsetof(c_loc(observ), c_loc(observ%requests(i)%state))
            k = kind(observ%requests(i)%state)
            call h5tinsert_f(type_id, trim(name), offset, h5kind_to_type(k, H5_INTEGER_KIND), stat)
            if (stat < 0) return

            write (name, '("requests_", i0, "_timeout")') i
            offset = h5offsetof(c_loc(observ), c_loc(observ%requests(i)%timeout))
            k = kind(observ%requests(i)%timeout)
            call h5tinsert_f(type_id, trim(name), offset, h5kind_to_type(k, H5_INTEGER_KIND), stat)
            if (stat < 0) return

            write (name, '("requests_", i0, "_nresponses")') i
            offset = h5offsetof(c_loc(observ), c_loc(observ%requests(i)%nresponses))
            k = kind(observ%requests(i)%nresponses)
            call h5tinsert_f(type_id, trim(name), offset, h5kind_to_type(k, H5_INTEGER_KIND), stat)
            if (stat < 0) return

            do j = 1, REQUEST_MAX_NRESPONSES
                write (name, '("requests_", i0, "_responses_", i0, "_name")') i, j
                dims(1) = int(RESPONSE_NAME_LEN, hsize_t)
                offset  = h5offsetof(c_loc(observ), c_loc(observ%requests(i)%responses(j)%name))
                call h5tarray_create_f(H5T_NATIVE_CHARACTER, 1, dims, tid, stat); if (stat < 0) return
                call h5tinsert_f(type_id, trim(name), offset, tid, stat);         if (stat < 0) return
                call h5tclose_f(tid, stat);                                       if (stat < 0) return

                write (name, '("requests_", i0, "_responses_", i0, "_unit")') i, j
                dims(1) = int(RESPONSE_UNIT_LEN, hsize_t)
                offset  = h5offsetof(c_loc(observ), c_loc(observ%requests(i)%responses(j)%unit))
                call h5tarray_create_f(H5T_NATIVE_CHARACTER, 1, dims, tid, stat); if (stat < 0) return
                call h5tinsert_f(type_id, trim(name), offset, tid, stat);         if (stat < 0) return
                call h5tclose_f(tid, stat);                                       if (stat < 0) return

                write (name, '("requests_", i0, "_responses_", i0, "_type")') i, j
                offset = h5offsetof(c_loc(observ), c_loc(observ%requests(i)%responses(j)%type))
                k = kind(observ%requests(i)%responses(j)%type)
                call h5tinsert_f(type_id, trim(name), offset, h5kind_to_type(k, H5_INTEGER_KIND), stat)
                if (stat < 0) return

                write (name, '("requests_", i0, "_responses_", i0, "_error")') i, j
                offset = h5offsetof(c_loc(observ), c_loc(observ%requests(i)%responses(j)%error))
                k = kind(observ%requests(i)%responses(j)%error)
                call h5tinsert_f(type_id, trim(name), offset, h5kind_to_type(k, H5_INTEGER_KIND), stat)
                if (stat < 0) return

                write (name, '("requests_", i0, "_responses_", i0, "_value")') i, j
                offset = h5offsetof(c_loc(observ), c_loc(observ%requests(i)%responses(j)%value))
                k = kind(observ%requests(i)%responses(j)%value)
                call h5tinsert_f(type_id, trim(name), offset, h5kind_to_type(k, H5_REAL_KIND), stat)
                if (stat < 0) return
            end do
        end do

        rc = E_NONE
    end function hdf5_create_observ

    integer function hdf5_create_sensor(type_id) result(rc)
        !! Creates compound memory data type for derived type `sensor_type`.
        !! The function returns `E_HDF5` in error.
        use :: dm_node
        use :: dm_sensor

        integer(hid_t), intent(out) :: type_id !! Data type id.

        integer                   :: stat
        integer(hid_t)            :: tid
        integer(hsize_t)          :: dims(1), offset
        type(sensor_type), target :: sensor

        rc = E_HDF5
        type_id = -1

        ! Create compound type.
        offset = int(SENSOR_TYPE_SIZE, hsize_t)
        call h5tcreate_f(H5T_COMPOUND_F, offset, type_id, stat);          if (stat < 0) return

        ! Sensor id.
        dims(1) = int(SENSOR_ID_LEN, hsize_t)
        offset  = h5offsetof(c_loc(sensor), c_loc(sensor%id))
        call h5tarray_create_f(H5T_NATIVE_CHARACTER, 1, dims, tid, stat); if (stat < 0) return
        call h5tinsert_f(type_id, 'id', offset, tid, stat);               if (stat < 0) return
        call h5tclose_f(tid, stat);                                       if (stat < 0) return

        ! Node id.
        dims(1) = int(NODE_ID_LEN, hsize_t)
        offset  = h5offsetof(c_loc(sensor), c_loc(sensor%node_id))
        call h5tarray_create_f(H5T_NATIVE_CHARACTER, 1, dims, tid, stat); if (stat < 0) return
        call h5tinsert_f(type_id, 'node_id', offset, tid, stat);          if (stat < 0) return
        call h5tclose_f(tid, stat);                                       if (stat < 0) return

        ! Sensor type.
        offset = h5offsetof(c_loc(sensor), c_loc(sensor%type))
        call h5tinsert_f(type_id, 'type', offset, h5kind_to_type(kind(sensor%type), H5_INTEGER_KIND), stat)
        if (stat < 0) return

        ! Sensor name.
        dims(1) = int(SENSOR_NAME_LEN, hsize_t)
        offset  = h5offsetof(c_loc(sensor), c_loc(sensor%name))
        call h5tarray_create_f(H5T_NATIVE_CHARACTER, 1, dims, tid, stat); if (stat < 0) return
        call h5tinsert_f(type_id, 'name', offset, tid, stat);             if (stat < 0) return
        call h5tclose_f(tid, stat);                                       if (stat < 0) return

        ! Sensor serial number.
        dims(1) = int(SENSOR_SN_LEN, hsize_t)
        offset  = h5offsetof(c_loc(sensor), c_loc(sensor%sn))
        call h5tarray_create_f(H5T_NATIVE_CHARACTER, 1, dims, tid, stat); if (stat < 0) return
        call h5tinsert_f(type_id, 'sn', offset, tid, stat);               if (stat < 0) return
        call h5tclose_f(tid, stat);                                       if (stat < 0) return

        ! Sensor meta.
        dims(1) = int(SENSOR_META_LEN, hsize_t)
        offset  = h5offsetof(c_loc(sensor), c_loc(sensor%meta))
        call h5tarray_create_f(H5T_NATIVE_CHARACTER, 1, dims, tid, stat); if (stat < 0) return
        call h5tinsert_f(type_id, 'meta', offset, tid, stat);             if (stat < 0) return
        call h5tclose_f(tid, stat);                                       if (stat < 0) return

        ! Sensor x.
        offset = h5offsetof(c_loc(sensor), c_loc(sensor%x))
        call h5tinsert_f(type_id, 'x', offset, h5kind_to_type(kind(sensor%x), H5_REAL_KIND), stat)
        if (stat < 0) return

        ! Sensor y.
        offset = h5offsetof(c_loc(sensor), c_loc(sensor%y))
        call h5tinsert_f(type_id, 'y', offset, h5kind_to_type(kind(sensor%y), H5_REAL_KIND), stat)
        if (stat < 0) return

        ! Sensor z.
        offset = h5offsetof(c_loc(sensor), c_loc(sensor%z))
        call h5tinsert_f(type_id, 'z', offset, h5kind_to_type(kind(sensor%z), H5_REAL_KIND), stat)
        if (stat < 0) return

        ! Sensor longitude.
        offset = h5offsetof(c_loc(sensor), c_loc(sensor%longitude))
        call h5tinsert_f(type_id, 'longitude', offset, h5kind_to_type(kind(sensor%longitude), H5_REAL_KIND), stat)
        if (stat < 0) return

        ! Sensor latitude.
        offset = h5offsetof(c_loc(sensor), c_loc(sensor%latitude))
        call h5tinsert_f(type_id, 'latitude', offset, h5kind_to_type(kind(sensor%latitude), H5_REAL_KIND), stat)
        if (stat < 0) return

        ! Sensor elevation.
        offset = h5offsetof(c_loc(sensor), c_loc(sensor%elevation))
        call h5tinsert_f(type_id, 'elevation', offset, h5kind_to_type(kind(sensor%elevation), H5_REAL_KIND), stat)
        if (stat < 0) return

        rc = E_NONE
    end function hdf5_create_sensor

    integer function hdf5_create_target(type_id) result(rc)
        !! Creates compound memory data type for derived type `target_type`.
        !! The function returns `E_HDF5` in error.
        use :: dm_target

        integer(hid_t), intent(out) :: type_id !! Data type id.

        integer                   :: stat
        integer(hid_t)            :: tid
        integer(hsize_t)          :: dims(1), offset
        type(target_type), target :: target

        rc = E_HDF5
        type_id = -1

        ! Create compound type.
        offset = int(TARGET_TYPE_SIZE, hsize_t)
        call h5tcreate_f(H5T_COMPOUND_F, offset, type_id, stat);          if (stat < 0) return

        ! Target id.
        dims(1) = int(TARGET_ID_LEN, hsize_t)
        offset  = h5offsetof(c_loc(target), c_loc(target%id))
        call h5tarray_create_f(H5T_NATIVE_CHARACTER, 1, dims, tid, stat); if (stat < 0) return
        call h5tinsert_f(type_id, 'id', offset, tid, stat);               if (stat < 0) return
        call h5tclose_f(tid, stat);                                       if (stat < 0) return

        ! Target name.
        dims(1) = int(TARGET_NAME_LEN, hsize_t)
        offset  = h5offsetof(c_loc(target), c_loc(target%name))
        call h5tarray_create_f(H5T_NATIVE_CHARACTER, 1, dims, tid, stat); if (stat < 0) return
        call h5tinsert_f(type_id, 'name', offset, tid, stat);             if (stat < 0) return
        call h5tclose_f(tid, stat);                                       if (stat < 0) return

        ! Target meta.
        dims(1) = int(TARGET_META_LEN, hsize_t)
        offset  = h5offsetof(c_loc(target), c_loc(target%meta))
        call h5tarray_create_f(H5T_NATIVE_CHARACTER, 1, dims, tid, stat); if (stat < 0) return
        call h5tinsert_f(type_id, 'meta', offset, tid, stat);             if (stat < 0) return
        call h5tclose_f(tid, stat);                                       if (stat < 0) return

        ! Target state.
        offset = h5offsetof(c_loc(target), c_loc(target%state))
        call h5tinsert_f(type_id, 'state', offset, h5kind_to_type(kind(target%state), H5_INTEGER_KIND), stat)
        if (stat < 0) return

        ! Target x.
        offset = h5offsetof(c_loc(target), c_loc(target%x))
        call h5tinsert_f(type_id, 'x', offset, h5kind_to_type(kind(target%x), H5_REAL_KIND), stat)
        if (stat < 0) return

        ! Target y.
        offset = h5offsetof(c_loc(target), c_loc(target%y))
        call h5tinsert_f(type_id, 'y', offset, h5kind_to_type(kind(target%y), H5_REAL_KIND), stat)
        if (stat < 0) return

        ! Target z.
        offset = h5offsetof(c_loc(target), c_loc(target%z))
        call h5tinsert_f(type_id, 'z', offset, h5kind_to_type(kind(target%z), H5_REAL_KIND), stat)
        if (stat < 0) return

        ! Target longitude.
        offset = h5offsetof(c_loc(target), c_loc(target%longitude))
        call h5tinsert_f(type_id, 'longitude', offset, h5kind_to_type(kind(target%longitude), H5_REAL_KIND), stat)
        if (stat < 0) return

        ! Target latitude.
        offset = h5offsetof(c_loc(target), c_loc(target%latitude))
        call h5tinsert_f(type_id, 'latitude', offset, h5kind_to_type(kind(target%latitude), H5_REAL_KIND), stat)
        if (stat < 0) return

        ! Target elevation.
        offset = h5offsetof(c_loc(target), c_loc(target%elevation))
        call h5tinsert_f(type_id, 'elevation', offset, h5kind_to_type(kind(target%elevation), H5_REAL_KIND), stat)
        if (stat < 0) return

        rc = E_NONE
    end function hdf5_create_target

    integer function hdf5_open_file(file, path, mode, create) result(rc)
        !! Opens HDF5 file, by default in read/write access mode, unless `mode`
        !! is passed. If `create` is `.true.`, a new file will be created.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_EXIST` if the file to create already exists.
        !! * `E_HDF5` if opening or creating the file failed.
        !! * `E_INVALID` if the file is opened already or argument `mode` is invalid.
        !! * `E_NOT_FOUND` if the file was not found.
        !!
        use :: dm_file, only: dm_file_exists

        type(hdf5_file_type), intent(out)          :: file   !! HDF5 file type.
        character(*),         intent(in)           :: path   !! Path to HDF5 file.
        integer,              intent(in), optional :: mode   !! Open mode (`HDF5_RDONLY` or `HDF5_RDWR`).
        logical,              intent(in), optional :: create !! Create HDF5 file.

        integer :: flags, mode_
        integer :: stat
        logical :: create_, exists

        rc = E_INVALID
        if (file%id > -1) return

        exists  = dm_file_exists(path)
        create_ = dm_present(create, .false.)
        mode_   = dm_present(mode, HDF5_RDWR)

        ! Create new file.
        if (create_) then
            rc = E_EXIST
            if (exists) return

            rc = E_HDF5
            call h5fcreate_f(trim(path), H5F_ACC_EXCL_F, file%id, stat)
            if (stat < 0) return

            rc = E_NONE
            return
        end if

        ! Open file.
        rc = E_INVALID
        select case (mode_)
            case (HDF5_RDONLY); flags = H5F_ACC_RDONLY_F
            case (HDF5_RDWR);   flags = H5F_ACC_RDWR_F
            case default;       return
        end select

        rc = E_NOT_FOUND
        if (.not. exists) return

        rc = E_HDF5
        call h5fopen_f(trim(path), flags, file%id, stat)
        if (stat < 0) return

        rc = E_NONE
    end function hdf5_open_file

    integer function hdf5_open_group(id, group, name, create) result(rc)
        !! Opens or creates group of name `name`. The function returns
        !! `E_INVALID` if the file is not opened, and `E_HDF5` if the group
        !! operation failed.
        class(hdf5_id_type),   intent(inout)        :: id     !! HDF5 file or group type.
        type(hdf5_group_type), intent(out)          :: group  !! HDF5 group type.
        character(*),          intent(in)           :: name   !! Group name.
        logical,               intent(in), optional :: create !! Create group.

        integer :: stat

        rc = E_INVALID
        if (id%id < 0) return

        rc = E_HDF5
        if (dm_present(create, .false.)) then
            ! Create group.
            call h5gcreate_f(id%id, trim(name), group%id, stat)
        else
            ! Open group.
            call h5gopen_f(id%id, trim(name), group%id, stat)
        end if

        if (stat < 0) return
        rc = E_NONE
    end function hdf5_open_group

    integer function hdf5_read_nodes(id, nodes, data_set) result(rc)
        !! Reads array of `node_type` from compound data in HDF5 file. If
        !! `data_set` is not passed, the name will be set to the value of
        !! `HDF5_DATASET_NODE`.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_INVALID` if the passed `id` is invalid.
        !! * `E_ALLOC` if allocation of array `nodes` failed.
        !! * `E_HDF5` if the HDF5 library call failed.
        !!
        use :: dm_node

        class(hdf5_id_type),                  intent(inout)        :: id       !! HDF5 file or group type.
        type(node_type), allocatable, target, intent(out)          :: nodes(:) !! Node type array.
        character(*),                         intent(in), optional :: data_set !! Name of data set.

        integer          :: stat
        integer(hid_t)   :: set_id, space_id, type_id
        integer(hsize_t) :: dims(1), max_dims(1)
        type(c_ptr)      :: ptr

        type_id  = -1
        set_id   = -1
        space_id = -1

        rc = E_INVALID
        if (id%id < 0) return

        rc = E_HDF5

        hdf5_block: block
            ! Open the data set.
            if (present(data_set)) then
                call h5dopen_f(id%id, data_set, set_id, stat)
            else
                call h5dopen_f(id%id, HDF5_DATASET_NODE, set_id, stat)
            end if
            if (stat < 0) exit hdf5_block

            ! Get data space.
            call h5dget_space_f(set_id, space_id, stat)
            if (stat < 0) exit hdf5_block

            ! Get dimensions. On success, `stat` is set to the rank of the array.
            call h5sget_simple_extent_dims_f(space_id, dims, max_dims, stat)
            if (stat < 0) exit hdf5_block

            ! Allocate array.
            rc = E_ALLOC
            allocate (nodes(dims(1)), stat=stat)
            if (stat /= 0) exit hdf5_block

            ! Create memory data type.
            rc = hdf5_create_node(type_id)
            if (dm_is_error(rc)) exit hdf5_block

            ! Read data from the data set.
            ptr = c_loc(nodes)
            call h5dread_f(set_id, type_id, ptr, stat)
            if (stat < 0) exit hdf5_block

            rc = E_NONE
        end block hdf5_block

        if (.not. allocated(nodes)) allocate (nodes(0))

        ! Release resources.
        if (type_id > -1)  call h5tclose_f(type_id, stat)
        if (space_id > -1) call h5sclose_f(space_id, stat)
        if (set_id > -1)   call h5dclose_f(set_id, stat)
    end function hdf5_read_nodes

    integer function hdf5_read_observs(id, observs, data_set) result(rc)
        !! Reads array of `observ_type` from compound data in HDF5 file. If
        !! `data_set` is not passed, the name will be set to the value of
        !! `HDF5_DATASET_OBSERV`.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_INVALID` if the passed `id` is invalid.
        !! * `E_ALLOC` if allocation of array `observs` failed.
        !! * `E_HDF5` if the HDF5 library call failed.
        !!
        use :: dm_observ

        class(hdf5_id_type),                    intent(inout)        :: id         !! HDF5 file or group type.
        type(observ_type), allocatable, target, intent(out)          :: observs(:) !! Observation type array.
        character(*),                           intent(in), optional :: data_set   !! Name of data set.

        integer          :: stat
        integer(hid_t)   :: set_id, space_id, type_id
        integer(hsize_t) :: dims(1), max_dims(1)
        type(c_ptr)      :: ptr

        type_id  = -1
        set_id   = -1
        space_id = -1

        rc = E_INVALID
        if (id%id < 0) return

        rc = E_HDF5

        hdf5_block: block
            ! Open the data set.
            if (present(data_set)) then
                call h5dopen_f(id%id, data_set, set_id, stat)
            else
                call h5dopen_f(id%id, HDF5_DATASET_OBSERV, set_id, stat)
            end if
            if (stat < 0) exit hdf5_block

            ! Get data space.
            call h5dget_space_f(set_id, space_id, stat)
            if (stat < 0) exit hdf5_block

            ! Get dimensions. On success, `stat` is set to the rank of the array.
            call h5sget_simple_extent_dims_f(space_id, dims, max_dims, stat)
            if (stat < 0) exit hdf5_block

            ! Allocate array.
            rc = E_ALLOC
            allocate (observs(dims(1)), stat=stat)
            if (stat /= 0) exit hdf5_block

            ! Create memory data type.
            rc = hdf5_create_observ(type_id)
            if (dm_is_error(rc)) exit hdf5_block

            ! Read data from the data set.
            ptr = c_loc(observs)
            call h5dread_f(set_id, type_id, ptr, stat)
            if (stat < 0) exit hdf5_block

            rc = E_NONE
        end block hdf5_block

        if (.not. allocated(observs)) allocate (observs(0))

        ! Release resources.
        if (type_id > -1)  call h5tclose_f(type_id, stat)
        if (space_id > -1) call h5sclose_f(space_id, stat)
        if (set_id > -1)   call h5dclose_f(set_id, stat)
    end function hdf5_read_observs

    integer function hdf5_read_sensors(id, sensors, data_set) result(rc)
        !! Reads array of `sensor_type` from compound data in HDF5 file. If
        !! `data_set` is not passed, the name will be set to the value of
        !! `HDF5_DATASET_SENSOR`.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_INVALID` if the passed `id` is invalid.
        !! * `E_ALLOC` if allocation of array `sensors` failed.
        !! * `E_HDF5` if the HDF5 library call failed.
        !!
        use :: dm_sensor

        class(hdf5_id_type),                    intent(inout)        :: id         !! HDF5 file or group type.
        type(sensor_type), allocatable, target, intent(out)          :: sensors(:) !! Sensor type array.
        character(*),                           intent(in), optional :: data_set   !! Name of data set.

        integer          :: stat
        integer(hid_t)   :: set_id, space_id, type_id
        integer(hsize_t) :: dims(1), max_dims(1)
        type(c_ptr)      :: ptr

        set_id   = -1
        space_id = -1
        type_id  = -1

        rc = E_INVALID
        if (id%id < 0) return

        rc = E_HDF5

        hdf5_block: block
            ! Open the data set.
            if (present(data_set)) then
                call h5dopen_f(id%id, data_set, set_id, stat)
            else
                call h5dopen_f(id%id, HDF5_DATASET_SENSOR, set_id, stat)
            end if
            if (stat < 0) exit hdf5_block

            ! Get data space.
            call h5dget_space_f(set_id, space_id, stat)
            if (stat < 0) exit hdf5_block

            ! Get dimensions. On success, `stat` is set to the rank of the array.
            call h5sget_simple_extent_dims_f(space_id, dims, max_dims, stat)
            if (stat < 0) exit hdf5_block

            ! Allocate array.
            rc = E_ALLOC
            allocate (sensors(dims(1)), stat=stat)
            if (stat /= 0) exit hdf5_block

            ! Create memory data type.
            rc = hdf5_create_sensor(type_id)
            if (dm_is_error(rc)) exit hdf5_block

            ! Read data from the data set.
            ptr = c_loc(sensors)
            call h5dread_f(set_id, type_id, ptr, stat)
            if (stat < 0) exit hdf5_block

            rc = E_NONE
        end block hdf5_block

        if (.not. allocated(sensors)) allocate (sensors(0))

        ! Release resources.
        if (type_id > -1)  call h5tclose_f(type_id, stat)
        if (space_id > -1) call h5sclose_f(space_id, stat)
        if (set_id > -1)   call h5dclose_f(set_id, stat)
    end function hdf5_read_sensors

    integer function hdf5_read_targets(id, targets, data_set) result(rc)
        !! Reads array of `target_type` from compound data in HDF5 file. If
        !! `data_set` is not passed, the name will be set to the value of
        !! `HDF5_DATASET_TARGET`.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_INVALID` if the passed `id` is invalid.
        !! * `E_ALLOC` if allocation of array `targets` failed.
        !! * `E_HDF5` if the HDF5 library call failed.
        !!
        use :: dm_target

        class(hdf5_id_type),                    intent(inout)        :: id         !! HDF5 file or group type.
        type(target_type), allocatable, target, intent(out)          :: targets(:) !! Target type array.
        character(*),                           intent(in), optional :: data_set   !! Name of data set.

        integer          :: stat
        integer(hid_t)   :: set_id, space_id, type_id
        integer(hsize_t) :: dims(1), max_dims(1)
        type(c_ptr)      :: ptr

        type_id  = -1
        set_id   = -1
        space_id = -1

        rc = E_INVALID
        if (id%id < 0) return

        rc = E_HDF5

        hdf5_block: block
            ! Open the data set.
            if (present(data_set)) then
                call h5dopen_f(id%id, data_set, set_id, stat)
            else
                call h5dopen_f(id%id, HDF5_DATASET_TARGET, set_id, stat)
            end if
            if (stat < 0) exit hdf5_block

            ! Get data space.
            call h5dget_space_f(set_id, space_id, stat)
            if (stat < 0) exit hdf5_block

            ! Get dimensions. On success, `stat` is set to the rank of the array.
            call h5sget_simple_extent_dims_f(space_id, dims, max_dims, stat)
            if (stat < 0) exit hdf5_block

            ! Allocate array.
            rc = E_ALLOC
            allocate (targets(dims(1)), stat=stat)
            if (stat /= 0) exit hdf5_block

            ! Create memory data type.
            rc = hdf5_create_target(type_id)
            if (dm_is_error(rc)) exit hdf5_block

            ! Read data from the data set.
            ptr = c_loc(targets)
            call h5dread_f(set_id, type_id, ptr, stat)
            if (stat < 0) exit hdf5_block

            rc = E_NONE
        end block hdf5_block

        if (.not. allocated(targets)) allocate (targets(0))

        ! Release resources.
        if (type_id > -1)  call h5tclose_f(type_id, stat)
        if (space_id > -1) call h5sclose_f(space_id, stat)
        if (set_id > -1)   call h5dclose_f(set_id, stat)
    end function hdf5_read_targets

    integer function hdf5_write(id, type_id, data_size, data_ptr, data_set) result(rc)
        !! Creates HDF5 data space and writes type array to HDF5 file or group.
        !! This function does not close type identifier `type_id`. Returns
        !! `E_HDF5` if the HDF5 library call failed.
        integer(hid_t),   intent(in) :: id        !! HDF5 file or group type.
        integer(hid_t),   intent(in) :: type_id   !! HDF5 type id.
        integer(hsize_t), intent(in) :: data_size !! Array size.
        type(c_ptr),      intent(in) :: data_ptr  !! C pointer to type array.
        character(*),     intent(in) :: data_set  !! Name of data set.

        integer          :: stat
        integer(hid_t)   :: set_id, space_id
        integer(hsize_t) :: dims(1)

        rc = E_HDF5

        set_id   = -1
        space_id = -1

        hdf5_block: block
            ! Create data space of rank 1.
            dims(1) = data_size
            call h5screate_simple_f(1, dims, space_id, stat)
            if (stat < 0) exit hdf5_block

            ! Create the data set in the data space.
            call h5dcreate_f(id, data_set, type_id, space_id, set_id, stat)
            if (stat < 0) exit hdf5_block

            ! Write data to the data set.
            call h5dwrite_f(set_id, type_id, data_ptr, stat)
            if (stat < 0) exit hdf5_block

            rc = E_NONE
        end block hdf5_block

        ! Release resources.
        if (space_id > -1) call h5sclose_f(space_id, stat)
        if (set_id > -1)   call h5dclose_f(set_id, stat)
    end function hdf5_write

    integer function hdf5_write_nodes(id, nodes, data_set) result(rc)
        !! Creates HDF5 data space and writes nodes to HDF5 file or group. If
        !! `data_set` is not passed, the name will be set to the value of
        !! `HDF5_DATASET_NODE`.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_INVALID` if the given HDF5 id type (file, group) is invalid.
        !! * `E_EMPTY` if the passed node array is of size 0.
        !! * `E_HDF5` if the HDF5 library call failed.
        !!
        use :: dm_node

        class(hdf5_id_type),     intent(inout)        :: id       !! HDF5 file or group type.
        type(node_type), target, intent(inout)        :: nodes(:) !! Node type array.
        character(*),            intent(in), optional :: data_set !! Name of data set.

        integer          :: stat
        integer(hid_t)   :: type_id
        integer(hsize_t) :: data_size

        rc = E_INVALID
        if (id%id < 0) return

        rc = E_EMPTY
        if (size(nodes) == 0) return

        hdf5_block: block
            rc = hdf5_create_node(type_id)
            if (dm_is_error(rc)) exit hdf5_block

            data_size = size(nodes, kind=hsize_t)

            if (present(data_set)) then
                rc = hdf5_write(id%id, type_id, data_size, c_loc(nodes), data_set)
            else
                rc = hdf5_write(id%id, type_id, data_size, c_loc(nodes), HDF5_DATASET_NODE)
            end if
        end block hdf5_block

        if (type_id > -1) call h5tclose_f(type_id, stat)
    end function hdf5_write_nodes

    integer function hdf5_write_observs(id, observs, data_set) result(rc)
        !! Creates HDF5 data space and writes observations to HDF5 file or
        !! group. If `data_set` is not passed, the name will be set to the
        !! value of `HDF5_DATASET_OBSERV`.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_INVALID` if the given HDF5 id type (file, group) is invalid.
        !! * `E_EMPTY` if the passed observation array is of size 0.
        !! * `E_HDF5` if the HDF5 library call failed.
        !!
        use :: dm_observ

        class(hdf5_id_type),       intent(inout)        :: id         !! HDF5 file or group type.
        type(observ_type), target, intent(inout)        :: observs(:) !! Observation type array.
        character(*),              intent(in), optional :: data_set   !! Name of data set.

        integer          :: stat
        integer(hid_t)   :: type_id
        integer(hsize_t) :: data_size

        rc = E_INVALID
        if (id%id < 0) return

        rc = E_EMPTY
        if (size(observs) == 0) return

        hdf5_block: block
            rc = hdf5_create_observ(type_id)
            if (dm_is_error(rc)) exit hdf5_block

            data_size = size(observs, kind=hsize_t)

            if (present(data_set)) then
                rc = hdf5_write(id%id, type_id, data_size, c_loc(observs), data_set)
            else
                rc = hdf5_write(id%id, type_id, data_size, c_loc(observs), HDF5_DATASET_OBSERV)
            end if
        end block hdf5_block

        if (type_id > -1) call h5tclose_f(type_id, stat)
    end function hdf5_write_observs

    integer function hdf5_write_sensors(id, sensors, data_set) result(rc)
        !! Creates HDF5 data space and writes sensors to HDF5 file or group. If
        !! `data_set` is not passed, the name will be set to the value of
        !! `HDF5_DATASET_SENSOR`.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_INVALID` if the given HDF5 id type (file, group) is invalid.
        !! * `E_EMPTY` if the passed sensor array is of size 0.
        !! * `E_HDF5` if the HDF5 library call failed.
        !!
        use :: dm_sensor

        class(hdf5_id_type),       intent(inout)        :: id         !! HDF5 file or group type.
        type(sensor_type), target, intent(inout)        :: sensors(:) !! Sensor type array.
        character(*),              intent(in), optional :: data_set   !! Name of data set.

        integer          :: stat
        integer(hid_t)   :: type_id
        integer(hsize_t) :: data_size

        rc = E_INVALID
        if (id%id < 0) return

        rc = E_EMPTY
        if (size(sensors) == 0) return

        hdf5_block: block
            rc = hdf5_create_sensor(type_id)
            if (dm_is_error(rc)) exit hdf5_block

            data_size = size(sensors, kind=hsize_t)

            if (present(data_set)) then
                rc = hdf5_write(id%id, type_id, data_size, c_loc(sensors), data_set)
            else
                rc = hdf5_write(id%id, type_id, data_size, c_loc(sensors), HDF5_DATASET_SENSOR)
            end if
        end block hdf5_block

        if (type_id > -1) call h5tclose_f(type_id, stat)
    end function hdf5_write_sensors

    integer function hdf5_write_targets(id, targets, data_set) result(rc)
        !! Creates HDF5 data space and writes targets to HDF5 file or group. If
        !! `data_set` is not passed, the name will be set to the value of
        !! `HDF5_DATASET_TARGET`.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_INVALID` if the given HDF5 id type (file, group) is invalid.
        !! * `E_EMPTY` if the passed target array is of size 0.
        !! * `E_HDF5` if the HDF5 library call failed.
        !!
        use :: dm_target

        class(hdf5_id_type),       intent(inout)        :: id         !! HDF5 file or group type.
        type(target_type), target, intent(inout)        :: targets(:) !! Target type array.
        character(*),              intent(in), optional :: data_set   !! Name of data set.

        integer          :: stat
        integer(hid_t)   :: type_id
        integer(hsize_t) :: data_size

        rc = E_INVALID
        if (id%id < 0) return

        rc = E_EMPTY
        if (size(targets) == 0) return

        hdf5_block: block
            rc = hdf5_create_target(type_id)
            if (dm_is_error(rc)) exit hdf5_block

            data_size = size(targets, kind=hsize_t)

            if (present(data_set)) then
                rc = hdf5_write(id%id, type_id, data_size, c_loc(targets), data_set)
            else
                rc = hdf5_write(id%id, type_id, data_size, c_loc(targets), HDF5_DATASET_TARGET)
            end if
        end block hdf5_block

        if (type_id > -1) call h5tclose_f(type_id, stat)
    end function hdf5_write_targets
end module dm_hdf5
