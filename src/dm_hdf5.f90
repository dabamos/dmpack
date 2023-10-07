! Author:  Philipp Engel
! Licence: ISC
module dm_hdf5
    !! Abstraction layer around HDF5. Has to be linked against `-lhdf5` and
    !! `-lhdf5_fortran`.
    !!
    !! * [HDF5 Reference Manual](https://docs.hdfgroup.org/hdf5/develop/_r_m.html)
    use :: hdf5
    use :: dm_error
    use :: dm_file
    use :: dm_kind
    use :: dm_node
    use :: dm_sensor
    use :: dm_target
    implicit none (type, external)
    private

    ! HDF5 data set names of DMPACK derived types.
    character(len=*), parameter, public :: HDF_DATA_SET_NODE   = 'nodes'   !! Default name of node data set.
    character(len=*), parameter, public :: HDF_DATA_SET_OBSERV = 'observs' !! Default name of observation data set.
    character(len=*), parameter, public :: HDF_DATA_SET_SENSOR = 'sensors' !! Default name of sensor data set.
    character(len=*), parameter, public :: HDF_DATA_SET_TARGET = 'targets' !! Default name of target data set.

    ! HDF5 access modes.
    integer, parameter, public :: HDF5_RDONLY = 0 !! Read-only access.
    integer, parameter, public :: HDF5_RDWR   = 1 !! Read/write access.

    type, public :: hdf5_id_type
        !! Opaque HDF5 id type.
        private
        integer(kind=hid_t) :: id = -1 !! Identifier.
    end type hdf5_id_type

    type, extends(hdf5_id_type), public :: hdf5_file_type
        !! Opaque HDF5 file type.
    end type hdf5_file_type

    type, extends(hdf5_id_type), public :: hdf5_group_type
        !! Opaque HDF5 group type.
    end type hdf5_group_type

    interface dm_hdf5_close
        !! Generic HDF5 close function.
        module procedure :: hdf5_close_file
        module procedure :: hdf5_close_group
    end interface

    interface dm_hdf5_open
        !! Generic HDF5 open function.
        module procedure :: hdf5_open_file
        module procedure :: hdf5_open_group
    end interface

    interface dm_hdf5_read
        !! Generic HDF5 read function.
        module procedure :: hdf5_read_nodes
        module procedure :: hdf5_read_sensors
        module procedure :: hdf5_read_targets
    end interface

    interface dm_hdf5_write
        !! Generic HDF5 write function.
        module procedure :: hdf5_write_nodes
        module procedure :: hdf5_write_targets
        module procedure :: hdf5_write_sensors
    end interface

    ! Public procedures.
    public :: dm_hdf5_close
    public :: dm_hdf5_destroy
    public :: dm_hdf5_file_free
    public :: dm_hdf5_file_path
    public :: dm_hdf5_file_valid
    public :: dm_hdf5_init
    public :: dm_hdf5_open
    public :: dm_hdf5_read
    public :: dm_hdf5_version
    public :: dm_hdf5_write

    ! Private procedures.
    private :: hdf5_close_file
    private :: hdf5_close_group
    private :: hdf5_open_file
    private :: hdf5_open_group

    private :: hdf5_read_nodes
    private :: hdf5_read_sensors
    private :: hdf5_read_targets

    private :: hdf5_write_nodes
    private :: hdf5_write_sensors
    private :: hdf5_write_targets

    private :: hdf5_type_node
    private :: hdf5_type_sensor
    private :: hdf5_type_target
contains
    ! ******************************************************************
    ! PUBLIC PROCEDURES.
    ! ******************************************************************
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
        type(hdf5_file_type), intent(inout) :: file       !! HDF5 file type.
        integer(kind=i8),     intent(out)   :: free_space !! Free space in bytes.

        integer                :: stat
        integer(kind=hssize_t) :: sz

        rc = E_INVALID
        free_space = 0_i8
        if (file%id < 0) return

        rc = E_HDF5
        call h5fget_freespace_f(file%id, sz, stat)
        if (stat < 0) return

        free_space = int(sz, kind=i8)
        rc = E_NONE
    end function dm_hdf5_file_free

    integer function dm_hdf5_file_path(file, path, n) result(rc)
        !! Returns file path of given HDF5 file in `path`. The argument `path`
        !! must be large enough to hold the full path. The actual length is
        !! returned in optional argument `n`.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_INVALID` if the passed HDF5 file is not opened.
        !! * `E_HDF5` if the HDF5 library call failed.
        type(hdf5_file_type), intent(inout)         :: file !! HDF5 file type.
        character(len=*),     intent(inout)         :: path !! Path of HDF5 file.
        integer,              intent(out), optional :: n    !! Path length.

        integer              :: stat
        integer(kind=size_t) :: sz

        rc = E_INVALID
        if (present(n)) n = 0
        if (file%id < 0) return

        rc = E_HDF5
        call h5fget_name_f(file%id, path, sz, stat)
        if (stat < 0) return

        if (present(n)) n = int(sz)
        rc = E_NONE
    end function dm_hdf5_file_path

    integer function dm_hdf5_file_valid(path) result(rc)
        !! Returns `E_NONE` if given file is an HDF5 file, else the appropriate
        !! error code:
        !!
        !! * `E_FORMAT` if the file is not an HDF5 file.
        !! * `E_NOT_FOUND` if the file does not exist.
        !! * `E_HDF5` if the HDF5 library call failed.
        character(len=*), intent(in) :: path !! File path.

        integer :: stat
        logical :: is_hdf5

        rc = E_NOT_FOUND
        if (.not. dm_file_exists(path)) return

        rc = E_HDF5
        call h5fis_hdf5_f(trim(path), is_hdf5, stat)
        if (stat < 0) return

        rc = E_FORMAT
        if (.not. is_hdf5) return

        rc = E_NONE
    end function dm_hdf5_file_valid

    integer function dm_hdf5_init() result(rc)
        !! Initialises HDF5 Fortran interface. Returns `E_HDF5` on error.
        integer :: stat

        rc = E_HDF5
        call h5open_f(stat)
        if (stat < 0) return
        rc = E_NONE
    end function dm_hdf5_init

    integer function dm_hdf5_version(major, minor, release) result(rc)
        !! Returns version numbers of HDF5 library. Returns `E_HDF5` on error.
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
    end function dm_hdf5_version

    ! ******************************************************************
    ! PRIVATE PROCEDURES.
    ! ******************************************************************
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

    integer function hdf5_open_file(file, path, mode, create) result(rc)
        !! Opens HDF5 file, by default in read/write access mode, unless `mode`
        !! is passed. If `create` is `.true.`, a new file will be created.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_EXIST` if the file to create already exists.
        !! * `E_HDF5` if opening or creating the file failed.
        !! * `E_INVALID` if the file is opened already or argument `mode` is
        !!    invalid.
        !! * `E_NOT_FOUND` if the file was not found.
        type(hdf5_file_type), intent(out)          :: file   !! HDF5 file type.
        character(len=*),     intent(in)           :: path   !! Path to HDF5 file.
        integer,              intent(in), optional :: mode   !! Open mode (`HDF5_RDONLY` or `HDF5_RDWR`).
        logical,              intent(in), optional :: create !! Create HDF5 file.

        integer :: flags, mode_
        integer :: stat
        logical :: create_, exists

        rc = E_INVALID
        if (file%id > -1) return

        exists  = dm_file_exists(path)
        create_ = .false.
        if (present(create)) create_ = create

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
        mode_ = HDF5_RDWR
        if (present(mode)) mode_ = mode

        select case (mode_)
            case (HDF5_RDONLY)
                flags = H5F_ACC_RDONLY_F
            case (HDF5_RDWR)
                flags = H5F_ACC_RDWR_F
            case default
                rc = E_INVALID
                return
        end select

        rc = E_NOT_FOUND
        if (.not. exists) return

        rc = E_HDF5
        call h5fopen_f(trim(path), flags, file%id, stat)
        if (stat < 0) return

        rc = E_NONE
    end function hdf5_open_file

    integer function hdf5_open_group(id, group, name, create) result(rc)
        !! Opens or creates group of name `name`. The function return
        !! `E_INVALID` if the file is not opened, and `E_HDF5` if the group
        !! operation failed.
        class(hdf5_id_type),   intent(inout)        :: id     !! HDF5 file or group type.
        type(hdf5_group_type), intent(out)          :: group  !! HDF5 group type.
        character(len=*),      intent(in)           :: name   !! Group name.
        logical,               intent(in), optional :: create !! Create group.

        integer :: stat
        logical :: create_

        rc = E_INVALID
        if (id%id < 0) return

        create_ = .false.
        if (present(create)) create_ = create

        rc = E_HDF5

        if (create_) then
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
        !! Reads array of `node_type` from compound data in HDF5 file.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_INVALID` if passed `id` is invalid.
        !! * `E_ALLOC` if the allocation of array `nodes` failed.
        !! * `E_HDF5` if the HDF5 library calls failed.
        class(hdf5_id_type),                  intent(inout)        :: id       !! HDF5 file or group type.
        type(node_type), allocatable, target, intent(out)          :: nodes(:) !! Node type array.
        character(len=*),                     intent(in), optional :: data_set !! Name of data set (or `HDF5_DATA_SET_NODE`).

        integer               :: stat
        integer(kind=hid_t)   :: set_id, space_id, type_id
        integer(kind=hsize_t) :: dims(1), max_dims(1)
        type(c_ptr)           :: ptr

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
                call h5dopen_f(id%id, HDF_DATA_SET_NODE, set_id, stat)
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
            rc = hdf5_type_node(type_id)
            if (dm_is_error(rc)) exit hdf5_block

            ! Read data from the data set.
            ptr = c_loc(nodes)
            call h5dread_f(set_id, type_id, ptr, stat)
            if (stat < 0) exit hdf5_block

            rc = E_NONE
        end block hdf5_block

        ! Release resources.
        if (type_id > -1)  call h5tclose_f(type_id, stat)
        if (space_id > -1) call h5sclose_f(space_id, stat)
        if (set_id > -1)   call h5dclose_f(set_id, stat)
    end function hdf5_read_nodes

    integer function hdf5_read_sensors(id, sensors, data_set) result(rc)
        !! Reads array of `sensor_type` from compound data in HDF5 file.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_INVALID` if passed `id` is invalid.
        !! * `E_ALLOC` if the allocation of array `sensors` failed.
        !! * `E_HDF5` if the HDF5 library calls failed.
        class(hdf5_id_type),                    intent(inout)        :: id         !! HDF5 file or group type.
        type(sensor_type), allocatable, target, intent(out)          :: sensors(:) !! Sensor type array.
        character(len=*),                       intent(in), optional :: data_set   !! Name of data set (or `HDF5_DATA_SET_SENSOR`).

        integer               :: stat
        integer(kind=hid_t)   :: set_id, space_id, type_id
        integer(kind=hsize_t) :: dims(1), max_dims(1)
        type(c_ptr)           :: ptr

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
                call h5dopen_f(id%id, HDF_DATA_SET_SENSOR, set_id, stat)
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
            rc = hdf5_type_sensor(type_id)
            if (dm_is_error(rc)) exit hdf5_block

            ! Read data from the data set.
            ptr = c_loc(sensors)
            call h5dread_f(set_id, type_id, ptr, stat)
            if (stat < 0) exit hdf5_block

            rc = E_NONE
        end block hdf5_block

        ! Release resources.
        if (type_id > -1)  call h5tclose_f(type_id, stat)
        if (space_id > -1) call h5sclose_f(space_id, stat)
        if (set_id > -1)   call h5dclose_f(set_id, stat)
    end function hdf5_read_sensors

    integer function hdf5_read_targets(id, targets, data_set) result(rc)
        !! Reads array of `target_type` from compound data in HDF5 file.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_INVALID` if passed `id` is invalid.
        !! * `E_ALLOC` if the allocation of array `targets` failed.
        !! * `E_HDF5` if the HDF5 library calls failed.
        class(hdf5_id_type),                    intent(inout)        :: id         !! HDF5 file or group type.
        type(target_type), allocatable, target, intent(out)          :: targets(:) !! Target type array.
        character(len=*),                       intent(in), optional :: data_set   !! Name of data set (or `HDF5_DATA_SET_TARGET`).

        integer               :: stat
        integer(kind=hid_t)   :: set_id, space_id, type_id
        integer(kind=hsize_t) :: dims(1), max_dims(1)
        type(c_ptr)           :: ptr

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
                call h5dopen_f(id%id, HDF_DATA_SET_TARGET, set_id, stat)
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
            rc = hdf5_type_target(type_id)
            if (dm_is_error(rc)) exit hdf5_block

            ! Read data from the data set.
            ptr = c_loc(targets)
            call h5dread_f(set_id, type_id, ptr, stat)
            if (stat < 0) exit hdf5_block

            rc = E_NONE
        end block hdf5_block

        ! Release resources.
        if (type_id > -1)  call h5tclose_f(type_id, stat)
        if (space_id > -1) call h5sclose_f(space_id, stat)
        if (set_id > -1)   call h5dclose_f(set_id, stat)
    end function hdf5_read_targets

    integer function hdf5_type_node(type_id) result(rc)
        !! Creates compound memory data type for derived type `node_type`.
        integer(kind=hid_t), intent(out) :: type_id !! Data type id.

        integer                 :: stat
        integer(kind=hid_t)     :: id_tid, meta_tid, name_tid
        integer(kind=hsize_t)   :: dims(1), offset
        type(node_type), target :: nodes(1)

        rc = E_HDF5

        ! Create compound type.
        offset = int(NODE_SIZE, kind=hsize_t)
        call h5tcreate_f(H5T_COMPOUND_F, offset, type_id, stat)
        if (stat < 0) return

        ! Node id.
        dims(1) = int(NODE_ID_LEN, kind=hsize_t)
        call h5tarray_create_f(H5T_NATIVE_CHARACTER, 1, dims, id_tid, stat)
        if (stat < 0) return
        offset = h5offsetof(c_loc(nodes(1)), c_loc(nodes(1)%id))
        call h5tinsert_f(type_id, 'id', offset, id_tid, stat)
        if (stat < 0) return

        ! Node name.
        dims(1) = int(NODE_NAME_LEN, kind=hsize_t)
        call h5tarray_create_f(H5T_NATIVE_CHARACTER, 1, dims, name_tid, stat)
        if (stat < 0) return
        offset = h5offsetof(c_loc(nodes(1)), c_loc(nodes(1)%name))
        call h5tinsert_f(type_id, 'name', offset, name_tid, stat)
        if (stat < 0) return

        ! Node meta.
        dims(1) = int(NODE_META_LEN, kind=hsize_t)
        call h5tarray_create_f(H5T_NATIVE_CHARACTER, 1, dims, meta_tid, stat)
        if (stat < 0) return
        offset = h5offsetof(c_loc(nodes(1)), c_loc(nodes(1)%meta))
        call h5tinsert_f(type_id, 'meta', offset, meta_tid, stat)
        if (stat < 0) return

        rc = E_NONE
    end function hdf5_type_node

    integer function hdf5_type_sensor(type_id) result(rc)
        !! Creates compound memory data type for derived type `sensor_type`.
        integer(kind=hid_t), intent(out) :: type_id !! Data type id.

        integer                   :: stat
        integer(kind=hid_t)       :: id_tid, meta_tid, name_tid, node_id_tid, sn_tid
        integer(kind=hsize_t)     :: dims(1), offset
        type(sensor_type), target :: sensors(1)

        rc = E_HDF5

        ! Create compound type.
        offset = int(SENSOR_SIZE, kind=hsize_t)
        call h5tcreate_f(H5T_COMPOUND_F, offset, type_id, stat)
        if (stat < 0) return

        ! Sensor id.
        dims(1) = int(SENSOR_ID_LEN, kind=hsize_t)
        call h5tarray_create_f(H5T_NATIVE_CHARACTER, 1, dims, id_tid, stat)
        if (stat < 0) return
        offset = h5offsetof(c_loc(sensors(1)), c_loc(sensors(1)%id))
        call h5tinsert_f(type_id, 'id', offset, id_tid, stat)
        if (stat < 0) return

        ! Node id.
        dims(1) = int(NODE_ID_LEN, kind=hsize_t)
        call h5tarray_create_f(H5T_NATIVE_CHARACTER, 1, dims, node_id_tid, stat)
        if (stat < 0) return
        offset = h5offsetof(c_loc(sensors(1)), c_loc(sensors(1)%node_id))
        call h5tinsert_f(type_id, 'node_id', offset, node_id_tid, stat)
        if (stat < 0) return

        ! Sensor type.
        offset = h5offsetof(c_loc(sensors(1)), c_loc(sensors(1)%type))
        call h5tinsert_f(type_id, 'type', offset, h5kind_to_type(kind(sensors(1)%type), H5_INTEGER_KIND), stat)

        ! Sensor name.
        dims(1) = int(SENSOR_NAME_LEN, kind=hsize_t)
        call h5tarray_create_f(H5T_NATIVE_CHARACTER, 1, dims, name_tid, stat)
        if (stat < 0) return
        offset = h5offsetof(c_loc(sensors(1)), c_loc(sensors(1)%name))
        call h5tinsert_f(type_id, 'name', offset, name_tid, stat)
        if (stat < 0) return

        ! Sensor serial number.
        dims(1) = int(SENSOR_SN_LEN, kind=hsize_t)
        call h5tarray_create_f(H5T_NATIVE_CHARACTER, 1, dims, sn_tid, stat)
        if (stat < 0) return
        offset = h5offsetof(c_loc(sensors(1)), c_loc(sensors(1)%sn))
        call h5tinsert_f(type_id, 'sn', offset, sn_tid, stat)
        if (stat < 0) return

        ! Sensor meta.
        dims(1) = int(SENSOR_META_LEN, kind=hsize_t)
        call h5tarray_create_f(H5T_NATIVE_CHARACTER, 1, dims, meta_tid, stat)
        if (stat < 0) return
        offset = h5offsetof(c_loc(sensors(1)), c_loc(sensors(1)%meta))
        call h5tinsert_f(type_id, 'meta', offset, meta_tid, stat)
        if (stat < 0) return

        rc = E_NONE
    end function hdf5_type_sensor

    integer function hdf5_type_target(type_id) result(rc)
        !! Creates compound memory data type for derived type `target_type`.
        integer(kind=hid_t), intent(out) :: type_id !! Data type id.

        integer                   :: stat
        integer(kind=hid_t)       :: id_tid, meta_tid, name_tid
        integer(kind=hsize_t)     :: dims(1), offset
        type(target_type), target :: targets(1)

        rc = E_HDF5

        ! Create compound type.
        offset = int(TARGET_SIZE, kind=hsize_t)
        call h5tcreate_f(H5T_COMPOUND_F, offset, type_id, stat)
        if (stat < 0) return

        ! Target id.
        dims(1) = int(TARGET_ID_LEN, kind=hsize_t)
        call h5tarray_create_f(H5T_NATIVE_CHARACTER, 1, dims, id_tid, stat)
        if (stat < 0) return
        offset = h5offsetof(c_loc(targets(1)), c_loc(targets(1)%id))
        call h5tinsert_f(type_id, 'id', offset, id_tid, stat)
        if (stat < 0) return

        ! Target name.
        dims(1) = int(TARGET_NAME_LEN, kind=hsize_t)
        call h5tarray_create_f(H5T_NATIVE_CHARACTER, 1, dims, name_tid, stat)
        if (stat < 0) return
        offset = h5offsetof(c_loc(targets(1)), c_loc(targets(1)%name))
        call h5tinsert_f(type_id, 'name', offset, name_tid, stat)
        if (stat < 0) return

        ! Target meta.
        dims(1) = int(TARGET_META_LEN, kind=hsize_t)
        call h5tarray_create_f(H5T_NATIVE_CHARACTER, 1, dims, meta_tid, stat)
        if (stat < 0) return
        offset = h5offsetof(c_loc(targets(1)), c_loc(targets(1)%meta))
        call h5tinsert_f(type_id, 'meta', offset, meta_tid, stat)
        if (stat < 0) return

        rc = E_NONE
    end function hdf5_type_target

    integer function hdf5_write_nodes(id, nodes, data_set) result(rc)
        !! Creates HDF5 data space `node_type` and writes nodes to HDF5 file or
        !! group.
        !!
        !! The function returns the followin error codes:
        !!
        !! * `E_INVALID` if the given HDF5 id type (file, group) is invalid.
        !! * `E_EMPTY` if the passed node array if of size 0.
        !! * `E_HDF5` if the HDF5 library calls failed.
        class(hdf5_id_type),     intent(inout)        :: id       !! HDF5 file or group type.
        type(node_type), target, intent(inout)        :: nodes(:) !! Node type array.
        character(len=*),        intent(in), optional :: data_set !! Name of data set (or `HDF5_DATA_SET_NODE`).

        integer               :: stat
        integer(kind=hid_t)   :: set_id, space_id, type_id
        integer(kind=hsize_t) :: dims(1)
        type(c_ptr)           :: ptr

        set_id   = -1
        space_id = -1
        type_id  = -1

        rc = E_INVALID
        if (id%id < 0) return

        rc = E_EMPTY
        if (size(nodes) == 0) return

        rc = E_HDF5

        hdf5_block: block
            ! Create data space of rank 1.
            dims(1) = size(nodes, kind=hsize_t)
            call h5screate_simple_f(1, dims, space_id, stat)
            if (stat < 0) exit hdf5_block

            ! Create memory data type.
            rc = hdf5_type_node(type_id)
            if (dm_is_error(rc)) exit hdf5_block

            ! Create the data set in the data space.
            if (present(data_set)) then
                call h5dcreate_f(id%id, data_set, type_id, space_id, set_id, stat)
            else
                call h5dcreate_f(id%id, HDF_DATA_SET_NODE, type_id, space_id, set_id, stat)
            end if
            if (stat < 0) exit hdf5_block

            ! Write data to the data set.
            ptr = c_loc(nodes)
            call h5dwrite_f(set_id, type_id, ptr, stat)
            if (stat < 0) exit hdf5_block

            rc = E_NONE
        end block hdf5_block

        ! Release resources.
        if (type_id > -1)  call h5tclose_f(type_id, stat)
        if (space_id > -1) call h5sclose_f(space_id, stat)
        if (set_id > -1)   call h5dclose_f(set_id, stat)
    end function hdf5_write_nodes

    integer function hdf5_write_sensors(id, sensors, data_set) result(rc)
        !! Creates HDF5 data space `sensor_type` and writes sensors to HDF5
        !! file or group.
        !!
        !! The function returns the followin error codes:
        !!
        !! * `E_INVALID` if the given HDF5 id type (file, group) is invalid.
        !! * `E_EMPTY` if the passed sensor array if of size 0.
        !! * `E_HDF5` if the HDF5 library calls failed.
        class(hdf5_id_type),       intent(inout)        :: id         !! HDF5 file or group type.
        type(sensor_type), target, intent(inout)        :: sensors(:) !! Sensor type array.
        character(len=*),          intent(in), optional :: data_set   !! Name of data set (or `HDF5_DATA_SET_SENSOR`).

        integer               :: stat
        integer(kind=hid_t)   :: set_id, space_id, type_id
        integer(kind=hsize_t) :: dims(1)
        type(c_ptr)           :: ptr

        set_id   = -1
        space_id = -1
        type_id  = -1

        rc = E_INVALID
        if (id%id < 0) return

        rc = E_EMPTY
        if (size(sensors) == 0) return

        rc = E_HDF5

        hdf5_block: block
            ! Create data space of rank 1.
            dims(1) = size(sensors, kind=hsize_t)
            call h5screate_simple_f(1, dims, space_id, stat)
            if (stat < 0) exit hdf5_block

            ! Create memory data type.
            rc = hdf5_type_sensor(type_id)
            if (dm_is_error(rc)) exit hdf5_block

            ! Create the data set in the data space.
            if (present(data_set)) then
                call h5dcreate_f(id%id, data_set, type_id, space_id, set_id, stat)
            else
                call h5dcreate_f(id%id, HDF_DATA_SET_SENSOR, type_id, space_id, set_id, stat)
            end if
            if (stat < 0) exit hdf5_block

            ! Write data to the data set.
            ptr = c_loc(sensors)
            call h5dwrite_f(set_id, type_id, ptr, stat)
            if (stat < 0) exit hdf5_block

            rc = E_NONE
        end block hdf5_block

        ! Release resources.
        if (type_id > -1)  call h5tclose_f(type_id, stat)
        if (space_id > -1) call h5sclose_f(space_id, stat)
        if (set_id > -1)   call h5dclose_f(set_id, stat)
    end function hdf5_write_sensors

    integer function hdf5_write_targets(id, targets, data_set) result(rc)
        !! Creates HDF5 data space `target_type` and writes targets to HDF5
        !! file or group.
        !!
        !! The function returns the followin error codes:
        !!
        !! * `E_INVALID` if the given HDF5 id type (file, group) is invalid.
        !! * `E_EMPTY` if the passed target array if of size 0.
        !! * `E_HDF5` if the HDF5 library calls failed.
        class(hdf5_id_type),       intent(inout)        :: id         !! HDF5 file or group type.
        type(target_type), target, intent(inout)        :: targets(:) !! Target type array.
        character(len=*),          intent(in), optional :: data_set   !! Name of data set (or `HDF5_DATA_SET_TARGET`).

        integer               :: stat
        integer(kind=hid_t)   :: set_id, space_id, type_id
        integer(kind=hsize_t) :: dims(1)
        type(c_ptr)           :: ptr

        set_id   = -1
        space_id = -1
        type_id  = -1

        rc = E_INVALID
        if (id%id < 0) return

        rc = E_EMPTY
        if (size(targets) == 0) return

        rc = E_HDF5

        hdf5_block: block
            ! Create data space of rank 1.
            dims(1) = size(targets, kind=hsize_t)
            call h5screate_simple_f(1, dims, space_id, stat)
            if (stat < 0) exit hdf5_block

            ! Create memory data type.
            rc = hdf5_type_target(type_id)
            if (dm_is_error(rc)) exit hdf5_block

            ! Create the data set in the data space.
            if (present(data_set)) then
                call h5dcreate_f(id%id, data_set, type_id, space_id, set_id, stat)
            else
                call h5dcreate_f(id%id, HDF_DATA_SET_TARGET, type_id, space_id, set_id, stat)
            end if
            if (stat < 0) exit hdf5_block

            ! Write data to the data set.
            ptr = c_loc(targets)
            call h5dwrite_f(set_id, type_id, ptr, stat)
            if (stat < 0) exit hdf5_block

            rc = E_NONE
        end block hdf5_block

        ! Release resources.
        if (type_id > -1)  call h5tclose_f(type_id, stat)
        if (space_id > -1) call h5sclose_f(space_id, stat)
        if (set_id > -1)   call h5dclose_f(set_id, stat)
    end function hdf5_write_targets
end module dm_hdf5
