! Author:  Philipp Engel
! Licence: ISC
module dm_hdf5
    !! Abstraction layer around HDF5. Has to be linked against `-lhdf5_fortran`
    !! and `-lhdf5`.
    !!
    !! * [HDF5 Reference Manual](https://docs.hdfgroup.org/hdf5/develop/_r_m.html)
    use :: hdf5
    use :: dm_error
    use :: dm_file
    use :: dm_kind
    implicit none (type, external)
    private

    integer, parameter, public :: HDF5_RDONLY = 0 !! Read-only access.
    integer, parameter, public :: HDF5_RDWR   = 1 !! Read/write access.

    type, public :: hdf5_file_type
        !! Opaque HDF5 file type.
        private
        integer(kind=hid_t) :: id = -1 !! File identifier.
    end type hdf5_file_type

    type, public :: hdf5_group_type
        !! Opaque HDF5 group type.
        private
        integer(kind=hid_t) :: id = -1 !! Group identifier.
    end type hdf5_group_type

    type, public :: hdf5_set_type
        !! Opaque HDF5 data set type.
        private
        integer(kind=hid_t) :: id = -1 !! Data set identifier.
    end type hdf5_set_type

    interface dm_hdf5_close
        !! Generic HDF5 close function.
        module procedure :: hdf5_file_close
        module procedure :: hdf5_group_close
        module procedure :: hdf5_set_close
    end interface

    interface dm_hdf5_open
        !! Generic HDF5 open function.
        module procedure :: hdf5_file_open
        module procedure :: hdf5_group_open
    end interface

    public :: dm_hdf5_close
    public :: dm_hdf5_destroy
    public :: dm_hdf5_file_free
    public :: dm_hdf5_file_path
    public :: dm_hdf5_file_valid
    public :: dm_hdf5_init
    public :: dm_hdf5_open
    public :: dm_hdf5_version

    private :: hdf5_file_close
    private :: hdf5_file_open
    private :: hdf5_group_close
    private :: hdf5_group_open
    private :: hdf5_set_close
contains
    ! ******************************************************************
    ! PUBLIC PROCEDURES.
    ! ******************************************************************
    integer function dm_hdf5_destroy() result(rc)
        !! Destroys HDF5 Fortran interface. Returns `E_IO` on error.
        integer :: stat

        rc = E_IO
        call h5close_f(stat)
        if (stat /= 0) return
        rc = E_NONE
    end function dm_hdf5_destroy

    integer function dm_hdf5_file_free(file, free_space) result(rc)
        !! Returns amount of free space within a file in argument `free_space`.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_INVALID` if the passed HDF5 file is not opened.
        !! * `E_IO` if an internal HDF5 library error occured.
        type(hdf5_file_type), intent(inout) :: file       !! HDF5 file type.
        integer(kind=i8),     intent(out)   :: free_space !! Free space in bytes.

        integer                :: stat
        integer(kind=hssize_t) :: sz

        rc = E_INVALID
        free_space = 0_i8
        if (file%id < 0) return

        rc = E_IO
        call h5fget_freespace_f(file%id, sz, stat)
        if (stat /= 0) return

        free_space = int(sz, kind=i8)
        rc = E_NONE
    end function dm_hdf5_file_free

    integer function dm_hdf5_file_path(file, path, n) result(rc)
        !! Returns file path of given HDF5 file in `path`. The argument `path`
        !! must be large enough to hold the full path. The actual length is
        !! returned in optional argument `n`.
        type(hdf5_file_type), intent(inout)         :: file !! HDF5 file type.
        character(len=*),     intent(inout)         :: path !! Path of HDF5 file.
        integer,              intent(out), optional :: n    !! Path length.

        integer              :: stat
        integer(kind=size_t) :: sz

        rc = E_INVALID
        if (present(n)) n = 0
        if (file%id < 0) return

        rc = E_IO
        call h5fget_name_f(file%id, path, sz, stat)
        if (stat /= 0) return

        if (present(n)) n = int(sz)
        rc = E_NONE
    end function dm_hdf5_file_path

    integer function dm_hdf5_file_valid(path) result(rc)
        !! Returns `E_NONE` if given file is an HDF5 file, else the appropriate
        !! error code:
        !!
        !! * `E_FORMAT` if the file is not an HDF5 file.
        !! * `E_IO` if an internal HDF5 library error occured.
        !! * `E_NOT_FOUND` if the file does not exist.
        character(len=*), intent(in) :: path !! File path.

        integer :: stat
        logical :: is_hdf5

        rc = E_NOT_FOUND
        if (.not. dm_file_exists(path)) return

        rc = E_IO
        call h5fis_hdf5_f(trim(path), is_hdf5, stat)
        if (stat /= 0) return

        rc = E_FORMAT
        if (.not. is_hdf5) return

        rc = E_NONE
    end function dm_hdf5_file_valid

    integer function dm_hdf5_init() result(rc)
        !! Initialises HDF5 Fortran interface. Returns `E_IO` on error.
        integer :: stat

        rc = E_IO
        call h5open_f(stat)
        if (stat /= 0) return
        rc = E_NONE
    end function dm_hdf5_init

    integer function dm_hdf5_version(major, minor, release) result(rc)
        !! Returns version numbers of HDF5 library. Returns `E_IO` on error.
        use :: h5lib
        integer, intent(out), optional :: major   !! Major version of HDF5 library.
        integer, intent(out), optional :: minor   !! Minor version of HDF5 library.
        integer, intent(out), optional :: release !! Release version of HDF5 library.

        integer :: major_, minor_, release_
        integer :: stat

        rc = E_IO
        call h5get_libversion_f(major_, minor_, release_, stat)

        if (present(major))   major   = major_
        if (present(minor))   minor   = minor_
        if (present(release)) release = release_

        if (stat /= 0) return
        rc = E_NONE
    end function dm_hdf5_version

    ! ******************************************************************
    ! PRIVATE PROCEDURES.
    ! ******************************************************************
    integer function hdf5_file_close(file) result(rc)
        !! Closes HDF5 file. Returns `E_INVALID` if the passed HDF5 file is not
        !! opened. Returns `E_IO` if closing the file failed.
        type(hdf5_file_type), intent(inout) :: file !! HDF5 file type.

        integer :: stat

        rc = E_INVALID
        if (file%id < 0) return

        rc = E_IO
        call h5fclose_f(file%id, stat)
        if (stat /= 0) return

        file = hdf5_file_type()
        rc = E_NONE
    end function hdf5_file_close

    integer function hdf5_file_open(file, path, mode, create) result(rc)
        !! Opens HDF5 file, by default in read/write access mode, unless `mode`
        !! is passed.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_EXIST` if the file to create already exists.
        !! * `E_INVALID` if the file is opened already or argument `mode` is
        !!    invalid.
        !! * `E_IO` if opening or creating the file failed.
        !! * `E_NOT_FOUND` if the file was not found.
        type(hdf5_file_type), intent(out)          :: file   !! HDF5 file type.
        character(len=*),     intent(in)           :: path   !! Path to HDF5 file.
        integer,              intent(in), optional :: mode   !! Access mode (`HDF5_RDONLY` or `HDF5_RDWR`).
        logical,              intent(in), optional :: create !! Create HDF5 file.

        integer :: flags, mode_
        integer :: stat
        logical :: create_, exists

        rc = E_INVALID
        if (file%id >= 0) return

        exists  = dm_file_exists(path)
        create_ = .false.
        if (present(create)) create_ = create

        ! Create new file.
        if (create_) then
            rc = E_EXIST
            if (exists) return

            rc = E_IO
            call h5fcreate_f(trim(path), H5F_ACC_TRUNC_F, file%id, stat)
            if (stat /= 0) return

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

        rc = E_IO
        call h5fopen_f(trim(path), flags, file%id, stat)
        if (stat /= 0) return

        rc = E_NONE
    end function hdf5_file_open

    integer function hdf5_group_close(group) result(rc)
        !! Closes HDF5 group. Returns `E_INVALID` if the passed HDF5 group
        !! is not opened. Returns `E_IO` if closing the group failed.
        type(hdf5_group_type), intent(inout) :: group !! HDF5 group type.

        integer :: stat

        rc = E_INVALID
        if (group%id < 0) return

        rc = E_IO
        call h5gclose_f(group%id, stat)
        if (stat /= 0) return

        group = hdf5_group_type()
        rc = E_NONE
    end function hdf5_group_close

    integer function hdf5_group_open(file, group, name, create) result(rc)
        !! Opens or creates group of name `name`. The function return
        !! `E_INVALID` if the file is not opened, and `E_IO` if the group
        !! operation failed.
        type(hdf5_file_type),  intent(inout)        :: file   !! HDF5 file type.
        type(hdf5_group_type), intent(out)          :: group  !! HDF5 group type.
        character(len=*),      intent(in)           :: name   !! Group name.
        logical,               intent(in), optional :: create !! Create group.

        integer :: stat
        logical :: create_

        rc = E_INVALID
        if (file%id < 0) return

        create_ = .false.
        if (present(create)) create_ = create

        rc = E_IO

        if (create_) then
            ! Create group.
            call h5gcreate_f(file%id, trim(name), group%id, stat)
        else
            ! Open group.
            call h5gopen_f(file%id, trim(name), group%id, stat)
        end if

        if (stat /= 0) return
        rc = E_NONE
    end function hdf5_group_open

    integer function hdf5_set_close(set) result(rc)
        !! Closes HDF5 data set. Returns `E_INVALID` if the passed HDF5 data
        !! set is not opened. Returns `E_IO` if closing the data set failed.
        type(hdf5_set_type), intent(inout) :: set !! HDF5 data set type.

        integer :: stat

        rc = E_INVALID
        if (set%id < 0) return

        rc = E_IO
        call h5dclose_f(set%id, stat)
        if (stat /= 0) return

        set = hdf5_set_type()
        rc = E_NONE
    end function hdf5_set_close
end module dm_hdf5
