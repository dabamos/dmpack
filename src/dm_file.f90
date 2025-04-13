! Author:  Philipp Engel
! Licence: ISC
module dm_file
    !! File access utility routines.
    use :: dm_error
    use :: dm_kind
    implicit none (type, external)
    private

    integer, parameter, public :: FILE_PATH_LEN = 2048 !! Maximum file path length.

    ! File types.
    integer, parameter, public :: FILE_TYPE_NONE   = 0 !! Unknown type.
    integer, parameter, public :: FILE_TYPE_BLOCK  = 1 !! Block device.
    integer, parameter, public :: FILE_TYPE_CHAR   = 2 !! Character device.
    integer, parameter, public :: FILE_TYPE_DIR    = 3 !! Directory.
    integer, parameter, public :: FILE_TYPE_FIFO   = 4 !! FIFO or pipe.
    integer, parameter, public :: FILE_TYPE_FILE   = 5 !! Regular file.
    integer, parameter, public :: FILE_TYPE_LINK   = 6 !! Symbolic link.
    integer, parameter, public :: FILE_TYPE_SOCKET = 7 !! Socket.

    type, public :: file_status_type
        !! Abstraction of C struct _stat(2)_ that stores parts of a file
        !! status. The file mode is usually an unsigned type (`uint32_t` on
        !! Linux, `uint16_t` on FreeBSD), and is therefore converted to signed
        !! integer after the syscall.
        integer          :: type   = FILE_TYPE_NONE !! File type.
        integer(kind=i8) :: mode   = 0              !! File mode as signed integer.
        integer(kind=i8) :: size   = 0_i8           !! File size in bytes.
        integer(kind=i8) :: a_time = 0_i8           !! Time of last access [Epoch].
        integer(kind=i8) :: m_time = 0_i8           !! Time of last modification [Epoch].
        integer(kind=i8) :: c_time = 0_i8           !! Time of last status change [Epoch].
    end type file_status_type

    public :: dm_file_exists
    public :: dm_file_delete
    public :: dm_file_is_directory
    public :: dm_file_is_executable
    public :: dm_file_is_readable
    public :: dm_file_is_writeable
    public :: dm_file_line_count
    public :: dm_file_read
    public :: dm_file_size
    public :: dm_file_status
    public :: dm_file_touch
    public :: dm_file_write
contains
    ! **************************************************************************
    ! PUBLIC FUNCTIONS.
    ! **************************************************************************
    logical function dm_file_exists(path) result(exists)
        !! Returns `.true.` if file at given file path exists.
        character(len=*), intent(in) :: path !! File path.

        logical :: l

        inquire (exist=l, file=trim(path))
        exists = l ! Workaround for Flang 20.
    end function dm_file_exists

    logical function dm_file_is_directory(path) result(is)
        !! Returns `.true.` if file at given file path is a directory.
        use :: unix, only: c_stat, c_stat_type, S_IFDIR, S_IFMT
        use :: dm_c, only: dm_f_c_string, dm_to_signed

        character(len=*), intent(in) :: path !! File path.

        integer           :: file_type, stat
        integer(kind=i8)  :: mode
        type(c_stat_type) :: fs

        is = .false.

        stat = c_stat(dm_f_c_string(path), fs)
        if (stat /= 0) return

        mode = dm_to_signed(fs%st_mode)
        file_type = int(iand(mode, int(S_IFMT, kind=i8)))

        is = (file_type == S_IFDIR)
    end function dm_file_is_directory

    logical function dm_file_is_executable(path) result(is)
        !! Returns `.true.` if current user has execute permission.
        use :: unix, only: c_access, X_OK
        use :: dm_c, only: dm_f_c_string

        character(len=*), intent(in) :: path !! File path.

        is = (c_access(dm_f_c_string(path), X_OK) == 0)
    end function dm_file_is_executable

    logical function dm_file_is_readable(path) result(is)
        !! Returns `.true.` if current user has read permission.
        use :: unix, only: c_access, R_OK
        use :: dm_c, only: dm_f_c_string

        character(len=*), intent(in) :: path !! File path.

        is = (c_access(dm_f_c_string(path), R_OK) == 0)
    end function dm_file_is_readable

    logical function dm_file_is_writeable(path) result(is)
        !! Returns `.true.` if current user has write permission.
        use :: unix, only: c_access, W_OK
        use :: dm_c, only: dm_f_c_string

        character(len=*), intent(in) :: path !! File path.

        is = (c_access(dm_f_c_string(path), W_OK) == 0)
    end function dm_file_is_writeable

    integer(kind=i8) function dm_file_line_count(path, error) result(n)
        !! Returns number of lines in given file by counting new lines. Sets
        !! `error` to `E_IO` if opening the file failed, and to `E_EMPTY` if
        !! the file has no lines.
        character(len=*), intent(in)            :: path  !! File path.
        integer,          intent(out), optional :: error !! Error code.

        character :: a
        integer   :: rc, stat, unit

        n = 0_i8

        if (present(error)) error = E_IO
        open (action='read', file=trim(path), iostat=stat, newunit=unit, status='old')
        if (stat /= 0) return

        do
            read (unit, *, iostat=stat) a
            if (is_iostat_end(stat)) exit
            n = n + 1
        end do

        close (unit)

        rc = E_EMPTY
        if (n > 0) rc = E_NONE
        if (present(error)) error = rc
    end function dm_file_line_count

    integer(kind=i8) function dm_file_size(path, error) result(nbytes)
        !! Returns file size in file storage units (usually, bytes). On error,
        !! size is 0 and the error code `E_NOT_FOUND` is returned in dummy
        !! argument `error`.
        character(len=*), intent(in)            :: path  !! File path.
        integer,          intent(out), optional :: error !! Error code.

        logical :: file_exists

        if (present(error)) error = E_NOT_FOUND
        nbytes = 0_i8
        inquire (exist=file_exists, file=trim(path), size=nbytes)
        if (.not. file_exists) return
        if (present(error)) error = E_NONE
    end function dm_file_size

    integer function dm_file_status(path, status) result(rc)
        !! Returns status of file at given path in `status`. The function
        !! returns `E_SYSTEM` on error.
        use :: unix
        use :: dm_c, only: dm_f_c_string, dm_to_signed

        character(len=*),       intent(in)  :: path   !! File path.
        type(file_status_type), intent(out) :: status !! File status type.

        integer           :: stat, file_type
        type(c_stat_type) :: fs

        rc = E_SYSTEM
        stat = c_stat(dm_f_c_string(path), fs)
        if (stat /= 0) return

        status%size = fs%st_size               ! File size in bytes.
        status%mode = dm_to_signed(fs%st_mode) ! File mode as signed integer.

        ! Dealing with unsigned types is always fun in Fortran ...
        file_type = int(iand(status%mode, int(S_IFMT, kind=i8)))

        select case (file_type)
            case (S_IFBLK);  status%type = FILE_TYPE_BLOCK
            case (S_IFCHR);  status%type = FILE_TYPE_CHAR
            case (S_IFDIR);  status%type = FILE_TYPE_DIR
            case (S_IFIFO);  status%type = FILE_TYPE_FIFO
            case (S_IFLNK);  status%type = FILE_TYPE_LINK
            case (S_IFREG);  status%type = FILE_TYPE_FILE
            case (S_IFSOCK); status%type = FILE_TYPE_SOCKET
            case default;    status%type = FILE_TYPE_NONE
        end select

        status%a_time = fs%st_atim%tv_sec ! Last access time.
        status%m_time = fs%st_mtim%tv_sec ! Last modification time.
        status%c_time = fs%st_ctim%tv_sec ! Last status change time.
        rc = E_NONE
    end function dm_file_status

    ! **************************************************************************
    ! PUBLIC SUBROUTINES.
    ! **************************************************************************
    subroutine dm_file_delete(path, error)
        !! Deletes file at given file path. Returns `E_IO` on error.
        character(len=*), intent(in)            :: path  !! File to delete.
        integer,          intent(out), optional :: error !! Error code.

        integer :: stat, unit

        if (present(error)) error = E_IO
        open (action='write', file=trim(path), iostat=stat, newunit=unit, status='old')
        if (stat /= 0) return
        close (unit, iostat=stat, status='delete')
        if (stat == 0 .and. present(error)) error = E_NONE
    end subroutine dm_file_delete

    subroutine dm_file_touch(path, error)
        !! Creates empty file at given file path. Returns `E_IO` on error.
        character(len=*), intent(in)            :: path  !! File to create.
        integer,          intent(out), optional :: error !! Error code.

        integer :: stat, unit

        if (present(error)) error = E_IO
        open (action='write', file=trim(path), iostat=stat, newunit=unit, status='unknown')
        if (stat /= 0) return
        close (unit, iostat=stat)
        if (stat == 0 .and. present(error)) error = E_NONE
    end subroutine dm_file_touch

    subroutine dm_file_read(path, content, size, error)
        !! Reads file contents as byte stream into allocatable character
        !! string.
        !!
        !! The routine returns the following error codes in argument `error`:
        !!
        !! * `E_ALLOC` if the allocation if `content` failed.
        !! * `E_IO` if opening the file failed.
        !! * `E_READ` if reading from file failed.
        !!
        character(len=*),              intent(in)            :: path    !! File path.
        character(len=:), allocatable, intent(out)           :: content !! Byte string.
        integer(kind=i8),              intent(out), optional :: size    !! Content size.
        integer,                       intent(out), optional :: error   !! Error code.

        integer          :: rc, stat, unit
        integer(kind=i8) :: size_

        unit  = -1
        size_ = -1

        read_block: block
            ! Open file for reading.
            rc = E_IO
            open (access  = 'stream', &
                  action  = 'read', &
                  file    = trim(path), &
                  form    = 'unformatted', &
                  iostat  = stat, &
                  newunit = unit)
            if (stat /= 0) exit read_block

            ! Get content size.
            inquire (unit=unit, size=size_)
            if (size_ < 0) exit read_block

            ! Allocate memory.
            rc = E_ALLOC
            allocate (character(len=size_) :: content, stat=stat)
            if (stat /= 0) exit read_block

            ! Read bytes.
            if (size_ > 0) then
                rc = E_READ
                read (unit, iostat=stat) content
                if (stat /= 0) exit read_block
            end if

            rc = E_NONE
        end block read_block

        if (unit >= 0) close (unit)
        if (.not. allocated(content)) content = ''

        if (present(size))  size  = size_
        if (present(error)) error = rc
    end subroutine dm_file_read

    subroutine dm_file_write(path, content, raw, error)
        !! Writes content to given file (ASCII or binary).
        !!
        !! The routine returns the following error codes in argument `error`:
        !!
        !! * `E_IO` if opening the file failed.
        !! * `E_WRITE` if writing to file failed.
        !!
        use :: dm_util, only: dm_present

        character(len=*), intent(in)            :: path    !! Output file path.
        character(len=*), intent(in)            :: content !! Bytes to write.
        logical,          intent(in),  optional :: raw     !! Unformatted output if true.
        integer,          intent(out), optional :: error   !! Error code.

        integer :: rc, stat, unit
        logical :: raw_

        raw_ = dm_present(raw, .false.)

        rc   = E_IO
        unit = -1

        write_block: block
            if (raw_) then
                ! Unformatted output.
                open (access  = 'stream', &
                      action  = 'write', &
                      file    = trim(path), &
                      form    = 'unformatted', &
                      iostat  = stat, &
                      newunit = unit, &
                      status  = 'replace')
                if (stat /= 0) exit write_block

                rc = E_WRITE
                write (unit, iostat=stat) content
                if (stat /= 0) exit write_block
            else
                ! Formatted output.
                open (action  = 'write', &
                      file    = trim(path), &
                      iostat  = stat, &
                      newunit = unit, &
                      status  = 'replace')
                if (stat /= 0) exit write_block

                rc = E_WRITE
                write (unit, '(a)', iostat=stat) content
                if (stat /= 0) exit write_block
            end if

            rc = E_NONE
        end block write_block

        if (unit >= 0) close (unit)
        if (present(error)) error = rc
    end subroutine dm_file_write
end module dm_file
