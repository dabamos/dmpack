! Author:  Philipp Engel
! Licence: ISC
module dm_file
    !! File access utility routines.
    use :: dm_error
    use :: dm_kind
    implicit none (type, external)
    private

    integer, parameter, public :: FILE_PATH_LEN  = 2048   !! Maximum file path length.
    integer, parameter, public :: FILE_UNIT_NONE = -99999 !! Default file unit (none).

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
        integer     :: type   = FILE_TYPE_NONE !! File type.
        integer(i8) :: mode   = 0              !! File access mode as signed integer.
        integer(i8) :: size   = 0_i8           !! File size in bytes.
        integer(i8) :: a_time = 0_i8           !! Time of last access [Epoch].
        integer(i8) :: m_time = 0_i8           !! Time of last modification [Epoch].
        integer(i8) :: c_time = 0_i8           !! Time of last status change [Epoch].
    end type file_status_type

    character(*), parameter :: RM_BIN       = '/bin/rm'
    character(*), parameter :: TOUCH_BINARY = '/usr/bin/touch'

    public :: dm_file_exists
    public :: dm_file_delete
    public :: dm_file_is_directory
    public :: dm_file_is_executable
    public :: dm_file_is_fifo
    public :: dm_file_is_readable
    public :: dm_file_is_valid
    public :: dm_file_is_writeable
    public :: dm_file_line_count
    public :: dm_file_make_directory
    public :: dm_file_read
    public :: dm_file_size
    public :: dm_file_status
    public :: dm_file_touch
    public :: dm_file_tree_size
    public :: dm_file_write
contains
    ! **************************************************************************
    ! PUBLIC FUNCTIONS
    ! **************************************************************************
    logical function dm_file_is_directory(path) result(is)
        !! Returns `.true.` if file at given file path is a directory.
        use :: unix, only: c_stat, c_stat_t, S_IFDIR, S_IFMT
        use :: dm_c, only: dm_f_c_string, dm_to_signed

        character(*), intent(in) :: path !! File path.

        integer        :: file_type, stat
        integer(i8)    :: mode
        type(c_stat_t) :: fs

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

        character(*), intent(in) :: path !! File path.

        is = (c_access(dm_f_c_string(path), X_OK) == 0)
    end function dm_file_is_executable

    logical function dm_file_exists(path) result(exists)
        !! Returns `.true.` if file at given file path exists.
        character(*), intent(in) :: path !! File path.

        logical :: l

        exists = .false.
        if (len_trim(path) == 0) return
        inquire (exist=l, file=trim(path))
        exists = l ! Workaround for Flang 20.
    end function dm_file_exists

    logical function dm_file_is_fifo(path) result(fifo)
        !! Returns `.true.` if file at given file path is a named pipe.
        use :: unix, only: c_stat, c_stat_t, S_IFIFO, S_IFMT
        use :: dm_c, only: dm_f_c_string, dm_to_signed

        character(*), intent(in) :: path !! File path.

        integer        :: stat
        integer(i8)    :: mode
        type(c_stat_t) :: fs

        fifo = .false.
        stat = c_stat(dm_f_c_string(path), fs)
        if (stat /= 0) return

        mode = dm_to_signed(fs%st_mode)
        fifo = (int(iand(mode, int(S_IFMT, i8))) == S_IFIFO)
    end function dm_file_is_fifo

    logical function dm_file_is_readable(path) result(is)
        !! Returns `.true.` if current user has read permission.
        use :: unix, only: c_access, R_OK
        use :: dm_c, only: dm_f_c_string

        character(*), intent(in) :: path !! File path.

        is = (c_access(dm_f_c_string(path), R_OK) == 0)
    end function dm_file_is_readable

    logical function dm_file_is_valid(path) result(is)
        !! Returns `.true.` if file path is not empty and contains only valid
        !! characters (printable ASCII).
        use :: dm_string, only: dm_string_is_printable

        character(*), intent(in) :: path !! File path.

        is = (len_trim(path) > 0 .and. dm_string_is_printable(path))
    end function dm_file_is_valid

    logical function dm_file_is_writeable(path) result(is)
        !! Returns `.true.` if current user has write permission.
        use :: unix, only: c_access, W_OK
        use :: dm_c, only: dm_f_c_string

        character(*), intent(in) :: path !! File path.

        is = (c_access(dm_f_c_string(path), W_OK) == 0)
    end function dm_file_is_writeable

    integer(i8) function dm_file_line_count(path, error) result(n)
        !! Returns number of lines in given file by counting new lines. Sets
        !! `error` to `E_IO` if opening the file failed, and to `E_EMPTY` if
        !! the file has no lines.
        character(*), intent(in)            :: path  !! File path.
        integer,      intent(out), optional :: error !! Error code.

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

    integer(i8) function dm_file_size(path, error) result(nbytes)
        !! Returns file size in file storage units (usually, bytes). On error,
        !! size is 0 and the error code `E_NOT_FOUND` is returned in dummy
        !! argument `error`.
        character(*), intent(in)            :: path  !! File path.
        integer,      intent(out), optional :: error !! Error code.

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
        use :: unix, only: S_IFBLK, S_IFCHR, S_IFDIR, S_IFIFO, S_IFLNK, S_IFMT, S_IFREG, S_IFSOCK, &
                           c_stat, c_stat_t
        use :: dm_c, only: dm_f_c_string, dm_to_signed

        character(*),           intent(in)  :: path   !! File path.
        type(file_status_type), intent(out) :: status !! File status type.

        integer        :: file_type
        type(c_stat_t) :: fs

        rc = E_SYSTEM
        if (c_stat(dm_f_c_string(path), fs) /= 0) return

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

    integer(i8) function dm_file_tree_size(path, error) result(nbytes)
        !! Returns size of file tree `path` (directory including all
        !! sub-directories).
        !!
        !! The function returns the following error in `error`:
        !!
        !! * `E_NOT_FOUND` if path does not exists.
        !! * `E_SYSTEM` if the system call failed.
        !!
        use :: unix,    only: c_nftw, c_stat_t
        use :: dm_c,    only: c_f_pointer, c_funloc, c_int, c_null_char, c_ptr
        use :: dm_util, only: dm_present_set

        character(*), intent(in)            :: path  !! File tree path.
        integer,      intent(out), optional :: error !! Error code.

        nbytes = 0_i8

        call dm_present_set(error, E_NONE)

        if (.not. dm_file_exists(path)) then
            call dm_present_set(error, E_NOT_FOUND)
            return
        end if

        if (c_nftw(trim(path) // c_null_char, c_funloc(callback), 1, 0) /= 0) then
            call dm_present_set(error, E_SYSTEM)
            return
        end if
    contains
        integer(c_int) function callback(path, stat, flag, ftw) bind(c)
            type(c_ptr),    intent(in), value :: path !! `c_char *`
            type(c_ptr),    intent(in), value :: stat !! `c_stat_t *`
            integer(c_int), intent(in), value :: flag !! `int`
            type(c_ptr),    intent(in), value :: ftw  !! `c_ftw_t *`

            type(c_stat_t), pointer :: stat_

            call c_f_pointer(stat, stat_)

            nbytes   = nbytes + stat_%st_size
            callback = 0
        end function callback
    end function dm_file_tree_size

    ! **************************************************************************
    ! PUBLIC SUBROUTINES
    ! **************************************************************************
    subroutine dm_file_delete(path, recursive, error)
        !! Deletes file at given file path. Returns `E_EXEC` on error.
        use :: dm_util, only: dm_present

        character(*), intent(in)            :: path      !! File to delete.
        logical,      intent(in),  optional :: recursive !! Delete recursive.
        integer,      intent(out), optional :: error     !! Error code.

        integer :: cmdstat, stat

        if (present(error)) error = E_NONE
        if (.not. dm_file_exists(path)) return

        if (dm_present(recursive, .false.)) then
            call execute_command_line(RM_BIN // ' -rf ' // trim(path), exitstat=stat, cmdstat=cmdstat)
        else
            call execute_command_line(RM_BIN // ' -f '  // trim(path), exitstat=stat, cmdstat=cmdstat)
        end if

        if (present(error) .and. (stat /= 0 .or. cmdstat /= 0)) error = E_EXEC
    end subroutine dm_file_delete

    subroutine dm_file_make_directory(path, mode, error)
        use :: unix, only: EACCES, EEXIST, EMLINK, ENAMETOOLONG, ENOENT, ENOSPC, ENOTDIR, EROFS, &
                           c_mode_t, c_errno, c_mkdir
        use :: dm_c, only: dm_f_c_string

        character(*), intent(in)            :: path  !! Directory to create.
        integer,      intent(in),  optional :: mode  !! Access mode.
        integer,      intent(out), optional :: error !! Error code.

        integer           :: rc
        integer(c_mode_t) :: mode_

        mode_ = int(o'0755', c_mode_t)
        if (present(mode)) mode_ = int(mode, c_mode_t)

        rc = E_NONE
        if (c_mkdir(dm_f_c_string(trim(path)), mode_) == -1) then
            select case (c_errno())
                case (EACCES);       rc = E_ACCESS    ! Insufficient permissions.
                case (EEXIST);       rc = E_EXIST     ! Directory already exists.
                case (EMLINK);       rc = E_LIMIT     ! Link count of parent directory exceeded.
                case (ENAMETOOLONG); rc = E_LIMIT     ! Name too long.
                case (ENOENT);       rc = E_NOT_FOUND ! Parent directory does not exist.
                case (ENOSPC);       rc = E_FULL      ! No space left.
                case (ENOTDIR);      rc = E_INVALID   ! Path is not a directory.
                case (EROFS);        rc = E_WRITE     ! Parent directory is read-only.
                case default;        rc = E_SYSTEM    ! System call failed.
            end select
        end if

        if (present(error)) error = rc
    end subroutine dm_file_make_directory

    subroutine dm_file_touch(path, modified, error)
        !! Creates empty file at given file path and optionally changes last
        !! modification date/time. This routine executes `/usr/bin/touch`
        !! internally.
        !!
        !! The subroutine returns the following error codes in `error`:
        !!
        !! * `E_EXEC` if command execution failed.
        !! * `E_FORMAT` if length of `path` is invalid.
        !! * `E_INVALID` if `modified` is not in ISO 8601 format.
        !!
        use :: dm_time, only: TIME_LEN, dm_time_is_valid

        character(*),        intent(in)            :: path     !! File to create.
        character(TIME_LEN), intent(in),  optional :: modified !! UTC modification date and time to use instead of the current time (ISO 8601).
        integer,             intent(out), optional :: error    !! Error code.

        integer :: rc

        io_block: block
            character(len=FILE_PATH_LEN) :: command
            integer                      :: cmdstat, stat

            if (present(modified)) then
                rc = E_INVALID
                if (.not. dm_time_is_valid(modified, strict=.true.)) exit io_block

                rc = E_FORMAT
                write (command, '(a, " -m -d ", a, "Z ", a)', iostat=stat) TOUCH_BINARY, modified(1:19), trim(path)
            else
                rc = E_FORMAT
                write (command, '(a, 1x, a)', iostat=stat) TOUCH_BINARY, trim(path)
            end if

            if (stat /= 0) exit io_block

            rc = E_EXEC
            call execute_command_line(trim(command), exitstat=stat, cmdstat=cmdstat)
            if (stat == 0 .and. cmdstat == 0) rc = E_NONE
        end block io_block

        if (present(error)) error = rc
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
        character(*),              intent(in)            :: path    !! File path.
        character(:), allocatable, intent(out)           :: content !! Byte string.
        integer(i8),               intent(out), optional :: size    !! Content size.
        integer,                   intent(out), optional :: error   !! Error code.

        integer     :: rc, stat, unit
        integer(i8) :: size_

        unit  = -1
        size_ = -1

        read_block: block
            ! Open file for reading.
            rc = E_IO
            open (access='stream', action='read', file=trim(path), form='unformatted', iostat=stat, newunit=unit)
            if (stat /= 0) exit read_block

            ! Get content size.
            inquire (unit=unit, size=size_)
            if (size_ < 0) exit read_block

            ! Allocate memory.
            rc = E_ALLOC
            allocate (character(size_) :: content, stat=stat)
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

        character(*), intent(in)            :: path    !! Output file path.
        character(*), intent(in)            :: content !! Bytes to write.
        logical,      intent(in),  optional :: raw     !! Unformatted output if true.
        integer,      intent(out), optional :: error   !! Error code.

        integer :: rc, unit

        rc   = E_IO
        unit = -1

        write_block: block
            integer :: stat

            if (dm_present(raw, .false.)) then
                ! Unformatted output.
                open (access='stream', action='write', file=trim(path), form='unformatted', iostat=stat, newunit=unit, status='replace')
                if (stat /= 0) exit write_block

                rc = E_WRITE
                write (unit, iostat=stat) content
                if (stat /= 0) exit write_block
            else
                ! Formatted output.
                open (action='write', file=trim(path), iostat=stat, newunit=unit, status='replace')
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
