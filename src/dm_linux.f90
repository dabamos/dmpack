! Author:  Philipp Engel
! Licence: ISC
module dm_linux
    !! Abstraction layer over Linux-specific APIs.
    use :: dm_error
    use :: dm_kind
    implicit none (type, external)
    private

    interface linux_pipe
        !! Reads value from pipe.
        module procedure :: linux_pipe_int32
        module procedure :: linux_pipe_int64
        module procedure :: linux_pipe_real32
        module procedure :: linux_pipe_real64
        module procedure :: linux_pipe_string
    end interface linux_pipe

    interface linux_read
        !! Reads value from file system.
        module procedure :: linux_read_int32
        module procedure :: linux_read_int64
        module procedure :: linux_read_real32
        module procedure :: linux_read_real64
        module procedure :: linux_read_string
    end interface linux_read

    public :: dm_linux_procfs_hardware_model
    public :: dm_linux_procfs_load_average

    private :: linux_pipe
    private :: linux_pipe_int32
    private :: linux_pipe_int64
    private :: linux_pipe_real32
    private :: linux_pipe_real64
    private :: linux_pipe_string
    private :: linux_read
    private :: linux_read_int32
    private :: linux_read_int64
    private :: linux_read_real32
    private :: linux_read_real64
    private :: linux_read_string
contains
    ! **************************************************************************
    ! PUBLIC FUNCTIONS.
    ! **************************************************************************
    integer function dm_linux_procfs_hardware_model(model) result(rc)
        !! Returns hardware model from `/proc`.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_EMPTY` if result is empty.
        !! * `E_FORMAT` if file format is invalid.
        !! * `E_IO` if opening file failed.
        !! * `E_READ` if reading from file system failed.
        !! * `E_NOT_FOUND` if file or variable does not exist.
        !!
        character(len=*), intent(inout) :: model !! Hardware model.

        rc = linux_read('/proc/cpuinfo', model, name='model name')
    end function dm_linux_procfs_hardware_model

    integer function dm_linux_procfs_load_average(avg1, avg5, avg15) result(rc)
        !! Returns load averages from `/proc`.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_EMPTY` if result is empty.
        !! * `E_FORMAT` if file format is invalid.
        !! * `E_IO` if opening file failed.
        !! * `E_READ` if reading from file system failed.
        !! * `E_NOT_FOUND` if file or variable does not exist.
        !!
        real, intent(out), optional :: avg1  !! Average, 1 min.
        real, intent(out), optional :: avg5  !! Average, 5 min.
        real, intent(out), optional :: avg15 !! Average, 15 min.

        character(len=64) :: output
        character(len=8)  :: ignore(2)
        integer           :: stat
        real              :: values(3)

        if (present(avg1))  avg1  = 0.0
        if (present(avg5))  avg5  = 0.0
        if (present(avg15)) avg15 = 0.0

        rc = linux_read('/proc/loadavg', output)
        if (dm_is_error(rc)) return

        values = 0.0
        read (output, *, iostat=stat) values, ignore
        if (stat /= 0) rc = E_FORMAT

        if (present(avg1))  avg1  = values(1)
        if (present(avg5))  avg5  = values(2)
        if (present(avg15)) avg15 = values(3)
    end function dm_linux_procfs_load_average

    ! **************************************************************************
    ! PRIVATE FUNCTIONS.
    ! **************************************************************************
    integer function linux_pipe_int32(command, value) result(rc)
        !! Reads 8-byte integer output from pipe.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_IO` if command failed.
        !! * `E_READ` if pipe returned no bytes.
        !! * `E_SYSTEM` if system call failed.
        !!
        character(len=*), intent(in)  :: command !! Command.
        integer(kind=i4), intent(out) :: value   !! Output value.

        character(len=64) :: output
        integer           :: stat

        value = 0_i4
        rc = linux_pipe_string(command, output); if (dm_is_error(rc)) return
        read (output, *, iostat=stat) value;     if (stat /= 0) rc = E_FORMAT
    end function linux_pipe_int32

    integer function linux_pipe_int64(command, value) result(rc)
        !! Reads 8-byte integer output from pipe.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_IO` if command failed.
        !! * `E_READ` if pipe returned no bytes.
        !! * `E_SYSTEM` if system call failed.
        !!
        character(len=*), intent(in)  :: command !! Command.
        integer(kind=i8), intent(out) :: value   !! Output value.

        character(len=64) :: output
        integer           :: stat

        value = 0_i8
        rc = linux_pipe_string(command, output); if (dm_is_error(rc)) return
        read (output, *, iostat=stat) value;     if (stat /= 0) rc = E_FORMAT
    end function linux_pipe_int64

    integer function linux_pipe_real32(command, value) result(rc)
        !! Reads 4-byte real output from pipe.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_IO` if command failed.
        !! * `E_READ` if pipe returned no bytes.
        !! * `E_SYSTEM` if system call failed.
        !!
        character(len=*), intent(in)  :: command !! Command.
        real(kind=r4),    intent(out) :: value   !! Output value.

        character(len=64) :: output
        integer           :: stat

        value = 0.0_r4
        rc = linux_pipe_string(command, output); if (dm_is_error(rc)) return
        read (output, *, iostat=stat) value;     if (stat /= 0) rc = E_FORMAT
    end function linux_pipe_real32

    integer function linux_pipe_real64(command, value) result(rc)
        !! Reads 8-byte real output from pipe.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_IO` if command failed.
        !! * `E_READ` if pipe returned no bytes.
        !! * `E_SYSTEM` if system call failed.
        !!
        character(len=*), intent(in)  :: command !! Command.
        real(kind=r8),    intent(out) :: value   !! Output value.

        character(len=64) :: output
        integer           :: stat

        value = 0.0_r8
        rc = linux_pipe_string(command, output); if (dm_is_error(rc)) return
        read (output, *, iostat=stat) value;     if (stat /= 0) rc = E_FORMAT
    end function linux_pipe_real64

    integer function linux_pipe_string(command, value, nbytes) result(rc)
        !! The function returns the following error codes:
        !!
        !! * `E_EXIST` if pipe is already connected.
        !! * `E_INVALID` if access mode is invalid.
        !! * `E_IO` if command failed.
        !! * `E_READ` if pipe returned no bytes.
        !! * `E_SYSTEM` if system call failed.
        !!
        use :: dm_pipe

        character(len=*), intent(in)            :: command !! Command to run.
        character(len=*), intent(inout)         :: value   !! Output string.
        integer(kind=i8), intent(out), optional :: nbytes  !! String length.

        integer          :: stat
        integer(kind=i8) :: n
        type(pipe_type)  :: pipe

        value = ' '
        if (present(nbytes)) nbytes = 0_i8

        rc = dm_pipe_open(pipe, command, PIPE_RDONLY)
        if (dm_is_error(rc)) return

        n = dm_pipe_read(pipe, value)
        if (n == 0) rc = E_READ

        call dm_pipe_close(pipe, exit_stat=stat)
        if (stat /= 0) rc = E_IO
        if (present(nbytes)) nbytes = n
    end function linux_pipe_string

    integer function linux_read_int32(path, value, name) result(rc)
        !! Reads 8-byte integer value from file system.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_EMPTY` if value is empty.
        !! * `E_FORMAT` if file format is invalid.
        !! * `E_INVALID` if name is invalid.
        !! * `E_IO` if opening file failed.
        !! * `E_READ` if reading from file system failed.
        !! * `E_NOT_FOUND` if file or variable does not exist.
        !!
        character(len=*), intent(in)           :: path  !! Path.
        integer(kind=i4), intent(out)          :: value !! Output value.
        character(len=*), intent(in), optional :: name  !! Variable name.

        character(len=64) :: output
        integer           :: stat

        value = 0_i4
        rc = linux_read_string(path, output, name); if (dm_is_error(rc)) return
        read (output, *, iostat=stat) value;        if (stat /= 0) rc = E_FORMAT
    end function linux_read_int32

    integer function linux_read_int64(path, value, name) result(rc)
        !! Reads 8-byte integer value from file system.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_EMPTY` if value is empty.
        !! * `E_FORMAT` if file format is invalid.
        !! * `E_INVALID` if name is invalid.
        !! * `E_IO` if opening file failed.
        !! * `E_READ` if reading from file system failed.
        !! * `E_NOT_FOUND` if file or variable does not exist.
        !!
        character(len=*), intent(in)           :: path  !! Path.
        integer(kind=i8), intent(out)          :: value !! Output value.
        character(len=*), intent(in), optional :: name  !! Variable name.

        character(len=64) :: output
        integer           :: stat

        value = 0_i8
        rc = linux_read_string(path, output, name); if (dm_is_error(rc)) return
        read (output, *, iostat=stat) value;        if (stat /= 0) rc = E_FORMAT
    end function linux_read_int64

    integer function linux_read_real32(path, value, name) result(rc)
        !! Reads 4-byte real value from file system.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_EMPTY` if value is empty.
        !! * `E_FORMAT` if file format is invalid.
        !! * `E_INVALID` if name is invalid.
        !! * `E_IO` if opening file failed.
        !! * `E_READ` if reading from file system failed.
        !! * `E_NOT_FOUND` if file or variable does not exist.
        !!
        character(len=*), intent(in)           :: path  !! Path.
        real(kind=r4),    intent(out)          :: value !! Output value.
        character(len=*), intent(in), optional :: name  !! Variable name.

        character(len=64) :: output
        integer           :: stat

        value = 0.0_r4
        rc = linux_read_string(path, output, name); if (dm_is_error(rc)) return
        read (output, *, iostat=stat) value;        if (stat /= 0) rc = E_FORMAT
    end function linux_read_real32

    integer function linux_read_real64(path, value, name) result(rc)
        !! Reads 8-byte real value from file system.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_EMPTY` if value is empty.
        !! * `E_FORMAT` if file format is invalid.
        !! * `E_INVALID` if name is invalid.
        !! * `E_IO` if opening file failed.
        !! * `E_READ` if reading from file system failed.
        !! * `E_NOT_FOUND` if file or variable does not exist.
        !!
        character(len=*), intent(in)           :: path  !! Path.
        real(kind=r8),    intent(out)          :: value !! Output value.
        character(len=*), intent(in), optional :: name  !! Variable name.

        character(len=64) :: output
        integer           :: stat

        value = 0.0_r8
        rc = linux_read_string(path, output, name); if (dm_is_error(rc)) return
        read (output, *, iostat=stat) value;        if (stat /= 0) rc = E_FORMAT
    end function linux_read_real64

    integer function linux_read_string(path, value, name, nbytes) result(rc)
        !! Reads string value from file system.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_EMPTY` if value is empty.
        !! * `E_FORMAT` if file format is invalid.
        !! * `E_IO` if opening file failed.
        !! * `E_READ` if reading from file system failed.
        !! * `E_NOT_FOUND` if file or variable does not exist.
        !!
        use :: dm_file,   only: dm_file_exists
        use :: dm_string, only: dm_string_is_present

        character(len=*), intent(in)            :: path   !! Path.
        character(len=*), intent(inout)         :: value  !! Output string.
        character(len=*), intent(in),  optional :: name   !! Variable name.
        integer,          intent(out), optional :: nbytes !! String length.

        character(len=2048) :: line, output
        integer             :: i, l, n, stat, unit

        value = ' '
        if (present(nbytes)) nbytes = 0

        rc = E_INVALID
        if (present(name)) then
            if (len_trim(name) == 0) return
        end if

        rc = E_NOT_FOUND
        if (.not. dm_file_exists(path)) return

        rc = E_IO
        open (action='read', file=trim(path), iostat=stat, newunit=unit, status='old')
        if (stat /= 0) return

        n = min(len(line), len_trim(name))
        output = ' '

        do
            rc = E_READ
            read (unit, '(a)', iostat=stat) line
            if (is_iostat_end(stat)) exit
            if (stat /= 0) exit
            if (present(name)) then
                rc = E_NOT_FOUND
                if (line(1:n) /= name(1:n)) cycle
                rc = E_FORMAT
                i = index(line(n + 1:), ':')
                if (i == 0 .or. i == len(line)) cycle
                output = adjustl(line(i + 1:))
            else
                output = adjustl(line)
            end if
            rc = E_NONE
            value = output
            l = len_trim(output)
            if (l == 0) rc = E_EMPTY
            exit
        end do

        close (unit)
        if (present(nbytes)) nbytes = l
    end function linux_read_string
end module dm_linux
