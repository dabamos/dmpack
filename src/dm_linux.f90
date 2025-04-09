! Author:  Philipp Engel
! Licence: ISC
module dm_linux
    !! Abstraction layer over Linux-specific APIs.
    use :: dm_error
    use :: dm_kind
    use :: dm_pipe
    use :: dm_platform
    implicit none (type, external)
    private

    character(len=*), parameter :: LANG_C     = 'LANG=C'
    character(len=*), parameter :: DF_BINARY  = '/usr/bin/df'
    character(len=*), parameter :: DF_COMMAND = LANG_C // ' ' // DF_BINARY // ' '

    character(len=*), parameter :: PROC_CPUINFO = '/proc/cpuinfo'
    character(len=*), parameter :: PROC_LOADAVG = '/proc/loadavg'
    character(len=*), parameter :: PROC_STAT    = '/proc/stat'

    interface linux_pipe
        !! Reads value from pipe.
        module procedure :: linux_pipe_int32
        module procedure :: linux_pipe_int64
        module procedure :: linux_pipe_real32
        module procedure :: linux_pipe_real64
    end interface linux_pipe

    interface linux_read
        !! Reads value from file system.
        module procedure :: linux_read_int32
        module procedure :: linux_read_int64
        module procedure :: linux_read_real32
        module procedure :: linux_read_real64
        module procedure :: linux_read_string
    end interface linux_read

    ! Public procedures.
    public :: dm_linux_disk_free
    public :: dm_linux_procfs_cpu_cores
    public :: dm_linux_procfs_cpu_idle
    public :: dm_linux_procfs_cpu_model
    public :: dm_linux_procfs_load_average
    public :: dm_linux_sys_cpu_temperature

    ! Private procedures.
    private :: linux_pipe
    private :: linux_pipe_int32
    private :: linux_pipe_int64
    private :: linux_pipe_real32
    private :: linux_pipe_real64

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
    integer function dm_linux_disk_free(path, file_system, size, used, available, capacity, mounted_on) result(rc)
        !! Returns free disk space of file or directory. Argument `path` must
        !! be a file or directory, for example, `/`  or `.`. The function
        !! calls _df(1)_ internally and expects sizes in 1024K blocks.
        !!
        !! Output of _df(1)_:
        !!
        !! ```
        !! $ df .
        !! Filesystem                1K-blocks     Used Available Use% Mounted on
        !! /dev/mapper/vgubuntu-root 486903968 66753060 395344100  15% /
        !! ```
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_FORMAT` if output format is unexpected.
        !! * `E_INVALID` if path is invalid or not readable.
        !! * `E_PLATFORM` if current system is not Linux.
        !! * `E_NOT_FOUND` if path does not exist.
        !! * `E_READ` if pipe returned no bytes.
        !! * `E_SYSTEM` if system call failed.
        !!
        use :: dm_ascii, only: ASCII_LF
        use :: dm_file,  only: dm_file_exists

        integer, parameter :: BLOCK_SIZE = 1024

        character(len=*), intent(in)              :: path        !! File or directory.
        character(len=*), intent(inout), optional :: file_system !! File system path (device, ZFS pool).
        integer(kind=i8), intent(out),   optional :: size        !! Size [byte].
        integer(kind=i8), intent(out),   optional :: used        !! Used space [byte].
        integer(kind=i8), intent(out),   optional :: available   !! Available space [byte]
        integer,          intent(out),   optional :: capacity    !! Capacity [%]
        character(len=*), intent(inout), optional :: mounted_on  !! Mount point.

        integer(kind=i8) :: values(4)
        type(pipe_type)  :: pipe

        values(:) = 0.0

        if (present(file_system)) file_system = ' '
        if (present(mounted_on))  mounted_on  = ' '

        io_block: block
            character(len=1024) :: output
            integer             :: i, j, stat

            rc = E_PLATFORM
            if (PLATFORM_SYSTEM /= PLATFORM_SYSTEM_LINUX) exit io_block

            rc = E_INVALID
            if (len_trim(path) == 0) exit io_block

            rc = E_NOT_FOUND
            if (.not. dm_file_exists(path)) exit io_block

            rc = dm_pipe_open(pipe, DF_COMMAND // path, PIPE_RDONLY)
            if (dm_is_error(rc)) exit io_block

            ! Read and discard first line.
            rc = dm_pipe_read_line(pipe, output)
            if (dm_is_error(rc)) exit io_block

            ! Read second line.
            rc = dm_pipe_read_line(pipe, output)
            if (dm_is_error(rc)) exit io_block

            rc = E_FORMAT
            i = index(output, ' ') ! End of file system path.
            if (i <= 1 .or. i == len(output)) exit io_block

            j = index(output, '%') ! End of values.
            if (j <= 1 .or. j == len(output)) exit io_block

            ! Sizes and capacity.
            read (output(i + 1:j - 1), *, iostat=stat) values
            if (stat /= 0) exit io_block

            if (present(file_system)) file_system = output(:i - 1)
            if (present(mounted_on))  mounted_on  = adjustl(output(j + 1:))
            rc = E_NONE
        end block io_block

        call dm_pipe_close(pipe)

        if (present(size))      size      = values(1) * BLOCK_SIZE
        if (present(used))      used      = values(2) * BLOCK_SIZE
        if (present(available)) available = values(3) * BLOCK_SIZE
        if (present(capacity))  capacity  = int(values(4))
    end function dm_linux_disk_free

    integer function dm_linux_procfs_cpu_cores(ncore) result(rc)
        !! Returns number of CPU cores from `/proc/cpuinfo` (`cpu cores` of
        !! processor 0).
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_FORMAT` if output format is unexpected.
        !! * `E_PLATFORM` if current system is not Linux.
        !! * `E_READ` if pipe returned no bytes.
        !! * `E_SYSTEM` if system call failed.
        !!
        integer, intent(out) :: ncore !! Number of CPU cores.

        ncore = 0
        rc = linux_read(PROC_CPUINFO, ncore, name='cpu cores', delimiter=':')
    end function dm_linux_procfs_cpu_cores

    integer function dm_linux_procfs_cpu_idle(idle) result(rc)
        !! Returns CPU idle time of from `/proc/stat` (`cpu`):
        !!
        !! ```
        !! $ cat /proc/stat
        !! cpu  569115 4513 94734 1378694599 139966 0 1666 0 0 0
        !! cpu0 134298 1652 22827 344794259 38716 0 613 0 0 0
        !! cpu1 129032 847 25983 344677697 19973 0 35 0 0 0
        !! cpu2 148695 1615 23690 344454047 44426 0 284 0 0 0
        !! cpu3 157088 398 22234 344768594 36849 0 733 0 0 0
        !! ```
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_FORMAT` if output format is unexpected.
        !! * `E_PLATFORM` if current system is not Linux.
        !! * `E_READ` if pipe returned no bytes.
        !!
        integer, intent(out) :: idle !! CPU idle time [%].

        character(len=128) :: output
        integer            :: stat, values(10)
        real               :: r

        idle = 0
        rc = linux_read(PROC_STAT, output, name='cpu', delimiter=' '); if (dm_is_error(rc)) return
        read (output, *, iostat=stat) values;                          if (stat /= 0) rc = E_FORMAT
        r = (real(values(4)) * 100) / sum(values(1:7))
        idle = nint(r)
    end function dm_linux_procfs_cpu_idle

    integer function dm_linux_procfs_cpu_model(model) result(rc)
        !! Returns model name of first CPU from `/proc/cpuinfo` (`model name` of
        !! processor 0), for instance:
        !!
        !! * `12th Gen Intel(R) Core(TM) i3-1215U`
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_EMPTY` if result is empty.
        !! * `E_FORMAT` if file format is invalid.
        !! * `E_IO` if opening file failed.
        !! * `E_PLATFORM` if current system is not Linux.
        !! * `E_READ` if reading from file system failed.
        !! * `E_NOT_FOUND` if file or variable does not exist.
        !!
        character(len=*), intent(inout) :: model !! Hardware model.

        rc = linux_read(PROC_CPUINFO, model, name='model name', delimiter=':')
    end function dm_linux_procfs_cpu_model

    integer function dm_linux_procfs_load_average(avg1, avg5, avg15) result(rc)
        !! Returns load averages from `/proc/loadavg`.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_EMPTY` if result is empty.
        !! * `E_FORMAT` if file format is invalid.
        !! * `E_IO` if opening file failed.
        !! * `E_PLATFORM` if current system is not Linux.
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

        values(:) = 0.0

        io_block: block
            rc = linux_read(PROC_LOADAVG, output);        if (dm_is_error(rc)) exit io_block
            read (output, *, iostat=stat) values, ignore; if (stat /= 0) rc = E_FORMAT
        end block io_block

        if (present(avg1))  avg1  = values(1)
        if (present(avg5))  avg5  = values(2)
        if (present(avg15)) avg15 = values(3)
    end function dm_linux_procfs_load_average

    integer function dm_linux_sys_cpu_temperature(temperature) result(rc)
        !! Reads temperature of first CPU in °C from
        !! `/sys/class/thermal/thermal_zone0/temp`. The CPU thermal zone may
        !! have to be enabled in BIOS/UEFI.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_IO` if command failed.
        !! * `E_PLATFORM` if current system is not Linux.
        !! * `E_READ` if pipe returned no bytes.
        !! * `E_SYSTEM` if system call failed.
        !!
        real, intent(out) :: temperature !! Temperature [°C]

        integer :: output

        temperature = 0.0
        rc = linux_read('/sys/class/thermal/thermal_zone0/temp', output)
        temperature = real(output) / 1000
    end function dm_linux_sys_cpu_temperature

    ! **************************************************************************
    ! PRIVATE FUNCTIONS.
    ! **************************************************************************
    integer function linux_pipe_int32(command, value) result(rc)
        !! Reads 8-byte integer output from pipe.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_IO` if command failed.
        !! * `E_PLATFORM` if current system is not Linux.
        !! * `E_READ` if pipe returned no bytes.
        !! * `E_SYSTEM` if system call failed.
        !!
        character(len=*), intent(in)  :: command !! Command.
        integer(kind=i4), intent(out) :: value   !! Output value.

        character(len=64) :: output
        integer           :: stat

        value = 0_i4

        rc = E_PLATFORM
        if (PLATFORM_SYSTEM /= PLATFORM_SYSTEM_LINUX) return

        rc = dm_pipe_execute(command, output); if (dm_is_error(rc)) return
        read (output, *, iostat=stat) value;   if (stat /= 0) rc = E_FORMAT
    end function linux_pipe_int32

    integer function linux_pipe_int64(command, value) result(rc)
        !! Reads 8-byte integer output from pipe.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_IO` if command failed.
        !! * `E_PLATFORM` if current system is not Linux.
        !! * `E_READ` if pipe returned no bytes.
        !! * `E_SYSTEM` if system call failed.
        !!
        character(len=*), intent(in)  :: command !! Command.
        integer(kind=i8), intent(out) :: value   !! Output value.

        character(len=64) :: output
        integer           :: stat

        value = 0_i8

        rc = E_PLATFORM
        if (PLATFORM_SYSTEM /= PLATFORM_SYSTEM_LINUX) return

        rc = dm_pipe_execute(command, output); if (dm_is_error(rc)) return
        read (output, *, iostat=stat) value;   if (stat /= 0) rc = E_FORMAT
    end function linux_pipe_int64

    integer function linux_pipe_real32(command, value) result(rc)
        !! Reads 4-byte real output from pipe.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_IO` if command failed.
        !! * `E_PLATFORM` if current system is not Linux.
        !! * `E_READ` if pipe returned no bytes.
        !! * `E_SYSTEM` if system call failed.
        !!
        character(len=*), intent(in)  :: command !! Command.
        real(kind=r4),    intent(out) :: value   !! Output value.

        character(len=64) :: output
        integer           :: stat

        value = 0.0_r4

        rc = E_PLATFORM
        if (PLATFORM_SYSTEM /= PLATFORM_SYSTEM_LINUX) return

        rc = dm_pipe_execute(command, output); if (dm_is_error(rc)) return
        read (output, *, iostat=stat) value;   if (stat /= 0) rc = E_FORMAT
    end function linux_pipe_real32

    integer function linux_pipe_real64(command, value) result(rc)
        !! Reads 8-byte real output from pipe.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_IO` if command failed.
        !! * `E_PLATFORM` if current system is not Linux.
        !! * `E_READ` if pipe returned no bytes.
        !! * `E_SYSTEM` if system call failed.
        !!
        character(len=*), intent(in)  :: command !! Command.
        real(kind=r8),    intent(out) :: value   !! Output value.

        character(len=64) :: output
        integer           :: stat

        value = 0.0_r8

        rc = E_PLATFORM
        if (PLATFORM_SYSTEM /= PLATFORM_SYSTEM_LINUX) return

        rc = dm_pipe_execute(command, output); if (dm_is_error(rc)) return
        read (output, *, iostat=stat) value;   if (stat /= 0) rc = E_FORMAT
    end function linux_pipe_real64

    integer function linux_read_int32(path, value, name, delimiter) result(rc)
        !! Reads 8-byte integer value from file system.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_EMPTY` if value is empty.
        !! * `E_FORMAT` if file format is invalid.
        !! * `E_INVALID` if name is invalid.
        !! * `E_IO` if opening file failed.
        !! * `E_PLATFORM` if current system is not Linux.
        !! * `E_READ` if reading from file system failed.
        !! * `E_NOT_FOUND` if file or variable does not exist.
        !!
        character(len=*), intent(in)           :: path      !! Path.
        integer(kind=i4), intent(out)          :: value     !! Output value.
        character(len=*), intent(in), optional :: name      !! Variable name.
        character,        intent(in), optional :: delimiter !! Key-value delimiter.

        character(len=64) :: output
        integer           :: stat

        value = 0_i4
        rc = linux_read_string(path, output, name, delimiter); if (dm_is_error(rc)) return
        read (output, *, iostat=stat) value;                   if (stat /= 0) rc = E_FORMAT
    end function linux_read_int32

    integer function linux_read_int64(path, value, name, delimiter) result(rc)
        !! Reads 8-byte integer value from file system.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_EMPTY` if value is empty.
        !! * `E_FORMAT` if file format is invalid.
        !! * `E_INVALID` if name is invalid.
        !! * `E_IO` if opening file failed.
        !! * `E_PLATFORM` if current system is not Linux.
        !! * `E_READ` if reading from file system failed.
        !! * `E_NOT_FOUND` if file or variable does not exist.
        !!
        character(len=*), intent(in)           :: path      !! Path.
        integer(kind=i8), intent(out)          :: value     !! Output value.
        character(len=*), intent(in), optional :: name      !! Variable name.
        character,        intent(in), optional :: delimiter !! Key-value delimiter.

        character(len=64) :: output
        integer           :: stat

        value = 0_i8
        rc = linux_read_string(path, output, name, delimiter); if (dm_is_error(rc)) return
        read (output, *, iostat=stat) value;                   if (stat /= 0) rc = E_FORMAT
    end function linux_read_int64

    integer function linux_read_real32(path, value, name, delimiter) result(rc)
        !! Reads 4-byte real value from file system.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_EMPTY` if value is empty.
        !! * `E_FORMAT` if file format is invalid.
        !! * `E_INVALID` if name is invalid.
        !! * `E_IO` if opening file failed.
        !! * `E_PLATFORM` if current system is not Linux.
        !! * `E_READ` if reading from file system failed.
        !! * `E_NOT_FOUND` if file or variable does not exist.
        !!
        character(len=*), intent(in)           :: path      !! Path.
        real(kind=r4),    intent(out)          :: value     !! Output value.
        character(len=*), intent(in), optional :: name      !! Variable name.
        character,        intent(in), optional :: delimiter !! Key-value delimiter.

        character(len=64) :: output
        integer           :: stat

        value = 0.0_r4
        rc = linux_read_string(path, output, name, delimiter); if (dm_is_error(rc)) return
        read (output, *, iostat=stat) value;                   if (stat /= 0) rc = E_FORMAT
    end function linux_read_real32

    integer function linux_read_real64(path, value, name, delimiter) result(rc)
        !! Reads 8-byte real value from file system.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_EMPTY` if value is empty.
        !! * `E_FORMAT` if file format is invalid.
        !! * `E_INVALID` if name is invalid.
        !! * `E_IO` if opening file failed.
        !! * `E_PLATFORM` if current system is not Linux.
        !! * `E_READ` if reading from file system failed.
        !! * `E_NOT_FOUND` if file or variable does not exist.
        !!
        character(len=*), intent(in)           :: path      !! Path.
        real(kind=r8),    intent(out)          :: value     !! Output value.
        character(len=*), intent(in), optional :: name      !! Variable name.
        character,        intent(in), optional :: delimiter !! Key-value delimiter.

        character(len=64) :: output
        integer           :: stat

        value = 0.0_r8
        rc = linux_read_string(path, output, name, delimiter); if (dm_is_error(rc)) return
        read (output, *, iostat=stat) value;                   if (stat /= 0) rc = E_FORMAT
    end function linux_read_real64

    integer function linux_read_string(path, value, name, delimiter, nbyte) result(rc)
        !! Reads string value from file system.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_EMPTY` if value is empty.
        !! * `E_FORMAT` if file format is invalid.
        !! * `E_IO` if opening file failed.
        !! * `E_PLATFORM` if current system is not Linux.
        !! * `E_READ` if reading from file system failed.
        !! * `E_NOT_FOUND` if file or variable does not exist.
        !!
        use :: dm_file, only: dm_file_exists
        use :: dm_util, only: dm_present

        character(len=*), intent(in)            :: path      !! Path.
        character(len=*), intent(inout)         :: value     !! Output string.
        character(len=*), intent(in),  optional :: name      !! Variable name.
        character,        intent(in),  optional :: delimiter !! Key-value delimiter.
        integer,          intent(out), optional :: nbyte     !! String length.

        character           :: s
        character(len=2048) :: line, output
        integer             :: i, l, n, stat, unit

        value = ' '

        s = dm_present(delimiter, ' ')
        if (present(nbyte)) nbyte = 0

        rc = E_PLATFORM
        if (PLATFORM_SYSTEM /= PLATFORM_SYSTEM_LINUX) return

        rc = E_INVALID
        if (present(name)) then
            if (len_trim(name) == 0) return
        end if

        rc = E_NOT_FOUND
        if (.not. dm_file_exists(path)) return

        rc = E_IO
        open (action='read', file=trim(path), iostat=stat, newunit=unit, status='old')
        if (stat /= 0) return

        l = 0
        n = min(len(line), len_trim(name))
        output = ' '

        rc = E_READ
        do
            read (unit, '(a)', iostat=stat) line

            if (is_iostat_end(stat)) exit

            if (stat /= 0) then
                rc = E_READ
                exit
            end if

            if (present(name)) then
                rc = E_NOT_FOUND
                if (line(1:n) /= name(1:n)) cycle

                rc = E_FORMAT
                i = index(line, s)
                if (i == 0 .or. i == len(line) .or. i <= n) cycle

                output = adjustl(line(i + 1:))
            else
                output = adjustl(line)
            end if

            rc = E_EMPTY
            l = len_trim(output)

            if (l > 0) then
                rc = E_NONE
                value = output
            end if

            exit
        end do

        close (unit)
        if (present(nbyte)) nbyte = l
    end function linux_read_string
end module dm_linux
