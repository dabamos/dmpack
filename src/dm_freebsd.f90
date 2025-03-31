! Author:  Philipp Engel
! Licence: ISC
module dm_freebsd
    !! Abstraction layer over FreeBSD-specific APIs.
    use :: dm_error
    use :: dm_kind
    use :: dm_pipe
    use :: dm_platform
    implicit none (type, external)
    private

    integer, parameter, public :: FREEBSD_VMSTAT_PROCS_R    = 1  !! Threads running.
    integer, parameter, public :: FREEBSD_VMSTAT_PROCS_B    = 2  !! Threads blocked.
    integer, parameter, public :: FREEBSD_VMSTAT_PROCS_W    = 3  !! Threads swapped.
    integer, parameter, public :: FREEBSD_VMSTAT_MEMORY_AVM = 4  !! Virtual memory [byte].
    integer, parameter, public :: FREEBSD_VMSTAT_MEMORY_FRE = 5  !! Free memory [byte].
    integer, parameter, public :: FREEBSD_VMSTAT_PAGE_FLT   = 6  !! Page faults.
    integer, parameter, public :: FREEBSD_VMSTAT_PAGE_RE    = 7  !! Pages reactivated.
    integer, parameter, public :: FREEBSD_VMSTAT_PAGE_PI    = 8  !! Pages paged in.
    integer, parameter, public :: FREEBSD_VMSTAT_PAGE_PO    = 9  !! Pages paged out
    integer, parameter, public :: FREEBSD_VMSTAT_PAGE_FR    = 10 !! Pages freed.
    integer, parameter, public :: FREEBSD_VMSTAT_PAGE_SR    = 11 !! Pages scanned.
    integer, parameter, public :: FREEBSD_VMSTAT_FAULTS_IN  = 12 !! Device interrupts.
    integer, parameter, public :: FREEBSD_VMSTAT_FAULTS_SY  = 13 !! System calls.
    integer, parameter, public :: FREEBSD_VMSTAT_FAULTS_CS  = 14 !! Context switches.
    integer, parameter, public :: FREEBSD_VMSTAT_CPU_US     = 15 !! User time [%].
    integer, parameter, public :: FREEBSD_VMSTAT_CPU_SY     = 16 !! System time [%].
    integer, parameter, public :: FREEBSD_VMSTAT_CPU_ID     = 17 !! Idle time [%].

    integer, parameter, public :: FREEBSD_NVMSTAT = 17 !! Size of vmstat array.

    character(len=*), parameter :: LANG_C = 'LANG=C '

    character(len=*), parameter :: DF_BINARY     = '/bin/df'
    character(len=*), parameter :: SYSCTL_BINARY = '/sbin/sysctl'
    character(len=*), parameter :: UPTIME_BINARY = '/usr/bin/uptime'
    character(len=*), parameter :: VMSTAT_BINARY = '/usr/bin/vmstat'

    character(len=*), parameter :: SYSCTL_ARGUMENTS = '-n'
    character(len=*), parameter :: VMSTAT_ARGUMENTS = '-H dummy 0.1 2'

    character(len=*), parameter :: DF_COMMAND     = LANG_C // DF_BINARY // ' '
    character(len=*), parameter :: SYSCTL_COMMAND = LANG_C // SYSCTL_BINARY // ' ' // SYSCTL_ARGUMENTS // ' '
    character(len=*), parameter :: UPTIME_COMMAND = LANG_C // UPTIME_BINARY
    character(len=*), parameter :: VMSTAT_COMMAND = LANG_C // VMSTAT_BINARY // ' ' // VMSTAT_ARGUMENTS // ' '

    interface freebsd_sysctl
        !! Returns value from _sysctl(8)_.
        module procedure :: freebsd_sysctl_int32
        module procedure :: freebsd_sysctl_int64
        module procedure :: freebsd_sysctl_real32
        module procedure :: freebsd_sysctl_real64
        module procedure :: freebsd_sysctl_string
    end interface freebsd_sysctl

    public :: dm_freebsd_disk_free
    public :: dm_freebsd_sysctl_battery_life
    public :: dm_freebsd_sysctl_cpu_cores
    public :: dm_freebsd_sysctl_cpu_model
    public :: dm_freebsd_sysctl_cpu_temperature
    public :: dm_freebsd_sysctl_memory
    public :: dm_freebsd_sysctl_mqueue
    public :: dm_freebsd_uptime_load_average
    public :: dm_freebsd_vmstat
    public :: dm_freebsd_vmstat_cpu_idle

    private :: freebsd_sysctl
    private :: freebsd_sysctl_int32
    private :: freebsd_sysctl_int64
    private :: freebsd_sysctl_real32
    private :: freebsd_sysctl_real64
    private :: freebsd_sysctl_string
contains
    ! **************************************************************************
    ! PUBLIC FUNCTIONS.
    ! **************************************************************************
    integer function dm_freebsd_disk_free(path, file_system, size, used, available, capacity, mounted_on) result(rc)
        !! Returns free disk space of file or directory. Argument `path` must
        !! be a file or directory, for example, `/`  or `.`. For security
        !! reasons, `path` must not be a file system or ZFS pool. The function
        !! calls _df(1)_ internally and expects sizes in 512K blocks.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_FORMAT` if output format is unexpected.
        !! * `E_INVALID` if path is invalid or not readable.
        !! * `E_NOT_FOUND` if path does not exist.
        !! * `E_PLATFORM` if current system is not FreeBSD.
        !! * `E_READ` if pipe returned no bytes.
        !! * `E_SYSTEM` if system call failed.
        !!
        use :: dm_ascii, only: ASCII_LF
        use :: dm_file,  only: dm_file_exists

        integer, parameter :: BLOCK_SIZE = 512

        character(len=*), intent(in)              :: path        !! File or directory.
        character(len=*), intent(inout), optional :: file_system !! File system path (device, ZFS pool).
        integer(kind=i8), intent(out),   optional :: size        !! Size [byte].
        integer(kind=i8), intent(out),   optional :: used        !! Used space [byte].
        integer(kind=i8), intent(out),   optional :: available   !! Available space [byte]
        integer,          intent(out),   optional :: capacity    !! Capacity [%]
        character(len=*), intent(inout), optional :: mounted_on  !! Mount point.

        integer(kind=i8) :: values(4)

        values(:) = 0.0
        if (present(file_system)) file_system = ' '
        if (present(mounted_on))  mounted_on  = ' '

        io_block: block
            character(len=2048) :: line, output
            integer             :: i, j, stat

            rc = E_PLATFORM
            if (PLATFORM_SYSTEM /= PLATFORM_SYSTEM_FREEBSD) exit io_block

            rc = E_INVALID
            if (len_trim(path) == 0) exit io_block

            rc = E_NOT_FOUND
            if (.not. dm_file_exists(path)) exit io_block

            rc = dm_pipe_execute(DF_COMMAND // path, output)
            if (dm_is_error(rc)) exit io_block

            rc = E_FORMAT
            ! Start of second line.
            i = index(output, ASCII_LF)
            if (i == 0 .or. i == len(output)) exit io_block
            line = output(i + 1:)

            i = index(line, ' ') ! End of file system path.
            if (i <= 1 .or. i == len(output)) exit io_block

            j = index(line, '%') ! End of values.
            if (j <= 1 .or. j == len(output)) exit io_block

            ! Sizes and capacity.
            read (line(i + 1:j - 1), *, iostat=stat) values
            if (stat /= 0) exit io_block

            if (present(file_system)) file_system = line(:i - 1)
            if (present(mounted_on))  mounted_on  = adjustl(line(j + 1:))
            rc = E_NONE
        end block io_block

        if (present(size))      size      = values(1) * BLOCK_SIZE
        if (present(used))      used      = values(2) * BLOCK_SIZE
        if (present(available)) available = values(3) * BLOCK_SIZE
        if (present(capacity))  capacity  = int(values(4))
    end function dm_freebsd_disk_free

    integer function dm_freebsd_sysctl_battery_life(life) result(rc)
        !! Returns the current battery life [%] from _sysctl(8)_.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_FORMAT` if output format is unexpected.
        !! * `E_PLATFORM` if current system is not FreeBSD.
        !! * `E_READ` if pipe returned no bytes.
        !! * `E_SYSTEM` if system call failed.
        !!
        integer, intent(out) :: life !! Battery life [%].

        rc = freebsd_sysctl('hw.acpi.battery.life', life)
    end function dm_freebsd_sysctl_battery_life

    integer function dm_freebsd_sysctl_cpu_cores(ncore) result(rc)
        !! Returns number of CPU cores from _sysctl(8)_ (`hw.ncpu`).
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_FORMAT` if output format is unexpected.
        !! * `E_PLATFORM` if current system is not FreeBSD.
        !! * `E_READ` if pipe returned no bytes.
        !! * `E_SYSTEM` if system call failed.
        !!
        integer, intent(out) :: ncore !! Number of CPUs.

        rc = freebsd_sysctl('hw.ncpu', ncore)
    end function dm_freebsd_sysctl_cpu_cores

    integer function dm_freebsd_sysctl_cpu_model(model) result(rc)
        !! Returns model name of first CPU from _sysctl(8)_ (`hw.model`).
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_FORMAT` if output format is unexpected.
        !! * `E_PLATFORM` if current system is not FreeBSD.
        !! * `E_READ` if pipe returned no bytes.
        !! * `E_SYSTEM` if system call failed.
        !!
        character(len=*), intent(inout) :: model !! Hardware model.

        rc = freebsd_sysctl('hw.model', model)
    end function dm_freebsd_sysctl_cpu_model

    integer function dm_freebsd_sysctl_cpu_temperature(temperature) result(rc)
        !! Reads temperature of first CPU in °C from _sysctl(8)_
        !! (`dev.cpu.0.temperature`).
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_FORMAT` if output format is unexpected.
        !! * `E_PLATFORM` if current system is not FreeBSD.
        !! * `E_READ` if pipe returned no bytes.
        !! * `E_SYSTEM` if system call failed.
        !!
        real, intent(out) :: temperature !! Temperature [°C]

        character(len=8) :: output
        integer          :: n, stat

        temperature = 0.0
        rc = freebsd_sysctl('dev.cpu.0.temperature', output)
        if (dm_is_error(rc)) return

        ! Remove the the unit character from sysctl output first:
        ! $ LANG=C sysctl -n dev.cpu.0.temperature
        ! 45.0C
        rc = E_FORMAT
        n = len_trim(output) - 1
        if (n < 1) return
        read (output(1:n), *, iostat=stat) temperature
        if (stat == 0) rc = E_NONE
    end function dm_freebsd_sysctl_cpu_temperature

    integer function dm_freebsd_sysctl_memory(phys_mem, real_mem, user_mem) result(rc)
        !! Returns physical, real, and user memory [byte] from _sysctl(8)_.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_FORMAT` if output format is unexpected.
        !! * `E_PLATFORM` if current system is not FreeBSD.
        !! * `E_READ` if pipe returned no bytes.
        !! * `E_SYSTEM` if system call failed.
        !!
        integer(kind=i8), intent(out), optional :: phys_mem !! Physical memory [byte].
        integer(kind=i8), intent(out), optional :: real_mem !! Real memory [byte].
        integer(kind=i8), intent(out), optional :: user_mem !! User memory [byte].

        character(len=256) :: output
        integer            :: stat
        integer(kind=i8)   :: values(3)

        values(:) = 0_i8

        io_block: block
            rc = freebsd_sysctl('hw.physmem hw.realmem hw.usermem', output)
            if (dm_is_error(rc)) exit io_block
            read (output, *, iostat=stat) values
            if (stat /= 0) rc = E_FORMAT
        end block io_block

        if (present(phys_mem)) phys_mem = values(1)
        if (present(real_mem)) real_mem = values(2)
        if (present(user_mem)) user_mem = values(3)
    end function dm_freebsd_sysctl_memory

    integer function dm_freebsd_sysctl_mqueue(max_mqueues, max_messages, max_message_size) result(rc)
        !! Returns POSIX message queue variables from _sysctl(8)_.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_FORMAT` if output format is unexpected.
        !! * `E_PLATFORM` if current system is not FreeBSD.
        !! * `E_READ` if pipe returned no bytes.
        !! * `E_SYSTEM` if system call failed.
        !!
        integer, intent(out), optional :: max_mqueues      !! Max. number of mqueues.
        integer, intent(out), optional :: max_messages     !! Max. number of messages.
        integer, intent(out), optional :: max_message_size !! Max. message size [byte].

        character(len=256) :: output
        integer            :: stat, values(3)

        values(:) = 0

        io_block: block
            rc = freebsd_sysctl('kern.mqueue.maxmq kern.mqueue.maxmsg kern.mqueue.maxmsgsize', output)
            if (dm_is_error(rc)) exit io_block
            read (output, *, iostat=stat) values
            if (stat /= 0) rc = E_FORMAT
        end block io_block

        if (present(max_mqueues))      max_mqueues      = values(1)
        if (present(max_messages))     max_messages     = values(2)
        if (present(max_message_size)) max_message_size = values(3)
    end function dm_freebsd_sysctl_mqueue

    integer function dm_freebsd_uptime_load_average(avg1, avg5, avg15) result(rc)
        !! Returns load averages from _uptime(1)_.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_FORMAT` if output format is unexpected.
        !! * `E_PLATFORM` if current system is not FreeBSD.
        !! * `E_READ` if pipe returned no bytes.
        !! * `E_SYSTEM` if system call failed.
        !!
        real, intent(out), optional :: avg1  !! Average, 1 min.
        real, intent(out), optional :: avg5  !! Average, 5 min.
        real, intent(out), optional :: avg15 !! Average, 15 min.

        real :: values(3)

        values(:) = 0.0

        io_block: block
            character(len=128) :: output
            integer            :: i, stat

            rc = E_PLATFORM
            if (PLATFORM_SYSTEM /= PLATFORM_SYSTEM_FREEBSD) exit io_block

            rc = dm_pipe_execute(UPTIME_COMMAND, output)
            if (dm_is_error(rc)) exit io_block

            rc = E_FORMAT
            i = index(output, ':', back=.true.)
            if (i == 0 .or. i == len(output)) exit io_block
            read (output(i + 1:), *, iostat=stat) values
            if (stat == 0) rc = E_NONE
        end block io_block

        if (present(avg1))  avg1  = values(1)
        if (present(avg5))  avg5  = values(2)
        if (present(avg15)) avg15 = values(3)
    end function dm_freebsd_uptime_load_average

    integer function dm_freebsd_vmstat(vmstat) result(rc)
        !! Returns output of _vmstat(8)_. The procedure runs for > 0.1 seconds!
        !! Executes the following command and reads the last output values:
        !!
        !! ```
        !! $ vmstat -H dummy 0.1 2
        !!  procs    memory    page                    faults       cpu
        !!  r  b  w  avm  fre  flt  re  pi  po   fr   sr   in   sy   cs us sy id
        !!  1  0  0 607119396864 5717385216 2116   0   1   0 3283  279 1396 9058 9120  4  2 92
        !!  0  0  0 607119396864 5717381120   30   0   0   0    0  300  120 1650 4000  0  3 96
        !! ```
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_FORMAT` if output format is unexpected.
        !! * `E_PLATFORM` if current system is not FreeBSD.
        !! * `E_READ` if reading failed or pipe returned no bytes.
        !! * `E_SYSTEM` if system call failed.
        !!
        use :: dm_ascii, only: ASCII_LF

        integer(kind=i8), intent(out) :: vmstat(17) !! Values.

        character(len=1024) :: output
        integer             :: i, stat

        vmstat(:) = 0_i8

        rc = E_PLATFORM
        if (PLATFORM_SYSTEM /= PLATFORM_SYSTEM_FREEBSD) return

        rc = dm_pipe_execute(VMSTAT_COMMAND, output)
        if (dm_is_error(rc)) return

        rc = E_FORMAT
        i = index(output, ASCII_LF, back=.true.)
        if (i == 0 .or. i == len(output)) return

        read (output(i + 1:), *, iostat=stat) vmstat
        if (stat == 0) rc = E_NONE
    end function dm_freebsd_vmstat

    integer function dm_freebsd_vmstat_cpu_idle(idle) result(rc)
        !! Returns CPU idle time from _vmstat(8)_. The procedure runs for > 0.1 sec!
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_FORMAT` if output format is unexpected.
        !! * `E_PLATFORM` if current system is not FreeBSD.
        !! * `E_READ` if reading failed or pipe returned no bytes.
        !! * `E_SYSTEM` if system call failed.
        !!
        integer, intent(out) :: idle !! Idle time [%].

        integer(kind=i8) :: vmstat(17)

        rc = dm_freebsd_vmstat(vmstat)
        idle = int(vmstat(FREEBSD_VMSTAT_CPU_ID))
    end function dm_freebsd_vmstat_cpu_idle

    ! **************************************************************************
    ! PRIVATE FUNCTIONS.
    ! **************************************************************************
    integer function freebsd_sysctl_int32(name, value) result(rc)
        !! Reads 8-byte integer output from _sysctl(8)_.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_PLATFORM` if system is not FreeBSD.
        !! * `E_READ` if pipe returned no bytes.
        !! * `E_SYSTEM` if system call failed.
        !!
        character(len=*), intent(in)  :: name  !! Variable name.
        integer(kind=i4), intent(out) :: value !! Variable value.

        character(len=64) :: output
        integer           :: stat

        value = 0_i4
        rc = freebsd_sysctl_string(name, output); if (dm_is_error(rc)) return
        read (output, *, iostat=stat) value;      if (stat /= 0) rc = E_FORMAT
    end function freebsd_sysctl_int32

    integer function freebsd_sysctl_int64(name, value) result(rc)
        !! Reads 8-byte integer output from _sysctl(8)_.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_PLATFORM` if system is not FreeBSD.
        !! * `E_READ` if pipe returned no bytes.
        !! * `E_SYSTEM` if system call failed.
        !!
        character(len=*), intent(in)  :: name  !! Variable name.
        integer(kind=i8), intent(out) :: value !! Variable value.

        character(len=64) :: output
        integer           :: stat

        value = 0_i8
        rc = freebsd_sysctl_string(name, output); if (dm_is_error(rc)) return
        read (output, *, iostat=stat) value;      if (stat /= 0) rc = E_FORMAT
    end function freebsd_sysctl_int64

    integer function freebsd_sysctl_real32(name, value) result(rc)
        !! Reads 4-byte real output from _sysctl(8)_.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_PLATFORM` if system is not FreeBSD.
        !! * `E_READ` if pipe returned no bytes.
        !! * `E_SYSTEM` if system call failed.
        !!
        character(len=*), intent(in)  :: name  !! Variable name.
        real(kind=r4),    intent(out) :: value !! Variable value.

        character(len=64) :: output
        integer           :: stat

        value = 0.0_r4
        rc = freebsd_sysctl_string(name, output); if (dm_is_error(rc)) return
        read (output, *, iostat=stat) value;      if (stat /= 0) rc = E_FORMAT
    end function freebsd_sysctl_real32

    integer function freebsd_sysctl_real64(name, value) result(rc)
        !! Reads 8-byte real output from _sysctl(8)_.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_PLATFORM` if system is not FreeBSD.
        !! * `E_READ` if pipe returned no bytes.
        !! * `E_SYSTEM` if system call failed.
        !!
        character(len=*), intent(in)  :: name  !! Variable name.
        real(kind=r8),    intent(out) :: value !! Variable value.

        character(len=64) :: output
        integer           :: stat

        value = 0.0_r8
        rc = freebsd_sysctl_string(name, output); if (dm_is_error(rc)) return
        read (output, *, iostat=stat) value;      if (stat /= 0) rc = E_FORMAT
    end function freebsd_sysctl_real64

    integer function freebsd_sysctl_string(name, value, nbyte) result(rc)
        !! Reads output from _sysctl(8)_.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_EXIST` if pipe is already connected.
        !! * `E_INVALID` if access mode is invalid.
        !! * `E_PLATFORM` if system is not FreeBSD.
        !! * `E_READ` if pipe returned no bytes.
        !! * `E_SYSTEM` if system call failed.
        !!
        character(len=*), intent(in)            :: name  !! Variable name.
        character(len=*), intent(inout)         :: value !! Variable value.
        integer(kind=i8), intent(out), optional :: nbyte !! String length.

        rc = E_PLATFORM
        if (PLATFORM_SYSTEM /= PLATFORM_SYSTEM_FREEBSD) return
        rc = dm_pipe_execute(SYSCTL_COMMAND // name, value, nbyte)
    end function freebsd_sysctl_string
end module dm_freebsd
