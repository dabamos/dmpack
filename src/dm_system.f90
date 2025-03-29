! Author:  Philipp Engel
! Licence: ISC
module dm_system
    !! Abstraction layers over system calls.
    use :: unix
    use :: dm_error
    use :: dm_kind
    use :: dm_platform
    implicit none (type, external)
    private

    integer, parameter :: UNAME_LEN = 256

    type, public :: uname_type
        !! Operating system information type.
        character(len=UNAME_LEN) :: system_name = ' ' !! OS name.
        character(len=UNAME_LEN) :: node_name   = ' ' !! Host name.
        character(len=UNAME_LEN) :: release     = ' ' !! OS release.
        character(len=UNAME_LEN) :: version     = ' ' !! OS version.
        character(len=UNAME_LEN) :: machine     = ' ' !! Platform.
    end type uname_type

    public :: dm_system_daemonize
    public :: dm_system_error_message
    public :: dm_system_fork
    public :: dm_system_cpu_cores
    public :: dm_system_cpu_model
    public :: dm_system_cpu_temperature
    public :: dm_system_disk_free
    public :: dm_system_host_name
    public :: dm_system_load_average
    public :: dm_system_path
    public :: dm_system_pid
    public :: dm_system_uname
    public :: dm_system_uptime
    public :: dm_system_wait
contains
    ! **************************************************************************
    ! PUBLIC FUNCTIONS.
    ! **************************************************************************
    integer function dm_system_daemonize(command) result(rc)
        !! Turns current running program into a daemon. On FreeBSD, it is
        !! probably easier to run the process through _daemon(8)_ instead.
        use :: dm_c, only: dm_f_c_string

        character(len=*), intent(in) :: command

        integer(kind=c_pid_t)  :: group, pid
        integer(kind=c_mode_t) :: mode

        rc = E_SYSTEM

        ! Clear file creation mask.
        mode = c_umask(0_c_mode_t)

        ! Spawn a new process and exit.
        pid = c_fork()

        if (pid < 0) then
            ! Fork error.
            return
        else if (pid > 0) then
            ! Parent process.
            call c_exit(EXIT_SUCCESS)
        end if

        ! Child process from here on. Detach from the current terminal session.
        group = c_setsid()

        ! Change working directory to root directory.
        if (c_chdir(dm_f_c_string('/')) < 0) return

        ! Open the log file.
        call c_openlog(dm_f_c_string(command), LOG_CONS, LOG_DAEMON)

        rc = E_NONE
    end function dm_system_daemonize

    function dm_system_error_message(error) result(string)
        !! Returns system error string from _strerror(3)_. If `error` is not
        !! passed, this function uses _errno(2)_ as error code.
        use :: dm_c, only: dm_c_f_string_pointer

        integer, intent(in), optional :: error  !! System error code.
        character(len=:), allocatable :: string !! Error message.

        type(c_ptr) :: ptr

        if (present(error)) then
            ptr = c_strerror(error)
        else
            ptr = c_strerror(c_errno())
        end if

        call dm_c_f_string_pointer(ptr, string)
    end function dm_system_error_message

    integer function dm_system_cpu_cores(ncore) result(rc)
        !! Returns number of CPU cores of first processor on Linux and FreeBSD
        !! in `ncore`. On error, argument `ncore` will be 0.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_EMPTY` if result is empty.
        !! * `E_FORMAT` if output format is unexpected.
        !! * `E_IO` if opening file failed.
        !! * `E_NOT_FOUND` if file or variable does not exist.
        !! * `E_READ` if reading failed.
        !! * `E_SYSTEM` if system call failed.
        !!
        use :: dm_freebsd, only: dm_freebsd_sysctl_cpu_cores
        use :: dm_linux,   only: dm_linux_procfs_cpu_cores

        integer, intent(out) :: ncore !! Number of CPU cores.

        ncore = 0

        select case (PLATFORM_SYSTEM)
            case (PLATFORM_SYSTEM_FREEBSD); rc = dm_freebsd_sysctl_cpu_cores(ncore)
            case (PLATFORM_SYSTEM_LINUX);   rc = dm_linux_procfs_cpu_cores(ncore)
            case default;                   rc = E_INVALID
        end select
    end function dm_system_cpu_cores

    integer function dm_system_cpu_temperature(temperature) result(rc)
        !! Returns CPU temperature in °C of first processor on Linux and FreeBSD
        !! in `temperature`. On error, argument `temperature` is set to 0.0.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_EMPTY` if result is empty.
        !! * `E_FORMAT` if output format is unexpected.
        !! * `E_IO` if opening file failed.
        !! * `E_NOT_FOUND` if file or variable does not exist.
        !! * `E_READ` if reading failed.
        !! * `E_SYSTEM` if system call failed.
        !!
        use :: dm_freebsd, only: dm_freebsd_sysctl_cpu_temperature
        use :: dm_linux,   only: dm_linux_sys_cpu_temperature

        real, intent(out) :: temperature !! Temperature [°C]

        temperature = 0.0

        select case (PLATFORM_SYSTEM)
            case (PLATFORM_SYSTEM_FREEBSD); rc = dm_freebsd_sysctl_cpu_temperature(temperature)
            case (PLATFORM_SYSTEM_LINUX);   rc = dm_linux_sys_cpu_temperature(temperature)
            case default;                   rc = E_INVALID
        end select
    end function dm_system_cpu_temperature

    integer function dm_system_cpu_model(model) result(rc)
        !! Returns model name of first CPU in `model` from `/proc/cpuinfo` on
        !! Linux and from _sysctl(8)_ on FreeBSD, for instance:
        !!
        !! * `Intel(R) Atom(TM) CPU D2550   @ 1.86GHz`
        !! * `Intel(R) Core(TM) i5-7200U CPU @ 2.50GHz`
        !!
        !! Argument `model` must be large enough to hold the name. On error,
        !! argument `model` will be empty.
        !!
        !!The function returns the following error codes:
        !!
        !! * `E_EMPTY` if result is empty.
        !! * `E_FORMAT` if output format is unexpected.
        !! * `E_IO` if opening file failed.
        !! * `E_NOT_FOUND` if file or variable does not exist.
        !! * `E_READ` if reading failed.
        !! * `E_SYSTEM` if system call failed.
        !!
        use :: dm_freebsd, only: dm_freebsd_sysctl_cpu_model
        use :: dm_linux,   only: dm_linux_procfs_cpu_model

        character(len=*), intent(inout) :: model !! Hardware model.

        model = ' '

        select case (PLATFORM_SYSTEM)
            case (PLATFORM_SYSTEM_FREEBSD); rc = dm_freebsd_sysctl_cpu_model(model)
            case (PLATFORM_SYSTEM_LINUX);   rc = dm_linux_procfs_cpu_model(model)
            case default;                   rc = E_INVALID
        end select
    end function dm_system_cpu_model

    integer function dm_system_disk_free(path, file_system, size, used, available, capacity, &
                                         mounted_on) result(rc)
        !! Returns free disk space of file or directory. Argument `path` must
        !! be a file or directory, for example, `/`  or `.`. For security
        !! reasons, `path` must not be a file system or ZFS pool. The function
        !! calls _df(1)_ internally and expects sizes in 512K (FreeBSD) or
        !! 1024K (Linux) blocks.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_FORMAT` if output format is unexpected.
        !! * `E_NOT_FOUND` if path does not exist.
        !! * `E_READ` if pipe returned no bytes.
        !! * `E_SYSTEM` if system call failed.
        !!
        use :: dm_freebsd, only: dm_freebsd_disk_free
        use :: dm_linux,   only: dm_linux_disk_free

        character(len=*), intent(in)              :: path        !! File or directory.
        character(len=*), intent(inout), optional :: file_system !! File system path (device, ZFS pool).
        integer(kind=i8), intent(out),   optional :: size        !! Size [byte].
        integer(kind=i8), intent(out),   optional :: used        !! Used space [byte].
        integer(kind=i8), intent(out),   optional :: available   !! Available space [byte]
        integer,          intent(out),   optional :: capacity    !! Capacity [%]
        character(len=*), intent(inout), optional :: mounted_on  !! Mount point.

        select case (PLATFORM_SYSTEM)
            case (PLATFORM_SYSTEM_FREEBSD)
                rc = dm_freebsd_disk_free(path, file_system, size, used, available, capacity, mounted_on)

            case (PLATFORM_SYSTEM_LINUX)
                rc = dm_linux_disk_free(path, file_system, size, used, available, capacity, mounted_on)

            case default
                rc = E_INVALID
                if (present(file_system)) file_system = ' '
                if (present(size))        size        = 0_i8
                if (present(used))        used        = 0_i8
                if (present(available))   available   = 0_i8
                if (present(capacity))    capacity    = 0
                if (present(mounted_on))  mounted_on  = ' '
        end select
    end function dm_system_disk_free

    integer function dm_system_host_name(name) result(rc)
        !! Returns host name from uname in `name`. The argument must be large
        !! enough to hold the name. On error, argument `name` will be empty.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_SYSTEM` if system call failed.
        !!
        character(len=*), intent(inout) :: name !! Host name.

        type(uname_type) :: uname

        name = ' '
        call dm_system_uname(uname, error=rc)
        if (dm_is_error(rc)) return
        name = uname%node_name
    end function dm_system_host_name

    integer function dm_system_load_average(avg1, avg5, avg15) result(rc)
        !! Returns load averages from _uptime(1)_ (FreeBSD) or `/proc/loadavg`
        !! (Linux). On error, the arguments will be set to 0.0.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_EMPTY` if result is empty.
        !! * `E_FORMAT` if file or output format is unexpected.
        !! * `E_IO` if opening file failed.
        !! * `E_NOT_FOUND` if file or variable does not exist.
        !! * `E_READ` if reading from file or pipe failed.
        !! * `E_SYSTEM` if system call failed.
        !!
        use :: dm_freebsd, only: dm_freebsd_uptime_load_average
        use :: dm_linux,   only: dm_linux_procfs_load_average

        real,    intent(out), optional :: avg1  !! Average, 1 min.
        real,    intent(out), optional :: avg5  !! Average, 5 min.
        real,    intent(out), optional :: avg15 !! Average, 15 min.

        if (present(avg1))  avg1  = 0.0
        if (present(avg5))  avg5  = 0.0
        if (present(avg15)) avg15 = 0.0

        select case (PLATFORM_SYSTEM)
            case (PLATFORM_SYSTEM_FREEBSD); rc = dm_freebsd_uptime_load_average(avg1, avg5, avg15)
            case (PLATFORM_SYSTEM_LINUX);   rc = dm_linux_procfs_load_average(avg1, avg5, avg15)
            case default;                   rc = E_INVALID
        end select
    end function dm_system_load_average

    integer function dm_system_wait(pid) result(rc)
        !! Waits for child process sets PID. Returns `E_SYSTEM` on error.
        integer, intent(out) :: pid !! Process id.
        integer :: stat

        rc = E_SYSTEM
        pid = c_wait(stat)
        if (stat == 0) rc = E_NONE
    end function dm_system_wait

    ! **************************************************************************
    ! PUBLIC SUBROUTINES.
    ! **************************************************************************
    subroutine dm_system_fork(pid)
        !! Forks process and returns PID.
        integer, intent(out) :: pid !! Process id.

        pid = c_fork()
    end subroutine dm_system_fork

    subroutine dm_system_path(path)
        !! Returns the relative path of the executable. The argument must be
        !! large enough to hold the path.
        character(len=*), intent(inout) :: path !! Returned path.

        call get_command_argument(0, path)
    end subroutine dm_system_path

    subroutine dm_system_pid(pid)
        !! Returns the process id (PID).
        integer, intent(out) :: pid !! Process id.

        pid = c_getpid()
    end subroutine dm_system_pid

    subroutine dm_system_uname(uname, error)
        !! Returns uname information (operating system, hostname, …). On error,
        !! argument `error` is set to `E_SYSTEM`.
        use :: dm_c, only: dm_c_f_string_characters

        type(uname_type), intent(out)           :: uname !! Uname type.
        integer,          intent(out), optional :: error !! Error code.

        integer         :: stat
        type(c_utsname) :: utsname

        if (present(error)) error = E_SYSTEM

        stat = c_uname(utsname)
        if (stat /= 0) return

        call dm_c_f_string_characters(utsname%sysname,  uname%system_name)
        call dm_c_f_string_characters(utsname%nodename, uname%node_name)
        call dm_c_f_string_characters(utsname%release,  uname%release)
        call dm_c_f_string_characters(utsname%version,  uname%version)
        call dm_c_f_string_characters(utsname%machine,  uname%machine)

        if (present(error)) error = E_NONE
    end subroutine dm_system_uname

    subroutine dm_system_uptime(time, error)
        !! Returns system uptime in `time` [sec]. On error, argument `error` is
        !! set to `E_SYSTEM`.
        integer(kind=i8), intent(out)           :: time  !! Uptime [sec].
        integer,          intent(out), optional :: error !! Error code.

        integer          :: stat
        type(c_timespec) :: tp

        if (present(error)) error = E_SYSTEM

        stat = c_clock_gettime(CLOCK_MONOTONIC, tp)
        if (stat /= 0) return

        time = int(tp%tv_sec, kind=i8)
        if (time > 60) time = time + 30

        if (present(error)) error = E_NONE
    end subroutine dm_system_uptime
end module dm_system
