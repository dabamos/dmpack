! Author:  Philipp Engel
! Licence: ISC
module dm_system
    !! Abstraction layers over system calls.
    use, intrinsic :: iso_c_binding
    use :: unix
    use :: dm_error
    use :: dm_kind
    implicit none (type, external)
    private

    integer, parameter :: UNAME_LEN = 256

    type, public :: uname_type
        !! Operating system information.
        character(len=UNAME_LEN) :: system_name = ' ' !! OS name.
        character(len=UNAME_LEN) :: node_name   = ' ' !! Host name.
        character(len=UNAME_LEN) :: release     = ' ' !! OS release.
        character(len=UNAME_LEN) :: version     = ' ' !! OS version.
        character(len=UNAME_LEN) :: machine     = ' ' !! Platform.
    end type uname_type

    public :: dm_system_daemonize
    public :: dm_system_fork
    public :: dm_system_path
    public :: dm_system_uname
    public :: dm_system_uptime
    public :: dm_system_wait
contains
    integer function dm_system_daemonize(command) result(rc)
        !! Turns current running program into a daemon. On FreeBSD, it is
        !! probably easier to run the process through daemon(8) instead.
        character(len=*), intent(in) :: command

        integer(kind=c_pid_t)  :: group, pid
        integer(kind=c_mode_t) :: mode

        rc = E_SYSTEM

        ! Clear file creation mask.
        mode = c_umask(0)

        ! Spawn a new process and exit.
        pid = c_fork()

        if (pid < 0) then
            ! Fork error.
            return
        else if (pid > 0) then
            ! Parent process.
            call c_exit(0)
        end if

        ! Child process from here on.
        ! Detach from the current terminal session.
        group = c_setsid()

        ! Change working directory to root directory.
        if (c_chdir('/' // c_null_char) < 0) return

        ! Open the log file.
        call c_openlog(command // c_null_char, LOG_CONS, LOG_DAEMON)

        rc = E_NONE
    end function dm_system_daemonize

    integer function dm_system_fork() result(pid)
        !! Forks process and returns PID.
        pid = c_fork()
    end function dm_system_fork

    integer function dm_system_wait(stat) result(pid)
        !! Waits for child process and returns PID.
        integer, intent(out) :: stat !! Returned status (POSIX).

        pid = c_wait(stat)
    end function dm_system_wait

    subroutine dm_system_path(path)
        !! Returns the relative path of the executable.
        character(len=*), intent(inout) :: path !! Returned path.

        call get_command_argument(0, path)
    end subroutine dm_system_path

    subroutine dm_system_uname(uname, stat)
        !! Returns uname information (operating system, hostname, ...).
        type(uname_type), intent(out)           :: uname !! Uname type.
        integer,          intent(out), optional :: stat  !! Error code.

        type(c_utsname) :: utsname

        if (present(stat)) stat = E_SYSTEM
        if (c_uname(utsname) /= 0) return

        call c_f_str_chars(utsname%sysname,  uname%system_name)
        call c_f_str_chars(utsname%nodename, uname%node_name)
        call c_f_str_chars(utsname%release,  uname%release)
        call c_f_str_chars(utsname%version,  uname%version)
        call c_f_str_chars(utsname%machine,  uname%machine)

        if (present(stat)) stat = E_NONE
    end subroutine dm_system_uname

    subroutine dm_system_uptime(time, stat)
        !! Returns system uptime.
        integer(kind=i8), intent(out)           :: time
        integer,          intent(out), optional :: stat

        type(c_timespec) :: tp

        if (present(stat)) stat = E_SYSTEM
        if (c_clock_gettime(CLOCK_MONOTONIC, tp) /= 0) return
        time = int(tp%tv_sec, kind=i8)
        if (time > 60) time = time + 30
        if (present(stat)) stat = E_NONE
    end subroutine dm_system_uptime
end module dm_system
