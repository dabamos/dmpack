! Author:  Philipp Engel
! Licence: ISC
module dm_freebsd
    !! Abstraction layer over FreeBSD-specific APIs.
    use :: dm_error
    use :: dm_kind
    implicit none (type, external)
    private

    character(len=*), parameter :: SYSCTL           = '/sbin/sysctl'
    character(len=*), parameter :: SYSCTL_ARGUMENTS = '-n'
    character(len=*), parameter :: SYSCTL_COMMAND   = SYSCTL // ' ' // SYSCTL_ARGUMENTS // ' '

    interface freebsd_sysctl
        module procedure :: freebsd_sysctl_int32
        module procedure :: freebsd_sysctl_int64
        module procedure :: freebsd_sysctl_real32
        module procedure :: freebsd_sysctl_real64
        module procedure :: freebsd_sysctl_string
    end interface freebsd_sysctl

    public :: dm_freebsd_sysctl_battery_life
    public :: dm_freebsd_sysctl_hardware_model
    public :: dm_freebsd_sysctl_memory
    public :: dm_freebsd_sysctl_mqueue

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
    integer function dm_freebsd_sysctl_battery_life(life) result(rc)
        !! Returns the current battery life [%] from _sysctl(8)_.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_FORMAT` if output format is unexpected.
        !! * `E_READ` if pipe returned no bytes.
        !! * `E_SYSTEM` if system call failed.
        !!
        integer, intent(out) :: life !! Battery life [%].

        rc = freebsd_sysctl('hw.acpi.battery.life', life)
    end function dm_freebsd_sysctl_battery_life

    integer function dm_freebsd_sysctl_hardware_model(model) result(rc)
        !! Returns the hardware model string from _sysctl(8)_.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_FORMAT` if output format is unexpected.
        !! * `E_READ` if pipe returned no bytes.
        !! * `E_SYSTEM` if system call failed.
        !!
        character(len=*), intent(inout) :: model !! Hardware model.

        rc = freebsd_sysctl('hw.model', model)
    end function dm_freebsd_sysctl_hardware_model

    integer function dm_freebsd_sysctl_memory(phys_mem, real_mem, user_mem) result(rc)
        !! Returns physical, real, and user memory [bytes] from _sysctl(8)_.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_FORMAT` if output format is unexpected.
        !! * `E_READ` if pipe returned no bytes.
        !! * `E_SYSTEM` if system call failed.
        !!
        integer(kind=i8), intent(out), optional :: phys_mem !! Physical memory [bytes].
        integer(kind=i8), intent(out), optional :: real_mem !! Real memory [bytes].
        integer(kind=i8), intent(out), optional :: user_mem !! User memory [bytes].

        character(len=256) :: output
        integer            :: stat
        integer(kind=i8)   :: values(3)

        if (present(phys_mem)) phys_mem = 0_i8
        if (present(real_mem)) real_mem = 0_i8
        if (present(user_mem)) user_mem = 0_i8

        rc = freebsd_sysctl('hw.physmem hw.realmem hw.usermem', output)
        if (dm_is_error(rc)) return

        read (output, *, iostat=stat) values
        if (stat /= 0) rc = E_FORMAT

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
        !! * `E_READ` if pipe returned no bytes.
        !! * `E_SYSTEM` if system call failed.
        !!
        integer, intent(out), optional :: max_mqueues      !! Max. number of mqueues.
        integer, intent(out), optional :: max_messages     !! Max. number of messages.
        integer, intent(out), optional :: max_message_size !! Max. message size [bytes].

        character(len=256) :: output
        integer            :: stat, values(3)

        if (present(max_mqueues))      max_mqueues      = 0
        if (present(max_messages))     max_messages     = 0
        if (present(max_message_size)) max_message_size = 0

        rc = freebsd_sysctl('kern.mqueue.maxmq kern.mqueue.maxmsg kern.mqueue.maxmsgsize', output)
        if (dm_is_error(rc)) return

        read (output, *, iostat=stat) values
        if (stat /= 0) rc = E_FORMAT

        if (present(max_mqueues))      max_mqueues      = values(1)
        if (present(max_messages))     max_messages     = values(2)
        if (present(max_message_size)) max_message_size = values(3)
    end function dm_freebsd_sysctl_mqueue

    ! **************************************************************************
    ! PRIVATE FUNCTIONS.
    ! **************************************************************************
    integer function freebsd_sysctl_int32(name, value) result(rc)
        !! Reads 8-byte integer output from _sysctl(8)_.
        !!
        !! The function returns the following error codes:
        !!
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

    integer function freebsd_sysctl_string(name, value, nbytes) result(rc)
        !! Reads output from _sysctl(8)_.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_EXIST` if pipe is already connected.
        !! * `E_INVALID` if access mode is invalid.
        !! * `E_READ` if pipe returned no bytes.
        !! * `E_SYSTEM` if system call failed.
        !!
        use :: dm_pipe

        character(len=*), intent(in)            :: name   !! Variable name.
        character(len=*), intent(inout)         :: value  !! Variable value.
        integer(kind=i8), intent(out), optional :: nbytes !! String length.

        integer(kind=i8) :: n
        type(pipe_type)  :: pipe

        value = ' '
        if (present(nbytes)) nbytes = 0_i8

        rc = dm_pipe_open(pipe, SYSCTL_COMMAND // name, PIPE_RDONLY)
        if (dm_is_error(rc)) return

        n = dm_pipe_read(pipe, value)
        if (n == 0) rc = E_READ

        call dm_pipe_close(pipe)
        if (present(nbytes)) nbytes = n
    end function freebsd_sysctl_string
end module dm_freebsd
