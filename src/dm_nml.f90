! Author:  Philipp Engel
! Licence: ISC
module dm_nml
    !! Fortran 95 Namelist import/export of DMPACK derived types.
    use :: dm_error
    use :: dm_kind
    use :: dm_util
    implicit none (type, external)
    private

    ! The actual size of the namelist data is slightly smaller,
    ! but due to the compiler-dependent implementation additional
    ! bytes are added as extra space.
    integer, parameter, public :: NML_BEAT_LEN   = 360       !! Max. size of `beat_type` namelist in bytes.
    integer, parameter, public :: NML_LOG_LEN    = 936       !! Max. size of `log_type` namelist in bytes.
    integer, parameter, public :: NML_NODE_LEN   = 368       !! Max. size of `node_type` namelist in bytes.
    integer, parameter, public :: NML_OBSERV_LEN = 46 * 1024 !! Max. size of `observ_type` namelist in bytes.
    integer, parameter, public :: NML_SENSOR_LEN = 512       !! Max. size of `sensor_type` namelist in bytes.
    integer, parameter, public :: NML_TARGET_LEN = 416       !! Max. size of `target_type` namelist in bytes.

    interface dm_nml_from
        !! Converts type to static or allocatable namelist string.
        module procedure :: nml_from_beat
        module procedure :: nml_from_log
        module procedure :: nml_from_node
        module procedure :: nml_from_observ
        module procedure :: nml_from_sensor
        module procedure :: nml_from_target

        module procedure :: nml_from_beat_allocatable
        module procedure :: nml_from_log_allocatable
        module procedure :: nml_from_node_allocatable
        module procedure :: nml_from_observ_allocatable
        module procedure :: nml_from_sensor_allocatable
        module procedure :: nml_from_target_allocatable
    end interface dm_nml_from

    interface dm_nml_to
        !! Converts namelist string to type.
        module procedure :: nml_to_beat
        module procedure :: nml_to_log
        module procedure :: nml_to_node
        module procedure :: nml_to_observ
        module procedure :: nml_to_sensor
        module procedure :: nml_to_target
    end interface dm_nml_to

    interface dm_nml_read
        !! Reads namelist from file or standard input.
        module procedure :: nml_read_log
        module procedure :: nml_read_observ
    end interface dm_nml_read

    interface dm_nml_write
        !! Writes namelist to file or standard output.
        module procedure :: nml_write_log
        module procedure :: nml_write_observ
    end interface dm_nml_write

    ! Public procedures.
    public :: dm_nml_from
    public :: dm_nml_read
    public :: dm_nml_to
    public :: dm_nml_write

    ! Private procedures.
    private :: nml_from_beat
    private :: nml_from_log
    private :: nml_from_node
    private :: nml_from_observ
    private :: nml_from_sensor
    private :: nml_from_target

    private :: nml_from_beat_allocatable
    private :: nml_from_log_allocatable
    private :: nml_from_node_allocatable
    private :: nml_from_observ_allocatable
    private :: nml_from_sensor_allocatable
    private :: nml_from_target_allocatable

    private :: nml_to_beat
    private :: nml_to_log
    private :: nml_to_node
    private :: nml_to_observ
    private :: nml_to_sensor
    private :: nml_to_target

    private :: nml_read_log
    private :: nml_read_observ

    private :: nml_write_log
    private :: nml_write_observ
contains
    ! **************************************************************************
    ! PRIVATE PROCEDURES.
    ! **************************************************************************
    integer function nml_from_beat(beat, string) result(rc)
        !! Writes beat namelist to string. The passed character string must
        !! have a minimum length of `NML_BEAT_LEN`. Returns `E_WRITE` on error.
        use :: dm_beat, only: beat_type

        type(beat_type),  intent(inout) :: beat   !! Beat type.
        character(len=*), intent(inout) :: string !! Output string.

        integer :: stat
        namelist /DMBEAT/ beat

        rc = E_WRITE
        write (string, nml=DMBEAT, delim='quote', iostat=stat)

        if (stat /= 0) then
            string = ' '
            return
        end if

        rc = E_NONE
    end function nml_from_beat

    integer function nml_from_beat_allocatable(beat, string, n) result(rc)
        !! Writes beat namelist to allocatable string of given length. Returns
        !! `E_ALLOC` if allocation of `string` failed, or `E_WRITE` if the
        !! serialisation failed.
        use :: dm_beat, only: beat_type

        type(beat_type),               intent(inout) :: beat   !! Beat type.
        character(len=:), allocatable, intent(out)   :: string !! Allocatable output string.
        integer,                       intent(in)    :: n      !! String length.

        integer :: stat
        namelist /DMBEAT/ beat

        rc = E_ALLOC
        allocate (character(len=n) :: string, stat=stat)
        if (stat /= 0) return

        rc = E_WRITE
        write (string, nml=DMBEAT, delim='quote', iostat=stat)

        if (stat /= 0) then
            string = ' '
            return
        end if

        rc = E_NONE
    end function nml_from_beat_allocatable

    integer function nml_from_log(log, string) result(rc)
        !! Writes log namelist to string. The passed character string must have
        !! a minimum length of `NML_LOG_LEN`. Returns `E_WRITE` on error.
        use :: dm_log, only: log_type

        type(log_type),   intent(inout) :: log    !! Log type.
        character(len=*), intent(inout) :: string !! Output string.

        integer :: stat
        namelist /DMLOG/ log

        rc = E_WRITE
        write (string, nml=DMLOG, delim='quote', iostat=stat)

        if (stat /= 0) then
            string = ' '
            return
        end if

        rc = E_NONE
    end function nml_from_log

    integer function nml_from_log_allocatable(log, string, n) result(rc)
        !! Writes log namelist to allocatable string of given length. Returns
        !! `E_ALLOC` if allocation of `string` failed, or `E_WRITE` if the
        !! serialisation failed.
        use :: dm_log, only: log_type

        type(log_type),                intent(inout) :: log    !! Log type.
        character(len=:), allocatable, intent(out)   :: string !! Allocatable output string.
        integer,                       intent(in)    :: n      !! String length.

        integer :: stat
        namelist /DMLOG/ log

        rc = E_ALLOC
        allocate (character(len=n) :: string, stat=stat)
        if (stat /= 0) return

        rc = E_WRITE
        write (string, nml=DMLOG, delim='quote', iostat=stat)

        if (stat /= 0) then
            string = ' '
            return
        end if

        rc = E_NONE
    end function nml_from_log_allocatable

    integer function nml_from_node(node, string) result(rc)
        !! Writes node namelist to string. The passed character string must
        !! have a minimum length of `NML_NODE_LEN`. Returns `E_WRITE` on
        !! error.
        use :: dm_node, only: node_type

        type(node_type),  intent(inout) :: node   !! Node type.
        character(len=*), intent(inout) :: string !! Output string.

        integer :: stat
        namelist /DMNODE/ node

        rc = E_WRITE
        write (string, nml=DMNODE, delim='quote', iostat=stat)

        if (stat /= 0) then
            string = ' '
            return
        end if

        rc = E_NONE
    end function nml_from_node

    integer function nml_from_node_allocatable(node, string, n) result(rc)
        !! Writes node namelist to allocatable string of given length. Returns
        !! `E_ALLOC` if allocation of `string` failed, or `E_WRITE` if the
        !! serialisation failed.
        use :: dm_node, only: node_type

        type(node_type),               intent(inout) :: node   !! Node type.
        character(len=:), allocatable, intent(out)   :: string !! Allocatable output string.
        integer,                       intent(in)    :: n      !! String length.

        integer :: stat
        namelist /DMNODE/ node

        rc = E_ALLOC
        allocate (character(len=n) :: string, stat=stat)
        if (stat /= 0) return

        rc = E_WRITE
        write (string, nml=DMNODE, delim='quote', iostat=stat)

        if (stat /= 0) then
            string = ' '
            return
        end if

        rc = E_NONE
    end function nml_from_node_allocatable

    integer function nml_from_observ(observ, string) result(rc)
        !! Writes observation namelist to string. The passed character string
        !! must have a minimum length of `NML_OBSERV_LEN`. Returns `E_WRITE`
        !! on error.
        use :: dm_observ, only: observ_type

        type(observ_type), intent(inout) :: observ !! Observation type.
        character(len=*),  intent(inout) :: string !! Output string.

        integer :: stat
        namelist /DMOBSERV/ observ

        rc = E_WRITE
        write (string, nml=DMOBSERV, delim='quote', iostat=stat)

        if (stat /= 0) then
            string = ' '
            return
        end if

        rc = E_NONE
    end function nml_from_observ

    integer function nml_from_observ_allocatable(observ, string, n) result(rc)
        !! Writes observation namelist to allocatable string of given length.
        !! Returns `E_ALLOC` if allocation of `string` failed, or `E_WRITE` if
        !! the serialisation failed.
        use :: dm_observ, only: observ_type

        type(observ_type),             intent(inout) :: observ !! Observation type.
        character(len=:), allocatable, intent(out)   :: string !! Allocatable output string.
        integer,                       intent(in)    :: n      !! String length.

        integer :: stat
        namelist /DMOBSERV/ observ

        rc = E_ALLOC
        allocate (character(len=n) :: string, stat=stat)
        if (stat /= 0) return

        rc = E_WRITE
        write (string, nml=DMOBSERV, delim='quote', iostat=stat)

        if (stat /= 0) then
            string = ' '
            return
        end if

        rc = E_NONE
    end function nml_from_observ_allocatable

    integer function nml_from_sensor(sensor, string) result(rc)
        !! Writes sensor namelist to string. The passed character string must
        !! have a minimum length of `NML_SENSOR_LEN`. Returns `E_WRITE` on
        !! error.
        use :: dm_sensor, only: sensor_type

        type(sensor_type), intent(inout) :: sensor !! Sensor type.
        character(len=*),  intent(inout) :: string !! Output string.

        integer :: stat
        namelist /DMSENSOR/ sensor

        rc = E_WRITE
        write (string, nml=DMSENSOR, delim='quote', iostat=stat)

        if (stat /= 0) then
            string = ' '
            return
        end if

        rc = E_NONE
    end function nml_from_sensor

    integer function nml_from_sensor_allocatable(sensor, string, n) result(rc)
        !! Writes sensor namelist to allocatable string of given length.
        !! Returns `E_ALLOC` if allocation of `string` failed, or `E_WRITE` if
        !! the serialisation failed.
        use :: dm_sensor, only: sensor_type

        type(sensor_type),             intent(inout) :: sensor !! Sensor type.
        character(len=:), allocatable, intent(out)   :: string !! Allocatable output string.
        integer,                       intent(in)    :: n      !! String length.

        integer :: stat
        namelist /DMSENSOR/ sensor

        rc = E_ALLOC
        allocate (character(len=n) :: string, stat=stat)
        if (stat /= 0) return

        rc = E_WRITE
        write (string, nml=DMSENSOR, delim='quote', iostat=stat)

        if (stat /= 0) then
            string = ' '
            return
        end if

        rc = E_NONE
    end function nml_from_sensor_allocatable

    integer function nml_from_target(target, string) result(rc)
        !! Writes target namelist to string. The passed character string must
        !! have a minimum length of `NML_TARGET_LEN`. Returns `E_WRITE` on
        !! error.
        use :: dm_target, only: target_type

        type(target_type), intent(inout) :: target !! Target type.
        character(len=*),  intent(inout) :: string !! Output string.

        integer :: stat
        namelist /DMTARGET/ target

        rc = E_WRITE
        write (string, nml=DMTARGET, delim='quote', iostat=stat)

        if (stat /= 0) then
            string = ' '
            return
        end if

        rc = E_NONE
    end function nml_from_target

    integer function nml_from_target_allocatable(target, string, n) result(rc)
        !! Writes target namelist to allocatable string of given length.
        !! Returns `E_ALLOC` if allocation of `string` failed, or `E_WRITE` if
        !! the serialisation failed.
        use :: dm_target, only: target_type

        type(target_type),             intent(inout) :: target !! Target type.
        character(len=:), allocatable, intent(out)   :: string !! Allocatable output string.
        integer,                       intent(in)    :: n      !! String length.

        integer :: stat
        namelist /DMTARGET/ target

        rc = E_ALLOC
        allocate (character(len=n) :: string, stat=stat)
        if (stat /= 0) return

        rc = E_WRITE
        write (string, nml=DMTARGET, delim='quote', iostat=stat)

        if (stat /= 0) then
            string = ' '
            return
        end if

        rc = E_NONE
    end function nml_from_target_allocatable

    integer function nml_read_log(log, unit) result(rc)
        !! Reads log from file or standard input. Returns `E_READ` on error.
        use :: dm_log, only: log_type

        type(log_type), intent(inout)        :: log  !! Log type.
        integer,        intent(in), optional :: unit !! File unit.

        integer :: stat, unit_
        namelist /DMLOG/ log

        rc = E_READ
        unit_ = dm_present(unit, stdin)
        read (unit_, nml=DMLOG, iostat=stat)
        if (stat /= 0) return
        rc = E_NONE
    end function nml_read_log

    integer function nml_read_observ(observ, unit) result(rc)
        !! Reads observation from file or standard input. Returns `E_READ` on error.
        use :: dm_observ, only: observ_type

        type(observ_type), intent(inout)        :: observ !! Observation type.
        integer,           intent(in), optional :: unit   !! File unit.

        integer :: stat, unit_
        namelist /DMOBSERV/ observ

        rc = E_READ
        unit_ = dm_present(unit, stdin)
        read (unit_, nml=DMOBSERV, iostat=stat)
        if (stat /= 0) return
        rc = E_NONE
    end function nml_read_observ

    impure elemental integer function nml_to_beat(string, beat) result(rc)
        !! Reads beat from namelist string. Returns `E_READ` on error.
        use :: dm_beat, only: beat_type

        character(len=*), intent(in)  :: string !! Beat namelist data.
        type(beat_type),  intent(out) :: beat   !! Beat type.

        integer :: stat
        namelist /DMBEAT/ beat

        rc = E_READ
        read (string, nml=DMBEAT, iostat=stat)
        if (stat /= 0) return
        rc = E_NONE
    end function nml_to_beat

    impure elemental integer function nml_to_log(string, log) result(rc)
        !! Reads log from namelist string. Returns `E_READ` on error.
        use :: dm_log, only: log_type

        character(len=*), intent(in)  :: string !! Log namelist data.
        type(log_type),   intent(out) :: log    !! Log type.

        integer :: stat
        namelist /DMLOG/ log

        rc = E_READ
        read (string, nml=DMLOG, iostat=stat)
        if (stat /= 0) return
        rc = E_NONE
    end function nml_to_log

    impure elemental integer function nml_to_node(string, node) result(rc)
        !! Reads node from namelist string. Returns `E_READ` on error.
        use :: dm_node, only: node_type

        character(len=*), intent(in)  :: string !! Node namelist data.
        type(node_type),  intent(out) :: node   !! Node type.

        integer :: stat
        namelist /DMNODE/ node

        rc = E_READ
        read (string, nml=DMNODE, iostat=stat)
        if (stat /= 0) return
        rc = E_NONE
    end function nml_to_node

    impure elemental integer function nml_to_observ(string, observ) result(rc)
        !! Reads observation from namelist string. Returns `E_READ` on error.
        use :: dm_observ, only: observ_type

        character(len=*),  intent(in)  :: string !! Observation namelist data.
        type(observ_type), intent(out) :: observ !! Observation type.

        integer :: stat
        namelist /DMOBSERV/ observ

        rc = E_READ
        read (string, nml=DMOBSERV, iostat=stat)
        if (stat /= 0) return
        rc = E_NONE
    end function nml_to_observ

    impure elemental integer function nml_to_sensor(string, sensor) result(rc)
        !! Reads sensor from namelist string. Returns `E_READ` on error.
        use :: dm_sensor, only: sensor_type

        character(len=*),  intent(in)  :: string !! Sensor namelist data.
        type(sensor_type), intent(out) :: sensor !! Sensor type.

        integer :: stat
        namelist /DMSENSOR/ sensor

        rc = E_READ
        read (string, nml=DMSENSOR, iostat=stat)
        if (stat /= 0) return
        rc = E_NONE
    end function nml_to_sensor

    impure elemental integer function nml_to_target(string, target) result(rc)
        !! Reads target from namelist string. Returns `E_READ` on error.
        use :: dm_target, only: target_type

        character(len=*),  intent(in)  :: string !! Node namelist data.
        type(target_type), intent(out) :: target !! Target type.

        integer :: stat
        namelist /DMTARGET/ target

        rc = E_READ
        read (string, nml=DMTARGET, iostat=stat)
        if (stat /= 0) return
        rc = E_NONE
    end function nml_to_target

    integer function nml_write_log(log, unit) result(rc)
        !! Writes log namelist to file or standard output. Returns
        !! `E_WRITE` on error.
        use :: dm_log, only: log_type

        type(log_type), intent(inout)        :: log  !! Log type.
        integer,        intent(in), optional :: unit !! File unit.

        integer :: stat, unit_
        namelist /DMLOG/ log

        rc = E_WRITE
        unit_ = dm_present(unit, stdin)
        write (unit_, nml=DMLOG, delim='quote', iostat=stat)
        if (stat /= 0) return
        rc = E_NONE
    end function nml_write_log

    integer function nml_write_observ(observ, unit) result(rc)
        !! Writes observation namelist to file or standard output. Returns
        !! `E_WRITE` on error.
        use :: dm_observ, only: observ_type

        type(observ_type), intent(inout)        :: observ !! Observation type.
        integer,           intent(in), optional :: unit   !! File unit.

        integer :: stat, unit_
        namelist /DMOBSERV/ observ

        rc = E_WRITE
        unit_ = dm_present(unit, stdin)
        write (unit_, nml=DMOBSERV, delim='quote', iostat=stat)
        if (stat /= 0) return
        rc = E_NONE
    end function nml_write_observ
end module dm_nml
