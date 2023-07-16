! Author:  Philipp Engel
! Licence: ISC
module dm_nml
    !! Fortran namelist import/export of DMPACK derived types.
    use :: dm_beat
    use :: dm_error
    use :: dm_file
    use :: dm_log
    use :: dm_node
    use :: dm_observ
    use :: dm_sensor
    use :: dm_target
    use :: dm_type
    implicit none (type, external)
    private

    ! The actual size of the namelist data is slightly smaller,
    ! but due to the compiler-dependent implementation additional
    ! bytes are added as extra space.
    integer, parameter, public :: NML_BEAT_LEN   = 320       !! Max. size of `beat_type` namelist in bytes.
    integer, parameter, public :: NML_LOG_LEN    = 960       !! Max. size of `log_type` namelist in bytes.
    integer, parameter, public :: NML_NODE_LEN   = 160       !! Max. size of `node_type` namelist in bytes.
    integer, parameter, public :: NML_OBSERV_LEN = 40 * 1024 !! Max. size of `observ_type` namelist in bytes.
    integer, parameter, public :: NML_SENSOR_LEN = 288       !! Max. size of `sensor_type` namelist in bytes.
    integer, parameter, public :: NML_TARGET_LEN = 160       !! Max. size of `target_type` namelist in bytes.

    interface dm_nml_to
        !! Converts string to type.
        module procedure :: nml_to_beat
        module procedure :: nml_to_log
        module procedure :: nml_to_node
        module procedure :: nml_to_observ
        module procedure :: nml_to_sensor
        module procedure :: nml_to_target
    end interface

    interface dm_nml_from
        !! Converts type to string.
        module procedure :: nml_from_beat
        module procedure :: nml_from_log
        module procedure :: nml_from_node
        module procedure :: nml_from_observ
        module procedure :: nml_from_sensor
        module procedure :: nml_from_target
    end interface

    interface dm_nml_write
        !! Writes type to file or standard output.
        module procedure :: nml_write_log
        module procedure :: nml_write_observ
    end interface

    public :: dm_nml_from
    public :: dm_nml_to
    public :: dm_nml_write

    private :: nml_from_beat
    private :: nml_from_log
    private :: nml_from_node
    private :: nml_from_observ
    private :: nml_from_sensor
    private :: nml_from_target

    private :: nml_to_beat
    private :: nml_to_log
    private :: nml_to_node
    private :: nml_to_observ
    private :: nml_to_sensor
    private :: nml_to_target

    private :: nml_write_log
    private :: nml_write_observ
contains
    ! ******************************************************************
    ! PRIVATE PROCEDURES.
    ! ******************************************************************
    integer function nml_from_beat(beat, str) result(rc)
        !! Writes beat namelist to string. The passed character string
        !! must have a minimum length of `NML_BEAT_LEN`.
        type(beat_type),  intent(inout) :: beat !! Beat type.
        character(len=*), intent(inout) :: str  !! Output string.
        integer                         :: stat

        namelist /DMBEAT/ beat

        rc = E_WRITE
        str = ' '
        write (str, nml=DMBEAT, iostat=stat)
        if (stat /= 0) return
        rc = E_NONE
    end function nml_from_beat

    integer function nml_from_log(log, str) result(rc)
        !! Writes log namelist to string. The passed character string
        !! must have a minimum length of `NML_LOG_LEN`.
        type(log_type),   intent(inout) :: log !! Log type.
        character(len=*), intent(inout) :: str !! Output string.
        integer                         :: stat

        namelist /DMLOG/ log

        rc = E_WRITE
        str = ' '
        write (str, nml=DMLOG, iostat=stat)
        if (stat /= 0) return
        rc = E_NONE
    end function nml_from_log

    integer function nml_from_node(node, str) result(rc)
        !! Writes node namelist to string. The passed character string
        !! must have a minimum length of `NML_NODE_LEN`.
        type(node_type),  intent(inout) :: node  !! Node type.
        character(len=*), intent(inout) :: str   !! Output string.
        integer                         :: stat

        namelist /DMNODE/ node

        rc = E_WRITE
        str = ' '
        write (str, nml=DMNODE, iostat=stat)
        if (stat /= 0) return
        rc = E_NONE
    end function nml_from_node

    integer function nml_from_observ(observ, str) result(rc)
        !! Writes observation namelist to string. The passed character string
        !! must have a minimum length of `NML_OBSERV_LEN`.
        type(observ_type), intent(inout) :: observ !! Observation type.
        character(len=*),  intent(inout) :: str    !! Output string.
        integer                          :: stat

        namelist /DMOBSERV/ observ

        rc = E_WRITE
        str = ' '
        write (str, nml=DMOBSERV, iostat=stat)
        if (stat /= 0) return
        rc = E_NONE
    end function nml_from_observ

    integer function nml_from_sensor(sensor, str) result(rc)
        !! Writes sensor namelist to string. The passed character string
        !! must have a minimum length of `NML_SENSOR_LEN`.
        type(sensor_type), intent(inout) :: sensor !! Sensor type.
        character(len=*),  intent(inout) :: str    !! Output string.
        integer                          :: stat

        namelist /DMSENSOR/ sensor

        rc = E_WRITE
        str = ' '
        write (str, nml=DMSENSOR, iostat=stat)
        if (stat /= 0) return
        rc = E_NONE
    end function nml_from_sensor

    integer function nml_from_target(target, str) result(rc)
        !! Writes target namelist to string. The passed character string
        !! must have a minimum length of `NML_TARGET_LEN`.
        type(target_type), intent(inout) :: target !! Target type.
        character(len=*),  intent(inout) :: str    !! Output string.
        integer                          :: stat

        namelist /DMTARGET/ target

        rc = E_WRITE
        str = ' '
        write (str, nml=DMTARGET, iostat=stat)
        if (stat /= 0) return
        rc = E_NONE
    end function nml_from_target

    impure elemental integer function nml_to_beat(str, beat) result(rc)
        !! Reads beat from namelist string.
        character(len=*), intent(in)  :: str  !! Beat namelist data.
        type(beat_type),  intent(out) :: beat !! Beat type.
        integer                       :: stat

        namelist /DMBEAT/ beat

        rc = E_READ
        read (str, nml=DMBEAT, iostat=stat)
        if (stat /= 0) return
        rc = E_NONE
    end function nml_to_beat

    impure elemental integer function nml_to_log(str, log) result(rc)
        !! Reads log from namelist string.
        character(len=*), intent(in)  :: str !! Log namelist data.
        type(log_type),   intent(out) :: log !! Log type.
        integer                       :: stat

        namelist /DMLOG/ log

        rc = E_READ
        read (str, nml=DMLOG, iostat=stat)
        if (stat /= 0) return
        rc = E_NONE
    end function nml_to_log

    impure elemental integer function nml_to_node(str, node) result(rc)
        !! Reads node from namelist string.
        character(len=*), intent(in)  :: str  !! Node namelist data.
        type(node_type),  intent(out) :: node !! Node type.
        integer                       :: stat

        namelist /DMNODE/ node

        rc = E_READ
        read (str, nml=DMNODE, iostat=stat)
        if (stat /= 0) return
        rc = E_NONE
    end function nml_to_node

    impure elemental integer function nml_to_observ(str, observ) result(rc)
        !! Reads observation from namelist string.
        character(len=*),  intent(in)  :: str    !! Observation namelist data.
        type(observ_type), intent(out) :: observ !! Observation type.
        integer                        :: stat

        namelist /DMOBSERV/ observ

        rc = E_READ
        read (str, nml=DMOBSERV, iostat=stat)
        if (stat /= 0) return
        rc = E_NONE
    end function nml_to_observ

    impure elemental integer function nml_to_sensor(str, sensor) result(rc)
        !! Reads sensor from namelist string.
        character(len=*),  intent(in)  :: str    !! Sensor namelist data.
        type(sensor_type), intent(out) :: sensor !! Sensor type.
        integer                        :: stat

        namelist /DMSENSOR/ sensor

        rc = E_READ
        read (str, nml=DMSENSOR, iostat=stat)
        if (stat /= 0) return
        rc = E_NONE
    end function nml_to_sensor

    impure elemental integer function nml_to_target(str, target) result(rc)
        !! Reads target from namelist string.
        character(len=*),  intent(in)  :: str    !! Node namelist data.
        type(target_type), intent(out) :: target !! Target type.
        integer                        :: stat

        namelist /DMTARGET/ target

        rc = E_READ
        read (str, nml=DMTARGET, iostat=stat)
        if (stat /= 0) return
        rc = E_NONE
    end function nml_to_target

    integer function nml_write_log(log, unit) result(rc)
        !! Writes log namelist to file or standard output.
        type(log_type), intent(inout)        :: log  !! Log type.
        integer,        intent(in), optional :: unit !! File unit.

        integer :: stat, unit_
        namelist /DMLOG/ log

        rc = E_WRITE
        unit_ = stdout
        if (present(unit)) unit_ = unit
        write (unit_, nml=DMLOG, iostat=stat)
        if (stat /= 0) return
        rc = E_NONE
    end function nml_write_log

    integer function nml_write_observ(observ, unit) result(rc)
        !! Writes observation namelist to file or standard output.
        type(observ_type), intent(inout)        :: observ !! Observation type.
        integer,           intent(in), optional :: unit   !! File unit.

        integer :: stat, unit_
        namelist /DMOBSERV/ observ

        rc = E_WRITE
        unit_ = stdout
        if (present(unit)) unit_ = unit
        write (unit_, nml=DMOBSERV, iostat=stat)
        if (stat /= 0) return
        rc = E_NONE
    end function nml_write_observ
end module dm_nml
