! Author:  Philipp Engel
! Licence: ISC
module dm_target
    !! Observation target declaration.
    use :: dm_id
    use :: dm_kind
    use :: dm_util
    implicit none (type, external)
    private

    integer, parameter, public :: TARGET_ID_LEN   = ID_LEN !! Max. target id length.
    integer, parameter, public :: TARGET_NAME_LEN = 32     !! Max. target name length.
    integer, parameter, public :: TARGET_META_LEN = 32     !! Max. target meta description length.

    ! Target states.
    integer, parameter, public :: TARGET_STATE_NONE     = 0 !! Default state.
    integer, parameter, public :: TARGET_STATE_REMOVED  = 1 !! Target was removed.
    integer, parameter, public :: TARGET_STATE_MISSING  = 2 !! Target is missing.
    integer, parameter, public :: TARGET_STATE_INVALID  = 3 !! Target is invalid.
    integer, parameter, public :: TARGET_STATE_IGNORE   = 4 !! Target should be ignored.
    integer, parameter, public :: TARGET_STATE_OBSOLETE = 5 !! Target is obsolete.
    integer, parameter, public :: TARGET_STATE_USER     = 6 !! User-defined state.
    integer, parameter, public :: TARGET_STATE_LAST     = 6 !! Never use this.

    integer, parameter, public :: TARGET_STATE_NAME_LEN = 8 !! Max. target state name length.

    character(*), parameter, public :: TARGET_STATE_NAMES(TARGET_STATE_NONE:TARGET_STATE_LAST) = [ &
        character(TARGET_STATE_NAME_LEN) :: &
        'none', 'removed', 'missing', 'invalid', 'ignore', 'obsolete', 'user' &
    ] !! Target state names.

    type, public :: target_type
        !! Target description.
        character(TARGET_ID_LEN)   :: id        = ' '               !! Target id (`-0-9A-Z_a-z`).
        character(TARGET_NAME_LEN) :: name      = ' '               !! Target name.
        character(TARGET_META_LEN) :: meta      = ' '               !! Target meta information (optional).
        integer                    :: state     = TARGET_STATE_NONE !! Target state (optional).
        real(r8)                   :: x         = 0.0_r8            !! Target x or easting (optional).
        real(r8)                   :: y         = 0.0_r8            !! Target y or northing (optional).
        real(r8)                   :: z         = 0.0_r8            !! Target z or elevation (optional).
        real(r8)                   :: longitude = 0.0_r8            !! Longitude in degrees (optional).
        real(r8)                   :: latitude  = 0.0_r8            !! Latitude in degrees (optional).
        real(r8)                   :: elevation = 0.0_r8            !! Elevation in metres (optional).
    end type target_type

    integer, parameter, public :: TARGET_TYPE_SIZE = storage_size(target_type()) / 8 !! Size of `target_type` in bytes.

    interface operator (==)
        !! Returns `.true.` if targets are equal.
        module procedure :: dm_target_equals
    end interface

    public :: operator (==)

    public :: dm_target_equals
    public :: dm_target_is_valid
    public :: dm_target_out
    public :: dm_target_state_is_valid
    public :: dm_target_state_name
contains
    pure elemental logical function dm_target_equals(target1, target2) result(equals)
        !! Returns `.true.` if given targets are equal.
        type(target_type), intent(in) :: target1 !! The first target.
        type(target_type), intent(in) :: target2 !! The second target.

        equals = (target1%id    == target2%id                     .and. &
                  target1%name  == target2%name                   .and. &
                  target1%meta  == target2%meta                   .and. &
                  target1%state == target2%state                  .and. &
                  dm_equals(target1%x,         target2%x)         .and. &
                  dm_equals(target1%y,         target2%y)         .and. &
                  dm_equals(target1%z,         target2%z)         .and. &
                  dm_equals(target1%longitude, target2%longitude) .and. &
                  dm_equals(target1%latitude,  target2%latitude)  .and. &
                  dm_equals(target1%elevation, target2%elevation))
    end function dm_target_equals

    pure elemental logical function dm_target_is_valid(target) result(valid)
        !! Returns `.true.` if given target type elements are valid.
        use :: dm_string, only: dm_string_is_printable

        type(target_type), intent(in) :: target !! Target.

        valid = (dm_id_is_valid(target%id)           .and. &
                 len_trim(target%name) > 0           .and. &
                 dm_string_is_printable(target%name) .and. &
                 dm_string_is_printable(target%meta) .and. &
                 dm_target_state_is_valid(target%state))
    end function dm_target_is_valid

    pure elemental logical function dm_target_state_is_valid(state) result(valid)
        !! Returns `.true.` if the state of the given target type is known.
        integer, intent(in) :: state !! Target state.

        valid = (state >= TARGET_STATE_NONE .and. state <= TARGET_STATE_LAST)
    end function dm_target_state_is_valid

    pure function dm_target_state_name(state) result(name)
        !! Returns the name of the known target state as an allocatable
        !! character string, or `unknown` if the state is not known.
        integer, intent(in)       :: state !! Target state.
        character(:), allocatable :: name  !! Target state name.

        if (.not. dm_target_state_is_valid(state) .and. state /= TARGET_STATE_NONE) then
            name = 'invalid'
            return
        end if

        name = trim(TARGET_STATE_NAMES(state))
    end function dm_target_state_name

    subroutine dm_target_out(target, unit)
        !! Prints target to standard output or given file unit. If not unit is
        !! passed, the target will be written to standard output.
        type(target_type), intent(inout)        :: target !! Target.
        integer,           intent(in), optional :: unit   !! File unit.

        integer :: unit_

        unit_ = dm_present(unit, stdout)

        write (unit_, '("target.id: ", a)')              trim(target%id)
        write (unit_, '("target.name: ", a)')            trim(target%name)
        write (unit_, '("target.meta: ", a)')            trim(target%meta)
        write (unit_, '("target.state: ", i0)')          target%state
        write (unit_, '("target.x: ", 1pg0.12)')         target%x
        write (unit_, '("target.y: ", 1pg0.12)')         target%y
        write (unit_, '("target.z: ", 1pg0.12)')         target%z
        write (unit_, '("target.longitude: ", 1pg0.12)') target%longitude
        write (unit_, '("target.latitude: ", 1pg0.12)')  target%latitude
        write (unit_, '("target.elevation: ", 1pg0.12)') target%elevation
    end subroutine dm_target_out
end module dm_target
