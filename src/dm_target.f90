! Author:  Philipp Engel
! Licence: ISC
module dm_target
    !! Observation target declaration.
    use :: dm_id
    use :: dm_kind
    use :: dm_util
    implicit none (type, external)
    private

    integer, parameter, public :: TARGET_ID_LEN   = ID_LEN
    integer, parameter, public :: TARGET_NAME_LEN = 32
    integer, parameter, public :: TARGET_META_LEN = 32

    integer, parameter, public :: TARGET_STATE_NONE     = 0 !! Default state.
    integer, parameter, public :: TARGET_STATE_REMOVED  = 1 !! Target was removed.
    integer, parameter, public :: TARGET_STATE_MISSING  = 2 !! Target is missing.
    integer, parameter, public :: TARGET_STATE_INVALID  = 3 !! Target is invalid.
    integer, parameter, public :: TARGET_STATE_IGNORE   = 4 !! Target should be ignored.
    integer, parameter, public :: TARGET_STATE_OBSOLETE = 5 !! Target is obsolete.
    integer, parameter, public :: TARGET_STATE_USER     = 6 !! User-defined state.
    integer, parameter, public :: TARGET_NSTATES        = 7 !! Number of known states.

    integer, parameter, public :: TARGET_STATE_NAME_LEN = 8 !! Length of target state name.

    character(len=*), parameter, public :: TARGET_STATE_NAMES(0:TARGET_NSTATES - 1) = [ &
        character(len=TARGET_STATE_NAME_LEN) :: &
        'none', 'removed', 'missing', 'invalid', 'ignore', 'obsolete', 'user' ] !! Target state names.

    type, public :: target_type
        !! Target description.
        character(len=TARGET_ID_LEN)   :: id    = ' '               !! Target id (-0-9A-Za-z).
        character(len=TARGET_NAME_LEN) :: name  = ' '               !! Target name.
        character(len=TARGET_META_LEN) :: meta  = ' '               !! Target meta information.
        integer                        :: state = TARGET_STATE_NONE !! Target state.
        real(kind=r8)                  :: x     = 0.0_r8            !! Target easting.
        real(kind=r8)                  :: y     = 0.0_r8            !! Target northing.
        real(kind=r8)                  :: z     = 0.0_r8            !! Target altitude.
    end type target_type

    integer, parameter, public :: TARGET_SIZE = storage_size(target_type()) / 8 !! Size of `target_type` in bytes.

    interface operator (==)
        !! Returns whether targets are equal.
        module procedure :: dm_target_equals
    end interface

    public :: operator (==)

    public :: dm_target_equals
    public :: dm_target_out
    public :: dm_target_state_name
    public :: dm_target_state_valid
    public :: dm_target_valid
contains
    pure elemental logical function dm_target_equals(target1, target2) result(equals)
        !! Returns `.true.` if given targets are equal.
        type(target_type), intent(in) :: target1 !! The first target.
        type(target_type), intent(in) :: target2 !! The second target.

        equals = .false.
        if (target1%id    /= target2%id)           return
        if (target1%name  /= target2%name)         return
        if (target1%meta  /= target2%meta)         return
        if (target1%state /= target2%state)        return
        if (.not. dm_equals(target1%x, target2%x)) return
        if (.not. dm_equals(target1%y, target2%y)) return
        if (.not. dm_equals(target1%z, target2%z)) return
        equals= .true.
    end function dm_target_equals

    pure function dm_target_state_name(state) result(str)
        !! Returns the name of the known target state as an allocatable
        !! character string, or `unknown` if the state is not known.
        integer, intent(in)           :: state !! Target state.
        character(len=:), allocatable :: str !! Target state name.

        if (.not. dm_target_state_valid(state)) then
            str = 'unknown'
            return
        end if

        str = trim(TARGET_STATE_NAMES(state))
    end function dm_target_state_name

    pure elemental logical function dm_target_state_valid(state) result(valid)
        !! Returns `.true.` if the state of the given target type is known.
        integer, intent(in) :: state !! Target state.

        valid = .false.
        if (state < 0 .or. state >= TARGET_NSTATES) return
        valid = .true.
    end function dm_target_state_valid

    pure elemental logical function dm_target_valid(target) result(valid)
        !! Returns `.true.` if given target type elements are valid.
        type(target_type), intent(in) :: target !! Target type.

        valid = .false.
        if (.not. dm_id_valid(target%id)) return
        if (len_trim(target%name) == 0) return
        if (target%state < 0 .or. target%state >= TARGET_NSTATES) return
        valid = .true.
    end function dm_target_valid

    subroutine dm_target_out(target, unit)
        !! Prints target to standard output or given file unit. If not unit is
        !! passed, the target will be written to standard output.
        type(target_type), intent(inout)        :: target !! Target type.
        integer,           intent(in), optional :: unit   !! File unit.

        integer :: unit_

        unit_ = stdout
        if (present(unit)) unit_ = unit

        write (unit_, '("target.id: ", a)')     trim(target%id)
        write (unit_, '("target.name: ", a)')   trim(target%name)
        write (unit_, '("target.meta: ", a)')   trim(target%meta)
        write (unit_, '("target.state: ", i0)') target%state
        write (unit_, '("target.x: ", f0.16)')  target%x
        write (unit_, '("target.y: ", f0.16)')  target%y
        write (unit_, '("target.z: ", f0.16)')  target%z
    end subroutine dm_target_out
end module dm_target
