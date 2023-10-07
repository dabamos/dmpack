! Author:  Philipp Engel
! Licence: ISC
module dm_target
    !! Observation target declaration.
    use :: dm_id
    use :: dm_kind
    implicit none (type, external)
    private

    integer, parameter, public :: TARGET_ID_LEN   = ID_LEN
    integer, parameter, public :: TARGET_NAME_LEN = 32
    integer, parameter, public :: TARGET_META_LEN = 32

    type, public :: target_type
        !! Target description.
        character(len=TARGET_ID_LEN)   :: id   = ' ' !! Target id (-0-9A-Za-z).
        character(len=TARGET_NAME_LEN) :: name = ' ' !! Target name.
        character(len=TARGET_META_LEN) :: meta = ' ' !! Target meta information.
    end type target_type

    integer, parameter, public :: TARGET_SIZE = storage_size(target_type()) / 8 !! Size of `target_type` in bytes.

    interface operator (==)
        !! Returns whether targets are equal.
        module procedure :: dm_target_equals
    end interface

    public :: operator (==)

    public :: dm_target_equals
    public :: dm_target_out
    public :: dm_target_valid
contains
    pure elemental logical function dm_target_equals(target1, target2) result(equals)
        !! Returns `.true.` if given targets are equal.
        type(target_type), intent(in) :: target1 !! The first target.
        type(target_type), intent(in) :: target2 !! The second target.

        equals = .false.
        if (target1%id   /= target2%id)   return
        if (target1%name /= target2%name) return
        if (target1%meta /= target2%meta) return
        equals= .true.
    end function dm_target_equals

    pure elemental logical function dm_target_valid(target) result(valid)
        !! Returns `.true.` if given target type elements are valid.
        type(target_type), intent(in) :: target !! Target type.

        valid = .false.
        if (.not. dm_id_valid(target%id)) return
        if (len_trim(target%name) == 0) return
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

        write (unit_, '("target.id: ", a)')   trim(target%id)
        write (unit_, '("target.name: ", a)') trim(target%name)
        write (unit_, '("target.meta: ", a)') trim(target%meta)
    end subroutine dm_target_out
end module dm_target
