! Author:  Philipp Engel
! Licence: ISC
module dm_kind
    !! ISO Fortran Environment imports and additional kind definitions.
    use, intrinsic :: iso_fortran_env, only: i4     => int32, &
                                             i8     => int64, &
                                             r4     => real32, &
                                             r8     => real64, &
                                             stderr => error_unit, &
                                             stdin  => input_unit, &
                                             stdout => output_unit
    implicit none (type, external)
    public
end module dm_kind
