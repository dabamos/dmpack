! Author:  Philipp Engel
! Licence: ISC
module dm_kind
    !! ISO Fortran Environment imports and additional kind definitions.
    !!
    !! This module provides the following integer and real kinds:
    !!
    !! * 2-byte integer: `i2` (⇒ `int16`)
    !! * 4-byte integer: `i4` (⇒ `int32`)
    !! * 8-byte integer: `i8` (⇒ `int64`)
    !! * 4-byte real:    `r4` (⇒ `real32`)
    !! * 8-byte real:    `r8` (⇒ `real64`)
    !!
    !! As well as the following input/output units:
    !!
    !! * standard error:  `stderr` (⇒ `error_unit`)
    !! * standard input:  `stdin`  (⇒ `input_unit`)
    !! * standard output: `stdout` (⇒ `output_unit`)
    !!
    use, intrinsic :: iso_fortran_env, only: i2     => int16, &
                                             i4     => int32, &
                                             i8     => int64, &
                                             r4     => real32, &
                                             r8     => real64, &
                                             stderr => error_unit, &
                                             stdin  => input_unit, &
                                             stdout => output_unit
    implicit none (type, external)
    private

    public :: i2
    public :: i4
    public :: i8
    public :: r4
    public :: r8
    public :: stderr
    public :: stdin
    public :: stdout
end module dm_kind
