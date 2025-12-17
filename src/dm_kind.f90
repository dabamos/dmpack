! Author:  Philipp Engel
! Licence: ISC
module dm_kind
    !! ISO Fortran Environment imports and additional kind definitions.
    !!
    !! This module provides the following integer and real kinds:
    !!
    !! * 1-byte signed integer:   `i1` (⇒ `int8`)
    !! * 2-byte signed integer:   `i2` (⇒ `int16`)
    !! * 4-byte signed integer:   `i4` (⇒ `int32`)
    !! * 8-byte signed integer:   `i8` (⇒ `int64`)
    !! * 1-byte unsigned integer: `u1` (⇒ `int8`)
    !! * 2-byte unsigned integer: `u2` (⇒ `int16`)
    !! * 4-byte unsigned integer: `u4` (⇒ `int32`)
    !! * 4-byte real:             `r4` (⇒ `real32`)
    !! * 8-byte real:             `r8` (⇒ `real64`)
    !!
    !! As well as the following input/output units:
    !!
    !! * standard error:  `STDERR` (⇒ `error_unit`)
    !! * standard input:  `STDIN`  (⇒ `input_unit`)
    !! * standard output: `STDOUT` (⇒ `output_unit`)
    !!
    use, intrinsic :: iso_fortran_env
    implicit none (type, external)
    private

    integer, parameter, public :: i1 = int8
    integer, parameter, public :: i2 = int16
    integer, parameter, public :: i4 = int32
    integer, parameter, public :: i8 = int64
    integer, parameter, public :: r4 = real32
    integer, parameter, public :: r8 = real64
    integer, parameter, public :: u1 = int8
    integer, parameter, public :: u2 = int16
    integer, parameter, public :: u4 = int32

    integer, parameter, public :: STDERR = error_unit
    integer, parameter, public :: STDIN  = input_unit
    integer, parameter, public :: STDOUT = output_unit
end module dm_kind
