! dmtestzlib.f90
!
! Author:  Philipp Engel
! Licence: ISC
program dmtestzlib
    !! Test program for deflate compression (zlib).
    use, intrinsic :: iso_fortran_env, only: compiler_options, compiler_version
    use :: dmpack
    implicit none (type, external)

    character(len=*), parameter :: TEST_NAME = 'dmtestzlib'
    integer,          parameter :: NTESTS    = 1

    type(test_type) :: tests(NTESTS)
    logical         :: stats(NTESTS)

    tests = [ &
        test_type('test01', test01) &
    ]

    call dm_init()
    call dm_test_run(TEST_NAME, tests, stats, compiler_version(), compiler_options())
contains
    logical function test01() result(stat)
        character(len=NML_OBSERV_LEN) :: input, output2
        character(len=:), allocatable :: output1
        integer                       :: rc
        type(observ_type)             :: observ1, observ2

        stat = TEST_FAILED

        call dm_test_dummy(observ1)
        rc = dm_nml_from(observ1, input)

        print *, 'Compressing ...'
        rc = dm_zlib_compress(input, output1)
        if (dm_is_error(rc)) return

        print *, 'Uncompressing ...'
        rc = dm_zlib_uncompress(output1, output2)
        if (dm_is_error(rc)) return

        print *, 'Matching ...'
        rc = dm_nml_to(output2, observ2)
        if (.not. (observ2 == observ1)) return

        stat = TEST_PASSED
    end function test01
end program dmtestzlib
