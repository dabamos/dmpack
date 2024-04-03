! dmtestzlib.f90
!
! Author:  Philipp Engel
! Licence: ISC
program dmtestzlib
    !! Test program for deflate compression (zlib).
    use :: dmpack
    implicit none (type, external)
    integer, parameter :: NTESTS = 1

    type(test_type) :: tests(NTESTS)
    logical         :: stats(NTESTS)

    tests(1) = test_type('dmtestzlib.test01', test01)

    call dm_init()
    call dm_test_run(tests, stats, dm_env_has('NO_COLOR'))
contains
    logical function test01() result(stat)
        character(len=NML_OBSERV_LEN) :: input, output2
        character(len=:), allocatable :: output1
        integer                       :: rc
        type(observ_type)             :: observ1, observ2

        stat = TEST_FAILED

        call dm_test_dummy(observ1)
        rc = dm_nml_from(observ1, input)

        print *, 'compressing ...'
        rc = dm_zlib_compress(input, output1)
        if (dm_is_error(rc)) return

        print *, 'uncompressing ...'
        rc = dm_zlib_uncompress(output1, output2)
        if (dm_is_error(rc)) return

        print *, 'matching ...'
        rc = dm_nml_to(output2, observ2)
        if (.not. (observ2 == observ1)) return

        stat = TEST_PASSED
    end function test01
end program dmtestzlib
