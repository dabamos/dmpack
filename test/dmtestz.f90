! dmtestz.f90
!
! Author:  Philipp Engel
! Licence: ISC
program dmtestz
    !! Test program for DEFLATE.
    use :: dmpack
    implicit none (type, external)
    integer, parameter :: NTESTS = 1

    type(test_type) :: tests(NTESTS)
    logical         :: stats(NTESTS)

    tests(1) = test_type('dmtestz.test01', test01)

    call dm_init()
    call dm_test_run(tests, stats, dm_env_has('NO_COLOR'))
contains
    logical function test01() result(stat)
        character(len=NML_OBSERV_LEN) :: input, output
        character(len=:), allocatable :: output1, output2
        integer                       :: rc
        type(observ_type)             :: observ1, observ2, observ3

        stat = TEST_FAILED

        call dm_test_dummy(observ1)
        rc = dm_nml_from(observ1, input)

        print *, 'deflate ...'
        rc = dm_z_deflate_mem(input, output1)
        if (dm_is_error(rc)) return

        print *, 'inflate ...'
        rc = dm_z_inflate_mem(output1, output2, len(input))
        if (dm_is_error(rc)) return

        print '(" source size.: ", i0)', len(input)
        print '(" deflate size: ", i0)', len(output1)
        print '(" inflate size: ", i0)', len(output2)

        print *, 'converting ...'
        rc = dm_nml_to(output2, observ2)
        call dm_perror(rc)
        if (dm_is_error(rc)) return

        print *, 'matching ...'
        if (.not. (observ2 == observ1)) return

        deallocate (output1, output2)

        print *, 'compress ...'
        rc = dm_z_compress(input, output1)
        if (dm_is_error(rc)) return

        print *, 'uncompress ...'
        rc = dm_z_uncompress(output1, output)
        if (dm_is_error(rc)) return

        print *, 'matching ...'
        rc = dm_nml_to(output, observ3)
        if (.not. (observ3 == observ1)) return

        stat = TEST_PASSED
    end function test01
end program dmtestz
