! dmtesthash.f90
!
! Author:  Philipp Engel
! Licence: ISC
program dmtesthash
    use :: dmpack
    implicit none (type, external)
    integer, parameter :: NTESTS = 1

    type(test_type) :: tests(NTESTS)
    logical         :: stats(NTESTS)

    tests(1) = test_type('dmtesthash.test01', test01)

    call dm_init()
    call dm_test_run(tests, stats, dm_env_has('NO_COLOR'))
contains
    logical function test01() result(stat)
        integer(kind=i8) :: hashes(2)

        stat = TEST_FAILED

        print *, 'Checking collision ...'
        hashes(1) = dm_hash_fnv1a('costarring')
        hashes(2) = dm_hash_fnv1a('liquid')
        if (hashes(1) /= hashes(2)) return

        stat = TEST_PASSED
    end function test01
end program dmtesthash
