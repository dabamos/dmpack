! dmtesthash.f90
!
! Author:  Philipp Engel
! Licence: ISC
program dmtesthash
    use :: dmpack
    implicit none (type, external)
    integer, parameter :: NTESTS = 3

    type(test_type) :: tests(NTESTS)
    logical         :: stats(NTESTS)

    tests(1) = test_type('dmtesthash.test01', test01)
    tests(2) = test_type('dmtesthash.test02', test02)
    tests(3) = test_type('dmtesthash.test03', test03)

    call dm_init()
    call dm_test_run(tests, stats, dm_env_has('NO_COLOR'))
contains
    logical function test01() result(stat)
        stat = TEST_FAILED

        print *, 'Checking hashes ...'
        if (dm_hash_djb2('?') /= 177636) return
        if (dm_hash_djb2a('?') /= 177562) return

        stat = TEST_PASSED
    end function test01

    logical function test02() result(stat)
        integer(kind=i8) :: hashes(2)

        stat = TEST_FAILED

        print *, 'Checking collision ...'
        hashes(1) = dm_hash_fnv1('creamwove')
        hashes(2) = dm_hash_fnv1('quists')
        if (hashes(1) /= hashes(2)) return

        stat = TEST_PASSED
    end function test02

    logical function test03() result(stat)
        integer(kind=i8) :: hashes(2)

        stat = TEST_FAILED

        print *, 'Checking collision ...'
        hashes(1) = dm_hash_fnv1a('costarring')
        hashes(2) = dm_hash_fnv1a('liquid')
        if (hashes(1) /= hashes(2)) return

        stat = TEST_PASSED
    end function test03
end program dmtesthash
