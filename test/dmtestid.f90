! dmtestid.f90
!
! Author:  Philipp Engel
! Licence: ISC
program dmtestid
    !! Test program for id validation.
    use :: dmpack
    implicit none (type, external)

    character(len=*), parameter :: TEST_NAME = 'dmtestid'
    integer,          parameter :: NTESTS    = 1

    type(test_type) :: tests(NTESTS)
    logical         :: stats(NTESTS)

    tests = [ &
        test_type('test01', test01) &
    ]

    call dm_init()
    call dm_test_run(TEST_NAME, tests, stats, dm_env_has('NO_COLOR'))
contains
    logical function test01() result(stat)
        stat = TEST_FAILED

        print *, 'Testing id validation ...'

        if (.not. dm_id_valid('foo_bar-12345')) return
        if (.not. dm_id_valid('2c62c1d467a843ac8cfed34f2152f63c')) return

        if (dm_id_valid('')) return
        if (dm_id_valid(repeat('a', ID_LEN + 1))) return
        if (dm_id_valid(repeat('a', 9), max_len=8)) return
        if (dm_id_valid('foo?bar')) return
        if (dm_id_valid(' foo_bar-12345')) return

        stat = TEST_PASSED
    end function test01
end program dmtestid
