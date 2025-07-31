! dmtestnet.f90
!
! Author:  Philipp Engel
! Licence: ISC
program dmtestnet
    !! Test program for network module.
    use, intrinsic :: iso_fortran_env, only: compiler_options, compiler_version
    use :: dmpack
    implicit none (type, external)

    character(len=*), parameter :: TEST_NAME = 'dmtestnet'
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
        stat = TEST_FAILED

        print *, 'Validating IPv4 addresses ...'

        if (.not. dm_net_ipv4_is_valid('0.0.0.0'))         return
        if (.not. dm_net_ipv4_is_valid('127.0.0.1'))       return
        if (.not. dm_net_ipv4_is_valid('000.000.000.000')) return
        if (.not. dm_net_ipv4_is_valid('192.100.100.100')) return

        if (dm_net_ipv4_is_valid('0'))                     return
        if (dm_net_ipv4_is_valid('0.0.0.'))                return
        if (dm_net_ipv4_is_valid('0.0.0.abc'))             return
        if (dm_net_ipv4_is_valid('0.0..00'))               return
        if (dm_net_ipv4_is_valid('0000.0000.0000.0000'))   return

        stat = TEST_PASSED
    end function test01
end program dmtestnet
