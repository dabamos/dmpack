! dmtestnet.f90
!
! Author:  Philipp Engel
! Licence: ISC
program dmtestnet
    !! Test program for network module.
    use :: dmpack
    implicit none (type, external)

    character(len=*), parameter :: TEST_NAME = 'dmtestnet'
    integer,          parameter :: NTESTS    = 2

    type(test_type) :: tests(NTESTS)
    logical         :: stats(NTESTS)

    tests = [ &
        test_type('test01', test01), &
        test_type('test02', test02)  &
    ]

    call dm_init()
    call dm_test_run(TEST_NAME, tests, stats)
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

    logical function test02() result(stat)
        stat = TEST_FAILED

        print *, 'Validating IPv6 addresses ...'

        if (.not. dm_net_ipv6_is_valid('ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff')) return
        if (.not. dm_net_ipv6_is_valid('fe80:0000:0000:0000:0202:b3ff:fe1e:8329')) return
        if (.not. dm_net_ipv6_is_valid('2001:db8:85a3:0:0:8a2e:370:7334'))         return
        if (.not. dm_net_ipv6_is_valid('fe80::202:b3ff:fe1e:8329'))                return
        if (.not. dm_net_ipv6_is_valid('2001:db8:1:2:3:4:5:6'))                    return
        if (.not. dm_net_ipv6_is_valid('2001:db8::1'))                             return
        if (.not. dm_net_ipv6_is_valid('fd00::1'))                                 return
        if (.not. dm_net_ipv6_is_valid('::1'))                                     return
        if (.not. dm_net_ipv6_is_valid('::'))                                      return

        if (dm_net_ipv6_is_valid('2001:::1'))           return
        if (dm_net_ipv6_is_valid('2001:db8::1::1'))     return
        if (dm_net_ipv6_is_valid('2001:db8:12345::1'))  return
        if (dm_net_ipv6_is_valid('2001:db8:1:2:3:4:5')) return
        if (dm_net_ipv6_is_valid('::ffff:192.168.1.1')) return
        if (dm_net_ipv6_is_valid('::127.0.0.1'))        return

        stat = TEST_PASSED
    end function test02
end program dmtestnet
