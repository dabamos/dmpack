! dmtestmodbus.f90
!
! Author:  Philipp Engel
! Licence: ISC
program dmtestmodbus
    !! Test program for Modbus communication.
    use :: dmpack
    implicit none (type, external)

    character(len=*), parameter :: TEST_NAME = 'dmtestmodbus'
    integer,          parameter :: NTESTS    = 2

    type(test_type) :: tests(NTESTS)
    logical         :: stats(NTESTS)

    tests = [ &
        test_type('test01', test01), &
        test_type('test02', test02)  &
    ]

    call dm_init()
    call dm_test_run(TEST_NAME, tests, stats, dm_env_has('NO_COLOR'))
contains
    logical function test01() result(stat)
        integer           :: rc
        type(modbus_type) :: modbus

        stat = TEST_FAILED

        print '(" libmodbus: ", a)', dm_modbus_version()

        mb_block: block
            print *, 'Creating Modbus TCP context ...'
            rc = dm_modbus_create(modbus, '127.0.0.1', 1502)
            if (dm_is_error(rc)) exit mb_block

            print *, 'Setting debug mode ...'
            rc = dm_modbus_set_debug(modbus, .true.)
            if (dm_is_error(rc)) exit mb_block
        end block mb_block

        call dm_modbus_destroy(modbus)
        print *, 'Context destroyed'

        if (dm_is_error(rc)) then
            call dm_error_out(rc)
            print '(" Error: ", a)', dm_modbus_error_message()
            return
        end if

        stat = TEST_PASSED
    end function test01

    logical function test02() result(stat)
        integer(kind=u2) :: regs(2)
        real             :: r

        stat = TEST_FAILED
        regs = 0_u2

        print *, 'Converting registers to real ...'
        r = dm_modbus_get_real_abcd(regs)
        print '(" Value: ", f5.3)', r
        if (.not. dm_equals(r, 0.0)) return

        stat = TEST_PASSED
    end function test02
end program dmtestmodbus
