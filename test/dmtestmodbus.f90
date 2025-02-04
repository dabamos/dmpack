! dmtestmodbus.f90
!
! Author:  Philipp Engel
! Licence: ISC
program dmtestmodbus
    !! Test program for Modbus communication.
    use :: dmpack
    implicit none (type, external)

    character(len=*), parameter :: TEST_NAME = 'dmtestmodbus'
    integer,          parameter :: NTESTS    = 3

    type(test_type) :: tests(NTESTS)
    logical         :: stats(NTESTS)

    tests = [ &
        test_type('test01', test01), &
        test_type('test02', test02), &
        test_type('test03', test03)  &
    ]

    call dm_init()
    call dm_test_run(TEST_NAME, tests, stats, dm_env_has('NO_COLOR'))
contains
    logical function test01() result(stat)
        integer               :: rc
        type(modbus_tcp_type) :: modbus

        stat = TEST_FAILED

        print '(" Library: ", a)', dm_modbus_version(.true.)

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
        r = dm_modbus_get_float_abcd(regs)
        print '(" Value: ", f5.3)', r
        if (.not. dm_equals(r, 0.0)) return

        stat = TEST_PASSED
    end function test02

    logical function test03() result(stat)
        character(len=*), parameter :: STRING1 = 'access=read,slave=10,address=50,float=abcd'
        character(len=*), parameter :: STRING2 = 'ACCESS = WRITE, SLAVE = 9, ADDRESS = 1, VALUE = 10'
        character(len=*), parameter :: STRING3 = 'access=none,slave=10,address=50,value=10,float=none'
        character(len=*), parameter :: STRING4 = 'access=write,slave=10,address=50,value=123456789,float=none'
        character(len=*), parameter :: STRING5 = 'access-read.slave-10.address-50'

        type(modbus_register_type) :: register
        integer                    :: rc

        stat = TEST_FAILED

        print *, 'Parsing strings ...'

        print *, STRING1
        call dm_modbus_parse(STRING1, register, error=rc)
        call dm_error_out(rc); if (dm_is_error(rc)) return

        if (register%access /= MODBUS_ACCESS_READ) return
        if (register%slave /= 10)                  return
        if (register%address /= 50)                return
        if (register%float /= MODBUS_FLOAT_ABCD)   return

        print *, STRING2
        call dm_modbus_parse(STRING2, register, error=rc)
        call dm_error_out(rc); if (dm_is_error(rc)) return

        if (register%access /= MODBUS_ACCESS_WRITE) return
        if (register%slave /= 9)                    return
        if (register%address /= 1)                  return
        if (register%value /= 10_u2)                return
        if (register%float /= MODBUS_FLOAT_NONE)    return

        print *, STRING3
        call dm_modbus_parse(STRING3, register, error=rc)
        if (rc /= E_TYPE) return

        print *, STRING4
        call dm_modbus_parse(STRING4, register, error=rc)
        if (rc /= E_TYPE) return

        print *, STRING5
        call dm_modbus_parse(STRING5, register, error=rc)
        if (rc /= E_FORMAT) return

        stat = TEST_PASSED
    end function test03
end program dmtestmodbus
