! dmtestmodbus.f90
!
! Author:  Philipp Engel
! Licence: ISC
program dmtestmodbus
    !! Test program for Modbus communication.
    use, intrinsic :: iso_fortran_env, only: compiler_options, compiler_version
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
    call dm_test_run(TEST_NAME, tests, stats, compiler_version(), compiler_options())
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
        character(len=*), parameter :: STRING1 = 'ACCESS = READ, SLAVE = 10, ADDRESS = 50, TYPE = FLOAT, ORDER = ABCD'
        character(len=*), parameter :: STRING2 = 'access=write,slave=9,address=1,value=123,type=int32,scale=10'
        character(len=*), parameter :: STRING3 = 'access=write,slave=1,address=1,code=0x05,value=1'
        character(len=*), parameter :: STRING4 = 'access=none,slave=10,address=50,value=10,type=int32,order=none'
        character(len=*), parameter :: STRING5 = 'access=write,slave=10,address=50,value=abc'
        character(len=*), parameter :: STRING6 = 'access-read.slave-10.address-50'

        integer                    :: rc
        real(kind=r8)              :: value
        type(modbus_register_type) :: register

        stat = TEST_FAILED

        print *, 'Parsing strings ...'

        print *, '(1) ', STRING1
        call dm_modbus_register_parse(STRING1, register, error=rc)
        call dm_error_out(rc); if (dm_is_error(rc)) return

        if (register%access /= MODBUS_ACCESS_READ) return
        if (register%slave /= 10)                  return
        if (register%address /= 50)                return
        if (register%type /= MODBUS_TYPE_FLOAT)    return
        if (register%order /= MODBUS_ORDER_ABCD)   return

        print *, '(2) ', STRING2
        call dm_modbus_register_parse(STRING2, register, error=rc)
        call dm_error_out(rc); if (dm_is_error(rc)) return

        if (register%access /= MODBUS_ACCESS_WRITE) return
        if (register%slave /= 9)                    return
        if (register%address /= 1)                  return
        if (register%type /= MODBUS_TYPE_INT32)     return
        if (register%order /= MODBUS_ORDER_NONE)    return
        if (register%value /= 123)                  return
        if (register%scale /= 10)                   return

        value = dm_to_real64(register%value)

        print '(" Value:  ", i0)',   register%value
        print '(" Scale:  ", i0)',   register%scale
        print '(" Float:  ", f5.1)', value

        call dm_modbus_register_scale(register, value)
        print '(" Scaled: ", f0.1)', value
        if (.not. dm_equals(value, 12.3_r8)) return

        print *, '(3) ', STRING3
        call dm_modbus_register_parse(STRING3, register, error=rc)
        call dm_error_out(rc); if (dm_is_error(rc)) return
        print '(" Code: ", i0)', register%code
        if (register%code /= 5) return

        print *, '(4) ', STRING4
        call dm_modbus_register_parse(STRING4, register, error=rc)
        if (rc /= E_TYPE) return

        print *, '(5) ', STRING5
        call dm_modbus_register_parse(STRING5, register, error=rc)
        if (rc /= E_TYPE) return

        print *, '(6) ', STRING6
        call dm_modbus_register_parse(STRING6, register, error=rc)
        if (rc /= E_FORMAT) return

        stat = TEST_PASSED
    end function test03
end program dmtestmodbus
