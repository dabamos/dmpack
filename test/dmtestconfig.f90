! dmtestconfig.f90
!
! Author:  Philipp Engel
! Licence: ISC
program dmtestconfig
    !! Test program for configuration file loading.
    use :: dmpack
    implicit none (type, external)

    character(len=*), parameter :: TEST_NAME   = 'dmtestconfig'
    character(len=*), parameter :: CONFIG_FILE = 'test/test.lua'
    integer,          parameter :: NTESTS      = 2

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
        character(len=32) :: str
        integer           :: i, rc
        logical           :: l
        real(kind=r8)     :: r
        type(config_type) :: config

        stat = TEST_FAILED

        print *, 'Loading ' // CONFIG_FILE // ' ...'
        rc = dm_config_open(config, CONFIG_FILE, 'dmtestconfig')

        print *, 'Reading configuration ...'
        config_if: if (dm_is_ok(rc)) then
            call dm_config_get(config, 'string', str, error=rc)
            if (dm_is_error(rc)) exit config_if

            call dm_config_get(config, 'integer', i, error=rc)
            if (dm_is_error(rc)) exit config_if

            call dm_config_get(config, 'logical', l, error=rc)
            if (dm_is_error(rc)) exit config_if

            call dm_config_get(config, 'real', r, error=rc)
            if (dm_is_error(rc)) exit config_if
        end if config_if

        call dm_config_close(config)

        call dm_error_out(rc)
        if (dm_is_error(rc)) return

        print *, 'Validating ...'
        if (str /= 'a\r\n') return
        if (i /= 420) return
        if (.not. l) return
        if (.not. dm_equals(1.0_r8, r)) return

        stat = TEST_PASSED
    end function test01

    logical function test02() result(stat)
        integer                                 :: rc
        type(config_type)                       :: config
        type(modbus_register_type), allocatable :: registers(:)

        stat = TEST_FAILED

        print *, 'Loading ' // CONFIG_FILE // ' ...'
        rc = dm_config_open(config, CONFIG_FILE, 'dmtestconfig')

        print *, 'Reading Modbus configuration ...'
        if (dm_is_ok(rc)) call dm_config_get(config, 'registers', registers, error=rc)
        call dm_config_close(config)

        call dm_error_out(rc)
        if (dm_is_error(rc)) return

        print *, 'Validating ...'

        if (.not. allocated(registers)) return
        if (size(registers) /= 2)       return

        print *, 'Modbus register 1 ...'
        call dm_modbus_register_out(registers(1))
        if (registers(1)%access /= MODBUS_ACCESS_READ) return
        if (registers(1)%slave /= 10)                  return
        if (registers(1)%address /= 50)                return
        if (registers(1)%type /= MODBUS_TYPE_FLOAT)    return
        if (registers(1)%order /= MODBUS_ORDER_ABCD)   return

        print *, 'Modbus register 2 ...'
        call dm_modbus_register_out(registers(2))
        if (registers(2)%name /= 'test')                return
        if (registers(2)%unit /= 'none')                return
        if (registers(2)%access /= MODBUS_ACCESS_WRITE) return
        if (registers(2)%slave /= 99)                   return
        if (registers(2)%address /= 10)                 return
        if (registers(2)%type /= MODBUS_TYPE_INT32)     return
        if (registers(2)%value /= 16)                   return

        stat = TEST_PASSED
    end function test02
end program dmtestconfig
