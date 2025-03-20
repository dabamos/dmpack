! dmtestconfig.f90
!
! Author:  Philipp Engel
! Licence: ISC
program dmtestconfig
    !! Test program for configuration file loading.
    use, intrinsic :: iso_fortran_env, only: compiler_options, compiler_version
    use :: dmpack
    implicit none (type, external)

    character(len=*), parameter :: TEST_NAME   = 'dmtestconfig'
    character(len=*), parameter :: CONFIG_FILE = 'test/test.lua'
    integer,          parameter :: NTESTS      = 1

    type(test_type) :: tests(NTESTS)
    logical         :: stats(NTESTS)

    tests = [ &
        test_type('test01', test01) &
    ]

    call dm_init()
    call dm_test_run(TEST_NAME, tests, stats, dm_env_has('NO_COLOR'), compiler_version(), compiler_options())
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
end program dmtestconfig
