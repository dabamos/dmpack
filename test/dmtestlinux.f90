! dmtestlinux.f90
!
! Author:  Philipp Engel
! Licence: ISC
program dmtestlinux
    !! Test program of Linux API module.
    use, intrinsic :: iso_fortran_env, only: compiler_options, compiler_version
    use :: dmpack
    implicit none (type, external)

    character(len=*), parameter :: TEST_NAME = 'dmtestlinux'
    integer,          parameter :: NTESTS    = 1

    logical         :: no_color
    logical         :: stats(NTESTS)
    type(test_type) :: tests(NTESTS)

    no_color = dm_env_has('NO_COLOR')

    tests = [ &
        test_type('test01', test01) &
    ]

    call dm_init()
    call dm_test_run(TEST_NAME, tests, stats, compiler_version(), compiler_options())
contains
    logical function test01() result(stat)
        integer :: rc

        stat = TEST_FAILED

        if (dm_system_type() /= SYSTEM_TYPE_LINUX) then
            stat = TEST_PASSED
            call dm_ansi_color(COLOR_YELLOW, no_color)
            print '("> This test is for Linux only and will be skipped.")'
            call dm_ansi_reset(no_color)
            return
        end if

        io_block: block
            character(len=64) :: model
            real              :: avgs(3)

            rc = dm_linux_procfs_hardware_model(model)
            if (dm_is_error(rc)) exit io_block

            rc = dm_linux_procfs_load_average(avgs(1), avgs(2), avgs(3))
            if (dm_is_error(rc)) exit io_block

            print '(" hardware model: ", a)',          trim(model)
            print '(" load averages.:", 3(1x, f3.2))', avgs
        end block io_block

        call dm_error_out(rc)
        if (dm_is_error(rc)) return

        stat = TEST_PASSED
    end function test01
end program dmtestlinux
