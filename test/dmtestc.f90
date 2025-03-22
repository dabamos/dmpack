! dmtestc.f90
!
! Author:  Philipp Engel
! Licence: ISC
program dmtestc
    !! Test program for C interoperability.
    use, intrinsic :: iso_fortran_env, only: compiler_options, compiler_version
    use :: dmpack
    implicit none (type, external)

    character(len=*), parameter :: TEST_NAME = 'dmtestc'
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

        print *, 'Converting logical values ...'
        if (.not. dm_c_f_logical(1))      return
        if (dm_c_f_logical(0))            return
        if (dm_f_c_logical(.true.)  /= 1) return
        if (dm_f_c_logical(.false.) /= 0) return

        stat = TEST_PASSED
    end function test01
end program dmtestc
