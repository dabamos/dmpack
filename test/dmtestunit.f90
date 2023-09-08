! dmtestunit.f90
!
! Author:  Philipp Engel
! Licence: ISC
program dmtestunit
    use :: dmpack
    implicit none (type, external)
    integer, parameter :: NTESTS = 1

    type(test_type) :: tests(NTESTS)
    logical         :: stats(NTESTS)

    tests(1) = test_type('dmtestunit.test01', test01)

    call dm_init()
    call dm_test_run(tests, stats, dm_env_has('NO_COLOR'))
contains
    logical function test01() result(stat)
        real(kind=r8), parameter :: A = PI / 2.0_r8
        real(kind=r8), parameter :: B = 100.0_r8
        real(kind=r8), parameter :: C = 90.0_r8

        stat = TEST_FAILED

        print *, 'GON to RAD ...'
        if (.not. dm_equals(A, dm_gon_to_rad(B))) return

        print *, 'DEG to RAD ...'
        if (.not. dm_equals(A, dm_deg_to_rad(C))) return

        print *, 'RAD to GON ...'
        if (.not. dm_equals(B, dm_rad_to_gon(A))) return

        print *, 'DEG to GON ...'
        if (.not. dm_equals(B, dm_deg_to_gon(C))) return

        print *, 'RAD to DEG ...'
        if (.not. dm_equals(C, dm_rad_to_deg(A))) return

        print *, 'GON to DEG ...'
        if (.not. dm_equals(C, dm_gon_to_deg(B))) return

        stat = TEST_PASSED
    end function test01
end program dmtestunit
