! dmtesttransform.f90
!
! Author:  Philipp Engel
! Licence: ISC
program dmtesttransform
    use, intrinsic :: iso_fortran_env, only: compiler_options, compiler_version
    use :: dmpack
    implicit none (type, external)

    character(len=*), parameter :: TEST_NAME = 'dmtesttransform'
    integer,          parameter :: NTESTS    = 1

    type(test_type) :: tests(NTESTS)
    logical         :: stats(NTESTS)

    tests = [ &
        test_type('test01', test01) &
    ]

    call dm_init()
    call dm_test_run(TEST_NAME, tests, stats, dm_env_has('NO_COLOR'), compiler_version(), compiler_options())
contains
    logical function test01() result(stat)
        real(kind=r8), parameter :: REF_R     = 1.7320508075688772_r8
        real(kind=r8), parameter :: REF_OMEGA = 0.9553166181245092_r8
        real(kind=r8), parameter :: REF_PHI   = 0.7853981633974484_r8
        real(kind=r8), parameter :: REF_X     = 1.0_r8
        real(kind=r8), parameter :: REF_Y     = 1.0_r8
        real(kind=r8), parameter :: REF_Z     = 1.0_r8

        real(kind=r8) :: r, omega, phi
        real(kind=r8) :: x, y, z
        real(kind=r8) :: c(3), p(3)

        stat = TEST_FAILED

        print *, 'Testing cartesian to polar transformation (array) ...'

        c = [ REF_X, REF_Y, REF_Z ]

        call dm_transform_cartesian_to_polar_3d(c, p)

        print '(" r:     ", f8.6)', p(1)
        print '(" omega: ", f8.6)', p(2)
        print '(" phi:   ", f8.6)', p(3)
        print '(" x:     ", f8.6)', c(1)
        print '(" y:     ", f8.6)', c(2)
        print '(" z:     ", f8.6)', c(3)

        if (.not. dm_equals(p(1), REF_R)     .or. &
            .not. dm_equals(p(2), REF_OMEGA) .or. &
            .not. dm_equals(p(3), REF_PHI)) return

        print *, 'Testing cartesian to polar transformation (scalar) ...'

        x = REF_X
        y = REF_Y
        z = REF_Z

        call dm_transform_cartesian_to_polar_3d(x, y, z, r, omega, phi)

        print '(" r:     ", f8.6)', r
        print '(" omega: ", f8.6)', omega
        print '(" phi:   ", f8.6)', phi
        print '(" x:     ", f8.6)', x
        print '(" y:     ", f8.6)', y
        print '(" z:     ", f8.6)', z

        if (.not. dm_equals(r,     REF_R)     .or. &
            .not. dm_equals(omega, REF_OMEGA) .or. &
            .not. dm_equals(phi,   REF_PHI)) return

        print *, 'Testing polar to cartesian transformation (array) ...'

        p = [ REF_R, REF_OMEGA, REF_PHI ]

        call dm_transform_polar_to_cartesian_3d(p, c)

        print '(" r:     ", f8.6)', p(1)
        print '(" omega: ", f8.6)', p(2)
        print '(" phi:   ", f8.6)', p(3)
        print '(" x:     ", f8.6)', c(1)
        print '(" y:     ", f8.6)', c(2)
        print '(" z:     ", f8.6)', c(3)

        if (.not. dm_equals(c(1), REF_X) .or. &
            .not. dm_equals(c(2), REF_Y) .or. &
            .not. dm_equals(c(3), REF_Z)) return

        print *, 'Testing polar to cartesian transformation (scalar) ...'

        r     = REF_R
        omega = REF_OMEGA
        phi   = REF_PHI

        call dm_transform_polar_to_cartesian_3d(r, omega, phi, x, y, z)

        print '(" r:     ", f8.6)', r
        print '(" omega: ", f8.6)', omega
        print '(" phi:   ", f8.6)', phi
        print '(" x:     ", f8.6)', x
        print '(" y:     ", f8.6)', y
        print '(" z:     ", f8.6)', z

        if (.not. dm_equals(x, REF_X) .or. &
            .not. dm_equals(y, REF_Y) .or. &
            .not. dm_equals(z, REF_Z)) return

        stat = TEST_PASSED
    end function test01
end program dmtesttransform
