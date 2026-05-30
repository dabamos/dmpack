! dmtestrandom.f90
!
! Author:  Philipp Engel
! Licence: ISC
program dmtestrandom
    !! Test program for random numbers.
    use :: dmpack
    implicit none (type, external)

    character(*), parameter :: TEST_NAME = 'dmtestrandom'
    integer,      parameter :: NTESTS    = 3

    type(test_type) :: tests(NTESTS)
    logical         :: stats(NTESTS)

    tests = [ &
        test_type('test01', test01), &
        test_type('test02', test02), &
        test_type('test03', test03)  &
    ]

    call dm_init()
    call dm_test_run(TEST_NAME, tests, stats)
contains
    logical function test01() result(stat)
        real(r8) :: x
        real(r8) :: v(5)

        stat = TEST_FAILED

        print *, 'Testing uniform real routines ...'

        call dm_random_uniform(x, 10.0_r8, 20.0_r8)
        call dm_random_uniform(v, -1.0_r8,  1.0_r8)

        print '("Scalar: ", f20.16)', x
        print '("Array:")'
        print '(*(f20.16, /))', v

        stat = TEST_PASSED
    end function test01

    logical function test02() result(stat)
        integer(i8) :: x
        integer(i8) :: v(5)

        stat = TEST_FAILED

        print *, 'Testing uniform integer routines ...'

        call dm_random_uniform(x, 10_i8, 20_i8)
        call dm_random_uniform(v, -1_i8,  1_i8)

        print '("Scalar: ", i0)', x
        print '("Array:")'
        print '(*(i3, /))', v

        stat = TEST_PASSED
    end function test02

    logical function test03() result(stat)
        integer :: i

        stat = TEST_FAILED

        print *, 'Testing random number functions ...'

        do i = 1, 3
            print '(" [", i1, "] ", f12.4)', i, dm_random_get()
            print '(" [", i1, "] ", i12)',   i, dm_random_get_uniform(      -10_i4,       10_i4)
            print '(" [", i1, "] ", i12)',   i, dm_random_get_uniform(  -100000_i8,   100000_i8)
            print '(" [", i1, "] ", f12.4)', i, dm_random_get_uniform(    -10.0_r4,     10.0_r4)
            print '(" [", i1, "] ", f12.4)', i, dm_random_get_uniform(-100000.0_r8, 100000.0_r8)
        end do

        stat = TEST_PASSED
    end function test03
end program dmtestrandom
