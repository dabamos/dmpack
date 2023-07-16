! Author:  Philipp Engel
! Licence: ISC
module dm_test
    !! Basic testing framework.
    use :: dm_ansi
    use :: dm_error
    use :: dm_time
    use :: dm_timer
    use :: dm_type
    use :: dm_version
    implicit none (type, external)
    private

    integer, parameter, public :: TEST_LINE_LEN = 72
    integer, parameter, public :: TEST_NAME_LEN = 32

    ! States of a test.
    integer, parameter, public :: TEST_STATE_UNKNOWN = 0
    integer, parameter, public :: TEST_STATE_RUNNING = 1
    integer, parameter, public :: TEST_STATE_PASSED  = 2
    integer, parameter, public :: TEST_STATE_FAILED  = 3

    ! Test results.
    logical, parameter, public :: TEST_PASSED = .true.
    logical, parameter, public :: TEST_FAILED = .false.

    character(len=*), parameter :: TEST_RULE        = repeat('=', TEST_LINE_LEN)
    character(len=*), parameter :: TEST_STATES(0:3) = [ 'UNKNOWN', 'RUNNING', ' PASSED', ' FAILED' ]
    integer,          parameter :: TEST_COLORS(0:3) = [ COLOR_WHITE, COLOR_YELLOW, COLOR_GREEN, COLOR_RED ]

    abstract interface
        logical function dm_test_function()
            !! Abstract test function that either returns `TEST_PASSED` or `TEST_FAILED`.
        end function dm_test_function
    end interface

    type, public :: test_type
        !! Test type.
        character(len=TEST_NAME_LEN)                 :: name !! Test name.
        procedure(dm_test_function), pointer, nopass :: proc !! Test procedure.
    end type test_type

    public :: dm_test_function
    public :: dm_test_run
contains
    ! ******************************************************************
    ! PUBLIC PROCEDURES.
    ! ******************************************************************
    subroutine dm_test_run(tests, stats, no_color)
        !! Runs all tests in given array `tests`, returns test states in array
        !! `stats`.
        use, intrinsic :: iso_fortran_env, only: compiler_options, compiler_version

        type(test_type), intent(inout)        :: tests(:) !! Test types.
        logical,         intent(out)          :: stats(:) !! `TEST_FAILED` or `TEST_PASSED`.
        logical,         intent(in), optional :: no_color

        integer          :: i, n, nfail, npass, state
        logical          :: no_color_
        real(kind=r8)    :: time, total_time
        type(timer_type) :: timer

        n = size(tests)
        if (n == 0) return

        no_color_ = .false.
        if (present(no_color)) no_color_ = no_color

        call test_title('TEST SESSION STARTS', TEST_LINE_LEN)
        print '("DMPACK..: ", a)',    DM_VERSION_STRING
        print '("Time....: ", a)',    dm_time_now()
        print '("Compiler: ", a)',    compiler_version()
        print '("Options.: ", a, /)', compiler_options()
        print '("Running ", i0, " test(s) ...", /)', n

        total_time = 0.0
        print '(a)', TEST_RULE

        do i = 1, n
            call test_print(i, n, tests(i)%name, TEST_STATE_RUNNING, no_color=no_color_)

            call dm_timer_start(timer)
            stats(i) = tests(i)%proc()
            time = dm_timer_stop(timer)
            total_time = total_time + time

            state = TEST_STATE_FAILED
            if (stats(i) .eqv. TEST_PASSED) state = TEST_STATE_PASSED

            call test_print(i, n, tests(i)%name, state, time, no_color=no_color_)
            print '(a)', TEST_RULE
        end do

        call test_title('TEST SUMMARY', TEST_LINE_LEN, '=')

        npass = count(stats)
        call dm_ansi_color(COLOR_GREEN, no_color_)
        print '(i0, " passed")', npass
        call dm_ansi_reset(no_color_)

        nfail = n - npass
        if (nfail > 0) call dm_ansi_color(COLOR_RED, no_color_)
        print '(i0, " failed")', nfail
        call dm_ansi_reset(no_color_)

        print '("Total execution time: ", f8.4, " sec")', total_time

        call test_title('TEST SESSION FINISHED', TEST_LINE_LEN, '=')

        if (nfail > 0) call dm_stop(1)
    end subroutine dm_test_run

    ! ******************************************************************
    ! PRIVATE PROCEDURES.
    ! ******************************************************************
    subroutine test_print(index, ntests, name, state, time, no_color)
        !! Outputs test states.
        character(len=*), parameter :: FMT_STATE = '("[TEST ",i2,"/",i2,"] ",a,20x,a)'
        character(len=*), parameter :: FMT_TIME  = '("[TEST ",i2,"/",i2,"] ",a," in ",f8.4," sec.",3x,a)'

        integer,          intent(in)           :: index    !! Test number.
        integer,          intent(in)           :: ntests   !! Number of tests.
        character(len=*), intent(in)           :: name     !! Test name.
        integer,          intent(in)           :: state    !! Test state.
        real(kind=r8),    intent(in), optional :: time     !! Test duration.
        logical,          intent(in), optional :: no_color !! No ANSI colours.

        logical :: no_color_

        no_color_ = .false.
        if (present(no_color)) no_color_ = no_color

        call dm_ansi_color(TEST_COLORS(state), no_color_)
        if (present(time)) then
            write (*, FMT_TIME)  index, ntests, name, time, TEST_STATES(state)
        else
            write (*, FMT_STATE) index, ntests, name, TEST_STATES(state)
        end if
        call dm_ansi_reset(no_color_)
    end subroutine test_print

    subroutine test_title(text, length, ch)
        !! Prints a header with given `test` and line length `len`.
        character(len=*), intent(in)           :: text   !! Title text.
        integer,          intent(in)           :: length !! Line length.
        character,        intent(in), optional :: ch     !! Optional line character.

        character :: a
        integer   :: i, j, k

        a = '*'
        if (present(ch)) a = ch

        i = length - len_trim(text) - 2
        j = i / 2
        k = modulo(i, 2)

        write (*, '(/, a, 1x)', advance='no') repeat(a, j)
        write (*, '(a, 1x)',    advance='no') trim(text)
        write (*, '(a)')                      repeat(a, j + k)
    end subroutine test_title
end module dm_test
