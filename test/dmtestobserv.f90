! dmtestobserv.f90
!
! Author:  Philipp Engel
! Licence: ISC
program dmtestobserv
    !! Test program that validates observation and response data handling,
    use, intrinsic :: iso_fortran_env, only: compiler_options, compiler_version
    use :: dmpack
    implicit none (type, external)

    character(*), parameter :: TEST_NAME = 'dmtestobserv'
    integer,      parameter :: NTESTS    = 4

    type(test_type) :: tests(NTESTS)
    logical         :: stats(NTESTS)

    tests = [ &
        test_type('test01', test01), &
        test_type('test02', test02), &
        test_type('test03', test03), &
        test_type('test04', test04)  &
    ]

    call dm_init()
    call dm_test_run(TEST_NAME, tests, stats, compiler_version(), compiler_options())
contains
    logical function test01() result(stat)
        stat = TEST_PASSED
        print '(" Observation size: ", i0)', OBSERV_TYPE_SIZE
    end function test01

    logical function test02() result(stat)
        integer             :: rc
        type(observ_type)   :: observ1, observ2
        type(response_type) :: response

        stat = TEST_FAILED

        print *, 'Creating new observation ...'

        observ1%id        = '9273ab62f9a349b6a4da6dd274ee83e7'
        observ1%node_id   = 'dummy-node'
        observ1%sensor_id = 'dummy-sensor'
        observ1%target_id = 'dummy-target'
        observ1%name      = 'dummy-observ'
        observ1%timestamp = TIME_DEFAULT
        observ1%source    = 'dmtestobserv'
        observ1%device    = '/dev/null'
        observ1%request   = 'A'
        observ1%response  = dm_ascii_escape('123.45' // ASCII_CR // ASCII_LF)
        observ1%delimiter = dm_ascii_escape(ASCII_CR // ASCII_LF)
        observ1%pattern   = '^(.*)$'
        observ1%delay     = 1000
        observ1%error     = E_NONE
        observ1%retries   = 0
        observ1%timeout   = 500

        print *, 'Adding receivers ...'
        rc = dm_observ_add_receiver(observ1, 'dummy-receiver1'); if (dm_is_error(rc)) return
        rc = dm_observ_add_receiver(observ1, 'dummy-receiver2'); if (dm_is_error(rc)) return
        rc = dm_observ_add_receiver(observ1, 'dummy-receiver3'); if (dm_is_error(rc)) return

        print *, 'Adding response ...'
        response = response_type('a', 'none', RESPONSE_TYPE_REAL64, E_NONE, 123.45_r8)
        rc = dm_observ_add_response(observ1, response); if (dm_is_error(rc)) return

        print *, 'Printing observation ...'
        print '(72("."))'
        call dm_observ_out(observ1)
        print '(72("."))'

        print *, 'Validating observation ...'
        observ2 = observ1
        if (.not. (observ1 == observ2)) return

        stat = TEST_PASSED
    end function test02

    logical function test03() result(stat)
        integer           :: rc
        integer(i4)       :: ival4
        integer(i8)       :: ival8
        logical           :: lval1
        real(r4)          :: rval4
        real(r8)          :: rval8
        type(observ_type) :: observ

        stat = TEST_FAILED

        ival4 = 1_i4
        ival8 = 1_i8
        lval1 = .true.
        rval4 = 1.0_r4
        rval8 = 1.0_r8

        print *, 'Setting responses ...'
        observ%nresponses = 5

        call dm_response_set(observ%responses(1), name='ival4', value=dm_to_real64(ival4))
        call dm_response_set(observ%responses(2), name='ival8', value=dm_to_real64(ival8))
        call dm_response_set(observ%responses(3), name='lval1', value=dm_to_real64(lval1))
        call dm_response_set(observ%responses(4), name='rval4', value=dm_to_real64(rval4))
        call dm_response_set(observ%responses(5), name='rval8', value=rval8)

        print *, 'Validating index ...'
        if (dm_observ_find(observ, 'ival4') /= 1) return

        print *, 'Getting responses ...'
        rc = dm_observ_get_response(observ, 'ival4', ival4)
        rc = dm_observ_get_response(observ, 'ival8', ival8)
        rc = dm_observ_get_response(observ, 'lval1', lval1)
        rc = dm_observ_get_response(observ, 'rval4', rval4)
        rc = dm_observ_get_response(observ, 'rval8', rval8)

        print *, 'Validating responses ...'
        if (ival4 /= 1_i4) return
        if (ival8 /= 1_i8) return
        if (.not. lval1) return
        if (.not. dm_equals(rval4, 1.0_r4)) return
        if (.not. dm_equals(rval8, 1.0_r8)) return

        print *, 'Calling get routine ...'
        ival4 = huge(0_i4)
        rc = dm_observ_get_response(observ, 'invalid', ival4)
        if (rc /= E_NOT_FOUND .or. ival4 /= huge(0_i4)) return

        observ%nresponses = 0
        rc = dm_observ_get_response(observ, 'invalid', ival4)
        if (rc /= E_EMPTY) return

        stat = TEST_PASSED
    end function test03

    logical function test04() result(stat)
        character(OBSERV_NAME_LEN) :: name
        character(TIME_LEN)        :: timestamp
        integer                    :: delay, i
        type(observ_type)          :: observs(2)

        stat = TEST_FAILED

        name      = 'test'
        timestamp = dm_time_now()
        delay     = dm_sec_to_msec(60)

        print *, 'Testing request setter routine ...'
        call dm_observ_set(observs, name=name, timestamp=timestamp, delay=delay)

        do i = 1, size(observs)
            if (observs(i)%name      /= name)       return
            if (observs(i)%timestamp /= timestamp)  return
            if (observs(i)%delay     /= delay)      return
        end do

        stat = TEST_PASSED
    end function test04
end program dmtestobserv
