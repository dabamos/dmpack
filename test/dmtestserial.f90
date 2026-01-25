! dmtestserial.f90
!
! Author:  Philipp Engel
! Licence: ISC
program dmtestserial
    !! Test program derived type serialisation.
    use, intrinsic :: iso_fortran_env, only: compiler_options, compiler_version
    use :: dmpack
    implicit none (type, external)

    character(len=*), parameter :: TEST_NAME = 'dmtestserial'
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
        character(len=2048) :: scratch
        integer             :: i, ios, j, n, unit
        integer             :: formats(5)
        type(sensor_type)   :: sensors(3)
        type(serial_class)  :: serial

        stat = TEST_FAILED

        formats = [ FORMAT_CSV, FORMAT_JSON, FORMAT_JSONL, FORMAT_NML, FORMAT_TSV ]

        do i = 1, size(formats)
            open (action='readwrite', iostat=ios, newunit=unit, status='scratch')
            if (ios /= 0) return

            print *, 'Serialising to format ' // trim(FORMAT_NAMES(formats(i))) // ' ...'
            print '(72("."))'

            call serial%create(sensors(1), formats(i), callback=test_callback, unit=unit, header=.true.)

            do j = 1, size(sensors)
                call dm_test_dummy(sensors(j), id='sensor-' // dm_itoa(j))
                call serial%next(sensors(j))
            end do

            call serial%finalize()
            print '(/, 72("."))'

            print *, 'Reading from scratch file ...'
            print '(72("."))'

            rewind (unit)

            n = size(sensors)
            if (formats(i) == FORMAT_CSV)  n = n + 1
            if (formats(i) == FORMAT_JSON) n = 1

            do j = 1, n
                read (unit, '(a)', iostat=ios) scratch
                if (ios /= 0 .or. len_trim(scratch) == 0) exit
                print '(a)', trim(scratch)
            end do

            print '(72("."))'
            close (unit)
        end do

        stat = TEST_PASSED
    end function test01

    logical function test02() result(stat)
        character(len=*), parameter :: ASSERT = &
            '[{"id":"dummy-sensor","node_id":"dummy-node","type":1,"name":"Dummy Sensor","sn":"12345",'     // &
            '"meta":"dummy description","x":1000.00000000,"y":2000.00000000,"z":100.000000000,"longitude":' // &
            '0.00000000000,"latitude":0.00000000000,"elevation":0.00000000000}]'

        character(len=2048) :: scratch
        integer             :: ios, unit
        type(sensor_type)   :: sensor
        type(serial_class)  :: serial

        stat = TEST_FAILED

        call dm_test_dummy(sensor)
        open (action='readwrite', iostat=ios, newunit=unit, status='scratch')
        if (ios /= 0) return

        print *, 'Serialising sensor ...'
        call serial%create(sensor, FORMAT_JSON, unit=unit)
        call serial%next(sensor)
        call serial%finalize()

        rewind (unit)
        read (unit, '(a)', iostat=ios) scratch
        close (unit)

        print '(72("."))'
        print '(a)', trim(scratch)
        print '(72("."))'

        print *, 'Validating ...'
        if (trim(scratch) /= ASSERT) return

        stat = TEST_PASSED
    end function test02

    logical function test03() result(stat)
        integer            :: rc
        type(sensor_type)  :: sensor
        type(serial_class) :: serial

        stat = TEST_FAILED

        test_block: block
            print *, 'Serialising empty sensors ...'
            call serial%create(sensor, FORMAT_JSON, callback=test_callback, empty=.true., error=rc)
            print *
            if (dm_is_error(rc)) exit test_block

            call serial%next(sensor, error=rc)
            if (rc /= E_EMPTY) exit test_block

            call serial%finalize(error=rc)
            if (dm_is_error(rc)) exit test_block
        end block test_block

        call dm_error_out(rc)
        if (dm_is_error(rc)) return

        stat = TEST_PASSED
    end function test03

    subroutine test_callback(string)
        character(len=*), intent(in) :: string

        write (*, '(a)', advance='no') string
    end subroutine test_callback
end program dmtestserial
