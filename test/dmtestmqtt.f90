! dmtestmqtt.f90
!
! Author:  Philipp Engel
! Licence: ISC
program dmtestmqtt
    !! Test program for MQTT connectivity.
    !!
    !! To run this program, set the following environment variables
    !! beforehand:
    !!
    !!      DM_MQTT_HOST - IP or FQDN of MQTT server.
    !!      DM_MQTT_PORT - MQTT server port.
    !!
    !! Some tests may be skipped if these are not set.
    use :: dmpack
    implicit none (type, external)

    character(len=*), parameter :: TEST_NAME = 'dmtestmqtt'
    integer,          parameter :: NTESTS    = 2

    logical         :: no_color
    logical         :: stats(NTESTS)
    type(test_type) :: tests(NTESTS)

    no_color = dm_env_has('NO_COLOR')

    tests = [ &
        test_type('test01', test01), &
        test_type('test02', test02)  &
    ]

    call dm_init()
    call dm_test_run(TEST_NAME, tests, stats, no_color)
contains
    logical function get_env(host, port) result(has)
        character(len=:), allocatable, intent(out) :: host
        integer,                       intent(out) :: port

        has = .false.

        if (dm_is_error(dm_env_get('DM_MQTT_HOST', host)) .or. &
            dm_is_error(dm_env_get('DM_MQTT_PORT', port))) then
            call dm_ansi_color(COLOR_YELLOW, no_color)
            print '("> Set environment variables DM_MQTT_HOST and DM_MQTT_PORT.")'
            print '("> This test will be skipped.")'
            call dm_ansi_reset(no_color)
            return
        end if

        has = .true.
    end function get_env

    logical function test01() result(stat)
        character(len=*), parameter :: URL1 = 'mqtt://127.0.0.1/dmpack'
        character(len=*), parameter :: URL2 = 'mqtt://127.0.0.1:1883/dmpack'
        character(len=*), parameter :: URL3 = 'mqtt://127.0.0.1:9999/dmpack'

        character(len=:), allocatable :: url

        stat = TEST_FAILED

        print *, 'Validating URLs ...'
        url = dm_mqtt_url('127.0.0.1', '/dmpack')
        print *, 'URL 1: ', url
        if (url /= URL1) return

        url = dm_mqtt_url('127.0.0.1', '/dmpack', 1883)
        print *, 'URL 2: ', url
        if (url /= URL2) return

        url = dm_mqtt_url('127.0.0.1', '/dmpack', 9999)
        print *, 'URL 3: ', url
        if (url /= URL3) return

        stat = TEST_PASSED
    end function test01

    logical function test02() result(stat)
        character(len=:), allocatable :: error_message, host, url
        integer                       :: port, rc

        stat = TEST_PASSED
        if (.not. get_env(host, port)) return

        stat = TEST_FAILED
        url = dm_mqtt_url(host=host, topic='/dmpack', port=port)

        print *, 'Publishing message on ' // url // ' ...'
        rc = dm_mqtt_publish(url, 'DMPACK', error_message=error_message)

        if (dm_is_error(rc)) then
            call dm_error_out(rc, error_message)
            return
        end if

        stat = TEST_PASSED
    end function test02
end program dmtestmqtt
