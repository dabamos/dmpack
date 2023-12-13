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
    integer, parameter :: NTESTS = 2

    character(len=:), allocatable :: env_host
    integer                       :: env_port

    logical         :: has_env
    logical         :: no_color
    logical         :: stats(NTESTS)
    type(test_type) :: tests(NTESTS)

    call dm_init()

    no_color = dm_env_has('NO_COLOR')
    has_env  = .true.

    if (dm_is_error(dm_env_get('DM_MQTT_HOST', env_host)) .or. &
        dm_is_error(dm_env_get('DM_MQTT_PORT', env_port))) then

        call dm_ansi_color(COLOR_RED, no_color)
        print '(/, "    Set the following environment variables to test MQTT")'
        print '("    connectivity:", /)'
        print '("        DM_MQTT_HOST - IP or FQDN of MQTT server.")'
        print '("        DM_MQTT_PORT - MQTT server port.", /)'
        print '("    Otherwise, some tests will be skipped.")'
        call dm_ansi_reset(no_color)

        has_env = .false.
    end if

    tests(1) = test_type('dmtestmqtt.test01', test01)
    tests(2) = test_type('dmtestmqtt.test02', test02)

    call dm_test_run(tests, stats, no_color)
contains
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
        character(len=:), allocatable :: error_message, url
        integer                       :: rc

        if (.not. has_env) then
            stat = TEST_PASSED
            print *, 'Skipping test ...'
            return
        end if

        stat = TEST_FAILED

        url = dm_mqtt_url(host=env_host, topic='/dmpack', port=env_port)

        print *, 'Publishing message on ' // url // ' ...'
        rc = dm_mqtt_publish(url, 'DMPACK', error_message=error_message)

        if (dm_is_error(rc)) then
            call dm_error_out(rc, error_message)
            return
        end if

        stat = TEST_PASSED
    end function test02
end program dmtestmqtt
