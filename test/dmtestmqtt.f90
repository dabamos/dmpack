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

    type(test_type) :: tests(NTESTS)
    logical         :: stats(NTESTS)
    logical         :: no_color

    call dm_init()

    no_color = dm_env_has('NO_COLOR')
    tests(1) = test_type('dmtestmqtt%dm_test01', dm_test01)
    tests(2) = test_type('dmtestmqtt%dm_test02', dm_test02)

    call dm_test_run(tests, stats, no_color)
contains
    logical function dm_test01() result(stat)
        character(len=*), parameter :: URL1 = 'mqtt://127.0.0.1/dmpack'
        character(len=*), parameter :: URL2 = 'mqtt://127.0.0.1:1883/dmpack'

        character(len=:), allocatable :: url

        stat = TEST_FAILED

        print *, 'Validating URLs ...'
        url = dm_mqtt_url('127.0.0.1', 0, '/dmpack')
        print *, url
        if (url /= URL1) return

        url = dm_mqtt_url('127.0.0.1', 0, 'dmpack')
        print *, url
        if (url /= URL1) return

        url = dm_mqtt_url('127.0.0.1', 1883, '/dmpack')
        print *, url
        if (url /= URL2) return

        stat = TEST_PASSED
    end function dm_test01

    logical function dm_test02() result(stat)
        character(len=:), allocatable :: env_host
        integer                       :: env_port

        character(len=:), allocatable :: error_message, url
        integer                       :: rc

        stat = TEST_FAILED

        if (dm_is_error(dm_env_get('DM_MQTT_HOST', env_host)) .or. &
            dm_is_error(dm_env_get('DM_MQTT_PORT', env_port))) then

            call dm_ansi_color(COLOR_RED, no_color)
            print '(/, "    Set the following environment variables to test MQTT")'
            print '("    connectivity:", /)'
            print '("        DM_MQTT_HOST - IP or FQDN of MQTT server.")'
            print '("        DM_MQTT_PORT - MQTT server port.", /)'
            print '("    Otherwise, this test will be skipped.", /)'
            call dm_ansi_reset(no_color)

            stat = TEST_PASSED
            return
        end if

        url = dm_mqtt_url(host=env_host, port=env_port, topic='/dmpack')

        print *, 'Publishing message on ' // url // ' ...'
        rc = dm_mqtt_publish(url, 'DMPACK', error_message=error_message)

        if (dm_is_error(rc)) then
            call dm_error_out(rc, error_message)
            return
        end if

        stat = TEST_PASSED
    end function dm_test02
end program dmtestmqtt
