! dmtestmail.f90
!
! Author:  Philipp Engel
! Licence: ISC
program dmtestmail
    !! Test program for sending e-mails.
    !!
    !! To run this program, set the following environment variables
    !! beforehand:
    !!
    !!      DM_MAIL_FROM     - Address of sender.
    !!      DM_MAIL_TO       - Address of recipient.
    !!      DM_MAIL_HOST     - SMTP host name (example.com).
    !!      DM_MAIL_USERNAME - SMTP user name.
    !!      DM_MAIL_PASSWORD - SMTP password.
    !!
    !! Some tests may be skipped if these are not set.
    use :: dmpack
    implicit none (type, external)
    integer, parameter :: NTESTS = 2

    type(test_type) :: tests(NTESTS)
    logical         :: stats(NTESTS)
    logical         :: no_color

    tests(1) = test_type('dmtestmail.test01', test01)
    tests(2) = test_type('dmtestmail.test02', test02)

    call dm_init()
    no_color = dm_env_has('NO_COLOR')

    call dm_test_run(tests, stats, no_color)
contains
    logical function test01() result(stat)
        !! Mock-testing of mail procedures.
        character(len=:), allocatable :: url
        integer                       :: rc

        type(mail_server_type) :: server
        type(mail_type)        :: mail
        type(person_type)      :: person
        type(person_type)      :: persons(2)

        stat = TEST_FAILED

        print *, 'Creating persons ...'
        person%name = 'Jane Doe'
        person%mail = 'jane.doe@local'

        if (.not. dm_person_has_name(person)) return
        if (.not. dm_person_has_mail(person)) return

        persons(1)%name = 'Alice'
        persons(1)%mail = 'alice@local'
        persons(2)%name = 'Bob'
        persons(2)%mail = 'bob@local'

        mail_block: block
            print *, 'Validating address data ...'
            print *, dm_mail_address(person)
            if (dm_mail_address(person) /= '"Jane Doe" <jane.doe@local>') return

            print *, dm_mail_address(persons)
            if (dm_mail_address(persons) /= '"Alice" <alice@local>, "Bob" <bob@local>') return

            print *, 'Creating URLs ...'
            url = dm_mail_url('local', 0, MAIL_PLAIN)
            print *, url
            if (url /= 'smtp://local') return
            deallocate (url)

            url = dm_mail_url('local', 999, MAIL_SSL)
            print *, url
            if (url /= 'smtps://local:999') return
            deallocate (url)

            url = dm_mail_url('local', 587, MAIL_TLS)
            print *, url
            if (url /= 'smtp://local:587') return
            deallocate (url)

            print *, 'Creating server ...'
            rc = dm_mail_create(server, 'example.com', 'user', 'secret')
            if (dm_is_error(rc)) exit mail_block

            print *, 'Creating mail ...'
            rc = dm_mail_create_mail(mail, person, persons, 'subject', 'message')
            if (dm_is_error(rc)) exit mail_block
        end block mail_block

        call dm_error_out(rc)

        stat = TEST_PASSED
    end function test01

    logical function test02() result(stat)
        !! Sends e-mail via SMTP, using parameters passed via environment
        !! variables.
        logical,          parameter :: DEBUG   = .true. !! Enable cURL debug output.
        character(len=*), parameter :: SUBJECT = 'DMPACK Test Message'
        character(len=*), parameter :: MESSAGE = &
            'Now is the time for all good men to come to the aid of the party.'

        character(len=:), allocatable :: env_from, env_to
        character(len=:), allocatable :: env_host, env_username, env_password
        character(len=:), allocatable :: error_message
        integer                       :: error_curl, rc

        type(mail_server_type) :: server
        type(mail_type)        :: mail
        type(person_type)      :: from
        type(person_type)      :: to(1)

        stat = TEST_FAILED

        if (dm_is_error(dm_env_get('DM_MAIL_FROM',     env_from))     .or. &
            dm_is_error(dm_env_get('DM_MAIL_TO',       env_to))       .or. &
            dm_is_error(dm_env_get('DM_MAIL_HOST',     env_host))     .or. &
            dm_is_error(dm_env_get('DM_MAIL_USERNAME', env_username)) .or. &
            dm_is_error(dm_env_get('DM_MAIL_PASSWORD', env_password))) then

            call dm_ansi_color(COLOR_RED, no_color)
            print '(/, "    Set the following environment variables to send an e-mail")'
            print '("    via SMTP:", /)'
            print '("        DM_MAIL_FROM     - Address of sender.")'
            print '("        DM_MAIL_TO       - Address of recipient.")'
            print '("        DM_MAIL_HOST     - SMTP host name (example.com).")'
            print '("        DM_MAIL_USERNAME - SMTP user name.")'
            print '("        DM_MAIL_PASSWORD - SMTP password.", /)'
            print '("    Otherwise, this test will be skipped. The server must support")'
            print '("    StartTLS.", /)'
            call dm_ansi_reset(no_color)

            stat = TEST_PASSED
            return
        end if

        mail_block: block
            print *, 'Creating server ...'
            rc = dm_mail_create(server, env_host, env_username, env_password, &
                                tls=MAIL_TLS, verify_ssl=.false.)
            if (dm_is_error(rc)) exit mail_block

            print *, 'Creating mail ...'
            from%mail  = env_from
            to(1)%mail = env_to

            rc = dm_mail_create_mail(mail, from, to, SUBJECT, MESSAGE)
            if (dm_is_error(rc)) exit mail_block

            print *, 'Sending mail ...'
            rc = dm_mail_send(mail, server, error_message, error_curl, debug=DEBUG)

            if (dm_is_error(rc)) then
                print *, error_curl, error_message
                exit mail_block
            end if
        end block mail_block

        call dm_error_out(rc)

        stat = TEST_PASSED
    end function test02
end program dmtestmail
