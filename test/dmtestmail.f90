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
    use, intrinsic :: iso_fortran_env, only: compiler_options, compiler_version
    use :: dmpack
    implicit none (type, external)

    character(len=*), parameter :: TEST_NAME = 'dmtestmail'
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
    call dm_test_run(TEST_NAME, tests, stats, compiler_version(), compiler_options())
contains
    logical function get_env(from, to, host, username, password) result(has)
        character(len=:), allocatable, intent(out) :: from
        character(len=:), allocatable, intent(out) :: to
        character(len=:), allocatable, intent(out) :: host
        character(len=:), allocatable, intent(out) :: username
        character(len=:), allocatable, intent(out) :: password

        has = .false.

        if (dm_is_error(dm_env_get('DM_MAIL_FROM',     from))     .or. &
            dm_is_error(dm_env_get('DM_MAIL_TO',       to))       .or. &
            dm_is_error(dm_env_get('DM_MAIL_HOST',     host))     .or. &
            dm_is_error(dm_env_get('DM_MAIL_USERNAME', username)) .or. &
            dm_is_error(dm_env_get('DM_MAIL_PASSWORD', password))) then

            call dm_ansi_color(COLOR_YELLOW, no_color)
            print '("> Set environment variables DM_MAIL_FROM, DM_MAIL_TO, DM_MAIL_HOST,")'
            print '("> DM_MAIL_USERNAME, and DM_MAIL_PASSWORD. This test will be skipped.")'
            call dm_ansi_reset(no_color)
            return
        end if

        has = .true.
    end function get_env

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
            url = dm_mail_url('local', port=0, tls=.false.)
            print *, url
            if (url /= 'smtp://local/') return

            url = dm_mail_url('local', port=999, tls=.true.)
            print *, url
            if (url /= 'smtps://local:999/') return

            url = dm_mail_url('local', port=587, tls=.false.)
            print *, url
            if (url /= 'smtp://local:587/') return

            print *, 'Creating server ...'
            rc = dm_mail_create(server, 'example.com', 'user', 'secret')
            if (dm_is_error(rc)) exit mail_block

            print *, 'Printing mail server ...'
            print '(72("."))'
            call dm_mail_out(server)
            print '(72("."))'

            print *, 'Creating mail ...'
            rc = dm_mail_create_mail(mail, person, persons, 'subject', 'message')
            if (dm_is_error(rc)) exit mail_block

            print *, 'Printing mail ...'
            print '(72("."))'
            call dm_mail_out(mail)
            print '(72("."))'
        end block mail_block

        call dm_error_out(rc)

        stat = TEST_PASSED
    end function test01

    logical function test02() result(stat)
        !! Sends e-mail via SMTP, using parameters passed through environment
        !! variables.
        logical,          parameter :: DEBUG   = .true.                !! Enable cURL debug output.
        character(len=*), parameter :: SUBJECT = 'DMPACK Test Message' !! E-mail subject.
        character(len=*), parameter :: MESSAGE = &                     !! E-mail body.
            'Now is the time for all good men to come to the aid of the party.'

        character(len=:), allocatable :: env_from, env_to
        character(len=:), allocatable :: env_host, env_username, env_password
        character(len=:), allocatable :: error_message
        integer                       :: error_curl, rc

        type(mail_server_type) :: server
        type(mail_type)        :: mail
        type(person_type)      :: from
        type(person_type)      :: to(1)

        stat = TEST_PASSED
        if (.not. get_env(env_from, env_to, env_host, env_username, env_password)) return

        stat = TEST_FAILED
        mail_block: block
            print *, 'Creating server ...'
            rc = dm_mail_create(server, env_host, env_username, env_password, &
                                tls=MAIL_TLS_IMPLICIT, verify_tls=.false.)
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
