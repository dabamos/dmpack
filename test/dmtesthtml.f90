! dmtesthtml.f90
!
! Author:  Philipp Engel
! Licence: ISC
program dmtesthtml
    use, intrinsic :: iso_fortran_env, only: compiler_options, compiler_version
    use :: dmpack
    implicit none (type, external)

    character(len=*), parameter :: TEST_NAME = 'dmtesthtml'
    integer,          parameter :: NTESTS    = 4

    logical         :: stats(NTESTS)
    type(test_type) :: tests(NTESTS)

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
        type(cgi_env_type) :: env
        type(node_type)    :: nodes(2)

        stat = TEST_FAILED

        call dm_cgi_env(env)

        nodes(1)%id   = 'test-node-1'
        nodes(1)%name = 'Test Node 1'
        nodes(1)%meta = 'A test node'

        nodes(2)%id   = 'test-node-2'
        nodes(2)%name = 'Test Node 2'
        nodes(2)%meta = 'Another test node'

        print *, 'Node to HTML ...'
        print '(72("."))'
        print '(a)', dm_html_node(nodes(1))
        print '(72("."))'

        print *, 'Nodes to HTML ...'
        print '(72("."))'
        print '(a)', dm_html_nodes(nodes)
        print '(72("."))'

        stat = TEST_PASSED
    end function test01

    logical function test02() result(stat)
        type(observ_type) :: observ

        stat = TEST_FAILED

        call dm_test_dummy(observ)

        print *, 'Observation to HTML ...'
        print '(72("."))'
        print '(a)', dm_html_observ(observ)
        print '(72("."))'

        stat = TEST_PASSED
    end function test02

    logical function test03() result(stat)
        type(string_type) :: styles(2)

        stat = TEST_FAILED

        styles = [ &
            string_type('style.min.css'), &
            string_type('extra.css')      &
        ]

        print *, 'Basic HTML page ...'
        print '(72("."))'
        print '(a)', dm_html_header('Title', 'Sub-Title', brand='Brand', styles=styles)
        print '(a)', dm_html_script('dmpack.js')
        print '(a)', dm_html_footer('Footer' // ASCII_LF)
        print '(72("."))'

        stat = TEST_PASSED
    end function test03

    logical function test04() result(stat)
        character(len=*), parameter :: EXP_HUMAN = '1970-01-01 00:00:00 +00:00'
        character(len=*), parameter :: EXP_HTML  = &
            '<time datetime="1970-01-01T00:00:00+00:00">1970-01-01 00:00:00 +00:00</time>'

        character(len=:), allocatable :: html, human
        character(len=TIME_LEN)       :: time

        stat = TEST_FAILED

        time  = TIME_DEFAULT
        human = dm_time_to_human(time)
        html  = dm_html_time(time, human=.true.)

        print *, 'Time.: ', time
        print *, 'Human: ', human
        print *, 'HTML.: ', html

        if (human /= EXP_HUMAN) return
        if (html  /= EXP_HTML)  return

        stat = TEST_PASSED
    end function test04
end program dmtesthtml

