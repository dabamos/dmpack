! dmtesthtml.f90
!
! Author:  Philipp Engel
! Licence: ISC
program dmtesthtml
    use :: dmpack
    implicit none (type, external)
    integer, parameter :: NTESTS = 3

    logical         :: stats(NTESTS)
    type(test_type) :: tests(NTESTS)

    call dm_init()

    tests(1) = test_type('dmtesthtml.test01', test01)
    tests(2) = test_type('dmtesthtml.test02', test02)
    tests(3) = test_type('dmtesthtml.test03', test03)

    call dm_test_run(tests, stats, dm_env_has('NO_COLOR'))
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
        stat = TEST_FAILED

        print *, 'Basic HTML page ...'
        print '(72("."))'
        print '(a)', dm_html_header('Title', 'Sub-Title', 'style.css', brand='Brand')
        print '(a)', dm_html_footer('Footer' // ASCII_LF, 'script.js')
        print '(72("."))'

        stat = TEST_PASSED
    end function test03
end program dmtesthtml

