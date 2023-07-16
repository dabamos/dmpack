! dmtesthtml.f90
!
! Author:  Philipp Engel
! Licence: ISC
program dmtesthtml
    use :: dmpack
    implicit none (type, external)
    integer, parameter :: NTESTS = 2

    type(test_type) :: tests(NTESTS)
    logical         :: stats(NTESTS)

    tests(1) = test_type('dmtesthtml%dm_test01', dm_test01)
    tests(2) = test_type('dmtesthtml%dm_test02', dm_test02)

    call dm_init()
    call dm_test_run(tests, stats, dm_env_has('NO_COLOR'))
contains
    logical function dm_test01() result(stat)
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

        print '(a)', dm_html_node(nodes(1))
        print '(a)', dm_html_nodes(nodes)

        stat = TEST_PASSED
    end function dm_test01

    logical function dm_test02() result(stat)
        type(observ_type) :: observ

        stat = TEST_FAILED

        call dm_dummy_observ(observ)
        print '(a)', dm_html_observ(observ)

        stat = TEST_PASSED
    end function dm_test02
end program dmtesthtml

