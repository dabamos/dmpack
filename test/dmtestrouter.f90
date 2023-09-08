! dmtestrouter.f90
!
! Author:  Philipp Engel
! Licence: ISC
program dmtestrouter
    use :: dmpack
    implicit none (type, external)
    integer, parameter :: NTESTS = 1

    type(test_type) :: tests(NTESTS)
    logical         :: stats(NTESTS)

    tests(1) = test_type('dmtestrouter.test01', test01)

    call dm_init()
    call dm_test_run(tests, stats, dm_env_has('NO_COLOR'))
contains
    logical function test01() result(stat)
        integer                    :: rc
        type(cgi_env_type)         :: env
        type(router_type)          :: router
        type(route_type), target   :: routes(2)
        type(route_type), pointer  :: ptr

        stat = TEST_FAILED

        print *, 'Creating router ...'
        rc = dm_router_create(router, 32)
        if (dm_is_error(rc)) return

        print *, 'Creating routes ...'
        routes(1)%path = '/'
        routes(1)%callback => default_resource

        routes(2)%path = '/index'
        routes(2)%callback => index_resource

        print *, 'Adding routes to router ...'
        rc = dm_router_add(router, routes(1))
        if (dm_is_error(rc)) return

        rc = dm_router_add(router, routes(2))
        if (dm_is_error(rc)) return

        print *, 'Retrieving routes ...'
        ptr => null()
        rc = dm_router_get(router, '/', ptr)
        if (dm_is_error(rc)) return
        if (.not. associated(ptr)) return
        if (.not. allocated(ptr%path)) return
        if (ptr%path /= '/') return
        call ptr%callback(env)

        ptr => null()
        rc = dm_router_get(router, '/index', ptr)
        if (dm_is_error(rc)) return
        if (.not. associated(ptr)) return
        if (.not. allocated(ptr%path)) return
        if (ptr%path /= '/index') return
        call ptr%callback(env)

        print *, 'Testing invalid route ...'
        ptr => null()
        rc = dm_router_get(router, '/invalid', ptr)
        if (rc == E_NONE) return
        if (associated(ptr)) return

        print *, 'Filtering request URI ...'
        env%path_info = '/index'
        call dm_router_dispatch(router, env, rc)
        if (rc /= HTTP_OK) return

        print *, 'Filtering invalid request URI ...'
        env%path_info = '/invalid'
        call dm_router_dispatch(router, env, rc)
        if (rc == HTTP_OK) return

        stat = TEST_PASSED
    end function test01

    subroutine default_resource(env)
        type(cgi_env_type), intent(inout) :: env

        print *, '-- default_resource'
    end subroutine default_resource

    subroutine index_resource(env)
        type(cgi_env_type), intent(inout) :: env

        print *, '-- index resource'
    end subroutine index_resource
end program dmtestrouter

