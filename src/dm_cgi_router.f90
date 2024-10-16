! Author:  Philipp Engel
! Licence: ISC
module dm_cgi_router
    !! Basic URI router for CGI. Depending on the path in the CGI environment
    !! variable `PATH_INFO`, a callback is invoked that returns the response of
    !! that URI.
    use :: dm_cgi
    use :: dm_error
    use :: dm_hash_table
    implicit none (type, external)
    private

    abstract interface
        subroutine dm_cgi_router_callback(env)
            !! Route callback routine to be invoked by router.
            import :: cgi_env_type
            implicit none
            type(cgi_env_type), intent(inout) :: env !! CGI environment variables.
        end subroutine dm_cgi_router_callback
    end interface

    type, public :: cgi_route_type
        !! Route type that stores URI path and callback routine.
        character(len=:), allocatable                      :: path               !! Route path.
        procedure(dm_cgi_router_callback), pointer, nopass :: callback => null() !! Callback routine.
    end type cgi_route_type

    type, public :: cgi_router_type
        !! Opaque router type that holds the routes.
        private
        type(hash_table_type) :: routes !! Hash table of routes.
    end type cgi_router_type

    public :: dm_cgi_router_callback

    public :: dm_cgi_router_add
    public :: dm_cgi_router_create
    public :: dm_cgi_router_destroy
    public :: dm_cgi_router_dispatch
    public :: dm_cgi_router_get
    public :: dm_cgi_router_set
contains
    ! **************************************************************************
    ! PUBLIC PROCEDURES.
    ! **************************************************************************
    integer function dm_cgi_router_add(router, route) result(rc)
        !! Adds route to router.
        type(cgi_router_type),        intent(inout) :: router !! Router type.
        type(cgi_route_type), target, intent(inout) :: route  !! Route to add to routing table.

        type(cgi_route_type), pointer :: ptr

        rc = E_INVALID
        if (.not. allocated(route%path)) return
        ptr => route
        rc = dm_hash_table_set(router%routes, route%path, ptr)
    end function dm_cgi_router_add

    integer function dm_cgi_router_create(router, max_routes) result(rc)
        !! Creates a new router with given maximum number of routes in
        !! the routes hash table.
        type(cgi_router_type), intent(out) :: router     !! Router type.
        integer,               intent(in)  :: max_routes !! Max. size of hash table.

        rc = dm_hash_table_create(router%routes, max_routes)
    end function dm_cgi_router_create

    subroutine dm_cgi_router_destroy(router)
        !! Finalises router.
        type(cgi_router_type), intent(inout) :: router !! Router type.

        call dm_hash_table_destroy(router%routes)
    end subroutine dm_cgi_router_destroy

    subroutine dm_cgi_router_dispatch(router, env, http_status)
        !! Searches the router hash table for path in CGI environment variable
        !! `PATH_INFO`, and calls the associated subroutine of the route. If no
        !! route has been found, `HTTP_NOT_FOUND` is returned in `http_status`.
        use :: dm_http
        type(cgi_router_type), intent(inout) :: router      !! Router type.
        type(cgi_env_type),    intent(inout) :: env         !! CGI environment variables.
        integer,               intent(out)   :: http_status !! Optional status.

        integer                       :: rc
        type(cgi_route_type), pointer :: route

        http_status = HTTP_NOT_FOUND
        rc = dm_cgi_router_get(router, trim(env%path_info), route)
        if (dm_is_error(rc)) return

        http_status = HTTP_INTERNAL_SERVER_ERROR
        if (.not. associated(route)) return
        if (.not. associated(route%callback)) return

        ! Invoke route callback.
        call route%callback(env)
        http_status = HTTP_OK
    end subroutine dm_cgi_router_dispatch

    integer function dm_cgi_router_get(router, path, route) result(rc)
        !! Returns route of associated path from internal hash table. The
        !! passed path string has to be trimmed.
        type(cgi_router_type),         intent(inout) :: router !! Router type.
        character(len=*),              intent(in)    :: path   !! URI.
        type(cgi_route_type), pointer, intent(out)   :: route  !! Associated route.

        class(*), pointer :: ptr

        route => null()
        ptr   => null()

        rc = dm_hash_table_get(router%routes, path, ptr)
        if (dm_is_error(rc)) return

        select type (p => ptr)
            type is (cgi_route_type)
                route => p
            class default
                rc = E_TYPE
                return
        end select

        rc = E_NONE
    end function dm_cgi_router_get

    integer function dm_cgi_router_set(router, routes) result(rc)
        !! Creates a new router and adds routes to endpoints.
        type(cgi_router_type), intent(inout) :: router    !! Router type.
        type(cgi_route_type),  intent(inout) :: routes(:) !! Endpoints.

        integer :: i

        rc = dm_cgi_router_create(router, max_routes=size(routes))
        if (dm_is_error(rc)) return

        do i = 1, size(routes)
            rc = dm_cgi_router_add(router, routes(i))
            if (dm_is_error(rc)) return
        end do
    end function dm_cgi_router_set
end module dm_cgi_router
