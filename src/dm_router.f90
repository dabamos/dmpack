! Author:  Philipp Engel
! Licence: ISC
module dm_router
    !! Basic URI router for CGI. Depending on the path in the CGI environment
    !! variable `PATH_INFO`, a callback is invoked that returns the response of
    !! that URI.
    use :: dm_cgi
    use :: dm_error
    use :: dm_hash_table
    use :: dm_html
    use :: dm_http
    implicit none (type, external)
    private

    abstract interface
        subroutine dm_router_callback(env)
            !! Callback routine to be invoked by router.
            import :: cgi_env_type
            implicit none
            type(cgi_env_type), intent(inout) :: env !! CGI environment variables.
        end subroutine dm_router_callback
    end interface

    type, public :: route_type
        !! Route type that stores URI path and callback routine.
        character(len=:), allocatable                  :: path               !! Route path.
        procedure(dm_router_callback), pointer, nopass :: callback => null() !! Callback routine.
    end type route_type

    type, public :: router_type
        !! Opaque router type that holds the routes.
        private
        type(hash_table_type) :: routes !! Hash table of routes.
    end type router_type

    public :: dm_router_callback

    public :: dm_router_add
    public :: dm_router_create
    public :: dm_router_destroy
    public :: dm_router_dispatch
    public :: dm_router_get
contains
    ! ******************************************************************
    ! PUBLIC PROCEDURES.
    ! ******************************************************************
    integer function dm_router_add(router, route) result(rc)
        !! Adds route to router.
        type(router_type),        intent(inout) :: router !! Router type.
        type(route_type), target, intent(inout) :: route  !! Route to add to routing table.

        type(route_type), pointer :: ptr

        rc = E_INVALID
        if (.not. allocated(route%path)) return
        ptr => route
        rc = dm_hash_table_set(router%routes, route%path, ptr)
    end function dm_router_add

    integer function dm_router_create(router, max_routes) result(rc)
        !! Creates a new router with given maximum number of routes in
        !! the routes hash table.
        type(router_type), intent(out) :: router     !! Router type.
        integer,           intent(in)  :: max_routes !! Max. size of hash table.

        rc = dm_hash_table_create(router%routes, max_routes)
    end function dm_router_create

    subroutine dm_router_destroy(router)
        !! Finalises router.
        type(router_type), intent(inout) :: router !! Router type.

        call dm_hash_table_destroy(router%routes)
    end subroutine dm_router_destroy

    subroutine dm_router_dispatch(router, env, http_status)
        !! Searches the router hash table for path in CGI environment variable
        !! `PATH_INFO`, and calls the associated subroutine of the route. If no
        !! route has been found, `HTTP_NOT_FOUND` is returned in `http_status`.
        type(router_type),  intent(inout) :: router      !! Router type.
        type(cgi_env_type), intent(inout) :: env         !! CGI environment variables.
        integer,            intent(out)   :: http_status !! Optional status.

        type(route_type), pointer :: route

        http_status = HTTP_NOT_FOUND
        if (dm_router_get(router, trim(env%path_info), route) /= E_NONE) return

        http_status = HTTP_INTERNAL_SERVER_ERROR
        if (.not. associated(route)) return
        if (.not. associated(route%callback)) return

        ! Invoke route callback.
        call route%callback(env)
        http_status = HTTP_OK
    end subroutine dm_router_dispatch

    integer function dm_router_get(router, path, route) result(rc)
        !! Returns route of associated path from internal hash table. The
        !! passed path string has to be trimmed.
        type(router_type),         intent(inout) :: router !! Router type.
        character(len=*),          intent(in)    :: path   !! URI.
        type(route_type), pointer, intent(out)   :: route  !! Associated route.

        class(*), pointer :: ptr

        route => null()
        ptr   => null()

        rc = dm_hash_table_get(router%routes, path, ptr)
        if (dm_is_error(rc)) return

        select type (p => ptr)
            type is (route_type)
                route => p
            class default
                rc = E_TYPE
                return
        end select

        rc = E_NONE
    end function dm_router_get
end module dm_router
