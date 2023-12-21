! Author:  Philipp Engel
! Licence: ISC
module dm_lua_geocom
    !! GeoCOM API for Lua.
    use, intrinsic :: iso_c_binding
    use :: dm_error
    use :: dm_geocom_api
    use :: dm_lua
    use :: dm_request
    implicit none (type, external)
    private

    ! Public procedures.
    public :: dm_lua_geocom_register

    ! Private procedures.
    private :: dm_lua_geocom_beep_alarm
contains
    ! **************************************************************************
    ! PUBLIC PROCEDURES.
    ! **************************************************************************
    integer function dm_lua_geocom_register(lua) result(rc)
        !! Registers all GeoCOM API functions. Returns `E_INVALID` if the Lua
        !! interpreter has not been initialised.
        !!
        !! The following Lua procedures are registered:
        !!
        !! * `geocom_beep_alarm()`
        type(lua_state_type), intent(inout) :: lua !! Lua type.

        rc = E_INVALID
        if (.not. dm_lua_is_opened(lua)) return

        call dm_lua_register(lua, 'geocom_beep_alarm', dm_lua_geocom_beep_alarm)

        rc = E_NONE
    end function dm_lua_geocom_register

    ! **************************************************************************
    ! PRIVATE LUA INTERFACE PROCEDURES.
    ! **************************************************************************
    function dm_lua_geocom_beep_alarm(ptr) bind(c) result(n)
        !! Lua function `geocom_beep_alarm()` that pushes the request table of
        !! GeoCOM command `BMM_BeepAlarm` onto the stack.
        type(c_ptr), intent(in), value :: ptr !! Pointer to Lua interpreter.
        integer(kind=c_int)            :: n   !! Number of results.

        type(lua_state_type) :: lua
        type(request_type)   :: request

        lua = lua_state_type(ptr)
        call dm_geocom_prepare_request_beep_alarm(request)
        call dm_lua_from(lua, request)
        n = 1
    end function dm_lua_geocom_beep_alarm
end module dm_lua_geocom
