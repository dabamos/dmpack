! Author:  Philipp Engel
! Licence: ISC
module dm_lua_lib
    !! Auto-registration procedure of the DMPACK Lua API.
    !!
    !! The postfix in the name of function `luaopen_libdmpack()` must match the
    !! name of the DMPACK shared library. This function is compatible to a
    !! library of the name `libdmpack.so` only.
    !!
    !! The following procedures are registered by default:
    !!
    !! * `deg2gon(deg)` – degrees to gradians
    !! * `deg2rad(deg)` – degrees to radiants
    !! * `gon2deg(gon)` – gradians to degrees
    !! * `gon2rad(gon)` – gradians to radiants
    !! * `rad2deg(rad)` – radiants to degrees
    !! * `rad2gon(rad)` – radiants to gradians
    !!
    !! Additionally, error codes, log parameters, and the GeoCOM API for Lua are
    !! exported as well.
    !!
    !! Import the shared library `libdmpack.so` in Lua 5.4, and then call any
    !! of the procedures, for example:
    !!
    !! ```lua
    !! require("libdmpack")
    !!
    !! print(deg2gon(360.0))
    !! -- Output: 400.0
    !! ```
    use, intrinsic :: iso_c_binding
    implicit none (type, external)
    private

    ! Public procedures.
    public :: luaopen_libdmpack
contains
    ! **************************************************************************
    ! PUBLIC PROCEDURES.
    ! **************************************************************************
    function luaopen_libdmpack(ptr) bind(c) result(rc)
        !! Registers the Lua parameters and interfaces of the DMPACK API,
        !! including the GeoCOM API. This function is invoked automatically by
        !! Lua 5.4 if library `libdmpack.so` is loaded.
        use :: dm_lua,        only: lua_state_type
        use :: dm_lua_api,    only: dm_lua_api_register
        use :: dm_lua_geocom, only: dm_lua_geocom_register

        type(c_ptr), intent(in), value :: ptr !! C pointer to Lua interpreter.
        integer(kind=c_int)            :: rc  !! Return code.

        integer              :: stat
        type(lua_state_type) :: lua

        lua  = lua_state_type(ptr)
        stat = dm_lua_api_register(lua)    ! Register DMPACK Lua API.
        stat = dm_lua_geocom_register(lua) ! Register DMPACK GeoCOM API.

        rc = 1
    end function luaopen_libdmpack
end module dm_lua_lib
