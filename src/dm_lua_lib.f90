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
    !! * `deg2gon(deg)` – degrees to gon
    !! * `deg2rad(deg)` – degrees to radiants
    !! * `gon2deg(gon)` – gon to degrees
    !! * `gon2rad(gon)` – gon to radiants
    !! * `rad2deg(rad)` – radiants to degrees
    !! * `rad2gon(rad)` – radiants to gon
    !!
    !! Import the shared library `libdmpack.so` in Lua 5.4, and then call any
    !! of the procedures, for example:
    !!
    !! ```lua
    !! #!/usr/bin/env lua54
    !! require("libdmpack")
    !! print(deg2gon(360.0))
    !! ```
    use, intrinsic :: iso_c_binding
    use :: lua, only: lua_register
    use :: dm_lua_api
    use :: dm_lua_geocom
    implicit none (type, external)
    private

    ! Public procedures.
    public :: luaopen_libdmpack
contains
    ! **************************************************************************
    ! PUBLIC PROCEDURES.
    ! **************************************************************************
    function luaopen_libdmpack(ptr) bind(c) result(rc)
        !! Registers the Lua interfaces of the DMPACK API. This function is
        !! invoked automatically by Lua 5.4.
        type(c_ptr), intent(in), value :: ptr !! C pointer to Lua interpreter.
        integer(kind=c_int)            :: rc  !! Return code.

        call lua_register(ptr, 'deg2gon', c_funloc(dm_lua_api_deg2gon))
        call lua_register(ptr, 'deg2rad', c_funloc(dm_lua_api_deg2rad))
        call lua_register(ptr, 'gon2deg', c_funloc(dm_lua_api_gon2deg))
        call lua_register(ptr, 'gon2rad', c_funloc(dm_lua_api_gon2rad))
        call lua_register(ptr, 'rad2deg', c_funloc(dm_lua_api_rad2deg))
        call lua_register(ptr, 'rad2gon', c_funloc(dm_lua_api_rad2gon))
        rc = 1
    end function luaopen_libdmpack
end module dm_lua_lib
