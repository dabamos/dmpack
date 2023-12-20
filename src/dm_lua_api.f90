! Author:  Philipp Engel
! Licence: ISC
module dm_lua_api
    !! DMPACK API for Lua.
    use, intrinsic :: iso_c_binding
    use :: lua
    use :: dm_error
    use :: dm_lua
    use :: dm_util
    implicit none (type, external)
    private

    ! Public procedures.
    public :: dm_lua_api_register

    public :: dm_lua_api_deg2gon
    public :: dm_lua_api_deg2rad
    public :: dm_lua_api_gon2deg
    public :: dm_lua_api_gon2rad
    public :: dm_lua_api_rad2deg
    public :: dm_lua_api_rad2gon
contains
    ! **************************************************************************
    ! PUBLIC PROCEDURES.
    ! **************************************************************************
    integer function dm_lua_api_register(lua, constants, procedures) result(rc)
        !! Injects DMPACK parameters (log levels) and registers all DMPACK API
        !! procedures. Parameters are disabled by setting `constants` to
        !! `.false.`, functions by setting `procedures` to `.false.`.
        !!
        !! The following log level parameters are injected:
        !!
        !! * `LOG_NONE`
        !! * `LOG_DEBUG`
        !! * `LOG_INFO`
        !! * `LOG_WARNING`
        !! * `LOG_ERROR`
        !! * `LOG_CRITICAL`
        !!
        !! The following Lua procedures are registered:
        !!
        !! * `deg2gon(deg)`
        !! * `deg2rad(deg)`
        !! * `gon2deg(gon)`
        !! * `gon2rad(gon)`
        !! * `rad2deg(rad)`
        !! * `rad2gon(rad)`
        !!
        !! This function returns `E_INVALID` if the Lua interpreter has not been
        !! initialised, or `E_LUA` if the registration failed.
        use :: dm_log

        type(lua_state_type), intent(inout)        :: lua        !! Lua type.
        logical,              intent(in), optional :: constants  !! Inject DMPACK parameters (log levels).
        logical,              intent(in), optional :: procedures !! Register DMPACK functions.

        logical :: constants_, procedures_

        rc = E_INVALID
        if (.not. dm_lua_is_opened(lua)) return

        constants_ = .true.
        if (present(constants)) constants_ = constants

        procedures_ = .true.
        if (present(procedures)) procedures_ = procedures

        ! Add log levels.
        if (constants_) then
            rc = dm_lua_eval(lua, 'LOG_NONE = '     // dm_itoa(LOG_NONE));     if (dm_is_error(rc)) return
            rc = dm_lua_eval(lua, 'LOG_DEBUG = '    // dm_itoa(LOG_DEBUG));    if (dm_is_error(rc)) return
            rc = dm_lua_eval(lua, 'LOG_INFO = '     // dm_itoa(LOG_INFO));     if (dm_is_error(rc)) return
            rc = dm_lua_eval(lua, 'LOG_WARNING = '  // dm_itoa(LOG_WARNING));  if (dm_is_error(rc)) return
            rc = dm_lua_eval(lua, 'LOG_ERROR = '    // dm_itoa(LOG_ERROR));    if (dm_is_error(rc)) return
            rc = dm_lua_eval(lua, 'LOG_CRITICAL = ' // dm_itoa(LOG_CRITICAL)); if (dm_is_error(rc)) return
        end if

        ! Register DMPACK API procedures.
        if (procedures_) then
            call dm_lua_register(lua, 'deg2gon', dm_lua_api_deg2gon)
            call dm_lua_register(lua, 'deg2rad', dm_lua_api_deg2rad)
            call dm_lua_register(lua, 'gon2deg', dm_lua_api_gon2deg)
            call dm_lua_register(lua, 'gon2rad', dm_lua_api_gon2rad)
            call dm_lua_register(lua, 'rad2deg', dm_lua_api_rad2deg)
            call dm_lua_register(lua, 'rad2gon', dm_lua_api_rad2gon)
        end if
    end function dm_lua_api_register

    ! **************************************************************************
    ! PRIVATE LUA INTERFACE PROCEDURES.
    ! **************************************************************************
    function dm_lua_api_deg2gon(ptr) bind(c) result(n)
        !! Lua function `deg2gon()` that converts number from [deg] to [gon].
        type(c_ptr), intent(in), value :: ptr !! Pointer to Lua interpreter.
        integer(kind=c_int)            :: n   !! Number of results.

        real(kind=lua_number) :: deg, gon

        deg = lua_tonumber(ptr, 1)
        gon = dm_deg_to_gon(deg)
        call lua_pushnumber(ptr, gon)
        n = 1
    end function dm_lua_api_deg2gon

    function dm_lua_api_deg2rad(ptr) bind(c) result(n)
        !! Lua function `deg2rad()` that converts number from [deg] to [rad].
        type(c_ptr), intent(in), value :: ptr !! Pointer to Lua interpreter.
        integer(kind=c_int)            :: n   !! Number of results.

        real(kind=lua_number) :: deg, rad

        deg = lua_tonumber(ptr, 1)
        rad = dm_deg_to_rad(deg)
        call lua_pushnumber(ptr, rad)
        n = 1
    end function dm_lua_api_deg2rad

    function dm_lua_api_gon2deg(ptr) bind(c) result(n)
        !! Lua function `gon2deg()` that converts number from [gon] to [deg].
        type(c_ptr), intent(in), value :: ptr !! Pointer to Lua interpreter.
        integer(kind=c_int)            :: n   !! Number of results.

        real(kind=lua_number) :: deg, gon

        gon = lua_tonumber(ptr, 1)
        deg = dm_gon_to_deg(gon)
        call lua_pushnumber(ptr, deg)
        n = 1
    end function dm_lua_api_gon2deg

    function dm_lua_api_gon2rad(ptr) bind(c) result(n)
        !! Lua function `gon2rad()` that converts number from [gon] to [rad].
        type(c_ptr), intent(in), value :: ptr !! Pointer to Lua interpreter.
        integer(kind=c_int)            :: n   !! Number of results.

        real(kind=lua_number) :: gon, rad

        gon = lua_tonumber(ptr, 1)
        rad = dm_gon_to_rad(gon)
        call lua_pushnumber(ptr, rad)
        n = 1
    end function dm_lua_api_gon2rad

    function dm_lua_api_rad2deg(ptr) bind(c) result(n)
        !! Lua function `rad2deg()` that converts number from [rad] to [deg].
        type(c_ptr), intent(in), value :: ptr !! Pointer to Lua interpreter.
        integer(kind=c_int)            :: n   !! Number of results.

        real(kind=lua_number) :: deg, rad

        rad = lua_tonumber(ptr, 1)
        deg = dm_rad_to_deg(rad)
        call lua_pushnumber(ptr, deg)
        n = 1
    end function dm_lua_api_rad2deg

    function dm_lua_api_rad2gon(ptr) bind(c) result(n)
        !! Lua function `rad2gon()` that converts number from [rad] to [gon].
        type(c_ptr), intent(in), value :: ptr !! Pointer to Lua interpreter.
        integer(kind=c_int)            :: n   !! Number of results.

        real(kind=lua_number) :: gon, rad

        rad = lua_tonumber(ptr, 1)
        gon = dm_rad_to_gon(rad)
        call lua_pushnumber(ptr, gon)
        n = 1
    end function dm_lua_api_rad2gon
end module dm_lua_api
