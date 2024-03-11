! Author:  Philipp Engel
! Licence: ISC
module dm_lua_api
    !! DMPACK API for Lua.
    use, intrinsic :: iso_c_binding
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
    integer function dm_lua_api_register(lua, add_errors, add_log_levels, add_procedures, add_response_types) result(rc)
        !! This function exports parameters and procedures of the DMPACK API to
        !! the given Lua session.
        !!
        !! By default, all parameters and procedures are registered. The export
        !! of error codes, log levels, and procedures may be disabled through
        !! the dummy arguments.
        !!
        !! All DMPACK error codes are exported if `add_errors` is not
        !! `.false.`.
        !!
        !! The following log level parameters are injected if `add_log_levels` is
        !! not `.false.`:
        !!
        !! * `LVL_NONE`
        !! * `LVL_DEBUG`
        !! * `LVL_INFO`
        !! * `LVL_WARNING`
        !! * `LVL_ERROR`
        !! * `LVL_CRITICAL`
        !!
        !! The following Lua procedures are registered if `add_procedures` is
        !! not `.false.`:
        !!
        !! * `deg2gon(deg)`
        !! * `deg2rad(deg)`
        !! * `gon2deg(gon)`
        !! * `gon2rad(gon)`
        !! * `rad2deg(rad)`
        !! * `rad2gon(rad)`
        !!
        !! The following response type parameters are injected if
        !! `add_response_types` is not `.false.`:
        !!
        !! * `RESPONSE_TYPE_REAL64`
        !! * `RESPONSE_TYPE_REAL32`
        !! * `RESPONSE_TYPE_INT64`
        !! * `RESPONSE_TYPE_INT32`
        !! * `RESPONSE_TYPE_LOGICAL`
        !! * `RESPONSE_TYPE_BYTE`
        !! * `RESPONSE_TYPE_STRING`
        !!
        !! The GeoCOM API is registered through function `dm_lua_geocom_register()`
        !! in module `dm_lua_geocom`.
        !!
        !! This function returns `E_INVALID` if the Lua interpreter has not been
        !! initialised, or `E_LUA` if the registration failed.
        use :: dm_log
        use :: dm_response
        type(lua_state_type), intent(inout)        :: lua                !! Lua state type.
        logical,              intent(in), optional :: add_errors         !! Export error codes.
        logical,              intent(in), optional :: add_log_levels     !! Export log level.
        logical,              intent(in), optional :: add_procedures     !! Export procedures.
        logical,              intent(in), optional :: add_response_types !! Export response type parameters.

        logical :: add_errors_, add_log_levels_, add_procedures_, add_response_types_

        rc = E_INVALID
        if (.not. dm_lua_is_opened(lua)) return

        add_errors_         = .true.
        add_log_levels_     = .true.
        add_procedures_     = .true.
        add_response_types_ = .true.

        if (present(add_errors))         add_errors_         = add_errors
        if (present(add_log_levels))     add_log_levels_     = add_log_levels
        if (present(add_procedures))     add_procedures_     = add_procedures
        if (present(add_response_types)) add_response_types_ = add_response_types

        ! Add error codes.
        if (add_errors_) then
            rc = dm_lua_eval(lua, 'E_NONE = '           // dm_itoa(E_NONE));           if (dm_is_error(rc)) return
            rc = dm_lua_eval(lua, 'E_ERROR = '          // dm_itoa(E_ERROR));          if (dm_is_error(rc)) return
            rc = dm_lua_eval(lua, 'E_DUMMY = '          // dm_itoa(E_DUMMY));          if (dm_is_error(rc)) return
            rc = dm_lua_eval(lua, 'E_INVALID = '        // dm_itoa(E_INVALID));        if (dm_is_error(rc)) return
            rc = dm_lua_eval(lua, 'E_INCOMPLETE = '     // dm_itoa(E_INCOMPLETE));     if (dm_is_error(rc)) return
            rc = dm_lua_eval(lua, 'E_TYPE = '           // dm_itoa(E_TYPE));           if (dm_is_error(rc)) return
            rc = dm_lua_eval(lua, 'E_IO = '             // dm_itoa(E_IO));             if (dm_is_error(rc)) return
            rc = dm_lua_eval(lua, 'E_READ = '           // dm_itoa(E_READ));           if (dm_is_error(rc)) return
            rc = dm_lua_eval(lua, 'E_WRITE = '          // dm_itoa(E_WRITE));          if (dm_is_error(rc)) return
            rc = dm_lua_eval(lua, 'E_EOF = '            // dm_itoa(E_EOF));            if (dm_is_error(rc)) return
            rc = dm_lua_eval(lua, 'E_EOR = '            // dm_itoa(E_EOR));            if (dm_is_error(rc)) return
            rc = dm_lua_eval(lua, 'E_ALLOC = '          // dm_itoa(E_ALLOC));          if (dm_is_error(rc)) return
            rc = dm_lua_eval(lua, 'E_BOUNDS = '         // dm_itoa(E_BOUNDS));         if (dm_is_error(rc)) return
            rc = dm_lua_eval(lua, 'E_EXIST = '          // dm_itoa(E_EXIST));          if (dm_is_error(rc)) return
            rc = dm_lua_eval(lua, 'E_SYSTEM = '         // dm_itoa(E_SYSTEM));         if (dm_is_error(rc)) return
            rc = dm_lua_eval(lua, 'E_TIMEOUT = '        // dm_itoa(E_TIMEOUT));        if (dm_is_error(rc)) return
            rc = dm_lua_eval(lua, 'E_EMPTY = '          // dm_itoa(E_EMPTY));          if (dm_is_error(rc)) return
            rc = dm_lua_eval(lua, 'E_LIMIT = '          // dm_itoa(E_LIMIT));          if (dm_is_error(rc)) return
            rc = dm_lua_eval(lua, 'E_FORMAT = '         // dm_itoa(E_FORMAT));         if (dm_is_error(rc)) return
            rc = dm_lua_eval(lua, 'E_NOT_FOUND = '      // dm_itoa(E_NOT_FOUND));      if (dm_is_error(rc)) return
            rc = dm_lua_eval(lua, 'E_READ_ONLY = '      // dm_itoa(E_READ_ONLY));      if (dm_is_error(rc)) return
            rc = dm_lua_eval(lua, 'E_CONFIG = '         // dm_itoa(E_CONFIG));         if (dm_is_error(rc)) return
            rc = dm_lua_eval(lua, 'E_HDF5 = '           // dm_itoa(E_HDF5));           if (dm_is_error(rc)) return
            rc = dm_lua_eval(lua, 'E_ZLIB = '           // dm_itoa(E_ZLIB));           if (dm_is_error(rc)) return

            rc = dm_lua_eval(lua, 'E_DB = '             // dm_itoa(E_DB));             if (dm_is_error(rc)) return
            rc = dm_lua_eval(lua, 'E_DB_ID = '          // dm_itoa(E_DB_ID));          if (dm_is_error(rc)) return
            rc = dm_lua_eval(lua, 'E_DB_MEM = '         // dm_itoa(E_DB_MEM));         if (dm_is_error(rc)) return
            rc = dm_lua_eval(lua, 'E_DB_BUSY = '        // dm_itoa(E_DB_BUSY));        if (dm_is_error(rc)) return
            rc = dm_lua_eval(lua, 'E_DB_LOCKED = '      // dm_itoa(E_DB_LOCKED));      if (dm_is_error(rc)) return
            rc = dm_lua_eval(lua, 'E_DB_EXEC = '        // dm_itoa(E_DB_EXEC));        if (dm_is_error(rc)) return
            rc = dm_lua_eval(lua, 'E_DB_CONSTRAINT = '  // dm_itoa(E_DB_CONSTRAINT));  if (dm_is_error(rc)) return
            rc = dm_lua_eval(lua, 'E_DB_TRANSACTION = ' // dm_itoa(E_DB_TRANSACTION)); if (dm_is_error(rc)) return
            rc = dm_lua_eval(lua, 'E_DB_ROLLBACK = '    // dm_itoa(E_DB_ROLLBACK));    if (dm_is_error(rc)) return
            rc = dm_lua_eval(lua, 'E_DB_PREPARE = '     // dm_itoa(E_DB_PREPARE));     if (dm_is_error(rc)) return
            rc = dm_lua_eval(lua, 'E_DB_FINALIZE = '    // dm_itoa(E_DB_FINALIZE));    if (dm_is_error(rc)) return
            rc = dm_lua_eval(lua, 'E_DB_BIND = '        // dm_itoa(E_DB_BIND));        if (dm_is_error(rc)) return
            rc = dm_lua_eval(lua, 'E_DB_TYPE = '        // dm_itoa(E_DB_TYPE));        if (dm_is_error(rc)) return
            rc = dm_lua_eval(lua, 'E_DB_STEP = '        // dm_itoa(E_DB_STEP));        if (dm_is_error(rc)) return
            rc = dm_lua_eval(lua, 'E_DB_NO_ROWS = '     // dm_itoa(E_DB_NO_ROWS));     if (dm_is_error(rc)) return
            rc = dm_lua_eval(lua, 'E_DB_BACKUP = '      // dm_itoa(E_DB_BACKUP));      if (dm_is_error(rc)) return
            rc = dm_lua_eval(lua, 'E_DB_ATTACH = '      // dm_itoa(E_DB_ATTACH));      if (dm_is_error(rc)) return
            rc = dm_lua_eval(lua, 'E_DB_DETACH = '      // dm_itoa(E_DB_DETACH));      if (dm_is_error(rc)) return

            rc = dm_lua_eval(lua, 'E_ARG = '            // dm_itoa(E_ARG));            if (dm_is_error(rc)) return
            rc = dm_lua_eval(lua, 'E_ARG_NOT_FOUND = '  // dm_itoa(E_ARG_NOT_FOUND));  if (dm_is_error(rc)) return
            rc = dm_lua_eval(lua, 'E_ARG_INVALID = '    // dm_itoa(E_ARG_INVALID));    if (dm_is_error(rc)) return
            rc = dm_lua_eval(lua, 'E_ARG_NO_VALUE = '   // dm_itoa(E_ARG_NO_VALUE));   if (dm_is_error(rc)) return
            rc = dm_lua_eval(lua, 'E_ARG_TYPE = '       // dm_itoa(E_ARG_TYPE));       if (dm_is_error(rc)) return
            rc = dm_lua_eval(lua, 'E_ARG_LENGTH = '     // dm_itoa(E_ARG_LENGTH));     if (dm_is_error(rc)) return
            rc = dm_lua_eval(lua, 'E_ARG_UNKNOWN = '    // dm_itoa(E_ARG_UNKNOWN));    if (dm_is_error(rc)) return

            rc = dm_lua_eval(lua, 'E_MQUEUE = '         // dm_itoa(E_MQUEUE));         if (dm_is_error(rc)) return
            rc = dm_lua_eval(lua, 'E_MQUEUE_EMPTY = '   // dm_itoa(E_MQUEUE_EMPTY));   if (dm_is_error(rc)) return

            rc = dm_lua_eval(lua, 'E_REGEX = '          // dm_itoa(E_REGEX));          if (dm_is_error(rc)) return
            rc = dm_lua_eval(lua, 'E_REGEX_COMPILE = '  // dm_itoa(E_REGEX_COMPILE));  if (dm_is_error(rc)) return
            rc = dm_lua_eval(lua, 'E_REGEX_EXCEEDED = ' // dm_itoa(E_REGEX_EXCEEDED)); if (dm_is_error(rc)) return
            rc = dm_lua_eval(lua, 'E_REGEX_NO_MATCH = ' // dm_itoa(E_REGEX_NO_MATCH)); if (dm_is_error(rc)) return
            rc = dm_lua_eval(lua, 'E_REGEX_NO_GROUP = ' // dm_itoa(E_REGEX_NO_GROUP)); if (dm_is_error(rc)) return

            rc = dm_lua_eval(lua, 'E_SENSOR = '         // dm_itoa(E_SENSOR));         if (dm_is_error(rc)) return

            rc = dm_lua_eval(lua, 'E_RPC = '            // dm_itoa(E_RPC));            if (dm_is_error(rc)) return
            rc = dm_lua_eval(lua, 'E_RPC_CONNECT = '    // dm_itoa(E_RPC_CONNECT));    if (dm_is_error(rc)) return
            rc = dm_lua_eval(lua, 'E_RPC_SSL = '        // dm_itoa(E_RPC_SSL));        if (dm_is_error(rc)) return
            rc = dm_lua_eval(lua, 'E_RPC_API = '        // dm_itoa(E_RPC_API));        if (dm_is_error(rc)) return
            rc = dm_lua_eval(lua, 'E_RPC_AUTH = '       // dm_itoa(E_RPC_AUTH));       if (dm_is_error(rc)) return
            rc = dm_lua_eval(lua, 'E_RPC_CONFLICT = '   // dm_itoa(E_RPC_CONFLICT));   if (dm_is_error(rc)) return
            rc = dm_lua_eval(lua, 'E_RPC_SERVER = '     // dm_itoa(E_RPC_SERVER));     if (dm_is_error(rc)) return

            rc = dm_lua_eval(lua, 'E_MAIL = '           // dm_itoa(E_MAIL));           if (dm_is_error(rc)) return
            rc = dm_lua_eval(lua, 'E_MQTT = '           // dm_itoa(E_MQTT));           if (dm_is_error(rc)) return

            rc = dm_lua_eval(lua, 'E_LUA = '            // dm_itoa(E_LUA));            if (dm_is_error(rc)) return
            rc = dm_lua_eval(lua, 'E_LUA_YIELD = '      // dm_itoa(E_LUA_YIELD));      if (dm_is_error(rc)) return
            rc = dm_lua_eval(lua, 'E_LUA_RUNTIME = '    // dm_itoa(E_LUA_RUNTIME));    if (dm_is_error(rc)) return
            rc = dm_lua_eval(lua, 'E_LUA_SYNTAX = '     // dm_itoa(E_LUA_SYNTAX));     if (dm_is_error(rc)) return
            rc = dm_lua_eval(lua, 'E_LUA_MEM = '        // dm_itoa(E_LUA_MEM));        if (dm_is_error(rc)) return
            rc = dm_lua_eval(lua, 'E_LUA_ERROR = '      // dm_itoa(E_LUA_ERROR));      if (dm_is_error(rc)) return
            rc = dm_lua_eval(lua, 'E_LUA_FILE = '       // dm_itoa(E_LUA_FILE));       if (dm_is_error(rc)) return
        end if

        ! Add log levels.
        if (add_log_levels_) then
            rc = dm_lua_eval(lua, 'LVL_NONE = '     // dm_itoa(LVL_NONE));     if (dm_is_error(rc)) return
            rc = dm_lua_eval(lua, 'LVL_DEBUG = '    // dm_itoa(LVL_DEBUG));    if (dm_is_error(rc)) return
            rc = dm_lua_eval(lua, 'LVL_INFO = '     // dm_itoa(LVL_INFO));     if (dm_is_error(rc)) return
            rc = dm_lua_eval(lua, 'LVL_WARNING = '  // dm_itoa(LVL_WARNING));  if (dm_is_error(rc)) return
            rc = dm_lua_eval(lua, 'LVL_ERROR = '    // dm_itoa(LVL_ERROR));    if (dm_is_error(rc)) return
            rc = dm_lua_eval(lua, 'LVL_CRITICAL = ' // dm_itoa(LVL_CRITICAL)); if (dm_is_error(rc)) return
        end if

        ! Register response type parameters.
        if (add_response_types_) then
            rc = dm_lua_eval(lua, 'RESPONSE_TYPE_REAL64 = '  // dm_itoa(RESPONSE_TYPE_REAL64));  if (dm_is_error(rc)) return
            rc = dm_lua_eval(lua, 'RESPONSE_TYPE_REAL32 = '  // dm_itoa(RESPONSE_TYPE_REAL32));  if (dm_is_error(rc)) return
            rc = dm_lua_eval(lua, 'RESPONSE_TYPE_INT64 = '   // dm_itoa(RESPONSE_TYPE_INT64));   if (dm_is_error(rc)) return
            rc = dm_lua_eval(lua, 'RESPONSE_TYPE_INT32 = '   // dm_itoa(RESPONSE_TYPE_INT32));   if (dm_is_error(rc)) return
            rc = dm_lua_eval(lua, 'RESPONSE_TYPE_LOGICAL = ' // dm_itoa(RESPONSE_TYPE_LOGICAL)); if (dm_is_error(rc)) return
            rc = dm_lua_eval(lua, 'RESPONSE_TYPE_BYTE = '    // dm_itoa(RESPONSE_TYPE_BYTE));    if (dm_is_error(rc)) return
            rc = dm_lua_eval(lua, 'RESPONSE_TYPE_STRING = '  // dm_itoa(RESPONSE_TYPE_STRING));  if (dm_is_error(rc)) return
        end if

        ! Register procedures.
        if (add_procedures_) then
            call dm_lua_register(lua, 'deg2gon', dm_lua_api_deg2gon)
            call dm_lua_register(lua, 'deg2rad', dm_lua_api_deg2rad)
            call dm_lua_register(lua, 'gon2deg', dm_lua_api_gon2deg)
            call dm_lua_register(lua, 'gon2rad', dm_lua_api_gon2rad)
            call dm_lua_register(lua, 'rad2deg', dm_lua_api_rad2deg)
            call dm_lua_register(lua, 'rad2gon', dm_lua_api_rad2gon)
        end if

        rc = E_NONE
    end function dm_lua_api_register

    ! **************************************************************************
    ! PUBLIC LUA INTERFACE PROCEDURES.
    ! **************************************************************************
    function dm_lua_api_deg2gon(ptr) bind(c) result(n)
        !! Lua function `deg2gon()` that converts angle from [deg] to [gon].
        use :: lua
        type(c_ptr), intent(in), value :: ptr !! Pointer to Lua interpreter.
        integer(kind=c_int)            :: n   !! Number of results.

        real(kind=lua_number) :: deg, gon

        deg = lua_tonumber(ptr, 1)
        gon = dm_deg_to_gon(deg)
        call lua_pushnumber(ptr, gon)
        n = 1
    end function dm_lua_api_deg2gon

    function dm_lua_api_deg2rad(ptr) bind(c) result(n)
        !! Lua function `deg2rad()` that converts angle from [deg] to [rad].
        use :: lua
        type(c_ptr), intent(in), value :: ptr !! Pointer to Lua interpreter.
        integer(kind=c_int)            :: n   !! Number of results.

        real(kind=lua_number) :: deg, rad

        deg = lua_tonumber(ptr, 1)
        rad = dm_deg_to_rad(deg)
        call lua_pushnumber(ptr, rad)
        n = 1
    end function dm_lua_api_deg2rad

    function dm_lua_api_gon2deg(ptr) bind(c) result(n)
        !! Lua function `gon2deg()` that converts angle from [gon] to [deg].
        use :: lua
        type(c_ptr), intent(in), value :: ptr !! Pointer to Lua interpreter.
        integer(kind=c_int)            :: n   !! Number of results.

        real(kind=lua_number) :: deg, gon

        gon = lua_tonumber(ptr, 1)
        deg = dm_gon_to_deg(gon)
        call lua_pushnumber(ptr, deg)
        n = 1
    end function dm_lua_api_gon2deg

    function dm_lua_api_gon2rad(ptr) bind(c) result(n)
        !! Lua function `gon2rad()` that converts angle from [gon] to [rad].
        use :: lua
        type(c_ptr), intent(in), value :: ptr !! Pointer to Lua interpreter.
        integer(kind=c_int)            :: n   !! Number of results.

        real(kind=lua_number) :: gon, rad

        gon = lua_tonumber(ptr, 1)
        rad = dm_gon_to_rad(gon)
        call lua_pushnumber(ptr, rad)
        n = 1
    end function dm_lua_api_gon2rad

    function dm_lua_api_rad2deg(ptr) bind(c) result(n)
        !! Lua function `rad2deg()` that converts angle from [rad] to [deg].
        use :: lua
        type(c_ptr), intent(in), value :: ptr !! Pointer to Lua interpreter.
        integer(kind=c_int)            :: n   !! Number of results.

        real(kind=lua_number) :: deg, rad

        rad = lua_tonumber(ptr, 1)
        deg = dm_rad_to_deg(rad)
        call lua_pushnumber(ptr, deg)
        n = 1
    end function dm_lua_api_rad2deg

    function dm_lua_api_rad2gon(ptr) bind(c) result(n)
        !! Lua function `rad2gon()` that converts angle from [rad] to [gon].
        use :: lua
        type(c_ptr), intent(in), value :: ptr !! Pointer to Lua interpreter.
        integer(kind=c_int)            :: n   !! Number of results.

        real(kind=lua_number) :: gon, rad

        rad = lua_tonumber(ptr, 1)
        gon = dm_rad_to_gon(rad)
        call lua_pushnumber(ptr, gon)
        n = 1
    end function dm_lua_api_rad2gon
end module dm_lua_api
