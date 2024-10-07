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
    integer function dm_lua_api_register(lua, errors, log_levels, procedures, response_types) result(rc)
        !! This function exports parameters and procedures of the DMPACK API to
        !! the given Lua environment `lua`.
        !!
        !! By default, all parameters and procedures are registered. The export
        !! of error codes, log levels, and procedures may be disabled through
        !! the dummy arguments.
        !!
        !! All DMPACK error codes are exported, starting from `E_NONE`, if
        !! `errors` is not `.false.`.
        !!
        !! The following log level parameters are injected if `log_levels` is
        !! not `.false.`:
        !!
        !! * `LL_NONE`
        !! * `LL_DEBUG`
        !! * `LL_INFO`
        !! * `LL_WARNING`
        !! * `LL_ERROR`
        !! * `LL_CRITICAL`
        !! * `LL_USER`
        !!
        !! The following Lua procedures are registered if `procedures` is not
        !! `.false.`:
        !!
        !! * `deg2gon(deg)`
        !! * `deg2rad(deg)`
        !! * `gon2deg(gon)`
        !! * `gon2rad(gon)`
        !! * `rad2deg(rad)`
        !! * `rad2gon(rad)`
        !!
        !! The following response type parameters are injected if
        !! `response_types` is not `.false.`:
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

        type(lua_state_type), intent(inout)        :: lua            !! Lua state type.
        logical,              intent(in), optional :: errors         !! Export error codes.
        logical,              intent(in), optional :: log_levels     !! Export log level.
        logical,              intent(in), optional :: procedures     !! Export procedures.
        logical,              intent(in), optional :: response_types !! Export response type parameters.

        logical :: errors_, log_levels_, procedures_, response_types_

        rc = E_INVALID
        if (.not. dm_lua_is_opened(lua)) return

        errors_         = .true.
        log_levels_     = .true.
        procedures_     = .true.
        response_types_ = .true.

        if (present(errors))         errors_         = errors
        if (present(log_levels))     log_levels_     = log_levels
        if (present(procedures))     procedures_     = procedures
        if (present(response_types)) response_types_ = response_types

        ! Add error codes.
        if (errors_) then
            rc = dm_lua_set(lua, 'E_NONE',           E_NONE);           if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'E_ERROR',          E_ERROR);          if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'E_DUMMY',          E_DUMMY);          if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'E_INVALID',        E_INVALID);        if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'E_INCOMPLETE',     E_INCOMPLETE);     if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'E_TYPE',           E_TYPE);           if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'E_IO',             E_IO);             if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'E_READ',           E_READ);           if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'E_WRITE',          E_WRITE);          if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'E_EOF',            E_EOF);            if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'E_EOR',            E_EOR);            if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'E_ALLOC',          E_ALLOC);          if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'E_BOUNDS',         E_BOUNDS);         if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'E_EXIST',          E_EXIST);          if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'E_SYSTEM',         E_SYSTEM);         if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'E_MEMORY',         E_MEMORY);         if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'E_FULL',           E_FULL);           if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'E_EMPTY',          E_EMPTY);          if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'E_NULL',           E_NULL);           if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'E_LIMIT',          E_LIMIT);          if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'E_TIMEOUT',        E_TIMEOUT);        if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'E_FORMAT',         E_FORMAT);         if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'E_NOT_FOUND',      E_NOT_FOUND);      if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'E_PERM',           E_PERM);           if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'E_READ_ONLY',      E_READ_ONLY);      if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'E_CORRUPT',        E_CORRUPT);        if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'E_CONFIG',         E_CONFIG);         if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'E_GEOCOM',         E_GEOCOM);         if (dm_is_error(rc)) return

            rc = dm_lua_set(lua, 'E_DB',             E_DB);             if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'E_DB_ID',          E_DB_ID);          if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'E_DB_BUSY',        E_DB_BUSY);        if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'E_DB_LOCKED',      E_DB_LOCKED);      if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'E_DB_EXEC',        E_DB_EXEC);        if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'E_DB_CONSTRAINT',  E_DB_CONSTRAINT);  if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'E_DB_TRANSACTION', E_DB_TRANSACTION); if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'E_DB_ROLLBACK',    E_DB_ROLLBACK);    if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'E_DB_PREPARE',     E_DB_PREPARE);     if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'E_DB_FINALIZE',    E_DB_FINALIZE);    if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'E_DB_BIND',        E_DB_BIND);        if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'E_DB_TYPE',        E_DB_TYPE);        if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'E_DB_STEP',        E_DB_STEP);        if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'E_DB_NO_ROWS',     E_DB_NO_ROWS);     if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'E_DB_BACKUP',      E_DB_BACKUP);      if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'E_DB_ATTACH',      E_DB_ATTACH);      if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'E_DB_DETACH',      E_DB_DETACH);      if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'E_DB_VERSION',     E_DB_VERSION);     if (dm_is_error(rc)) return

            rc = dm_lua_set(lua, 'E_ARG',            E_ARG);            if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'E_ARG_NOT_FOUND',  E_ARG_NOT_FOUND);  if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'E_ARG_INVALID',    E_ARG_INVALID);    if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'E_ARG_NO_VALUE',   E_ARG_NO_VALUE);   if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'E_ARG_TYPE',       E_ARG_TYPE);       if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'E_ARG_LENGTH',     E_ARG_LENGTH);     if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'E_ARG_UNKNOWN',    E_ARG_UNKNOWN);    if (dm_is_error(rc)) return

            rc = dm_lua_set(lua, 'E_MQUEUE',         E_MQUEUE);         if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'E_MQUEUE_EMPTY',   E_MQUEUE_EMPTY);   if (dm_is_error(rc)) return

            rc = dm_lua_set(lua, 'E_REGEX',          E_REGEX);          if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'E_REGEX_COMPILE',  E_REGEX_COMPILE);  if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'E_REGEX_EXCEEDED', E_REGEX_EXCEEDED); if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'E_REGEX_NO_MATCH', E_REGEX_NO_MATCH); if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'E_REGEX_NO_GROUP', E_REGEX_NO_GROUP); if (dm_is_error(rc)) return

            rc = dm_lua_set(lua, 'E_SENSOR',         E_SENSOR);         if (dm_is_error(rc)) return

            rc = dm_lua_set(lua, 'E_RPC',            E_RPC);            if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'E_RPC_CONNECT',    E_RPC_CONNECT);    if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'E_RPC_SSL',        E_RPC_SSL);        if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'E_RPC_API',        E_RPC_API);        if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'E_RPC_AUTH',       E_RPC_AUTH);       if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'E_RPC_CONFLICT',   E_RPC_CONFLICT);   if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'E_RPC_SERVER',     E_RPC_SERVER);     if (dm_is_error(rc)) return

            rc = dm_lua_set(lua, 'E_MAIL',           E_MAIL);           if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'E_MAIL_CONNECT',   E_MAIL_CONNECT);   if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'E_MAIL_SSL',       E_MAIL_SSL);       if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'E_MAIL_AUTH',      E_MAIL_AUTH);      if (dm_is_error(rc)) return

            rc = dm_lua_set(lua, 'E_MQTT',           E_MQTT);           if (dm_is_error(rc)) return

            rc = dm_lua_set(lua, 'E_LUA',            E_LUA);            if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'E_LUA_YIELD',      E_LUA_YIELD);      if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'E_LUA_RUNTIME',    E_LUA_RUNTIME);    if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'E_LUA_SYNTAX',     E_LUA_SYNTAX);     if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'E_LUA_MEM',        E_LUA_MEM);        if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'E_LUA_ERROR',      E_LUA_ERROR);      if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'E_LUA_FILE',       E_LUA_FILE);       if (dm_is_error(rc)) return

            rc = dm_lua_set(lua, 'E_LIB',            E_LIB);            if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'E_MODBUS',         E_MODBUS);         if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'E_HDF5',           E_HDF5);           if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'E_ZLIB',           E_ZLIB);           if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'E_ZSTD',           E_ZSTD);           if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'E_XMPP',           E_XMPP);           if (dm_is_error(rc)) return
        end if

        ! Add log levels.
        if (log_levels_) then
            rc = dm_lua_set(lua, 'LL_NONE',     LL_NONE);     if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'LL_DEBUG',    LL_DEBUG);    if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'LL_INFO',     LL_INFO);     if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'LL_WARNING',  LL_WARNING);  if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'LL_ERROR',    LL_ERROR);    if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'LL_CRITICAL', LL_CRITICAL); if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'LL_USER',     LL_USER);     if (dm_is_error(rc)) return
        end if

        ! Register response type parameters.
        if (response_types_) then
            rc = dm_lua_set(lua, 'RESPONSE_TYPE_REAL64',  RESPONSE_TYPE_REAL64);  if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'RESPONSE_TYPE_REAL32',  RESPONSE_TYPE_REAL32);  if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'RESPONSE_TYPE_INT64',   RESPONSE_TYPE_INT64);   if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'RESPONSE_TYPE_INT32',   RESPONSE_TYPE_INT32);   if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'RESPONSE_TYPE_LOGICAL', RESPONSE_TYPE_LOGICAL); if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'RESPONSE_TYPE_BYTE',    RESPONSE_TYPE_BYTE);    if (dm_is_error(rc)) return
            rc = dm_lua_set(lua, 'RESPONSE_TYPE_STRING',  RESPONSE_TYPE_STRING);  if (dm_is_error(rc)) return
        end if

        ! Register procedures.
        if (procedures_) then
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
