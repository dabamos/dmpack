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
    !! require("libdmpack")
    !!
    !! print(deg2gon(360.0))
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
        !! Registers the Lua parameters and interfaces of the DMPACK API. This
        !! function is invoked automatically by Lua 5.4.
        use :: dm_error
        use :: dm_lua_api
        use :: lua,     only: lual_dostring, lua_register
        use :: dm_log,  only: LOG_NONE, LOG_DEBUG, LOG_INFO, LOG_WARNING, &
                              LOG_ERROR, LOG_CRITICAL
        use :: dm_util, only: dm_itoa
        type(c_ptr), intent(in), value :: ptr !! C pointer to Lua interpreter.
        integer(kind=c_int)            :: rc  !! Return code.

        ! Add error codes.
        rc = lual_dostring(ptr, 'E_NONE = '           // dm_itoa(E_NONE))
        rc = lual_dostring(ptr, 'E_ERROR = '          // dm_itoa(E_ERROR))
        rc = lual_dostring(ptr, 'E_DUMMY = '          // dm_itoa(E_DUMMY))
        rc = lual_dostring(ptr, 'E_INVALID = '        // dm_itoa(E_INVALID))
        rc = lual_dostring(ptr, 'E_INCOMPLETE = '     // dm_itoa(E_INCOMPLETE))
        rc = lual_dostring(ptr, 'E_TYPE = '           // dm_itoa(E_TYPE))
        rc = lual_dostring(ptr, 'E_IO = '             // dm_itoa(E_IO))
        rc = lual_dostring(ptr, 'E_READ = '           // dm_itoa(E_READ))
        rc = lual_dostring(ptr, 'E_WRITE = '          // dm_itoa(E_WRITE))
        rc = lual_dostring(ptr, 'E_EOF = '            // dm_itoa(E_EOF))
        rc = lual_dostring(ptr, 'E_EOR = '            // dm_itoa(E_EOR))
        rc = lual_dostring(ptr, 'E_ALLOC = '          // dm_itoa(E_ALLOC))
        rc = lual_dostring(ptr, 'E_BOUNDS = '         // dm_itoa(E_BOUNDS))
        rc = lual_dostring(ptr, 'E_EXIST = '          // dm_itoa(E_EXIST))
        rc = lual_dostring(ptr, 'E_SYSTEM = '         // dm_itoa(E_SYSTEM))
        rc = lual_dostring(ptr, 'E_TIMEOUT = '        // dm_itoa(E_TIMEOUT))
        rc = lual_dostring(ptr, 'E_EMPTY = '          // dm_itoa(E_EMPTY))
        rc = lual_dostring(ptr, 'E_LIMIT = '          // dm_itoa(E_LIMIT))
        rc = lual_dostring(ptr, 'E_FORMAT = '         // dm_itoa(E_FORMAT))
        rc = lual_dostring(ptr, 'E_NOT_FOUND = '      // dm_itoa(E_NOT_FOUND))
        rc = lual_dostring(ptr, 'E_READ_ONLY = '      // dm_itoa(E_READ_ONLY))
        rc = lual_dostring(ptr, 'E_CONFIG = '         // dm_itoa(E_CONFIG))
        rc = lual_dostring(ptr, 'E_HDF5 = '           // dm_itoa(E_HDF5))

        rc = lual_dostring(ptr, 'E_DB = '             // dm_itoa(E_DB))
        rc = lual_dostring(ptr, 'E_DB_ID = '          // dm_itoa(E_DB_ID))
        rc = lual_dostring(ptr, 'E_DB_MEM = '         // dm_itoa(E_DB_MEM))
        rc = lual_dostring(ptr, 'E_DB_BUSY = '        // dm_itoa(E_DB_BUSY))
        rc = lual_dostring(ptr, 'E_DB_LOCKED = '      // dm_itoa(E_DB_LOCKED))
        rc = lual_dostring(ptr, 'E_DB_EXEC = '        // dm_itoa(E_DB_EXEC))
        rc = lual_dostring(ptr, 'E_DB_CONSTRAINT = '  // dm_itoa(E_DB_CONSTRAINT))
        rc = lual_dostring(ptr, 'E_DB_TRANSACTION = ' // dm_itoa(E_DB_TRANSACTION))
        rc = lual_dostring(ptr, 'E_DB_ROLLBACK = '    // dm_itoa(E_DB_ROLLBACK))
        rc = lual_dostring(ptr, 'E_DB_PREPARE = '     // dm_itoa(E_DB_PREPARE))
        rc = lual_dostring(ptr, 'E_DB_FINALIZE = '    // dm_itoa(E_DB_FINALIZE))
        rc = lual_dostring(ptr, 'E_DB_BIND = '        // dm_itoa(E_DB_BIND))
        rc = lual_dostring(ptr, 'E_DB_TYPE = '        // dm_itoa(E_DB_TYPE))
        rc = lual_dostring(ptr, 'E_DB_STEP = '        // dm_itoa(E_DB_STEP))
        rc = lual_dostring(ptr, 'E_DB_NO_ROWS = '     // dm_itoa(E_DB_NO_ROWS))
        rc = lual_dostring(ptr, 'E_DB_BACKUP = '      // dm_itoa(E_DB_BACKUP))

        rc = lual_dostring(ptr, 'E_ARG = '            // dm_itoa(E_ARG))
        rc = lual_dostring(ptr, 'E_ARG_NOT_FOUND = '  // dm_itoa(E_ARG_NOT_FOUND))
        rc = lual_dostring(ptr, 'E_ARG_INVALID = '    // dm_itoa(E_ARG_INVALID))
        rc = lual_dostring(ptr, 'E_ARG_NO_VALUE = '   // dm_itoa(E_ARG_NO_VALUE))
        rc = lual_dostring(ptr, 'E_ARG_TYPE = '       // dm_itoa(E_ARG_TYPE))
        rc = lual_dostring(ptr, 'E_ARG_LENGTH = '     // dm_itoa(E_ARG_LENGTH))

        rc = lual_dostring(ptr, 'E_MQUEUE = '         // dm_itoa(E_MQUEUE))
        rc = lual_dostring(ptr, 'E_MQUEUE_EMPTY = '   // dm_itoa(E_MQUEUE_EMPTY))

        rc = lual_dostring(ptr, 'E_REGEX = '          // dm_itoa(E_REGEX))
        rc = lual_dostring(ptr, 'E_REGEX_COMPILE = '  // dm_itoa(E_REGEX_COMPILE))
        rc = lual_dostring(ptr, 'E_REGEX_EXCEEDED = ' // dm_itoa(E_REGEX_EXCEEDED))
        rc = lual_dostring(ptr, 'E_REGEX_NO_MATCH = ' // dm_itoa(E_REGEX_NO_MATCH))
        rc = lual_dostring(ptr, 'E_REGEX_NO_GROUP = ' // dm_itoa(E_REGEX_NO_GROUP))

        rc = lual_dostring(ptr, 'E_SENSOR = '         // dm_itoa(E_SENSOR))

        rc = lual_dostring(ptr, 'E_RPC = '            // dm_itoa(E_RPC))
        rc = lual_dostring(ptr, 'E_RPC_CONNECT = '    // dm_itoa(E_RPC_CONNECT))
        rc = lual_dostring(ptr, 'E_RPC_SSL = '        // dm_itoa(E_RPC_SSL))
        rc = lual_dostring(ptr, 'E_RPC_API = '        // dm_itoa(E_RPC_API))
        rc = lual_dostring(ptr, 'E_RPC_AUTH = '       // dm_itoa(E_RPC_AUTH))
        rc = lual_dostring(ptr, 'E_RPC_CONFLICT = '   // dm_itoa(E_RPC_CONFLICT))
        rc = lual_dostring(ptr, 'E_RPC_SERVER = '     // dm_itoa(E_RPC_SERVER))

        rc = lual_dostring(ptr, 'E_MAIL = '           // dm_itoa(E_MAIL))
        rc = lual_dostring(ptr, 'E_MQTT = '           // dm_itoa(E_MQTT))

        rc = lual_dostring(ptr, 'E_LUA = '            // dm_itoa(E_LUA))
        rc = lual_dostring(ptr, 'E_LUA_YIELD = '      // dm_itoa(E_LUA_YIELD))
        rc = lual_dostring(ptr, 'E_LUA_RUNTIME = '    // dm_itoa(E_LUA_RUNTIME))
        rc = lual_dostring(ptr, 'E_LUA_SYNTAX = '     // dm_itoa(E_LUA_SYNTAX))
        rc = lual_dostring(ptr, 'E_LUA_MEM = '        // dm_itoa(E_LUA_MEM))
        rc = lual_dostring(ptr, 'E_LUA_ERROR = '      // dm_itoa(E_LUA_ERROR))
        rc = lual_dostring(ptr, 'E_LUA_FILE = '       // dm_itoa(E_LUA_FILE))

        ! Add log levels.
        rc = lual_dostring(ptr, 'LOG_NONE = '     // dm_itoa(LOG_NONE))
        rc = lual_dostring(ptr, 'LOG_DEBUG = '    // dm_itoa(LOG_DEBUG))
        rc = lual_dostring(ptr, 'LOG_INFO = '     // dm_itoa(LOG_INFO))
        rc = lual_dostring(ptr, 'LOG_WARNING = '  // dm_itoa(LOG_WARNING))
        rc = lual_dostring(ptr, 'LOG_ERROR = '    // dm_itoa(LOG_ERROR))
        rc = lual_dostring(ptr, 'LOG_CRITICAL = ' // dm_itoa(LOG_CRITICAL))

        ! Add utility functions.
        call lua_register(ptr, 'deg2gon', c_funloc(dm_lua_api_deg2gon))
        call lua_register(ptr, 'deg2rad', c_funloc(dm_lua_api_deg2rad))
        call lua_register(ptr, 'gon2deg', c_funloc(dm_lua_api_gon2deg))
        call lua_register(ptr, 'gon2rad', c_funloc(dm_lua_api_gon2rad))
        call lua_register(ptr, 'rad2deg', c_funloc(dm_lua_api_rad2deg))
        call lua_register(ptr, 'rad2gon', c_funloc(dm_lua_api_rad2gon))

        rc = 1
    end function luaopen_libdmpack
end module dm_lua_lib
