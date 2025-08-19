! lua.f90
!
! A collection of ISO C binding interfaces to Lua 5.4 for Fortran 2008.
!
! Author:  Philipp Engel
! Licence: ISC
module lua
    use, intrinsic :: iso_c_binding
    use, intrinsic :: iso_fortran_env, only: i8 => int64
    implicit none
    private

    ! The integer and float types used by Lua are platform-specific. Select the
    ! types according to your local Lua library.
    integer, parameter, public :: lua_integer  = c_long_long ! c_int, c_long, c_long_long, c_int64_t
    integer, parameter, public :: lua_number   = c_double    ! c_float, c_double, c_long_double
    integer, parameter, public :: lua_kcontext = c_intptr_t  ! c_intptr_t, c_ptrdiff_t

    integer(kind=c_int), parameter, public :: LUA_VERSION_NUM = 504

    ! Option for multiple returns in `lua_pcall()` and `lua_call()`.
    integer(kind=c_int), parameter, public :: LUA_MULTRET = -1

    ! Basic types.
    integer(kind=c_int), parameter, public :: LUA_TNONE          = -1
    integer(kind=c_int), parameter, public :: LUA_TNIL           = 0
    integer(kind=c_int), parameter, public :: LUA_TBOOLEAN       = 1
    integer(kind=c_int), parameter, public :: LUA_TLIGHTUSERDATA = 2
    integer(kind=c_int), parameter, public :: LUA_TNUMBER        = 3
    integer(kind=c_int), parameter, public :: LUA_TSTRING        = 4
    integer(kind=c_int), parameter, public :: LUA_TTABLE         = 5
    integer(kind=c_int), parameter, public :: LUA_TFUNCTION      = 6
    integer(kind=c_int), parameter, public :: LUA_TUSERDATA      = 7
    integer(kind=c_int), parameter, public :: LUA_TTHREAD        = 8

    ! Comparison and arithmetic options.
    integer(kind=c_int), parameter, public :: LUA_OPADD  = 0
    integer(kind=c_int), parameter, public :: LUA_OPSUB  = 1
    integer(kind=c_int), parameter, public :: LUA_OPMUL  = 2
    integer(kind=c_int), parameter, public :: LUA_OPMOD  = 3
    integer(kind=c_int), parameter, public :: LUA_OPPOW  = 4
    integer(kind=c_int), parameter, public :: LUA_OPDIV  = 5
    integer(kind=c_int), parameter, public :: LUA_OPIDIV = 6
    integer(kind=c_int), parameter, public :: LUA_OPBAND = 7
    integer(kind=c_int), parameter, public :: LUA_OPBOR  = 8
    integer(kind=c_int), parameter, public :: LUA_OPBXOR = 9
    integer(kind=c_int), parameter, public :: LUA_OPSHL  = 10
    integer(kind=c_int), parameter, public :: LUA_OPSHR  = 11
    integer(kind=c_int), parameter, public :: LUA_OPUNM  = 12
    integer(kind=c_int), parameter, public :: LUA_OPBNOT = 13

    integer(kind=c_int), parameter, public :: LUA_OPEQ = 0
    integer(kind=c_int), parameter, public :: LUA_OPLT = 1
    integer(kind=c_int), parameter, public :: LUA_OPLE = 2

    ! Garbage-collection options.
    integer(kind=c_int), parameter, public :: LUA_GCSTOP       = 0
    integer(kind=c_int), parameter, public :: LUA_GCRESTART    = 1
    integer(kind=c_int), parameter, public :: LUA_GCCOLLECT    = 2
    integer(kind=c_int), parameter, public :: LUA_GCCOUNT      = 3
    integer(kind=c_int), parameter, public :: LUA_GCCOUNTB     = 4
    integer(kind=c_int), parameter, public :: LUA_GCSTEP       = 5
    integer(kind=c_int), parameter, public :: LUA_GCSETPAUSE   = 6
    integer(kind=c_int), parameter, public :: LUA_GCSETSTEPMUL = 7
    integer(kind=c_int), parameter, public :: LUA_GCISRUNNING  = 9
    integer(kind=c_int), parameter, public :: LUA_GCGEN        = 10
    integer(kind=c_int), parameter, public :: LUA_GCINC        = 11

    ! Error codes.
    integer(kind=c_int), parameter, public :: LUA_OK        = 0
    integer(kind=c_int), parameter, public :: LUA_YIELD     = 1
    integer(kind=c_int), parameter, public :: LUA_ERRRUN    = 2
    integer(kind=c_int), parameter, public :: LUA_ERRSYNTAX = 3
    integer(kind=c_int), parameter, public :: LUA_ERRMEM    = 4
    integer(kind=c_int), parameter, public :: LUA_ERRERR    = 5
    integer(kind=c_int), parameter, public :: LUA_ERRFILE   = LUA_ERRERR + 1

    public :: lua_arith
    public :: lua_call
    public :: lua_callk
    public :: lua_checkstack
    public :: lua_close
    public :: lua_compare
    public :: lua_concat
    public :: lua_copy
    public :: lua_createtable
    public :: lua_gc
    public :: lua_getfield
    public :: lua_getglobal
    public :: lua_gettable
    public :: lua_gettop
    public :: lua_isboolean
    public :: lua_iscfunction
    public :: lua_isfunction
    public :: lua_isinteger
    public :: lua_isnil
    public :: lua_isnone
    public :: lua_isnoneornil
    public :: lua_isnumber
    public :: lua_isstring
    public :: lua_istable
    public :: lua_isthread
    public :: lua_isuserdata
    public :: lua_isyieldable
    public :: lua_load
    public :: lua_newtable
    public :: lua_pcall
    public :: lua_pcallk
    public :: lua_pop
    public :: lua_pushboolean
    public :: lua_pushcclosure
    public :: lua_pushinteger
    public :: lua_pushlightuserdata
    public :: lua_pushlstring
    public :: lua_pushnil
    public :: lua_pushnumber
    public :: lua_pushstring
    public :: lua_pushthread
    public :: lua_pushvalue
    public :: lua_rawget
    public :: lua_rawgeti
    public :: lua_rawlen
    public :: lua_rawset
    public :: lua_rawseti
    public :: lua_register
    public :: lua_setfield
    public :: lua_setglobal
    public :: lua_seti
    public :: lua_settable
    public :: lua_settop
    public :: lua_status
    public :: lua_toboolean
    public :: lua_tointeger
    public :: lua_tointegerx
    public :: lua_tonumber
    public :: lua_tonumberx
    public :: lua_tostring
    public :: lua_type
    public :: lua_typename
    public :: lua_version
    public :: lual_checkversion_
    public :: lual_dofile
    public :: lual_dostring
    public :: lual_len
    public :: lual_loadfile
    public :: lual_loadfilex
    public :: lual_loadstring
    public :: lual_newstate
    public :: lual_openlibs

    private :: c_f_str_ptr

    ! Interfaces to libc.
    interface
        function c_strlen(str) bind(c, name='strlen')
            import :: c_ptr, c_size_t
            implicit none
            type(c_ptr), intent(in), value :: str
            integer(kind=c_size_t)         :: c_strlen
        end function c_strlen
    end interface

    ! Interfaces to Lua 5.4.
    interface
        ! int lua_checkstack(lua_State *L, int n)
        function lua_checkstack(l, n) bind(c, name='lua_checkstack')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr),         intent(in), value :: l
            integer(kind=c_int), intent(in), value :: n
            integer(kind=c_int)                    :: lua_checkstack
        end function lua_checkstack

        ! int lua_compare(lua_State *L, int index1, int index2, int op)
        function lua_compare(l, index1, index2, op) bind(c, name='lua_compare')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr),         intent(in), value :: l
            integer(kind=c_int), intent(in), value :: index1
            integer(kind=c_int), intent(in), value :: index2
            integer(kind=c_int), intent(in), value :: op
            integer(kind=c_int)                    :: lua_compare
        end function lua_compare

        ! int lua_gc(lua_State *L, int what, int data)
        function lua_gc(l, what, data) bind(c, name='lua_gc')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr),         intent(in), value :: l
            integer(kind=c_int), intent(in), value :: what
            integer(kind=c_int), intent(in), value :: data
            integer(kind=c_int)                    :: lua_gc
        end function lua_gc

        ! int lua_getfield(lua_State *L, int idx, const char *k)
        function lua_getfield_(l, idx, k) bind(c, name='lua_getfield')
            import :: c_char, c_int, c_ptr
            implicit none
            type(c_ptr),            intent(in), value :: l
            integer(kind=c_int),    intent(in), value :: idx
            character(kind=c_char), intent(in)        :: k
            integer(kind=c_int)                       :: lua_getfield_
        end function lua_getfield_

        ! int lua_getglobal(lua_State *L, const char *name)
        function lua_getglobal_(l, name) bind(c, name='lua_getglobal')
            import :: c_char, c_int, c_ptr
            implicit none
            type(c_ptr),            intent(in), value :: l
            character(kind=c_char), intent(in)        :: name
            integer(kind=c_int)                       :: lua_getglobal_
        end function lua_getglobal_

        ! int lua_gettable (lua_State *L, int idx)
        function lua_gettable(l, idx) bind(c, name='lua_gettable')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr),         intent(in), value :: l
            integer(kind=c_int), intent(in), value :: idx
            integer(kind=c_int)                    :: lua_gettable
        end function lua_gettable

        ! int lua_gettop(lua_State *L)
        function lua_gettop(l) bind(c, name='lua_gettop')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr), intent(in), value :: l
            integer(kind=c_int)            :: lua_gettop
        end function lua_gettop

        ! int lua_iscfunction(lua_State *L, int idx)
        function lua_iscfunction(l, idx) bind(c, name='lua_iscfunction')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr),         intent(in), value :: l
            integer(kind=c_int), intent(in), value :: idx
            integer(kind=c_int)                    :: lua_iscfunction
        end function lua_iscfunction

        ! int lua_isinteger(lua_State *L, int idx)
        function lua_isinteger(l, idx) bind(c, name='lua_isinteger')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr),         intent(in), value :: l
            integer(kind=c_int), intent(in), value :: idx
            integer(kind=c_int)                    :: lua_isinteger
        end function lua_isinteger

        ! int lua_isnumber(lua_State *L, int idx)
        function lua_isnumber(l, idx) bind(c, name='lua_isnumber')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr),         intent(in), value :: l
            integer(kind=c_int), intent(in), value :: idx
            integer(kind=c_int)                    :: lua_isnumber
        end function lua_isnumber

        ! int lua_isstring(lua_State *L, int idx)
        function lua_isstring(l, idx) bind(c, name='lua_isstring')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr),         intent(in), value :: l
            integer(kind=c_int), intent(in), value :: idx
            integer(kind=c_int)                    :: lua_isstring
        end function lua_isstring

        ! int lua_isuserdata(lua_State *L, int idx)
        function lua_isuserdata(l, idx) bind(c, name='lua_isuserdata')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr),         intent(in), value :: l
            integer(kind=c_int), intent(in), value :: idx
            integer(kind=c_int)                    :: lua_isuserdata
        end function lua_isuserdata

        ! int lua_isyieldable(lua_State *L)
        function lua_isyieldable(l) bind(c, name='lua_isyielable')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr), intent(in), value :: l
            integer(kind=c_int)            :: lua_isyieldable
        end function lua_isyieldable

        ! int lua_load(lua_State *L, lua_Reader reader, void *data, const char *chunkname, const char *mode)
        function lua_load(l, reader, data, chunkname, mode) bind(c, name='lua_load')
            import :: c_char, c_funptr, c_int, c_ptr
            implicit none
            type(c_ptr),            intent(in), value :: l
            type(c_funptr),         intent(in), value :: reader
            type(c_ptr),            intent(in), value :: data
            character(kind=c_char), intent(in)        :: chunkname
            character(kind=c_char), intent(in)        :: mode
            integer(kind=c_int)                       :: lua_load
        end function lua_load

        ! int lua_rawget(lua_State *L, int idx)
        function lua_rawget(l, idx) bind(c, name='lua_rawget')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr),         intent(in), value :: l
            integer(kind=c_int), intent(in), value :: idx
            integer(kind=c_int)                    :: lua_rawget
        end function lua_rawget

        ! int lua_rawgeti(lua_State *L, int idx, lua_Integer n)
        function lua_rawgeti(l, idx, n) bind(c, name='lua_rawgeti')
            import :: c_int, c_ptr, lua_integer
            implicit none
            type(c_ptr),               intent(in), value :: l
            integer(kind=c_int),       intent(in), value :: idx
            integer(kind=lua_integer), intent(in), value :: n
            integer(kind=c_int)                          :: lua_rawgeti
        end function lua_rawgeti

        ! size_t lua_rawlen(lua_State *L, int idx)
        function lua_rawlen(l, idx) bind(c, name='lua_rawlen')
            import :: c_int, c_ptr, c_size_t
            implicit none
            type(c_ptr),         intent(in), value :: l
            integer(kind=c_int), intent(in), value :: idx
            integer(kind=c_size_t)                 :: lua_rawlen
        end function lua_rawlen

        ! int lua_status(lua_State *L)
        function lua_status(l) bind(c, name='lua_status')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr), intent(in), value :: l
            integer(kind=c_int)            :: lua_status
        end function lua_status

        ! int lua_toboolean(lua_State *L, int idx)
        function lua_toboolean_(l, idx) bind(c, name='lua_toboolean')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr),         intent(in), value :: l
            integer(kind=c_int), intent(in), value :: idx
            integer(kind=c_int)                    :: lua_toboolean_
        end function lua_toboolean_

        ! float lua_tonumberx(lua_State *L, int idx, int *isnum)
        function lua_tonumberx(l, idx, isnum) bind(c, name='lua_tonumberx')
            import :: c_int, c_ptr, lua_number
            implicit none
            type(c_ptr),         intent(in), value :: l
            integer(kind=c_int), intent(in), value :: idx
            type(c_ptr),         intent(in), value :: isnum
            real(kind=lua_number)                  :: lua_tonumberx
        end function lua_tonumberx

        ! lua_Integer lua_tointegerx(lua_State *L, int idx, int *isnum)
        function lua_tointegerx(l, idx, isnum) bind(c, name='lua_tointegerx')
            import :: c_int, c_ptr, lua_integer
            implicit none
            type(c_ptr),         intent(in), value :: l
            integer(kind=c_int), intent(in), value :: idx
            type(c_ptr),         intent(in), value :: isnum
            integer(kind=lua_integer)              :: lua_tointegerx
        end function lua_tointegerx

        ! const char *lua_tolstring(lua_State *L, int idx, size_t *len)
        function lua_tolstring(l, idx, len) bind(c, name='lua_tolstring')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr),         intent(in), value :: l
            integer(kind=c_int), intent(in), value :: idx
            type(c_ptr),         intent(in), value :: len
            type(c_ptr)                            :: lua_tolstring
        end function lua_tolstring

        ! int lua_type(lua_State *L, int idx)
        function lua_type(l, idx) bind(c, name='lua_type')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr),         intent(in), value :: l
            integer(kind=c_int), intent(in), value :: idx
            integer(kind=c_int)                    :: lua_type
        end function lua_type

        ! const char *lua_typename(lua_State *L, int tp)
        function lua_typename_(l, tp) bind(c, name='lua_typename')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr),         intent(in), value :: l
            integer(kind=c_int), intent(in), value :: tp
            type(c_ptr)                            :: lua_typename_
        end function lua_typename_

        ! int lua_pcallk(lua_State *L, int nargs, int nresults, int errfunc, lua_KContext ctx, lua_KFunction k)
        function lua_pcallk(l, nargs, nresults, errfunc, ctx, k) bind(c, name='lua_pcallk')
            import :: c_funptr, c_int, c_ptr, lua_kcontext
            implicit none
            type(c_ptr),                intent(in), value :: l
            integer(kind=c_int),        intent(in), value :: nargs
            integer(kind=c_int),        intent(in), value :: nresults
            integer(kind=c_int),        intent(in), value :: errfunc
            integer(kind=lua_kcontext), intent(in), value :: ctx
            type(c_funptr),             intent(in), value :: k
            integer(kind=c_int)                           :: lua_pcallk
        end function lua_pcallk

        ! const char *lua_pushlstring(lua_State *L, const char *s, size_t len)
        function lua_pushlstring_(l, s, len) bind(c, name='lua_pushlstring')
            import :: c_char, c_ptr, c_size_t
            implicit none
            type(c_ptr),            intent(in), value :: l
            character(kind=c_char), intent(in)        :: s
            integer(kind=c_size_t), intent(in), value :: len
            type(c_ptr)                               :: lua_pushlstring_
        end function lua_pushlstring_

        ! const char *lua_pushstring(lua_State *L, const char *s)
        function lua_pushstring_(l, s) bind(c, name='lua_pushstring')
            import :: c_char, c_ptr
            implicit none
            type(c_ptr),            intent(in), value :: l
            character(kind=c_char), intent(in)        :: s
            type(c_ptr)                               :: lua_pushstring_
        end function lua_pushstring_

        ! int lua_pushthread(lua_State *L)
        function lua_pushthread(l) bind(c, name='lua_pushthread')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr), intent(in), value :: l
            integer(kind=c_int)            :: lua_pushthread
        end function lua_pushthread

        ! lua_Number lua_version(lua_State *L)
        function lua_version(l) bind(c, name='lua_version')
            import :: c_ptr, lua_number
            implicit none
            type(c_ptr), intent(in), value :: l
            real(kind=lua_number)          :: lua_version
        end function lua_version

        ! int luaL_len(lua_State *L, int idx)
        function lual_len(l, idx) bind(c, name='luaL_len')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr),         intent(in), value :: l
            integer(kind=c_int), intent(in), value :: idx
            integer(kind=c_int)                    :: lual_len
        end function lual_len

        ! int luaL_loadfilex(lua_State *L, const char *filename, const char *mode)
        function lual_loadfilex(l, filename, mode) bind(c, name='luaL_loadfilex')
            import :: c_char, c_int, c_ptr
            implicit none
            type(c_ptr),            intent(in), value :: l
            character(kind=c_char), intent(in)        :: filename
            type(c_ptr),            intent(in), value :: mode
            integer(kind=c_int)                       :: lual_loadfilex
        end function lual_loadfilex

        ! int luaL_loadstring(lua_State *L, const char *s)
        function lual_loadstring_(l, s) bind(c, name='luaL_loadstring')
            import :: c_char, c_int, c_ptr
            implicit none
            type(c_ptr),            intent(in), value :: l
            character(kind=c_char), intent(in)        :: s
            integer(kind=c_int)                       :: lual_loadstring_
        end function lual_loadstring_

        ! lua_State *luaL_newstate(void)
        function lual_newstate() bind(c, name='luaL_newstate')
            import :: c_ptr
            implicit none
            type(c_ptr) :: lual_newstate
        end function lual_newstate

        ! void lua_arith(lua_State *L, int op)
        subroutine lua_arith(l, op) bind(c, name='lua_arith')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr),         intent(in), value :: l
            integer(kind=c_int), intent(in), value :: op
        end subroutine lua_arith

        ! void lua_callk(lua_State *L, int nargs, int nresults, lua_KContext ctx, lua_CFunction k)
        subroutine lua_callk(l, nargs, nresults, ctx, k) bind(c, name='lua_callk')
            import :: c_funptr, c_int, c_ptr, lua_kcontext
            implicit none
            type(c_ptr),                intent(in), value :: l
            integer(kind=c_int),        intent(in), value :: nargs
            integer(kind=c_int),        intent(in), value :: nresults
            integer(kind=lua_kcontext), intent(in), value :: ctx
            type(c_funptr),             intent(in), value :: k
        end subroutine lua_callk

        ! void lua_close(lua_State *L)
        subroutine lua_close(l) bind(c, name='lua_close')
            import :: c_ptr
            implicit none
            type(c_ptr), intent(in), value :: l
        end subroutine lua_close

        ! void lua_concat(lua_State *L, int n)
        subroutine lua_concat(l, n) bind(c, name='lua_concat')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr),         intent(in), value :: l
            integer(kind=c_int), intent(in), value :: n
        end subroutine lua_concat

        ! void lua_copy(lua_State *L, int fromidx, int toidx)
        subroutine lua_copy(l, fromidx, toidx) bind(c, name='lua_copy')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr),         intent(in), value :: l
            integer(kind=c_int), intent(in), value :: fromidx
            integer(kind=c_int), intent(in), value :: toidx
        end subroutine lua_copy

        ! void lua_createtable(lua_State *L, int narr, int nrec)
        subroutine lua_createtable(l, narr, nrec) bind(c, name='lua_createtable')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr),         intent(in), value :: l
            integer(kind=c_int), intent(in), value :: narr
            integer(kind=c_int), intent(in), value :: nrec
        end subroutine lua_createtable

        ! void lua_pushboolean(lua_State *L, int b)
        subroutine lua_pushboolean(l, b) bind(c, name='lua_pushboolean')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr),         intent(in), value :: l
            integer(kind=c_int), intent(in), value :: b
        end subroutine lua_pushboolean

        ! void lua_pushcclosure(lua_State *L, lua_CFunction fn, int n)
        subroutine lua_pushcclosure(l, fn, n) bind(c, name='lua_pushcclosure')
            import :: c_funptr, c_int, c_ptr
            implicit none
            type(c_ptr),         intent(in), value :: l
            type(c_funptr),      intent(in), value :: fn
            integer(kind=c_int), intent(in), value :: n
        end subroutine lua_pushcclosure

        ! void lua_pushinteger(lua_State *L, lua_Integer n)
        subroutine lua_pushinteger(l, n) bind(c, name='lua_pushinteger')
            import :: c_ptr, lua_integer
            implicit none
            type(c_ptr),               intent(in), value :: l
            integer(kind=lua_integer), intent(in), value :: n
        end subroutine lua_pushinteger

        ! void  lua_pushlightuserdata(lua_State *L, void *p)
        subroutine lua_pushlightuserdata(l, p) bind(c, name='lua_pushlightuserdata')
            import :: c_ptr
            implicit none
            type(c_ptr), intent(in), value :: l
            type(c_ptr), intent(in), value :: p
        end subroutine lua_pushlightuserdata

        ! void lua_pushnil(lua_State *L)
        subroutine lua_pushnil(l) bind(c, name='lua_pushnil')
            import :: c_ptr
            implicit none
            type(c_ptr), intent(in), value :: l
        end subroutine lua_pushnil

        ! void lua_pushnumber(lua_State *L, lua_Number n)
        subroutine lua_pushnumber(l, n) bind(c, name='lua_pushnumber')
            import :: c_ptr, lua_number
            implicit none
            type(c_ptr),           intent(in), value :: l
            real(kind=lua_number), intent(in), value :: n
        end subroutine lua_pushnumber

        ! void lua_pushvalue(lua_State *L, int idx)
        subroutine lua_pushvalue(l, idx) bind(c, name='lua_pushvalue')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr),         intent(in), value :: l
            integer(kind=c_int), intent(in), value :: idx
        end subroutine lua_pushvalue

        ! void lua_rawset(lua_State *L, int idx)
        subroutine lua_rawset(l, idx) bind(c, name='lua_rawset')
            import :: c_int, c_ptr, lua_integer
            implicit none
            type(c_ptr),         intent(in), value :: l
            integer(kind=c_int), intent(in), value :: idx
        end subroutine lua_rawset

        ! void lua_rawseti(lua_State *L, int idx, lua_Integer n)
        subroutine lua_rawseti(l, idx, n) bind(c, name='lua_rawseti')
            import :: c_int, c_ptr, lua_integer
            implicit none
            type(c_ptr),               intent(in), value :: l
            integer(kind=c_int),       intent(in), value :: idx
            integer(kind=lua_integer), intent(in), value :: n
        end subroutine lua_rawseti

        ! void lua_setfield(lua_State *L, int idx, const char *k)
        subroutine lua_setfield_(l, idx, k) bind(c, name='lua_setfield')
            import :: c_char, c_int, c_ptr
            implicit none
            type(c_ptr),            intent(in), value :: l
            integer(kind=c_int),    intent(in), value :: idx
            character(kind=c_char), intent(in)        :: k
        end subroutine lua_setfield_

        ! void lua_setglobal(lua_State *L, const char *name)
        subroutine lua_setglobal_(l, name) bind(c, name='lua_setglobal')
            import :: c_char, c_ptr
            implicit none
            type(c_ptr),            intent(in), value :: l
            character(kind=c_char), intent(in)        :: name
        end subroutine lua_setglobal_

        ! void lua_seti(lua_State *L, int idx, lua_Integer n)
        subroutine lua_seti(l, idx, n) bind(c, name='lua_seti')
            import :: c_int, c_ptr, lua_integer
            implicit none
            type(c_ptr),               intent(in), value :: l
            integer(kind=c_int),       intent(in), value :: idx
            integer(kind=lua_integer), intent(in), value :: n
        end subroutine lua_seti

        ! void lua_settable(lua_State *L, int idx)
        subroutine lua_settable(l, idx) bind(c, name='lua_settable')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr),         intent(in), value :: l
            integer(kind=c_int), intent(in), value :: idx
        end subroutine lua_settable

        ! void lua_settop(lua_State *L, int idx)
        subroutine lua_settop(l, idx) bind(c, name='lua_settop')
            import :: c_int, c_ptr
            implicit none
            type(c_ptr),         intent(in), value :: l
            integer(kind=c_int), intent(in), value :: idx
        end subroutine lua_settop

        ! void luaL_checkversion_(lua_State *L, lua_Number ver, size_t sz)
        subroutine lual_checkversion_(l, ver, sz) bind(c, name='luaL_checkversion_')
            import :: c_ptr, c_size_t, lua_number
            implicit none
            type(c_ptr),            intent(in), value :: l
            real(kind=lua_number),  intent(in), value :: ver
            integer(kind=c_size_t), intent(in), value :: sz
        end subroutine lual_checkversion_

        ! void luaL_openlibs(lua_State *L)
        subroutine lual_openlibs(l) bind(c, name='luaL_openlibs')
            import :: c_ptr
            implicit none
            type(c_ptr), intent(in), value :: l
        end subroutine lual_openlibs
    end interface
contains
    ! int lua_getfield(lua_State *L, int idx, const char *k)
    function lua_getfield(l, idx, k)
        !! Wrapper for `lua_getfield_()` that null-terminates string `k`.
        type(c_ptr),      intent(in) :: l
        integer,          intent(in) :: idx
        character(len=*), intent(in) :: k
        integer                      :: lua_getfield

        lua_getfield = lua_getfield_(l, idx, k // c_null_char)
    end function lua_getfield

    ! int lua_getglobal(lua_State *L, const char *name)
    function lua_getglobal(l, name)
        !! Wrapper for `lua_getglobal_()` that null-terminates string `name`.
        type(c_ptr),      intent(in) :: l
        character(len=*), intent(in) :: name
        integer                      :: lua_getglobal

        lua_getglobal = lua_getglobal_(l, name // c_null_char)
    end function lua_getglobal

    ! int lua_isboolean(lua_State *L, int index)
    function lua_isboolean(l, idx)
        !! Macro replacement that returns whether the stack variable is
        !! boolean.
        type(c_ptr), intent(in) :: l
        integer,     intent(in) :: idx
        integer                 :: lua_isboolean

        lua_isboolean = 0
        if (lua_type(l, idx) == LUA_TBOOLEAN) lua_isboolean= 1
    end function lua_isboolean

    ! int lua_isfunction(lua_State *L, int index)
    function lua_isfunction(l, idx)
        !! Macro replacement that returns whether the stack variable is a
        !! function.
        type(c_ptr), intent(in) :: l
        integer,     intent(in) :: idx
        integer                 :: lua_isfunction

        lua_isfunction = 0
        if (lua_type(l, idx) == LUA_TFUNCTION) lua_isfunction = 1
    end function lua_isfunction

    ! int lua_islightuserdata(lua_State *L, int index)
    function lua_islightuserdata(l, idx)
        !! Macro replacement that returns whether the stack variable is
        !! light user data.
        type(c_ptr), intent(in) :: l
        integer,     intent(in) :: idx
        integer                 :: lua_islightuserdata

        lua_islightuserdata = 0
        if (lua_type(l, idx) == LUA_TLIGHTUSERDATA) lua_islightuserdata = 1
    end function lua_islightuserdata

    ! int lua_isnil(lua_State *L, int index)
    function lua_isnil(l, idx)
        !! Macro replacement that returns whether the stack variable is
        !! nil.
        type(c_ptr), intent(in) :: l
        integer,     intent(in) :: idx
        integer                 :: lua_isnil

        lua_isnil = 0
        if (lua_type(l, idx) == LUA_TNIL) lua_isnil = 1
    end function lua_isnil

    ! int lua_isnone(lua_State *L, int index)
    function lua_isnone(l, idx)
        !! Macro replacement that returns whether the stack variable is
        !! none.
        type(c_ptr), intent(in) :: l
        integer,     intent(in) :: idx
        integer                 :: lua_isnone

        lua_isnone = 0
        if (lua_type(l, idx) == LUA_TNONE) lua_isnone = 1
    end function lua_isnone

    ! int lua_isnoneornil(lua_State *L, int index)
    function lua_isnoneornil(l, idx)
        !! Macro replacement that returns whether the stack variable is
        !! none or nil.
        type(c_ptr), intent(in) :: l
        integer,     intent(in) :: idx
        integer                 :: lua_isnoneornil

        lua_isnoneornil = 0
        if (lua_type(l, idx) <= 0) lua_isnoneornil = 1
    end function lua_isnoneornil

    ! int lua_istable(lua_State *L, int index)
    function lua_istable(l, idx)
        !! Macro replacement that returns whether the stack variable is a
        !! table.
        type(c_ptr), intent(in) :: l
        integer,     intent(in) :: idx
        integer                 :: lua_istable

        lua_istable = 0
        if (lua_type(l, idx) == LUA_TTABLE) lua_istable = 1
    end function lua_istable

    ! int lua_isthread(lua_State *L, int index)
    function lua_isthread(l, idx)
        !! Macro replacement that returns whether the stack variable is a
        !! thread.
        type(c_ptr), intent(in) :: l
        integer,     intent(in) :: idx
        integer                 :: lua_isthread

        lua_isthread = 0
        if (lua_type(l, idx) == LUA_TTHREAD) lua_isthread = 1
    end function lua_isthread

    ! int lua_pcall(lua_State *L, int nargs, int nresults, int msgh)
    function lua_pcall(l, nargs, nresults, errfunc)
        !! Macro replacement that calls `lua_pcallk()`.
        type(c_ptr), intent(in) :: l
        integer,     intent(in) :: nargs
        integer,     intent(in) :: nresults
        integer,     intent(in) :: errfunc
        integer                 :: lua_pcall

        lua_pcall = lua_pcallk(l, nargs, nresults, errfunc, int(0, kind=lua_kcontext), c_null_funptr)
    end function lua_pcall

    ! lua_Integer lua_tointeger(lua_State *l, int idx)
    function lua_tointeger(l, idx)
        type(c_ptr), intent(in)   :: l
        integer,     intent(in)   :: idx
        integer(kind=lua_integer) :: lua_tointeger

        lua_tointeger = lua_tointegerx(l, idx, c_null_ptr)
    end function lua_tointeger

    ! logical lua_toboolean(lua_State *L, int index)
    function lua_toboolean(l, idx)
        type(c_ptr), intent(in) :: l
        integer,     intent(in) :: idx
        logical                 :: lua_toboolean

        lua_toboolean = (lua_toboolean_(l, idx) /= 0)
    end function lua_toboolean

    ! lua_Number lua_tonumber(lua_State *l, int idx)
    function lua_tonumber(l, idx)
        type(c_ptr), intent(in) :: l
        integer,     intent(in) :: idx
        real(kind=lua_number)   :: lua_tonumber

        lua_tonumber = lua_tonumberx(l, idx, c_null_ptr)
    end function lua_tonumber

    ! const char *lua_tostring(lua_State *L, int index)
    function lua_tostring(l, i)
        !! Wrapper that calls `lua_tolstring()` and converts the returned C
        !! pointer to Fortran string. Returns an unallocated character on error.
        type(c_ptr), intent(in)       :: l
        integer,     intent(in)       :: i
        character(len=:), allocatable :: lua_tostring
        type(c_ptr)                   :: ptr

        ptr = lua_tolstring(l, i, c_null_ptr)
        if (.not. c_associated(ptr)) return
        call c_f_str_ptr(ptr, lua_tostring)
    end function lua_tostring

    ! const char *lua_typename(lua_State *L, int tp)
    function lua_typename(l, tp)
        !! Wrapper that calls `lua_typename_()` and converts the returned C
        !! pointer to Fortran string. Returns an unallocated character on error.
        type(c_ptr), intent(in)       :: l
        integer,     intent(in)       :: tp
        character(len=:), allocatable :: lua_typename
        type(c_ptr)                   :: ptr

        ptr = lua_typename_(l, tp)
        if (.not. c_associated(ptr)) return
        call c_f_str_ptr(ptr, lua_typename)
    end function lua_typename

    ! int luaL_dofile(lua_State *L, const char *filename)
    function lual_dofile(l, fn)
        !! Macro replacement that calls `lual_loadfile()` and `lua_pcall()`.
        type(c_ptr),      intent(in) :: l
        character(len=*), intent(in) :: fn
        integer                      :: lual_dofile

        lual_dofile = lual_loadfile(l, fn)
        if (lual_dofile == 0) lual_dofile = lua_pcall(l, 0, LUA_MULTRET, 0)
    end function lual_dofile

    ! int luaL_dostring(lua_State *L, const char *str)
    function lual_dostring(l, str)
        !! Macro replacement that calls `lual_loadstring()` and `lua_pcall()`.
        type(c_ptr),      intent(in) :: l
        character(len=*), intent(in) :: str
        integer                      :: lual_dostring

        lual_dostring = lual_loadstring(l, str)
        if (lual_dostring == 0) lual_dostring = lua_pcall(l, 0, LUA_MULTRET, 0)
    end function lual_dostring

    ! int luaL_loadfile(lua_State *L, const char *filename)
    function lual_loadfile(l, fn)
        !! Macro replacement that calls `lual_loadfilex()`.
        type(c_ptr),      intent(in) :: l
        character(len=*), intent(in) :: fn
        integer                      :: lual_loadfile

        lual_loadfile = lual_loadfilex(l, fn // c_null_char, c_null_ptr)
    end function lual_loadfile

    ! int luaL_loadstring(lua_State *L, const char *s)
    function lual_loadstring(l, s)
        !! Wrapper for `lual_loadstring()` that null-terminates the given
        !! string.
        type(c_ptr),      intent(in) :: l
        character(len=*), intent(in) :: s
        integer                      :: lual_loadstring

        lual_loadstring = lual_loadstring_(l, s // c_null_char)
    end function lual_loadstring

    ! const char *lua_pushlstring(lua_State *L, const char *s, size_t len)
    function lua_pushlstring(l, s, len)
        type(c_ptr),            intent(in) :: l
        character(len=*),       intent(in) :: s
        integer(kind=c_size_t), intent(in) :: len
        type(c_ptr)                        :: lua_pushlstring

        lua_pushlstring = lua_pushlstring_(l, s // c_null_char, len)
    end function lua_pushlstring

    ! const char *lua_pushstring(lua_State *L, const char *s)
    function lua_pushstring(l, s)
        type(c_ptr),      intent(in) :: l
        character(len=*), intent(in) :: s
        type(c_ptr)                  :: lua_pushstring

        lua_pushstring = lua_pushstring_(l, s // c_null_char)
    end function lua_pushstring

    ! void lua_call(lua_State *L, int nargs, int nresults)
    subroutine lua_call(l, nargs, nresults)
        type(c_ptr), intent(in) :: l
        integer,     intent(in) :: nargs
        integer,     intent(in) :: nresults

        call lua_callk(l, nargs, nresults, int(0, kind=c_size_t), c_null_funptr)
    end subroutine lua_call

    ! void lua_newtable(lua_State *L)
    subroutine lua_newtable(l)
        type(c_ptr), intent(in) :: l

        call lua_createtable(l, 0, 0)
    end subroutine lua_newtable

    ! void lua_pop(lua_State *l, int n)
    subroutine lua_pop(l, n)
        type(c_ptr), intent(in) :: l
        integer,     intent(in) :: n

        call lua_settop(l, -n - 1)
    end subroutine lua_pop

    ! void lua_pushcfunction(lua_State *L, lua_CFunction f)
    subroutine lua_pushcfunction(l, f)
        type(c_ptr),    intent(in) :: l
        type(c_funptr), intent(in) :: f

        call lua_pushcclosure(l, f, 0)
    end subroutine lua_pushcfunction

    ! void lua_register(lua_State *L, const char *name, lua_CFunction f)
    subroutine lua_register(l, n, f)
        !! Macro replacement.
        type(c_ptr),      intent(in) :: l
        character(len=*), intent(in) :: n
        type(c_funptr),   intent(in) :: f

        call lua_pushcfunction(l, f)
        call lua_setglobal_(l, n // c_null_char)
    end subroutine lua_register

    ! void lua_setfield(lua_State *L, int idx, const char *k)
    subroutine lua_setfield(l, idx, k)
        type(c_ptr),      intent(in) :: l
        integer,          intent(in) :: idx
        character(len=*), intent(in) :: k

        call lua_setfield_(l, idx, k // c_null_char)
    end subroutine lua_setfield

    ! int lua_getglobal(lua_State *L, const char *name)
    subroutine lua_setglobal(l, name)
        type(c_ptr),            intent(in) :: l
        character(kind=c_char), intent(in) :: name

        call lua_setglobal_(l, name // c_null_char)
    end subroutine lua_setglobal

    subroutine c_f_str_ptr(c_str, f_str)
        !! Copies a C string, passed as a C pointer, to a Fortran string.
        type(c_ptr),                   intent(in)  :: c_str
        character(len=:), allocatable, intent(out) :: f_str

        character(kind=c_char), pointer :: ptrs(:)
        integer(kind=c_size_t)          :: i, sz

        copy_block: block
            if (.not. c_associated(c_str)) exit copy_block
            sz = c_strlen(c_str)
            if (sz < 0) exit copy_block
            call c_f_pointer(c_str, ptrs, [ sz ])
            allocate (character(len=sz) :: f_str)

            do i = 1, sz
                f_str(i:i) = ptrs(i)
            end do

            return
        end block copy_block

        if (.not. allocated(f_str)) f_str = ''
    end subroutine c_f_str_ptr
end module lua
