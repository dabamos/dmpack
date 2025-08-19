# fortran-lua54

A collection of ISO C binding interfaces to Lua 5.4 for Fortran 2008, to call
Lua from Fortran and vice versa.

Similar projects:

* [AOTUS](https://geb.sts.nt.uni-siegen.de/doxy/aotus/): Library that provides a Fortran wrapper to use Lua scripts as configuration files (MIT).
* [FortLua](https://github.com/adolgert/FortLua): Example that shows how to load Lua configuration files from Fortran, based on AOTUS (MIT).
* [fortran-lua53](https://github.com/interkosmos/fortran-lua53): Fortran 2008 bindings to Lua 5.3 (ISC).
* [f2k3-lua](https://github.com/MaikBeckmann/f2k3-lua): Lua bindings for loading configuration files only (MIT).
* [luaf](https://bitbucket.org/vadimz/luaf/): Selected bindings to Lua 5.1 (MIT).

## Build Instructions

Install Lua 5.4 with development headers. On FreeBSD, run:

```
# pkg install devel/pkgconf lang/lua54
```

On Debian Linux, install:

```
# apt-get install pkg-config liblua5.4 liblua5.4-dev
```

Use [xmake](https://github.com/xmake-io/xmake) to build *fortran-lua54*:

```
$ xmake
```

This outputs `libfortran-lua54.a` and `lua.mod` to `build/`. Without xmake, just
compile and install the library using the provided `Makefile`:

```
$ make
$ make install PREFIX=/opt
```

Or, run the Fortran Package Manager:

```
$ fpm build --profile release
```

Link your Fortran applications against `libfortran-lua54.a`, and `liblua-5.4.a`
or `-llua54`. On Linux, you have to link against `liblua5.4.a` or `-llua5.4`
respectively instead. The include and library search paths may differ as well.

## Example

The following basic example shows how to call the Lua function `hello()` in
`script.lua` from Fortran.

```lua
-- script.lua
function hello()
    print('Hello from Lua!')
end
```

Make sure that `script.lua` is stored in the same directory as the Fortran
application.

```fortran
! example.f90
program main
    use, intrinsic :: iso_c_binding
    use :: lua
    implicit none

    type(c_ptr) :: l
    integer     :: rc

    l = lual_newstate()
    call lual_openlibs(l)

    rc = lual_dofile(l, 'script.lua')
    rc = lua_getglobal(l, 'hello')
    rc = lua_pcall(l, 0, 0, 0)

    call lua_close(l)
end program main
```

If the the interface bindings are installed to `/opt`, then compile,
link, and run the example with:

```
$ gfortran -I/opt/include/libfortran-lua54/ -L/usr/local/lib/lua/5.4/ \
  -o example example.f90 /opt/lib/libfortran-lua54.a -llua-5.4
$ ./example
Hello from Lua!
```

On Linux, change the prefix `/usr/local` to `/usr`. To link Lua 5.4 statically,
run instead:

```
$ gfortran -o example example.f90 libfortran-lua54.a /usr/local/lib/liblua-5.4.a
```

## Further Examples

Additional examples can be found in `examples/`.

* **fibonacci:** calls a recursive Lua routine loaded from file.
* **library:** calls a Fortran routine inside a shared library from Lua (copy
  `fortran.so` to `share/`, then run `share/library.lua`).
* **string:** runs Lua code stored in a Fortran string.
* **table:** reads values from a Lua table.

## Fortran Package Manager

You can add *fortran-lua54* as an [FPM](https://github.com/fortran-lang/fpm)
dependency:

```toml
[dependencies]
fortran-lua54 = { git = "https://github.com/interkosmos/fortran-lua54.git" }
```

## Compatibility

The integer and float types used by Lua internally depend on the targeted
platform. The program `test/types.c` outputs the types:

```
$ make test
$ ./types
lua_integer.: c_long_long
lua_number..: c_double
lua_kcontext: c_intptr_t
```

You may have to alter `lua_integer`, `lua_number`, and `lua_kcontext` in
`src/lua.f90` accordingly.

## Coverage

| Function Name           | Fortran Interface Name  | Bound | Wrapper |
|-------------------------|-------------------------|-------|---------|
| `luaL_addchar`          |                         |       |         |
| `luaL_addlstring`       |                         |       |         |
| `luaL_addsize`          |                         |       |         |
| `luaL_addstring`        |                         |       |         |
| `luaL_addvalue`         |                         |       |         |
| `luaL_argcheck`         |                         |       |         |
| `luaL_argerror`         |                         |       |         |
| `luaL_buffinit`         |                         |       |         |
| `luaL_buffinitsize`     |                         |       |         |
| `luaL_callmeta`         |                         |       |         |
| `luaL_checkany`         |                         |       |         |
| `luaL_checkinteger`     |                         |       |         |
| `luaL_checklstring`     |                         |       |         |
| `luaL_checknumber`      |                         |       |         |
| `luaL_checkoption`      |                         |       |         |
| `luaL_checkstack`       |                         |       |         |
| `luaL_checkstring`      |                         |       |         |
| `luaL_checktype`        |                         |       |         |
| `luaL_checkudata`       |                         |       |         |
| `luaL_checkversion`     |                         |       |         |
| `luaL_dofile`           | `lual_dofile`           |   ✓   |         |
| `luaL_dostring`         | `lual_dostring`         |   ✓   |         |
| `luaL_error`            |                         |       |         |
| `luaL_execresult`       |                         |       |         |
| `luaL_fileresult`       |                         |       |         |
| `luaL_getmetafield`     |                         |       |         |
| `luaL_getmetatable`     |                         |       |         |
| `luaL_getsubtable`      |                         |       |         |
| `luaL_gsub`             |                         |       |         |
| `luaL_len`              | `lual_len`              |   ✓   |         |
| `luaL_loadbufferx`      |                         |       |         |
| `luaL_loadfile`         | `lual_loadfile`         |   ✓   |         |
| `luaL_loadfilex`        | `lual_loadfilex`        |   ✓   |         |
| `luaL_loadstring`       | `lual_loadstring`       |   ✓   |    ✓    |
| `luaL_newlib`           |                         |       |         |
| `luaL_newlibtable`      |                         |       |         |
| `luaL_newmetatable`     |                         |       |         |
| `luaL_newstate`         | `lual_newstate`         |   ✓   |         |
| `luaL_openlibs`         | `lual_openlibs`         |   ✓   |         |
| `luaL_opt`              |                         |       |         |
| `luaL_optinteger`       |                         |       |         |
| `luaL_optlstring`       |                         |       |         |
| `luaL_optnumber`        |                         |       |         |
| `luaL_optstring`        |                         |       |         |
| `luaL_prepbuffer`       |                         |       |         |
| `luaL_prepbuffsize`     |                         |       |         |
| `luaL_pushresult`       |                         |       |         |
| `luaL_pushresultsize`   |                         |       |         |
| `luaL_ref`              |                         |       |         |
| `luaL_register`         |                         |       |         |
| `luaL_requiref`         |                         |       |         |
| `luaL_setfuncs`         |                         |       |         |
| `luaL_setmetatable`     |                         |       |         |
| `luaL_testudata`        |                         |       |         |
| `luaL_tolstring`        |                         |       |         |
| `luaL_traceback`        |                         |       |         |
| `luaL_typename`         |                         |       |         |
| `luaL_unref`            |                         |       |         |
| `luaL_where`            |                         |       |         |
| `lua_absindex`          |                         |       |         |
| `lua_arith`             | `lua_arith`             |   ✓   |         |
| `lua_atpanic`           |                         |       |         |
| `lua_call`              | `lua_call`              |   ✓   |         |
| `lua_callk`             | `lua_callk`             |   ✓   |         |
| `lua_checkstack`        | `lua_checkstack`        |   ✓   |         |
| `lua_close`             | `lua_close`             |   ✓   |         |
| `lua_compare`           | `lua_compare`           |   ✓   |         |
| `lua_concat`            | `lua_concat`            |   ✓   |         |
| `lua_copy`              | `lua_copy`              |   ✓   |         |
| `lua_createtable`       | `lua_createtable`       |   ✓   |         |
| `lua_dump`              |                         |       |         |
| `lua_error`             |                         |       |         |
| `lua_gc`                | `lua_gc`                |   ✓   |         |
| `lua_getallocf`         |                         |       |         |
| `lua_getextraspace`     |                         |       |         |
| `lua_getfield`          | `lua_getfield`          |   ✓   |    ✓    |
| `lua_getglobal`         | `lua_getglobal`         |   ✓   |    ✓    |
| `lua_geti`              |                         |       |         |
| `lua_getmetatable`      |                         |       |         |
| `lua_gettable`          |                         |       |         |
| `lua_gettop`            | `lua_gettop`            |   ✓   |         |
| `lua_getuservalue`      |                         |       |         |
| `lua_insert`            |                         |       |         |
| `lua_isboolean`         | `lua_isboolean`         |   ✓   |         |
| `lua_iscfunction`       | `lua_iscfunction`       |   ✓   |         |
| `lua_isfunction`        | `lua_isfunction`        |   ✓   |         |
| `lua_isinteger`         | `lua_isinteger`         |   ✓   |         |
| `lua_islightuserdata`   |                         |       |         |
| `lua_isnil`             | `lua_isnil`             |   ✓   |         |
| `lua_isnone`            | `lua_isnone`            |   ✓   |         |
| `lua_isnoneornil`       | `lua_isnoneornil`       |   ✓   |         |
| `lua_isnumber`          | `lua_isnumber`          |   ✓   |         |
| `lua_isstring`          | `lua_isstring`          |   ✓   |         |
| `lua_istable`           | `lua_istable`           |   ✓   |         |
| `lua_isthread`          | `lua_isthread`          |   ✓   |         |
| `lua_isuserdata`        | `lua_isuserdata`        |   ✓   |         |
| `lua_isyieldable`       | `lua_isyieldable`       |   ✓   |         |
| `lua_len`               |                         |       |         |
| `lua_load`              | `lua_load`              |   ✓   |         |
| `lua_newstate`          |                         |       |         |
| `lua_newtable`          | `lua_newtable`          |   ✓   |         |
| `lua_newthread`         |                         |       |         |
| `lua_newuserdata`       |                         |       |         |
| `lua_next`              |                         |       |         |
| `lua_numbertointeger`   |                         |       |         |
| `lua_pcall`             | `lua_pcall`             |   ✓   |         |
| `lua_pcallk`            | `lua_pcallk`            |   ✓   |         |
| `lua_pop`               | `lua_pop`               |   ✓   |         |
| `lua_pushboolean`       | `lua_pushboolean`       |   ✓   |         |
| `lua_pushcclosure`      | `lua_pushcclosure`      |   ✓   |         |
| `lua_pushcfunction`     |                         |       |         |
| `lua_pushfstring`       |                         |       |         |
| `lua_pushglobaltable`   |                         |       |         |
| `lua_pushinteger`       | `lua_pushinteger`       |   ✓   |         |
| `lua_pushlightuserdata` | `lua_pushlightuserdata` |   ✓   |         |
| `lua_pushliteral`       |                         |       |         |
| `lua_pushlstring`       | `lua_pushlstring`       |   ✓   |    ✓    |
| `lua_pushnil`           | `lua_pushnil`           |   ✓   |         |
| `lua_pushnumber`        | `lua_pushnumber`        |   ✓   |         |
| `lua_pushstring`        | `lua_pushstring`        |   ✓   |    ✓    |
| `lua_pushthread`        | `lua_pushthread`        |   ✓   |         |
| `lua_pushvalue`         | `lua_pushvalue`         |   ✓   |         |
| `lua_pushvfstring`      |                         |       |         |
| `lua_rawequal`          |                         |       |         |
| `lua_rawget`            | `lua_rawget`            |   ✓   |         |
| `lua_rawgeti`           | `lua_rawgeti`           |   ✓   |         |
| `lua_rawgetp`           |                         |       |         |
| `lua_rawlen`            | `lua_rawlen`            |   ✓   |         |
| `lua_rawset`            | `lua_rawset`            |   ✓   |         |
| `lua_rawseti`           | `lua_rawseti`           |   ✓   |         |
| `lua_rawsetp`           |                         |       |         |
| `lua_register`          | `lua_register`          |   ✓   |         |
| `lua_remove`            |                         |       |         |
| `lua_replace`           |                         |       |         |
| `lua_resume`            |                         |       |         |
| `lua_rotate`            |                         |       |         |
| `lua_setallocf`         |                         |       |         |
| `lua_setfield`          | `lua_setfield`          |   ✓   |    ✓    |
| `lua_setglobal`         | `lua_setglobal`         |   ✓   |    ✓    |
| `lua_seti`              | `lua_seti`              |   ✓   |         |
| `lua_setmetatable`      |                         |       |         |
| `lua_settable`          | `lua_settable`          |   ✓   |         |
| `lua_settop`            | `lua_settop`            |   ✓   |         |
| `lua_setuservalue`      |                         |       |         |
| `lua_status`            | `lua_status`            |   ✓   |         |
| `lua_stringtonumber`    |                         |       |         |
| `lua_toboolean`         | `lua_toboolean`         |   ✓   |    ✓    |
| `lua_tocfunction`       |                         |       |         |
| `lua_tointeger`         | `lua_tointeger`         |   ✓   |         |
| `lua_tointegerx`        | `lua_tointegerx`        |   ✓   |         |
| `lua_tolstring`         |                         |       |         |
| `lua_tonumber`          | `lua_tonumber`          |   ✓   |         |
| `lua_tonumberx`         | `lua_tonumberx`         |   ✓   |         |
| `lua_topointer`         |                         |       |         |
| `lua_tostring`          | `lua_tostring`          |   ✓   |         |
| `lua_tothread`          |                         |       |         |
| `lua_touserdata`        |                         |       |         |
| `lua_type`              | `lua_type`              |   ✓   |         |
| `lua_typename`          | `lua_typename`          |   ✓   |    ✓    |
| `lua_upvalueindex`      |                         |       |         |
| `lua_version`           | `lua_version`           |   ✓   |         |
| `lua_xmove`             |                         |       |         |
| `lua_yield`             |                         |       |         |
| `lua_yieldk`            |                         |       |         |

## Licence

ISC
