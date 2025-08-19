/* Test program that outputs the type of lua_Integer, lua_Number, and
 * lua_KContext.
 *
 * To compile, run:
 *
 * $ cc `pkg-config --cflags lua-5.4` -o types types.c `pkg-config --libs lua-5.4`
 */
#include <stdio.h>
#include <luaconf.h>

int main(int argc, char *argv[])
{
    printf("lua_integer.: ");

#if LUA_INT_TYPE == LUA_INT_INT
    printf("c_int\n");
#elif LUA_INT_TYPE == LUA_INT_LONG
    printf("c_long\n");
#elif LUA_INT_TYPE == LUA_INT_LONGLONG
    printf("c_long_long\n");
#else
    printf("?\n");
#endif

    printf("lua_number..: ");

#if LUA_FLOAT_TYPE == LUA_FLOAT_FLOAT
    printf("c_float\n");
#elif LUA_FLOAT_TYPE == LUA_FLOAT_LONGDOUBLE
    printf("c_long_double\n");
#elif LUA_FLOAT_TYPE == LUA_FLOAT_DOUBLE
    printf("c_double\n");
#else
    printf("?\n");
#endif

    printf("lua_kcontext: ");

#if !defined(LUA_USE_C89) && defined(__STDC_VERSION__) && __STDC_VERSION__ >= 199901L
#if defined(INTPTR_MAX)
    printf("c_intptr_t\n");
#else
    printf("c_ptrdiff_t\n");
#endif
#else
    printf("c_ptrdiff_t\n");
#endif
}
