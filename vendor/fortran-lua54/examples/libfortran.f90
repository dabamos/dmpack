! libfortran.f90
module libfortran
    !! The function `luaopen_libfortran()` is called by Lua to register the
    !! Fortran routine `hello()`. Compile this Fortran module to shared library
    !! `libfortran.so`, copy it to `share/`, and run `library.lua`.
    use, intrinsic :: iso_c_binding, only: c_int, c_funloc, c_ptr
    use :: lua
    implicit none

    public :: luaopen_libfortran ! Module registration function.
    public :: hello              ! Routine callable from Lua.
contains
    integer(c_int) function luaopen_libfortran(l) bind(c) result(n)
        !! Utility function to register the Fortran routine `hello()`.
        !!
        !! The function postfix (`libfortran`) must match the name of the shared
        !! library (`libfortran.so`).
        type(c_ptr), intent(in), value :: l

        call lua_register(l, 'hello', c_funloc(hello))
        n = 1
    end function luaopen_libfortran

    subroutine hello(l) bind(c)
        !! The Fortran routine callable from Lua.
        type(c_ptr), intent(in), value :: l

        print '(a)', 'Hello from Fortran!'
    end subroutine hello
end module libfortran
