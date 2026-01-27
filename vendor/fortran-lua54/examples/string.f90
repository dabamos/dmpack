! string.f90
program main
    !! Example that shows how to run Lua code from a string.
    use :: lua
    implicit none
    type(c_ptr) :: l
    integer     :: rc

    l = lual_newstate()
    call lual_openlibs(l)

    rc = lual_loadstring(l, 'print("Hello from Lua!")')
    rc = lua_pcall(l, 0, 0, 0)

    call lua_close(l)
end program main
