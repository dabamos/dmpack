! string.f90
! Example that shows how to run Lua code from a string.
program main
    use, intrinsic :: iso_c_binding, only: c_ptr
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
