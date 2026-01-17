! fibonacci.f90
!
! Example program that calls the recursive Lua routine `fib()` in file
! `fibonacci.lua` and outputs the result.
program main
    use, intrinsic :: iso_c_binding, only: c_ptr
    use :: lua
    implicit none
    character(len=*), parameter :: FILE_NAME = 'share/fibonacci.lua'

    type(c_ptr) :: l
    integer     :: nargs, nresults
    integer     :: rc, x
    logical     :: file_exists

    integer(kind=lua_integer) :: r1, r2

    inquire (file=FILE_NAME, exist=file_exists)
    if (.not. file_exists) stop 'Error: Lua file not found'

    nargs = 1
    nresults = 2
    x = 10

    l = lual_newstate()
    call lual_openlibs(l)
    rc = lual_dofile(l, FILE_NAME)
    rc = lua_getglobal(l, 'fib')

    if (lua_isfunction(l, -1) == 1) then
        call lua_pushinteger(l, int(x, kind=lua_integer))
        rc = lua_pcall(l, nargs, nresults, 0)

        r1 = lua_tointeger(l, -1)
        r2 = lua_tointeger(l, -2)

        call lua_pop(l, 2)

        print '("fibonacci(", i0, ") = ", i0)', x, r2
    end if

    call lua_close(l)
end program main
