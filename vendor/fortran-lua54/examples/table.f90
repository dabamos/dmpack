! table.f90
! Example that shows how to access a Lua table.
program main
    use, intrinsic :: iso_c_binding, only: c_ptr
    use, intrinsic :: iso_fortran_env, only: r8 => real64
    use :: lua
    implicit none

    character(len=*), parameter :: FILE_NAME = 'share/table.lua'

    integer       :: i, rc
    real(kind=r8) :: pi
    type(c_ptr)   :: l

    l = lual_newstate()            ! Create Lua state.
    call lual_openlibs(l)          ! Open Lua standard library.
    rc = lual_dofile(l, FILE_NAME) ! Open Lua and run file.

    ! Get the table.
    rc = lua_getglobal(l, 'a')

    if (lua_istable(l, -1) == 1) then
        ! Get table field.
        rc = lua_getfield(l, -1, 'pi')

        if (lua_isnumber(l, -1) == 1) then
            ! Convert to real.
            pi  = lua_tonumber(l, -1)
            print '("pi: ", f13.11)', pi
        end if

        call lua_pop(l, 1)

        ! Get next table field.
        rc = lua_getfield(l, -1, 'foo')

        if (lua_isstring(l, -1) == 1) then
            print '("foo: ", a)', lua_tostring(l, -1)
        end if

        call lua_pop(l, 1)
    end if

    call lua_pop(l, -1)

    ! Get the numerical table.
    rc = lua_getglobal(l, 'i')

    if (lua_istable(l, -1) == 1) then
        ! Get table value.
        do i = 1, 3
            call lua_pushnumber(l, real(i, 8))
            rc = lua_rawget(l, -2)

            if (lua_isnumber(l, -1) == 1) then
                print '("i: ", i4)', lua_tointeger(l, -1)
            end if

            call lua_pop(l, 1)
        end do
    end if

    call lua_pop(l, -1)

    ! Get the numerical table.
    rc = lua_getglobal(l, 'i')

    if (lua_istable(l, -1) == 1) then
        ! Get table value.
        print '("table length: ", i0)', lua_rawlen(l, -1)

        do i = 1, 3
            rc = lua_rawgeti(l, -1, int(i, kind=lua_integer))

            if (lua_isnumber(l, -1) == 1) then
                print '("i: ", i4)', lua_tointeger(l, -1)
            end if

            call lua_pop(l, 1)
        end do
    end if

    call lua_pop(l, -1)
    call lua_close(l)
end program main
