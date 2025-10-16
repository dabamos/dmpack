! test_modbus.f90
!
! Author:  Philipp Engel
! Licence: ISC
program main
    use :: modbus
    use :: modbus_tcp
    implicit none (type, external)

    logical :: stat

    print '("Version: ", i0, ".", i0, ".", i0)', LIBMODBUS_VERSION_MAJOR, &
                                                 LIBMODBUS_VERSION_MINOR, &
                                                 LIBMODBUS_VERSION_MICRO
    stat = test_modbus_tcp()
    if (.not. stat) error stop
contains
    logical function test_modbus_tcp() result(stat)
        type(c_ptr) :: ctx

        stat = .false.

        print '("Creating Modbus TCP context ...")'
        ctx = modbus_new_tcp('127.0.0.1', 1502)

        if (.not. c_associated(ctx)) then
            print '("Error: modbus_new_tcp() failed")'
            print '(a)', modbus_strerror(0)
            return
        end if

        print '("Releasing Modbus TCP context ...")'
        call modbus_free(ctx)

        if (c_associated(ctx)) then
            print '("Error: modbus context is still associated (compiler bug)")'
            return
        end if

        stat = .true.
    end function test_modbus_tcp
end program main
