# fortran-modbus

A collection of Fortran 2018 interface bindings to
[libmodbus](https://libmodbus.org/), for Modbus RTU/TCP communication.

## Build Instructions

The package _libmodbus_ has to be installed with development headers. On
FreeBSD, run:

```
# pkg install comms/libmodbus
```

On Linux, instead:

```
# apt-get install libmodbus5 libmodbus-dev
```

Build and install the Fortran library using the provided Makefile:

```
$ make
$ make install PREFIX=/opt
```

Link your programs against `/opt/lib/libfortran-modbus.a -lmodbus`. Optionally,
overwrite the default compiler and the compiler flags:

```
$ make FC=ifx FFLAGS="-O3"
```

Or, use the [Fortran Package Manager](https://github.com/fortran-lang/fpm):

```
$ fpm build --profile release
```

Build and run the test program:

```
$ make test
$ ./test_modbus
```

## Example

The following example program connects to a device via Modbus RTU and outputs
two registers as a real number:

```fortran
! example.f90
program main
    use, intrinsic :: iso_c_binding
    use :: modbus
    use :: modbus_rtu
    implicit none (type, external)

    integer, parameter :: ADDRESS = 50
    integer, parameter :: SLAVE   = 10

    integer                  :: stat
    integer(kind=c_uint16_t) :: regs(2)
    real                     :: f
    type(c_ptr)              :: ctx

    ctx  = c_null_ptr
    stat = -1
    regs = 0

    mb_block: block
        ! Create Modbux RTU context.
        ctx = modbus_new_rtu('/dev/ttyUSB0', 19200, 'E', 8, 1)
        if (.not. c_associated(ctx)) exit mb_block

        ! Connect to device.
        stat = modbus_connect(ctx)
        if (stat == -1) exit mb_block

        ! Set slave number.
        stat = modbus_set_slave(ctx, SLAVE)
        if (stat == -1) exit mb_block

        ! Read registers.
        stat = modbus_read_registers(ctx, ADDRESS, size(regs), regs)
        if (stat == -1) exit mb_block

        ! Convert to real.
        f = modbus_get_float_abcd(regs)
        print '(f12.8)', f
    end block mb_block

    call modbus_close(ctx)
    call modbus_free(ctx)

    if (stat == -1) print '(a)', 'Error: operation failed'
end program main
```

If the Fortran library is installed to `/opt/lib`, run:

```
$ gfortran -o example example.f90 /opt/lib/libfortran-modbus.a -lmodbus
```

## Fortran Package Manager

You can add *fortran-modbus* as an FPM dependency:

```toml
[dependencies]
fortran-modbus = { git = "https://github.com/interkosmos/fortran-modbus.git" }
```

## Licence

ISC
