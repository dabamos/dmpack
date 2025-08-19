# fortran-zstd

A collection of Fortran 2018 interface bindings to selected
[Zstandard](http://www.zstd.net/) functions (zstd ≥ 1.5.5). In comparison to the
original C API, the Fortran interfaces, types, and arguments have been converted
to snake case. See [COVERAGE](COVERAGE.md) for an overview of bound procedures.

## Build Instructions

The zstd library has to be installed with development headers. On FreeBSD, run:

```
# pkg install archivers/zstd
```

On Linux, instead:

```
# apt-get install libzstd1 libzstd-dev
```

Or, to build the zstd library from source, and to install to `/usr/local`, run:

```
$ cd /tmp/
$ git clone --depth 1 https://github.com/facebook/zstd
$ cd zstd/build/cmake/
$ mkdir build && cd build/
$ cmake ..
$ cmake --build . --config Release
$ sudo cmake --install . --prefix /usr/local
```

Build and install the Fortran library using the provided Makefile:

```
$ make
$ make install PREFIX=/opt
```

Link your programs against `/opt/lib/libfortran-zstd.a -lzstd`. Optionally,
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
$ ./test_zstd
```

## Example

The following basic example compresses and uncompresses an input string.

```fortran
! example.f90
program main
    use, intrinsic :: iso_c_binding
    use :: zstd
    implicit none (type, external)

    character(len=:), allocatable :: dst1, dst2, src
    integer(kind=c_size_t)        :: dst_len, src_len, stat

    src = repeat('Hello, there! ', 32)

    src_len = len(src, kind=c_size_t)
    dst_len = zstd_compress_bound(src_len)

    allocate (character(len=dst_len) :: dst1)
    allocate (character(len=src_len) :: dst2)

    stat = zstd_compress(dst1, dst_len, src, src_len, zstd_default_c_level())
    dst_len = stat

    if (zstd_is_error(stat)) then
        print '("zstd_compress: ", a)', zstd_get_error_name(stat)
        error stop
    end if

    stat = zstd_decompress(dst2, src_len, dst1, dst_len)

    if (zstd_is_error(stat)) then
        print '("zstd_decompress: ", a)', zstd_get_error_name(stat)
        error stop
    end if
end program main
```

If the library has been installed to `/opt`, then compile, link, and run the
example program with:

```
$ gfortran -I/opt/include/libfortran-zstd -o example example.f90 \
  /opt/lib/libfortran-zstd.a -lzstd
$ ./example
```

## Fortran Package Manager

You can add *fortran-zstd* as an FPM dependency:

```toml
[dependencies]
fortran-zstd = { git = "https://github.com/interkosmos/fortran-zstd.git" }
```

## References

* [Zstandard manual](http://facebook.github.io/zstd/zstd_manual.html)
* [Zstandard repository](https://github.com/facebook/zstd)
* [Zstandard website](http://www.zstd.net/)

## Licence

ISC
