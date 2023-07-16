# fortran-zlib

A collection of Fortran 2018 ISO_C_BINDING interfaces to selected zlib
functions. The library is also available on
[MacPorts](https://ports.macports.org/port/fortran-zlib/).

## Build Instructions

Simply run the provided Makefile:

```
$ make
```

This outputs the static library `libfortran-zlib.a`. Link your program against
`libfortran-zlib.a -lz`. Optionally, overwrite the default compiler:

```
$ make FC=ifort
```

Alternatively, you can compile the library with
[fpm](https://github.com/fortran-lang/fpm):

```
$ fpm build --profile=release
```

Build and run the test program:

```
$ make test
$ ./test_zlib
```

## Example

The following basic example compresses and uncompresses an input string.

```fortran
! example.f90
program main
    use :: zlib
    implicit none (type, external)

    character(len=:), allocatable :: str_in, str_out, str_x
    integer(kind=z_ulong)         :: sz_in, sz_out, sz_x
    integer                       :: rc

    ! Input.
    str_in = repeat('Fortran ', 10)
    sz_in  = len(str_in, kind=z_ulong)

    ! Compress.
    sz_x = compress_bound(sz_in)
    allocate (character(len=sz_x) :: str_x)
    rc = compress(str_x, sz_x, str_in, sz_in)
    if (rc /= Z_OK) stop 'Error: compress() failed'

    ! Uncompress.
    sz_out = sz_in
    allocate (character(len=sz_out) :: str_out)
    rc = uncompress(str_out, sz_out, str_x, sz_x)
    if (rc /= Z_OK) stop 'Error: uncompress() failed'
end program main
```

Compile, link, and run the example program with:

```
$ gfortran -o example example.f90 libfortran-zlib.a -lz
$ ./example
```

## Coverage

| C function      | Fortran interface |
|-----------------|-------------------|
| `adler32`       | `adler32`         |
| `adler32_z`     | `adler32_z`       |
| `compress`      | `compress`        |
| `compress2`     | `compress2`       |
| `compressBound` | `compress_bound`  |
| `crc32`         | `crc32`           |
| `crc32_z`       | `crc32_z`         |
| `deflate`       | `deflate`         |
| `deflateEnd`    | `deflate_end`     |
| `deflateInit`   | `deflate_init`    |
| `deflateInit2`  | `deflate_init2`   |
| `inflate`       | `inflate`         |
| `inflateEnd`    | `inflate_end`     |
| `inflateInit`   | `inflate_init`    |
| `inflateInit2`  | `inflate_init2`   |
| `uncompress`    | `uncompress`      |
| `uncompress2`   | `uncompress2`     |

## Fortran Package Manager

You can add *fortran-zlib* as an *fpm* dependency:

```toml
[dependencies]
fortran-zlib = { git = "https://github.com/interkosmos/fortran-zlib.git" }
```

## Licence

ISC
