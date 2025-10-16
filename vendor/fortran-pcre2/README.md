# fortran-pcre2

A work-in-progress collection of Fortran 2018 ISO_C_BINDING interfaces to
Perl-compatible Regular Expressions 2
([PCRE2](https://www.pcre.org/current/doc/html/)). The library is also available
on [MacPorts](https://ports.macports.org/port/fortran-pcre2/).

## Build Instructions

You will need *libpcre2* with development headers. On FreeBSD, run:

```
# pkg install devel/pcre2
```

On Debian Linux, install:

```
# apt-get install libpcre2-8-0 libpcre2-dev
```

Clone the repository and execute the provided `Makefile` to create the static
library `libfortran-pcre2.a` containing the interfaces:

```
$ git clone https://github.com/interkosmos/fortran-pcre2
$ cd fortran-pcre2/
$ make
```

Install the library and the modules files system-wide to `/opt`:

```
$ make install PREFIX=/opt
```

Instead of `make`, you may want to build the library using the Fortran Package
Manager:

```
$ fpm build --profile release
```

Link your Fortran programs against `libfortran-pcre2.a` and `-lpcre2-8`.

## Example

The following program just compiles and executes a basic regular expression.

```fortran
! example.f90
program main
    use :: pcre2
    implicit none (type, external)

    integer, parameter :: OVECSIZE = 30 ! Must be multiple of 3.

    character(len=128)            :: buffer
    character(len=:), allocatable :: pattern, subject
    integer                       :: err_code, rc
    integer(kind=pcre2_size)      :: err_offset
    type(c_ptr)                   :: match_data, re

    pattern = '^([A-Z][a-z]+)$'
    subject = 'Fortran'

    ! Compile regular expression.
    re = pcre2_compile(pattern     = pattern, &
                       length      = len(pattern, kind=pcre2_size), &
                       options     = 0, &
                       errorcode   = err_code, &
                       erroroffset = err_offset, &
                       ccontext    = c_null_ptr)

    if (.not. c_associated(re)) then
        buffer = ' '
        rc = pcre2_get_error_message(err_code, buffer, len(buffer, kind=pcre2_size))
        print '("Error ", i0, ": ", a)', err_code, trim(buffer)
        stop
    end if

    ! Execute regular expression.
    match_data = pcre2_match_data_create(OVECSIZE, c_null_ptr)

    rc = pcre2_match(code        = re, &
                     subject     = subject, &
                     length      = len(subject, kind=pcre2_size), &
                     startoffset = int(0, kind=pcre2_size), &
                     options     = 0, &
                     match_data  = match_data, &
                     mcontext    = c_null_ptr)

    if (rc == 0) then
        print '("OVECSIZE too small")'
    else if (rc < 0) then
        select case (rc)
            case (PCRE2_ERROR_NOMATCH)
                print '("No match")'
            case default
                print '("Matching error ", i0)', rc
        end select
    else if (rc > 0) then
        print '("Match!")'
    end if

    call pcre2_match_data_free(match_data)
    call pcre2_code_free(re)
end program main
```

If the library has been installed to `/opt`, then compile, link, and run the
program with:

```
$ gfortran -I/opt/include/libfortran-pcre2 -o example example.f90 /opt/lib/libfortran-pcre2.a -lpcre2-8
$ ./example
```

## Fortran Package Manager

You can add *fortran-pcre2* as an [FPM](https://github.com/fortran-lang/fpm)
dependency:

```toml
[dependencies]
fortran-pcre2 = { git = "https://github.com/interkosmos/fortran-pcre2.git" }
```

## Compatibility

It is not necessary to null-terminate character strings given to the procedures
of *fortran-pcre2*. In contrast to the C API of PCRE2, you must not free
substrings with `pcre2_substring_free()`, as this will be done by the wrapper
functions.

## Coverage

| C API                                | Fortran interface                   |
|--------------------------------------|-------------------------------------|
| `pcre2_code_free_8`                  |  `pcre2_code_free`                  |
| `pcre2_compile_8`                    |  `pcre2_compile`                    |
| `pcre2_compile_context_free_8`       |  `pcre2_compile_context_free`       |
| `pcre2_get_error_message_8`          |  `pcre2_get_error_message`          |
| `pcre2_get_ovector_count_8`          |  `pcre2_get_ovector_count`          |
| `pcre2_get_ovector_pointer_8`        |  `pcre2_get_ovector_pointer`        |
| `pcre2_match_8`                      |  `pcre2_match`                      |
| `pcre2_match_data_create_8`          |  `pcre2_match_data_create`          |
| `pcre2_match_data_free_8`            |  `pcre2_match_data_free`            |
| `pcre2_substring_copy_byname_8`      |  `pcre2_substring_copy_byname`      |
| `pcre2_substring_copy_bynumber_8`    |  `pcre2_substring_copy_bynumber`    |
| `pcre2_substring_free_8`             |  `pcre2_substring_free`             |
| `pcre2_substring_get_byname_8`       |  `pcre2_substring_get_byname`       |
| `pcre2_substring_get_bynumber_8`     |  `pcre2_substring_get_bynumber`     |
| `pcre2_substring_number_from_name_8` |  `pcre2_substring_number_from_name` |

## Licence

ISC
