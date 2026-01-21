# fortran-nng

![Language](https://img.shields.io/badge/-Fortran-734f96?logo=fortran&logoColor=white)
![License](https://img.shields.io/github/license/interkosmos/fortran-nng?color=blue)
![Build](https://github.com/interkosmos/fortran-nng/actions/workflows/build.yml/badge.svg)

A collection of Fortran 2018 ISO C binding interfaces to the lightweight
messaging library [NNG](https://nng.nanomsg.org/) **v1.11.0**:

> NNG, like its predecessors [nanomsg](https://github.com/nanomsg/nanomsg) (and
> to some extent [ZeroMQ](https://zeromq.org/)), is a lightweight, broker-less
> library, offering a simple API to solve common recurring messaging problems,
> such as publish/subscribe, RPC-style request/reply, or service discovery. The
> API frees the programmer from worrying about details like connection
> management, retries, and other common considerations, so that they can focus
> on the application instead of the plumbing.

The bindings cover the API of NNG v1 (stable) and are not compatible to NNG v2
(yet). Link your Fortran programs with `libfortran-nng.a` and `libnng.so`. If
_fortran-nng_ is installed to `/opt`, run:

```
$ gfortran -I/opt/include/libfortran-nng -o example example.f90 /opt/lib/libfortran-nng.a -lnng
```

For Fortran bindings to nanomsg, see
[nanofort](https://github.com/jshahbazi/nanofort).

## Dependencies

On Linux, install NNG with development headers first:

```
$ sudo apt-get install libnng1 libnng-dev nng-utils
```

On FreeBSD:

```
$ doas pkg install net/nng
```

Alternatively, build NNG simply from source and install it to `/opt`:

```
$ git clone --depth 1 --branch v1.11 https://github.com/nanomsg/nng.git
$ mkdir -p nng/build && cd nng/build/
$ cmake -DCMAKE_INSTALL_PREFIX:PATH=/opt ..
$ make
$ make install
```

## Build Instructions

If the NNG library is installed globally, build and install the interfaces
library with GNU Fortran by running:

```
$ make
```

If you prefer LLVM, run, for example:

```
$ make CC=clang21 FC=flang21
```

If the NNG library is installed in `/opt`, pass the prefix:

```
$ make PREFIX=/opt
```

To link against a static `libnng.a` library instead, run:

```
$ make LDLIBS="/path/to/libnng.a -lpthread"
```

Install the _fortran-nng_ library and module files to `/opt`:

```
$ make install PREFIX=/opt
```

## Fortran Package Manager

This project supports the
[Fortran Package Manager](https://github.com/fortran-lang/fpm) (FPM). To build
the project with FPM, run:

```
$ fpm build --profile release
```

The example programs are available with the ``fpm run --example`` command. You
can add *fortran-nng* to your `fpm.toml` with:

```toml
[dependencies]
fortran-nng = { git = "https://github.com/interkosmos/fortran-nng.git" }
```

## Examples

The following programs can be found in directory `examples/`:

* **async** implements an asynchronous RPC service.
* **bus** creates an interconnected mesh network.
* **http_client** sends an HTTP GET request and outputs the response.
* **pair** shows one-to-one peer relationship.
* **pipeline** solves producer/consumer problem with pipeline (one-way pipe).
* **pubsub** shows pub/sub messaging pattern.
* **pubsub_forwarder** implements a simple pub/sub forwarder.
* **reqrep** demonstrates request-response pattern between client and server.
* **survey** sends a timed survey out.

Build the examples with:

```
$ make examples
```

## Compatibility

The _fortran-nng_ bindings differ slightly from the C API:

* Derived types do not have to be initialised like in C.
* All strings passed to the NNG interfaces have to be properly null-terminated
  with constant `c_null_char`, except for parameter strings already provided by
  the Fortran modules. You can use function `f_c_str()` from module `nng_util`
  to add the null-termination.
* C string pointers returned by NNG functions are converted to Fortran
  allocatable character through wrappers. The interfaces are public and end with
  suffix `_`.
* The prefix of `nng_log_level` enumerators has been changed from `NNG_LOG_` to
  `NNG_LOG_LEVEL_` due to conflicts with procedures of the same name.
* The prefix of `nng_log_facility` enumerators has been changed from `NNG_LOG_` to
  `NNG_LOG_FACILITY_` due to conflicts with procedures of the same name.
* The interfaces to the NNG log functions expect a single (non-variadic) string
  argument only.

## Source Code Documentation

The source code documentation has to be created with
[FORD](https://github.com/Fortran-FOSS-Programmers/ford). Install the Python
package with:

```
$ python3 -m pip install -U ford
```

In the _fortran-nng_ repository, run:

```
$ make doc
```

The HTML files will be written to directory `doc/`. Open `index.html` in a web
browser.

## References

* [NNG Web Site](https://nng.nanomsg.org/)
* [NNG Repository](https://github.com/nanomsg/nng)
* [NNG Reference Manual](https://nng.nanomsg.org/ref/preface.html)
* [NNG API Documentation](https://nng.nanomsg.org/man/v1.10.0/index.html) (v1.10.0)
* [Getting Started with NNG](https://nanomsg.org/gettingstarted/nng/index.html)

## Licence

ISC
