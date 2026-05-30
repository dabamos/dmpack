# fortran-zeromq

![Language](https://img.shields.io/badge/-Fortran-734f96?logo=fortran&logoColor=white)
![License](https://img.shields.io/github/license/interkosmos/fortran-zeromq?color=blue)
![Build](https://img.shields.io/github/actions/workflow/status/interkosmos/fortran-zeromq/build.yml)

A work-in-progress collection of Fortran 2018 ISO C binding interfaces to the
ZeroMQ core library _libzmq_ (≥ Version 4.3.5) and the high-level C binding CZMQ
(≥ Version 4.2.1). For older Fortran 2003 bindings to ZeroMQ, see
[FZMQ](https://github.com/richsnyder/fzmq).

## Build Instructions

Install ZeroMQ 4 and CZMQ 4 with development headers. On FreeBSD, run:

```
$ doas pkg install net/libzmq4 net/czmq4
```

On Linux, run instead:

```
$ sudo apt install libzmq5 libzmq3-dev libczmq4 libczmq-dev
```

If the packages provided in the repository are too old, you may want to build
ZeroMQ 4 simply from source and install it to `/opt`:

```
$ cd /tmp/
$ curl -O -L -s https://github.com/zeromq/libzmq/releases/download/v4.3.5/zeromq-4.3.5.tar.gz
$ tar xfvz zeromq-4.3.5.tar.gz
$ cd zeromq-4.3.5/
$ ./configure --prefix=/opt
$ make
$ make install
```

And to build CZMQ 4 from source and install it to `/opt`:

```
$ cd /tmp/
$ curl -O -L -s https://github.com/zeromq/czmq/releases/download/v4.2.1/czmq-4.2.1.tar.gz
$ tar xfvz czmq-4.2.1.tar.gz
$ cd czmq-4.2.1/
$ ./configure --prefix=/opt
$ make
$ make install
```

### Make

Execute the Makefile to build all bindings (`libfortran-zeromq.a`):

```
$ make
```

Build with ZeroMQ draft and poller procedure bindings:

```
$ make FFLAGS="-O2 -DHAS_DRAFT -DHAS_POLLER"
```

Build only _libzmq_ bindings (`libfortran-zmq.a`):

```
$ make zmq
```

Build only CZMQ bindings (`libfortran-czmq.a`):

```
$ make czmq
```

Install the libraries and the module files to `/opt`:

```
$ make install PREFIX=/opt
```

Link your programs against `/opt/lib/libfortran-zeromq.a` and `-lzmq` (ZeroMQ)
and `-lczmq` (CZMQ). Make sure to pass the path to the _fortran-zeromq_ module
files, for instance, with argument `-I/opt/include/fortran-zeromq`.

Examples are provided in directory `examples/`. Build them with:

```
$ make examples
```

### Fortran Package Manager

Run FPM to build the library:

```
$ fpm build --profile release
```

You can add *fortran-zeromq* as a dependency to your `fpm.toml`:

```toml
[dependencies]
fortran-zeromq = { git = "https://github.com/interkosmos/fortran-zeromq.git" }
```

## Example

The following programs `client.f90` and `server.f90` implement the REQ/REP
pattern using CZMQ. The _fortran-zeromq_ library is assumed to be installed to
`/opt`.

### Client

The client opens a REQ socket and sends string `Hello` to the server, expecting
a response:

``` fortran
! client.f90
program main
    use :: zeromq
    implicit none (type, external)

    character(:), allocatable :: msg
    integer                   :: rc
    type(c_ptr)               :: req

    req = zsock_new_req('tcp://localhost:5555')
    if (.not. c_associated(req)) error stop

    rc  = zstr_send(req, 'Hello')
    msg = zstr_recv(req)

    if (len(msg) > 0) print '("received reply: ", a)', msg
    call zsock_destroy(req)
end program main
```

Build and run the client:

```
$ gfortran -I/opt/include/fortran-zeromq -o client client.f90 /opt/lib/libfortran-zeromq.a -lzmq -lczmq
$ ./client
```

### Server

The server opens a REP socket and replies to requests:

``` fortran
! server.f90
program main
    use :: zeromq
    implicit none (type, external)

    character(:), allocatable :: msg
    integer                   :: rc
    type(c_ptr)               :: rep

    rep = zsock_new_rep('tcp://localhost:5555')
    if (.not. c_associated(rep)) error stop
    print '("server waiting on port 5555 ...")'

    do
        msg = zstr_recv(rep)
        if (len(msg) == 0) exit
        print '("received: ", a)', msg
        rc = zstr_send(rep, 'World')
    end do

    call zsock_destroy(rep)
end program main
```

Build and run the server:

```
$ gfortran -I/opt/include/fortran-zeromq -o server server.f90 /opt/lib/libfortran-zeromq.a -lzmq -lczmq
$ ./server
server waiting on port 5555 ...
```

## Further Examples

Additional example programs are provided in `examples/`. Run:

```
$ make examples
```

If ZeroMQ and CZMQ are installed to `/opt`, run instead:

```
$ make examples LIBZMQ="-Wl,-rpath=/opt/lib -L/opt/lib -lzmq" LIBCZMQ="-Wl,-rpath=/opt/lib -L/opt/lib -lczmq"
```

## References

* [CZMQ – High-level C Binding for ZeroMQ](http://czmq.zeromq.org/)
* [ZMQ API reference](https://libzmq.readthedocs.io/en/latest/)
* [ZMQ man pages](https://libzmq.readthedocs.io/en/latest/zmq.html)
* [ZeroMQ web site](https://zeromq.org/)
* [ØMQ – The Guide](https://zguide.zeromq.org/)

## Licence

ISC
