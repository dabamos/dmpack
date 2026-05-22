# fortran-zeromq

A work-in-progress collection of Fortran 2018 ISO C binding interfaces to the
ZeroMQ core library _libzmq_ (Version 4) and the high-level C binding CZMQ
(Version 4).

## Build Instructions

Install ZeroMQ 4 and CZMQ 4 with development headers. On FreeBSD, run:

```
# pkg install net/czmq4 net/libzmq4
```

### Make

Execute the Makefile:

```
$ make
```

Install the library and the module files to `/opt`:

```
$ make install PREFIX=/opt
```

Link your programs against `/opt/lib/libfortran-zeromq.a` and `-lzmq` (ZeroMQ)
or `-lczmq` (CZMQ). Make sure to pass the path to the _fortran-zeromq_ module
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
$ gfortran -I/opt/include/fortran-zeromq -o client client.f90 /opt/lib/libfortran-zeromq.a -lczmq
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
$ gfortran -I/opt/include/fortran-zeromq -o server server.f90 /opt/lib/libfortran-zeromq.a -lczmq
$ ./server
server waiting on port 5555 ...
```

## References

* [CZMQ – High-level C Binding for ZeroMQ](http://czmq.zeromq.org/)
* [ZMQ API reference](https://libzmq.readthedocs.io/en/latest/)
* [ZMQ man pages](https://libzmq.readthedocs.io/en/latest/zmq.html)
* [ZeroMQ web site](https://zeromq.org/)
* [ØMQ – The Guide](https://zguide.zeromq.org/)

## Licence

ISC
