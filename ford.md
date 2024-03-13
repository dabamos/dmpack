project:         DMPACK
version:         0.9.4
license:         isc
doc_license:     by
source:          false
incl_src:        false
graph:           true
search:          false
favicon:         favicon.png
summary:         **Deformation Monitoring Package (DMPACK)** – A software
                 library in Fortran 2018 for automatic deformation monitoring in
                 the Internet of Things on Linux and FreeBSD. DMPACK is a
                 scientific monitoring system developed for automated control
                 measurements of buildings, infrastructure, terrain, geodetic
                 nets, and other objects. The software runs on sensor nodes
                 and obtains observation data from arbitrary sensors, like
                 robotic total stations, digital levels, inclinometers, weather
                 stations, or GNSS receivers. The raw sensor data is then
                 processed, stored, and optionally transmitted to a server.
author:          Philipp Engel
project_github:  https://github.com/dabamos/dmpack
project_website: https://www.dabamos.de

This is the source code documentation of the DMPACK library. Import module
`dmpack` to access any of the procedures in the library:

```fortran
! app.f90
program main
    use :: dmpack
    implicit none (type, external)

    ! Initialise DMPACK.
    call dm_init()

    ! Call any DMPACK procedures here.
    call dm_version_out()
end program main
```

Link the program against `libdmpack.a` or `-ldmpack`:

```
$ gfortran -I/usr/local/include/dmpack -o app app.f90 /usr/local/lib/libdmpack.a
```

The path of the include search directory containing the DMPACK module files has
to be passed through argument `-I`.
