project:         DMPACK
version:         0.9
license:         isc
doc_license:     by
incl_src:        false
graph:           true
search:          false
favicon:         favicon.png
summary:         **Deformation Monitoring Package (DMPACK)** – A collections
                 of programs for automatic deformation monitoring in the
                 Internet of Things, written in Fortran 2018, for Linux and
                 FreeBSD.
author:          Philipp Engel
date:            September 8, 2023
project_website: https://www.dabamos.de
project_url:     https://www.dabamos.de/dmpack/doc
project_github:  https://github.com/dabamos/dmpack

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
    ! ...
end program main
```

Link the program against `libdmpack.a`:

```
$ gfortran -o app app.f90 libdmpack.a
```

You may have to pass the path of the include search directory containing the
DMPACK module files (argument `-I`).
