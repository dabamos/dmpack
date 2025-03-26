# Deformation Monitoring Package (DMPACK)

![Build](https://img.shields.io/github/actions/workflow/status/dabamos/dmpack/build.yml)
![License](https://img.shields.io/github/license/dabamos/dmpack?color=blue)
![Version](https://img.shields.io/badge/version-0.9.6-blue)

**DMPACK** is a free software package for IoT-based automatic deformation
monitoring and distributed control measurements in engineering geodesy and
geotechnics. The project is the successor of
[OpenADMS](https://www.dabamos.de/openadms.html) and in active development.

The library and the accompanying programs are written in Fortran 2018, with
some smaller parts in Lua 5.4. At the moment, **DMPACK** runs on 64-bit Linux
and FreeBSD only.

* [Project Website](https://www.dabamos.de/dmpack.html)
* [User Guide](https://www.dabamos.de/dmpack/guide/)
* [Screen Shots](https://www.dabamos.de/dmpack.html#screen-shots)
* [Source Code Documentation](https://www.dabamos.de/dmpack/doc/)
* [GitHub Repository](https://github.com/dabamos/dmpack)

Breaking API changes have to be expected until version 1.0.0.

## Overview

**DMPACK** is a scientific monitoring system and sensor network middleware
developed for automated control measurements of buildings, infrastructure,
terrain, geodetic nets, and other objects.

The software runs on sensor nodes, usually IoT gateways, industrial embedded
systems, or single-board computers, and obtains observation data from arbitrary
sensors, like robotic total stations, digital levels, inclinometers, weather
stations, or GNSS receivers.

The raw sensor data is then processed, stored, and optionally transmitted to a
server. The software package may be used to monitor objects like:

* bridges, tunnels, dams
* motorways, railways
* construction sites, mining areas
* landslides, cliffs, glaciers
* churches, monasteries, and other heritage buildings

**DMPACK** is built around the relational SQLite database for time series and
log storage on client and server. The server component is optional. It is
possible to run **DMPACK** on clients only, without data distribution. The
client-side inter-process communication is based on POSIX message queues and
POSIX semaphores.

## System Architecture

![Schema](https://raw.githubusercontent.com/dabamos/dmpack/refs/heads/master/guide/resources/images/schema.png)

## Features

**DMPACK** includes modules for:

* sensor control (RS-232/422/485, TTL/UART, Modbus RTU/TCP, 1-Wire,
  sub-process, file system)
* SQLite database access
* message passing via POSIX message queues
* process synchronisation via POSIX semaphores
* data serialisation (ASCII, CSV, GeoJSON, HDF5, JSON, JSON Lines, Namelist)
* server-side web applications (CGI, FastCGI)
* HTTP-based remote procedure call API
* concurrent data synchronisation between client and server
* client status messages (heartbeats)
* distributed logging
* Leica GeoCOM API for Fortran
* Deutscher Wetterdienst (DWD) API
* MPPT and solar battery monitoring (VE.Direct)
* camera access (V4L2, RTSP)
* time series plotting
* Lua scripting
* MQTT and XMPP connectivity
* HTML5 reports
* Atom XML web feeds
* e-mail via SMTP
* compression (deflate, zstd)
* regular expression matching
* configuration file evaluation

## Requirements

**DMPACK** has the following requirements:

* Linux or FreeBSD operating system
* 64-bit platform (x86-64, AArch64)
* Fortran 2018 and ANSI C compiler (GNU, LLVM, Intel)

Third-party dependencies have to be present to build and run the software of
this package:

* FastCGI
* HDF5
* LAPACK
* libcurl
* libmodbus
* libstrophe
* Lua 5.4
* PCRE2
* SQLite 3
* zlib
* zstd

On Linux, development headers are required for compilation. It is recommended
to additionally install:

* FFmpeg
* Gnuplot
* GraphicsMagick

To generate the [man pages](adoc/README.md), the [user guide](guide/README.md),
and the source code documentation, you will need furthermore:

* [AsciiDoctor](https://asciidoctor.org/), [Pygments](https://pygments.org/), and
  [pygments.rb](https://rubygems.org/gems/pygments.rb/versions/2.2.0)
* [FORD](https://github.com/Fortran-FOSS-Programmers/ford)

## Program Overview

The following programs are based on the **DMPACK** library.

| Name                           | Description                                                         |
|--------------------------------|---------------------------------------------------------------------|
| [dmapi](adoc/dmapi.adoc)       | FastCGI-based HTTP-RPC API service.                                 |
| [dmbackup](adoc/dmbackup.adoc) | Creates online backups of DMPACK databases.                         |
| [dmbeat](adoc/dmbeat.adoc)     | Sends status messages (heartbeats) periodically to RPC service.     |
| [dmbot](adoc/dmbot.adoc)       | XMPP chat bot that answers to commands from authorised users.       |
| [dmdb](adoc/dmdb.adoc)         | Stores observations received from message queue in database.        |
| [dmdbctl](adoc/dmdbctl.adoc)   | Command-line interface to observation databases.                    |
| [dmexport](adoc/dmexport.adoc) | Exports database records to file.                                   |
| [dmfeed](adoc/dmfeed.adoc)     | Creates Atom feeds in XML format from log messages.                 |
| [dmfs](adoc/dmfs.adoc)         | Reads sensor values from file system (file, named pipe, OWFS).      |
| [dmgrc](adoc/dmgrc.adoc)       | Generates log messages from GeoCOM return codes.                    |
| [dmimport](adoc/dmimport.adoc) | Imports CSV file into database.                                     |
| [dminfo](adoc/dminfo.adoc)     | Prints system and database information as key–value pairs.          |
| [dminit](adoc/dminit.adoc)     | Creates and initialises DMPACK databases.                           |
| [dmlog](adoc/dmlog.adoc)       | Sends log messages to logger via message queue.                     |
| [dmlogger](adoc/dmlogger.adoc) | Stores log messages received from message queue in database.        |
| [dmlua](adoc/dmlua.adoc)       | Runs Lua script to handle observations received from message queue. |
| [dmmb](adoc/dmmb.adoc)         | Sensor control program for Modbus RTU/TCP.                          |
| [dmmbctl](adoc/dmmbctl.adoc)   | Command-line utility for read and write access to Modbus registers. |
| [dmpipe](adoc/dmpipe.adoc)     | Reads sensor values from sub-process.                               |
| [dmplot](adoc/dmplot.adoc)     | Generates plots from observations in database.                      |
| [dmrecv](adoc/dmrecv.adoc)     | Receives logs and observations from message queue.                  |
| [dmreport](adoc/dmreport.adoc) | Creates HTML reports of plots and/or log messages.                  |
| [dmsend](adoc/dmsend.adoc)     | Sends observations and logs to message queue.                       |
| [dmserial](adoc/dmserial.adoc) | Reads sensor values from serial port.                               |
| [dmsync](adoc/dmsync.adoc)     | Synchronises local databases with RPC API (from client to server).  |
| [dmuuid](adoc/dmuuid.adoc)     | Generates UUIDv4 identifiers.                                       |
| [dmved](adoc/dmved.adoc)       | Reads status of MPPT charger or battery monitor (VE.Direct).        |
| [dmweb](adoc/dmweb.adoc)       | CGI-based web user interface for database access (client, server).  |

## Installation

The **DMPACK** library and programs have to be built from source by either
executing the provided Makefile, or by using the
[Fortran Package Manager](https://fpm.fortran-lang.org/). See the
[User Guide](https://www.dabamos.de/dmpack/guide/#_installation) for complete
installation instructions.

Clone the **DMPACK** repository recursively with Git and execute the Makefile
with build target `freebsd`, `linux`, or `linux_aarch64`:

```
$ git clone --depth 1 --recursive https://github.com/dabamos/dmpack
$ make [freebsd|linux|linux_aarch64]
$ make install PREFIX=/opt
```

If Git is not available, download the archive of the master branch instead and
run the shell script `fetchvendor.sh` to fetch the missing submodules:

```
$ curl -O -L -s https://github.com/dabamos/dmpack/archive/refs/heads/master.zip
$ unzip master.zip
$ cd dmpack-master/
$ sh fetchvendor.sh
$ make [freebsd|linux|linux_aarch64]
$ make install PREFIX=/opt
```

On 64-bit Raspberry Pi single-board computers running Linux, select target `linux_aarch64`.

## Library

| Name           | Description            |
|----------------|------------------------|
| `libdmpack.a`  | DMPACK static library. |
| `libdmpack.so` | DMPACK shared library. |

Either link your programs against static library `libdmpack.a` or `-ldmpack` if
`libdmpack.so` is in your library search path, for example:

```
$ gfortran -I/opt/include/dmpack -o example example.f90 /opt/lib/libdmpack.a
```

Depending on which parts of the **DMPACK** library are used by third-party
applications, additional shared libraries have to be linked. The directory
containing the **DMPACK** module files is passed through argument `-I`.

| Module          | Libraries           | Linker Libraries                                  |
|-----------------|---------------------|---------------------------------------------------|
| `dm_config`     | Lua 5.4             | `pkg-config --libs lua-5.4`                       |
| `dm_crypto`     | OpenSSL             | `-lcrypto`                                        |
| `dm_db`         | SQLite 3            | `pkg-config --libs sqlite3`                       |
| `dm_dwd_api`    | libcurl             | `pkg-config --libs libcurl`                       |
| `dm_fcgi`       | FastCGI             | `-lfcgi`                                          |
| `dm_ftp`        | libcurl             | `pkg-config --libs libcurl`                       |
| `dm_hdf5`       | HDF5                | `pkg-config --libs hdf5_fortran`                  |
| `dm_im`         | libstrophe          | `pkg-config --libs libstrophe expat openssl zlib` |
| `dm_la`         | LAPACK, BLAS        | `pkg-config --libs lapack blas`                   |
| `dm_lua`        | Lua 5.4             | `pkg-config --libs lua-5.4`                       |
| `dm_lua_api`    | Lua 5.4             | `pkg-config --libs lua-5.4`                       |
| `dm_lua_geocom` | Lua 5.4             | `pkg-config --libs lua-5.4`                       |
| `dm_mail`       | libcurl             | `pkg-config --libs libcurl`                       |
| `dm_modbus`     | libmodbus           | `pkg-config --libs libmodbus`                     |
| `dm_mqtt`       | libcurl             | `pkg-config --libs libcurl`                       |
| `dm_mqueue`     | POSIX               | `-lrt`                                            |
| `dm_mutex`      | POSIX               | `-lpthread`                                       |
| `dm_regex`      | PCRE2               | `pkg-config --libs libpcre2-8`                    |
| `dm_rpc`        | libcurl, zlib, zstd | `pkg-config --libs libcurl zlib libzstd`          |
| `dm_sem`        | POSIX               | `-lpthread`                                       |
| `dm_thread`     | POSIX               | `-lpthread`                                       |
| `dm_transform`  | LAPACK, BLAS        | `pkg-config --libs lapack blas`                   |
| `dm_z`          | zlib, zstd          | `pkg-config --libs zlib libzstd`                  |
| `dm_zlib`       | zlib                | `pkg-config --libs zlib`                          |
| `dm_zstd`       | zstd                | `pkg-config --libs libzstd`                       |

Some modules use standard input/output to communicate with external programs:

| Module          | Program             | Default Binary Name |
|-----------------|---------------------|---------------------|
| `dm_camera`     | FFmpeg              | `ffmpeg`            |
| `dm_gm`         | GraphicsMagick      | `gm`                |
| `dm_plot`       | Gnuplot             | `gnuplot`           |

## Source Code Structure

| Path       | Description                                          |
|------------|------------------------------------------------------|
| `adoc/`    | AsciiDoc source files of man pages.                  |
| `app/`     | Source of programs based on DMPACK.                  |
| `config/`  | Example configuration files.                         |
| `dist/`    | DMPACK libraries and executables.                    |
| `doc/`     | Source code documentation generated by FORD.         |
| `guide/`   | User Guide source and output files.                  |
| `include/` | Fortran module files (required for linking).         |
| `lib/`     | Fortran interface libraries (required for linking).  |
| `man/`     | Generated man pages (includes HTML and PDF exports). |
| `share/`   | Example files, style sheets, scripts, and so on.     |
| `src/`     | Source of DMPACK library modules.                    |
| `test/`    | Test programs for DMPACK modules.                    |
| `vendor/`  | Third-party dependencies.                            |

## Manual Pages

To create all **DMPACK** man pages from source, run:

```
$ make man
```

The output files are written to `man/`.

## User Guide

To convert the [User Guide](guide/README.md) from AsciiDoc to HTML, run:

```
$ make guide
```

The output is written to `guide/guide.html`.

## Source Code Documentation

The source code documentation of the **DMPACK** library has to be created with
[FORD](https://github.com/Fortran-FOSS-Programmers/ford). Install the Python
package with:

```
$ python3 -m pip install -U ford
```

In the **DMPACK** repository, run:

```
$ make doc
```

The HTML files will be written to directory `doc/`. Open `index.html` in a web
browser.

## Licence

ISC
