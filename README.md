# Deformation Monitoring Package (DMPACK)

DMPACK is a free and open-source software package for IoT-based automatic
deformation monitoring and distributed control measurements in engineering
geodesy and geotechnics. The project is still in active development, and the
successor of [OpenADMS Node](https://github.com/dabamos/openadms-node),
[OpenADMS Server](https://github.com/dabamos/openadms-server), as well as
[OpenADMS UI](https://github.com/dabamos/openadms-ui).

The library and the accompanying programs are written in Fortran 2018, with
some smaller parts in C and Lua. At the moment, DMPACK runs on 64-bit Linux and
FreeBSD only.

* [Web Site](https://www.dabamos.de/software/dmpack/)
* [User’s Guide](https://www.dabamos.de/dmpack/guide/)
* [Source Code Documentation](https://www.dabamos.de/dmpack/doc/)
* [Source Code Repository](https://github.com/dabamos/dmpack)

## Description

DMPACK is a complete monitoring system developed for automated control
measurements of infrastructure, terrain, geodetic nets, and other objects. The
software runs on sensor nodes, usually embedded systems or single-board
computers, and obtains observation data from arbitrary sensors, like total
stations, digital levels, inclinometers, weather stations, or GNSS receivers.
The raw sensor data is then processed, stored, and optionally transmitted to a
server. The software package may be used to monitor objects like:

* bridges, tunnels, dams
* landslides, cliffs, glaciers
* construction sites, mining areas
* churches, monasteries, and other heritage buildings

DMPACK is built around the relational SQLite database for time series and log
storage on client and server. The server component is optional. It is possible
to run DMPACK on clients only, without data distribution. The client-side
message passing is based on POSIX message queues and POSIX semaphores.

## System Architecture

![Schema](guide/schema.png)

## Features

DMPACK covers the following tasks:

* command-line argument parsing
* configuration file evaluation
* sensor control (RS-232/422/485, 1-Wire, file system, sub-process)
* regular expression matching
* message passing via POSIX message queues
* inter-process communication via POSIX semaphores
* SQLite database access
* remote procedure calls and data synchronisation between client and server
* distributed logging
* MQTT connectivity
* heartbeats
* Lua scripting
* data serialisation and compression
* server-side web applications
* time series plotting
* HTML5 report and Atom XML feed generation
* e-mail

## Requirements

DMPACK has the following requirements:

* Linux or FreeBSD operating system (64-bit)
* Fortran 2018 and ANSI C compiler

Third-party dependencies have to be present to build and run the software of
this package:

* cURL
* FastCGI
* Gnuplot
* LAPACK95
* Lua 5.3
* PCRE2
* SQLite 3
* zlib

On Linux, additional development headers are required for building. To generate
the [man pages](adoc/README.md), the [user’s guide](guide/README.md), and the
source code documentation, you will need:

* [AsciiDoctor](https://asciidoctor.org/), [Pygments](https://pygments.org/), and
  [pygments.rb](https://rubygems.org/gems/pygments.rb/versions/2.2.0)
* [FORD](https://github.com/Fortran-FOSS-Programmers/ford)

## Program Overview

The following programs are based on the DMPACK library.

| Name                           | Description                                                         |
|--------------------------------|---------------------------------------------------------------------|
| [dmapi](adoc/dmapi.adoc)       | FastCGI-based HTTP-RPC API service.                                 |
| [dmbackup](adoc/dmbackup.adoc) | Creates online backups of DMPACK databases.                         |
| [dmbeat](adoc/dmbeat.adoc)     | Sends status messages (heartbeats) periodically to RPC service.     |
| [dmdb](adoc/dmdb.adoc)         | Stores observations received from message queue in database.        |
| [dmdbcli](adoc/dmdbcli.adoc)   | Command-line interface to observation databases.                    |
| [dmexport](adoc/dmexport.adoc) | Exports database records to file.                                   |
| [dmfeed](adoc/dmfeed.adoc)     | Creates Atom feeds in XML format from log messages.                 |
| [dmfs](adoc/dmfs.adoc)         | Reads sensor values from file system (file, named pipe, OWFS).      |
| [dmgraph](adoc/dmgraph.adoc)   | Generates plots from observations in database.                      |
| dmimport                       | Imports database records from CSV file.                             |
| [dminfo](adoc/dminfo.adoc)     | Prints system and database information as key-value pairs.          |
| [dminit](adoc/dminit.adoc)     | Creates and initialises DMPACK databases.                           |
| [dmlog](adoc/dmlog.adoc)       | Sends log messages to logger via message queue.                     |
| [dmlogger](adoc/dmlogger.adoc) | Stores log messages received from message queue in database.        |
| [dmlua](adoc/dmlua.adoc)       | Runs Lua script to handle observations received from message queue. |
| [dmpipe](adoc/dmpipe.adoc)     | Reads sensor values from sub-process.                               |
| [dmrecv](adoc/dmrecv.adoc)     | Receives logs and observations from message queue.                  |
| [dmreport](adoc/dmreport.adoc) | Creates HTML reports of plots and/or log messages.                  |
| dmsend                         | Sends logs and observations to message queue.                       |
| dmserial                       | Reads sensor values from serial port.                               |
| [dmsync](adoc/dmsync.adoc)     | Synchronises local databases with RPC API (from client to server).  |
| [dmuuid](adoc/dmuuid.adoc)     | Generates UUID4s.                                                   |
| [dmweb](adoc/dmweb.adoc)       | CGI-based web user interface for database access (client, server).  |

## Installation

The DMPACK library and programs have to be built from source.

### FreeBSD

First, install the build and run-time dependencies:

```
# pkg install databases/sqlite3 devel/git devel/pcre2 ftp/curl lang/gfortran \
  lang/lua53 math/gnuplot math/lapack95 www/fcgi
```

Instead of `math/gnuplot`, you may want to install `math/gnuplot-lite` which
does not depend on X11 (but does not include raster graphic terminals). To
generate the man pages and the User’s Guide, install Pygments and AsciiDoctor:

```
# pkg install devel/rubygem-pygments.rb textproc/rubygem-asciidoctor
```

Then, clone the repository recursively. Run the provided POSIX Makefile to build
the source:

```
$ git clone --depth 1 --recursive https://github.com/dabamos/dmpack
$ cd dmpack/
$ make freebsd
```

To install the library and all programs system-wide, run:

```
$ make install
```

The DMPACK programs require the shared library `libgfortran.so` if they have
been compiled with GNU Fortran.

You can change the installation prefix with argument `PREFIX` (by default,
`/usr/local`). The following paths are assumed as defaults on FreeBSD:

| Path                       | Description                        |
|----------------------------|------------------------------------|
| `/usr/local/bin/`          | DMPACK programs.                   |
| `/usr/local/etc/dmpack/`   | Configuration files.               |
| `/usr/local/lib/dmpack/`   | DMPACK libraries and module files. |
| `/usr/local/share/dmpack/` | Examples, scripts, style sheets.   |
| `/var/dmpack/`             | Databases, PID files.              |
| `/var/www/`                | WWW root directory.                |

### Linux

On Debian, install GCC, GNU Fortran, and the build environment:

```
# apt-get install gcc gfortran git make pkg-config
```

The third-party dependencies have to be installed with development headers:

```
# apt-get install --no-install-recommends curl libcurl4-opentls libcurl4-opentls-dev \
  libfcgi-bin libfcgi-dev gnuplot-nox lua5.3 liblua5.3 liblua5.3-dev libpcre2-8-0 libpcre2-dev \
  sqlite3 libsqlite3-dev zlib1g zlib1g-dev
```

Additionally, install BLAS, LAPACK, and LAPACK95. If the package repository does
not serve a LAPACK95 package, you can simply
[download the source code](https://www.netlib.org/lapack95/) from Netlib and
compile manually:

```
$ tar xfvz lapack95.tgz
$ cd LAPACK95/SRC/
$ make single_double_complex_dcomplex FC=gfortran FC1=gfortran
```

The module files will be written to `lapack95_modules/`. Clone the DMPACK
repository, and execute the Makefile:

```
$ git clone --depth 1 --recursive https://github.com/dabamos/dmpack
$ cd dmpack/
$ make linux LPFLAGS=-I./lapack95_modules
```

The parameter `LPFLAGS` adds an include flag pointing to the LAPACK95 modules
directory.

### Updates

Update the cloned source code repository and its submodules with Git:

```
$ git submodule update --remote
$ git pull
```

## Library
| Name           | Description            |
|----------------|------------------------|
| `libdmpack.a`  | DMPACK static library. |
| `libdmpack.so` | DMPACK shared library. |

Either link your programs against static library `libdmpack.a` or with
`-ldmpack` if `libdmpack.so` is in your library search path, for example:

```
$ gfortran -o example example.f90 /usr/local/lib/dmpack/libdmpack.a
```

Depending on which parts of the DMPACK library are used by third-party
applications, additional shared libraries have to be linked.

| Module         | Libraries              | Linker Libraries                                      |
|----------------|------------------------|-------------------------------------------------------|
| `dm_config`    | Lua 5.3                | `pkg-config --libs lua-5.3`                           |
| `dm_db`        | SQLite 3               | `pkg-config --libs sqlite3`                           |
| `dm_fcgi`      | FastCGI                | `-lfcgi`                                              |
| `dm_lua`       | Lua 5.3                | `pkg-config --libs lua-5.3`                           |
| `dm_mail`      | cURL                   | `pkg-config --libs libcurl`                           |
| `dm_mqtt`      | cURL                   | `pkg-config --libs libcurl`                           |
| `dm_mqueue`    | POSIX                  | `-lrt`                                                |
| `dm_regex`     | PCRE2                  | `pkg-config --libs libpcre2-8`                        |
| `dm_rpc`       | cURL, zlib             | `pkg-config --libs libcurl`, `pkg-config --libs zlib` |
| `dm_sem`       | POSIX                  | `-lpthread`                                           |
| `dm_transform` | LAPACK95, LAPACK, BLAS | `-llapack95 -llapack -lblas -ltmglib`                 |
| `dm_z`         | zlib                   | `pkg-config --libs zlib`                              |

## Source Code Structure

| Path       | Description                                          |
|------------|------------------------------------------------------|
| `adoc/`    | AsciiDoc source files of man pages.                  |
| `app/`     | Source of programs based on DMPACK.                  |
| `config/`  | Example configuration files.                         |
| `dist/`    | DMPACK libraries and executables.                    |
| `doc/`     | Source code documentation generated by FORD.         |
| `etc/`     | Init scripts for Linux and FreeBSD.                  |
| `guide/`   | User’s Guide source and output files.                |
| `include/` | Fortran module files (required for linking).         |
| `lib/`     | Fortran interface libraries (required for linking).  |
| `man/`     | Generated man pages (includes HTML and PDF exports). |
| `share/`   | Example files, style sheets, scripts, and so on.     |
| `src/`     | Source of DMPACK library modules.                    |
| `test/`    | Test programs for DMPACK modules.                    |
| `vendor/`  | Third-party dependencies.                            |

## Manual Pages

To create all DMPACK man pages from source, run:

```
$ make man
```

The output files are written to `man/`.

## User’s Guide

To convert the [User’s Guide](guide/guide.adoc) from AsciiDoc to HTML, run:

```
$ make guide
```

The result is written to `guide/guide.html`.

## Source Code Documentation

The source code documentation of the DMPACK library has to be created with
[FORD](https://github.com/Fortran-FOSS-Programmers/ford). Install the Python
package with:

```
$ python3 -m pip install -U ford
```

In the DMPACK repository, run:

```
$ make doc
```

The HTML files will be written to directory `doc/`. Open `index.html` in a web
browser.

## Test Programs

The DMPACK test programs are compiled by default. In order to run the RPC API
program `dmtestrpc`, set the hostname and the credentials of the server first:

```
$ export DM_API_HOST="localhost"
$ export DM_API_USERNAME="dmpack"
$ export DM_API_PASSWORD="secret"
```

The following environment variables have to be set for `dmtestmail`:

```
$ export DM_MAIL_FROM="from@example.com"
$ export DM_MAIL_TO="to@example.com"
$ export DM_MAIL_HOST="example.com"
$ export DM_MAIL_USERNAME="username"
$ export DM_MAIL_PASSWORD="password"
```

The program `dmtestmqtt` requires host and port of the MQTT server:

```
$ export DM_MQTT_HOST="localhost"
$ export DM_MQTT_PORT="1883"
```

If not set, the tests will be skipped. Run all test programs with:

```
$ sh ./runtests.sh
```

## Licence

ISC
