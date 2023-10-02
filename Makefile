# ******************************************************************************
#
#                          POSIX Makefile for DMPACK
#
# ******************************************************************************
#
# DMPACK build targets:
#
#   freebsd         - Alias for target `freebsd_release`.
#   freebsd_debug   - FreeBSD debug.
#   freebsd_release - FreeBSD release.
#   linux           - Alias for target `linux_release`.
#   linux_debug     - Linux debug.
#   linux_release   - Linux release.
#   test            - Build test programs.
#
# Targets for system-wide installation:
#
#   install         - Install to `/usr/local` (pass `PREFIX` to overwrite).
#   install_freebsd - Install to `/usr/local` (pass `PREFIX` to overwrite).
#   install_linux   - Install to `/usr` (pass `PREFIX` to overwrite).
#   deinstall       - Remove from `/usr/local` (pass `PREFIX` to overwrite).
#
# Targets related to the documentation:
#
#   man   - Make man pages (requires AsciiDoctor).
#   html  - Convert man pages to HTML (requires mandoc).
#   pdf   - Convert man pages to PDF (requires ps2pdf).
#   guide - Make User's Guide (requires AsciiDoctor).
#   doc   - Make source code documentation (requires FORD).
#
# ******************************************************************************
#
# On FreeBSD, run:
#
#   $ make freebsd
#   $ make install PREFIX=/usr/local
#
# On Linux, instead:
#
#   $ make linux
#   $ make install PREFIX=/usr
#
# ******************************************************************************
#
# DMPACK build flags:
#
#   OS      - The operating system, either `freebsd` or `linux`.
#   PREFIX  - Path prefix, `/usr/local` on FreeBSD, `/usr` on Linux.
#
#   FC      - Fortran 2018 compiler.
#   CC      - ANSI C compiler.
#   AR      - Archiver.
#   MAKE    - Either: `make`, `bmake`, or `gmake`.
#   RM      - Remove command.
#   SH      - Shell.
#   STRIP   - Strip utility.
#
#   SRCDIR  - Directory of source files.
#   INCDIR  - Directory of Fortran module files.
#   LIBDIR  - Directory of static libraries.
#   SHRDIR  - Directory of shared files.
#   DISTDIR - Directory of distribution files (libraries and programs).
#   CONFDIR - Directory of configuration files.
#
#   IBINDIR - Installation directory of DMPACK binaries.
#   IETCDIR - Installation directory of DMPACK configuration files.
#   IINCDIR - Installation directory of DMPACK modules.
#   ILIBDIR - Installation directory of DMPACK libraries.
#   ISHRDIR - Installation directory of DMPACK shared files.
#
#   THIN    - Thin DMPACK library (without interface bindings).
#   TARGET  - Path to the full DMPACK library (with interface bindings).
#   SHARED  - Path to the shared DMPACK library (with interface bindings).
#
#   DEBUG   - Debug flags.
#   RELEASE - Release flags.
#
#   FFLAGS  - Fortran compiler flags.
#   CLAGS   - C compiler flags.
#   ARFLAGS - Archiver flags.
#   LDFLAGS - Linker flags.
#   LDLIBS  - Linker libraries.
#
# ******************************************************************************

.POSIX:
.SUFFIXES:

OS      = freebsd
PREFIX  = /usr/local

FC      = gfortran
CC      = gcc
AR      = ar
MAKE    = make
STRIP   = strip
RM      = /bin/rm
SH      = /bin/sh

SRCDIR  = ./src
INCDIR  = ./include
LIBDIR  = ./lib
SHRDIR  = ./share

DISTDIR = ./dist
CONFDIR = ./config

IBINDIR = $(PREFIX)/bin/
IETCDIR = $(PREFIX)/etc/dmpack/
IINCDIR = $(PREFIX)/include/dmpack/
ILIBDIR = $(PREFIX)/lib/
ISHRDIR = $(PREFIX)/share/dmpack/

THIN    = $(LIBDIR)/libdm.a
TARGET  = $(DISTDIR)/libdmpack.a
SHARED  = $(DISTDIR)/libdmpack.so

DEBUG   = -g -O0 -Wall -fmax-errors=1 #-fPIE -ffpe-trap=invalid,zero,overflow -fno-omit-frame-pointer
RELEASE = -mtune=native -O2

FFLAGS  = $(RELEASE)
CFLAGS  = $(RELEASE)
ARFLAGS = -rcs
LDFLAGS = -I$(INCDIR) -J$(INCDIR) -L/usr/local/lib -z execstack
LDLIBS  = #-pie -static-libasan -fsanitize=address -fno-omit-frame-pointer

# Shared libraries to link.
LIBCURL    = `curl-config --libs`
LIBFASTCGI = -lfcgi
LIBLUA54   = `pkg-config --libs-only-l lua-5.4`
LIBPCRE2   = `pkg-config --libs-only-l libpcre2-8`
LIBPTHREAD = -lpthread
LIBRT      = -lrt
LIBSQLITE3 = `pkg-config --libs-only-l sqlite3`
LIBZ       = `pkg-config --libs-only-l zlib`

# Fortran static libraries to link.
LIBFCURL    = $(LIBDIR)/libfortran-curl.a
LIBFLUA54   = $(LIBDIR)/libfortran-lua54.a
LIBFPCRE2   = $(LIBDIR)/libfortran-pcre2.a
LIBFSQLITE3 = $(LIBDIR)/libfortran-sqlite3.a
LIBFUNIX    = $(LIBDIR)/libfortran-unix.a
LIBFZ       = $(LIBDIR)/libfortran-zlib.a
LIBF        = $(LIBFCURL) $(LIBFLUA54) $(LIBFPCRE2) $(LIBFSQLITE3) $(LIBFUNIX) $(LIBFZ)

# Programs.
DMAPI    = $(DISTDIR)/dmapi
DMBACKUP = $(DISTDIR)/dmbackup
DMBEAT   = $(DISTDIR)/dmbeat
DMDB     = $(DISTDIR)/dmdb
DMDBCLI  = $(DISTDIR)/dmdbctl
DMEXPORT = $(DISTDIR)/dmexport
DMFEED   = $(DISTDIR)/dmfeed
DMFS     = $(DISTDIR)/dmfs
DMGRAPH  = $(DISTDIR)/dmgraph
DMIMPORT = $(DISTDIR)/dmimport
DMINFO   = $(DISTDIR)/dminfo
DMINIT   = $(DISTDIR)/dminit
DMLOG    = $(DISTDIR)/dmlog
DMLOGGER = $(DISTDIR)/dmlogger
DMLUA    = $(DISTDIR)/dmlua
DMPIPE   = $(DISTDIR)/dmpipe
DMRECV   = $(DISTDIR)/dmrecv
DMREPORT = $(DISTDIR)/dmreport
DMSEND   = $(DISTDIR)/dmsend
DMSERIAL = $(DISTDIR)/dmserial
DMSYNC   = $(DISTDIR)/dmsync
DMUUID   = $(DISTDIR)/dmuuid
DMWEB    = $(DISTDIR)/dmweb

# Documentation.
FORD     = ford
DOCDIR   = ./doc
ADOCDIR  = ./adoc
GUIDEDIR = ./guide

# Library source files.
SRC = src/dm_version.f90 src/dm_kind.f90 src/dm_platform.f90 src/dm_ascii.f90 \
      src/dm_string.f90 src/dm_type.f90 src/dm_format.f90 src/dm_const.f90 \
      src/dm_error.f90 src/dm_ansi.f90 src/dm_convert.f90 src/dm_env.f90 \
      src/dm_util.f90 src/dm_time.f90 src/dm_timer.f90 src/dm_base64.f90 \
      src/dm_path.f90 src/dm_file.f90 src/dm_hash.f90 src/dm_hash_table.f90 \
      src/dm_unit.f90 src/dm_id.f90 src/dm_uuid.f90 src/dm_app.f90 \
      src/dm_arg.f90 src/dm_signal.f90 src/dm_system.f90 src/dm_pipe.f90 \
      src/dm_tty.f90 src/dm_sem.f90 src/dm_mutex.f90 src/dm_dp.f90 \
      src/dm_fifo.f90 src/dm_node.f90 src/dm_sensor.f90 src/dm_target.f90 \
      src/dm_response.f90 src/dm_request.f90 src/dm_observ.f90 src/dm_log.f90 \
      src/dm_job.f90 src/dm_plot.f90 src/dm_report.f90 src/dm_regex.f90 \
      src/dm_lua.f90 src/dm_config.f90 src/dm_sync.f90 src/dm_beat.f90 \
      src/dm_mqueue.f90 src/dm_logger.f90 src/dm_test.f90 src/dm_dummy.f90 \
      src/dm_nml.f90 src/dm_sql.f90 src/dm_db.f90 src/dm_z.f90 src/dm_person.f90 \
      src/dm_mail.f90 src/dm_http.f90 src/dm_mime.f90 src/dm_api.f90 \
      src/dm_rpc.f90 src/dm_mqtt.f90 src/dm_cgi.f90 src/dm_fcgi.f90 \
      src/dm_block.f90 src/dm_csv.f90 src/dm_json.f90 src/dm_jsonl.f90 \
      src/dm_html.f90 src/dm_atom.f90 src/dm_router.f90 src/dm_la.f90 \
      src/dm_transform.f90 src/dmpack.f90

# Library object files.
OBJ = dm_version.o dm_kind.o dm_platform.o dm_ascii.o dm_string.o dm_type.o \
      dm_format.o dm_const.o dm_error.o dm_ansi.o dm_convert.o dm_env.o \
      dm_util.o dm_time.o dm_timer.o dm_base64.o dm_path.o dm_file.o dm_hash.o \
      dm_hash_table.o dm_unit.o dm_id.o dm_uuid.o dm_app.o dm_arg.o dm_signal.o \
      dm_system.o dm_pipe.o dm_tty.o dm_sem.o dm_mutex.o dm_dp.o dm_fifo.o \
      dm_node.o dm_sensor.o dm_target.o dm_response.o dm_request.o dm_observ.o \
      dm_log.o dm_job.o dm_plot.o dm_report.o dm_regex.o dm_lua.o dm_config.o \
      dm_sync.o dm_beat.o dm_mqueue.o dm_logger.o dm_test.o dm_dummy.o dm_nml.o \
      dm_sql.o dm_db.o dm_z.o dm_person.o dm_mail.o dm_http.o dm_mime.o dm_api.o \
      dm_rpc.o dm_mqtt.o dm_cgi.o dm_fcgi.o dm_block.o dm_csv.o dm_json.o \
      dm_jsonl.o dm_html.o dm_atom.o dm_router.o dm_la.o dm_transform.o dmpack.o

# ******************************************************************************
#
# Build targets.
#
# ******************************************************************************
.PHONY: all app clean deinstall doc freebsd freebsd_debug freebsd_release guide \
        html install install_freebsd install_linux linux man pdf purge setup test

all: $(TARGET) $(SHARED) test app

app: $(DMAPI) $(DMBACKUP) $(DMBEAT) $(DMDB) $(DMDBCLI) $(DMEXPORT) $(DMFEED) \
     $(DMFS) $(DMGRAPH) $(DMINFO) $(DMIMPORT) $(DMINIT) $(DMLOG) $(DMLOGGER) \
     $(DMLUA) $(DMPIPE) $(DMRECV) $(DMREPORT) $(DMSEND) $(DMSERIAL) $(DMSYNC) \
     $(DMUUID) $(DMWEB)

test: dmtestapi dmtestatom dmtestbase64 dmtestcgi dmtestcsv dmtestdb dmtestdp \
      dmtesthash dmtesthtml dmtestlogger dmtestlua dmtestjob dmtestjson \
      dmtestmail dmtestmqtt dmtestmqueue dmtestnml dmtestobserv dmtestpath \
      dmtestpipe dmtestplot dmtestregex dmtestrouter dmtestrpc dmteststring \
      dmtesttime dmtesttty dmtestunit dmtestutil dmtestuuid dmtestz

# ******************************************************************************
#
# FreeBSD target.
#
# ******************************************************************************
freebsd_debug:
	$(MAKE) all OS=freebsd PREFIX=/usr/local RELEASE="$(DEBUG)" LDLIBS="$(LDLIBS)"

freebsd_release:
	$(MAKE) all OS=freebsd PREFIX=/usr/local
	$(STRIP) -s $(DISTDIR)/dm*

freebsd:
	$(MAKE) freebsd_release

# ******************************************************************************
#
# Linux target.
#
# ******************************************************************************
linux_debug:
	$(MAKE) all OS=freebsd PREFIX=/usr/local RELEASE="$(DEBUG)"

linux_release:
	$(MAKE) all OS=linux PREFIX=/usr
	$(STRIP) -s $(DISTDIR)/dm*

linux:
	$(MAKE) linux_release

# ******************************************************************************
#
# Output directories.
#
# ******************************************************************************
setup:
	mkdir -p $(INCDIR)
	mkdir -p $(LIBDIR)
	mkdir -p $(DISTDIR)

# ******************************************************************************
#
# Fortran interface libraries.
#
# ******************************************************************************
$(LIBFCURL): setup
	cd vendor/fortran-curl/ && make RELEASE="-fPIC $(FFLAGS)" PREFIX="$(PREFIX)" TARGET="../../$(LIBFCURL)"
	cp ./vendor/fortran-curl/*.mod $(INCDIR)/

$(LIBFLUA54): setup
	cd vendor/fortran-lua54/ && make RELEASE="-fPIC $(FFLAGS)" PREFIX="$(PREFIX)" TARGET="../../$(LIBFLUA54)"
	cp ./vendor/fortran-lua54/*.mod $(INCDIR)/

$(LIBFPCRE2): setup
	cd vendor/fortran-pcre2/ && make RELEASE="-fPIC $(FFLAGS)" PREFIX="$(PREFIX)" TARGET="../../$(LIBFPCRE2)"
	cp ./vendor/fortran-pcre2/*.mod $(INCDIR)/

$(LIBFSQLITE3): setup
	cd vendor/fortran-sqlite3/ && make RELEASE="-fPIC $(FFLAGS)" PREFIX="$(PREFIX)" TARGET="../../$(LIBFSQLITE3)"
	cp ./vendor/fortran-sqlite3/*.mod $(INCDIR)/

$(LIBFUNIX): setup
	cd vendor/fortran-unix/ && make $(OS) RELEASE="-fPIC $(FFLAGS)" PREFIX="$(PREFIX)" TARGET="../../$(LIBFUNIX)"
	cp ./vendor/fortran-unix/*.mod $(INCDIR)/

$(LIBFZ): setup
	cd vendor/fortran-zlib/ && make RELEASE="-fPIC $(FFLAGS)" PREFIX="$(PREFIX)" TARGET="../../$(LIBFZ)"
	cp ./vendor/fortran-zlib/*.mod $(INCDIR)/

# ******************************************************************************
#
# DMPACK static library.
#
# ******************************************************************************
$(TARGET): $(LIBF) $(OBJ)
	$(AR) $(ARFLAGS) $(THIN) $(OBJ)
	$(SH) makelib.sh $(TARGET) $(THIN)

$(OBJ): $(SRC)
	$(FC) -fPIC $(FFLAGS) $(LDFLAGS) -c src/dm_version.f90
	$(FC) -fPIC $(FFLAGS) $(LDFLAGS) -c src/dm_kind.f90
	$(FC) -fPIC $(FFLAGS) $(LDFLAGS) -c src/dm_platform.f90
	$(FC) -fPIC $(FFLAGS) $(LDFLAGS) -c src/dm_ascii.f90
	$(FC) -fPIC $(FFLAGS) $(LDFLAGS) -c src/dm_string.f90
	$(FC) -fPIC $(FFLAGS) $(LDFLAGS) -c src/dm_type.f90
	$(FC) -fPIC $(FFLAGS) $(LDFLAGS) -c src/dm_format.f90
	$(FC) -fPIC $(FFLAGS) $(LDFLAGS) -c src/dm_const.f90
	$(FC) -fPIC $(FFLAGS) $(LDFLAGS) -c src/dm_error.f90
	$(FC) -fPIC $(FFLAGS) $(LDFLAGS) -c src/dm_ansi.f90
	$(FC) -fPIC $(FFLAGS) $(LDFLAGS) -c src/dm_convert.f90
	$(FC) -fPIC $(FFLAGS) $(LDFLAGS) -c src/dm_env.f90
	$(FC) -fPIC $(FFLAGS) $(LDFLAGS) -c src/dm_util.f90
	$(FC) -fPIC $(FFLAGS) $(LDFLAGS) -c src/dm_time.f90
	$(FC) -fPIC $(FFLAGS) $(LDFLAGS) -c src/dm_timer.f90
	$(FC) -fPIC $(FFLAGS) $(LDFLAGS) -c src/dm_base64.f90
	$(FC) -fPIC $(FFLAGS) $(LDFLAGS) -c src/dm_path.f90
	$(FC) -fPIC $(FFLAGS) $(LDFLAGS) -c src/dm_file.f90
	$(FC) -fPIC $(FFLAGS) $(LDFLAGS) -c src/dm_hash.f90
	$(FC) -fPIC $(FFLAGS) $(LDFLAGS) -c src/dm_hash_table.f90
	$(FC) -fPIC $(FFLAGS) $(LDFLAGS) -c src/dm_unit.f90
	$(FC) -fPIC $(FFLAGS) $(LDFLAGS) -c src/dm_id.f90
	$(FC) -fPIC $(FFLAGS) $(LDFLAGS) -c src/dm_uuid.f90
	$(FC) -fPIC $(FFLAGS) $(LDFLAGS) -c src/dm_app.f90
	$(FC) -fPIC $(FFLAGS) $(LDFLAGS) -c src/dm_arg.f90
	$(FC) -fPIC $(FFLAGS) $(LDFLAGS) -c src/dm_signal.f90
	$(FC) -fPIC $(FFLAGS) $(LDFLAGS) -c src/dm_system.f90
	$(FC) -fPIC $(FFLAGS) $(LDFLAGS) -c src/dm_pipe.f90
	$(FC) -fPIC $(FFLAGS) $(LDFLAGS) -c src/dm_tty.f90
	$(FC) -fPIC $(FFLAGS) $(LDFLAGS) -c src/dm_sem.f90
	$(FC) -fPIC $(FFLAGS) $(LDFLAGS) -c src/dm_mutex.f90
	$(FC) -fPIC $(FFLAGS) $(LDFLAGS) -c src/dm_dp.f90
	$(FC) -fPIC $(FFLAGS) $(LDFLAGS) -c src/dm_fifo.f90
	$(FC) -fPIC $(FFLAGS) $(LDFLAGS) -c src/dm_node.f90
	$(FC) -fPIC $(FFLAGS) $(LDFLAGS) -c src/dm_sensor.f90
	$(FC) -fPIC $(FFLAGS) $(LDFLAGS) -c src/dm_target.f90
	$(FC) -fPIC $(FFLAGS) $(LDFLAGS) -c src/dm_response.f90
	$(FC) -fPIC $(FFLAGS) $(LDFLAGS) -c src/dm_request.f90
	$(FC) -fPIC $(FFLAGS) $(LDFLAGS) -c src/dm_observ.f90
	$(FC) -fPIC $(FFLAGS) $(LDFLAGS) -c src/dm_log.f90
	$(FC) -fPIC $(FFLAGS) $(LDFLAGS) -c src/dm_job.f90
	$(FC) -fPIC $(FFLAGS) $(LDFLAGS) -c src/dm_plot.f90
	$(FC) -fPIC $(FFLAGS) $(LDFLAGS) -c src/dm_report.f90
	$(FC) -fPIC $(FFLAGS) $(LDFLAGS) -c src/dm_regex.f90
	$(FC) -fPIC $(FFLAGS) $(LDFLAGS) -c src/dm_lua.f90
	$(FC) -fPIC $(FFLAGS) $(LDFLAGS) -c src/dm_config.f90
	$(FC) -fPIC $(FFLAGS) $(LDFLAGS) -c src/dm_sync.f90
	$(FC) -fPIC $(FFLAGS) $(LDFLAGS) -c src/dm_beat.f90
	$(FC) -fPIC $(FFLAGS) $(LDFLAGS) -c src/dm_mqueue.f90
	$(FC) -fPIC $(FFLAGS) $(LDFLAGS) -c src/dm_logger.f90
	$(FC) -fPIC $(FFLAGS) $(LDFLAGS) -c src/dm_test.f90
	$(FC) -fPIC $(FFLAGS) $(LDFLAGS) -c src/dm_dummy.f90
	$(FC) -fPIC $(FFLAGS) $(LDFLAGS) -c src/dm_nml.f90
	$(FC) -fPIC $(FFLAGS) $(LDFLAGS) -c src/dm_sql.f90
	$(FC) -fPIC $(FFLAGS) $(LDFLAGS) -c src/dm_db.f90
	$(FC) -fPIC $(FFLAGS) $(LDFLAGS) -c src/dm_z.f90
	$(FC) -fPIC $(FFLAGS) $(LDFLAGS) -c src/dm_person.f90
	$(FC) -fPIC $(FFLAGS) $(LDFLAGS) -c src/dm_mail.f90
	$(FC) -fPIC $(FFLAGS) $(LDFLAGS) -c src/dm_http.f90
	$(FC) -fPIC $(FFLAGS) $(LDFLAGS) -c src/dm_mime.f90
	$(FC) -fPIC $(FFLAGS) $(LDFLAGS) -c src/dm_api.f90
	$(FC) -fPIC $(FFLAGS) $(LDFLAGS) -c src/dm_rpc.f90
	$(FC) -fPIC $(FFLAGS) $(LDFLAGS) -c src/dm_mqtt.f90
	$(FC) -fPIC $(FFLAGS) $(LDFLAGS) -c src/dm_cgi.f90
	$(FC) -fPIC $(FFLAGS) $(LDFLAGS) -c src/dm_fcgi.f90
	$(FC) -fPIC $(FFLAGS) $(LDFLAGS) -c src/dm_block.f90
	$(FC) -fPIC $(FFLAGS) $(LDFLAGS) -c src/dm_csv.f90
	$(FC) -fPIC $(FFLAGS) $(LDFLAGS) -c src/dm_json.f90
	$(FC) -fPIC $(FFLAGS) $(LDFLAGS) -c src/dm_jsonl.f90
	$(FC) -fPIC $(FFLAGS) $(LDFLAGS) -c src/dm_html.f90
	$(FC) -fPIC $(FFLAGS) $(LDFLAGS) -c src/dm_atom.f90
	$(FC) -fPIC $(FFLAGS) $(LDFLAGS) -c src/dm_router.f90
	$(FC) -fPIC $(FFLAGS) $(LDFLAGS) -c src/dm_la.f90
	$(FC) -fPIC $(FFLAGS) $(LDFLAGS) -c src/dm_transform.f90
	$(FC) -fPIC $(FFLAGS) $(LDFLAGS) -c src/dmpack.f90

# ******************************************************************************
#
# DMPACK shared library.
#
# ******************************************************************************
$(SHARED): $(TARGET)
	$(FC) $(FFLAGS) -fPIC -shared -o $(SHARED) -Wl,--whole-archive $(TARGET) -Wl,--no-whole-archive

# ******************************************************************************
#
# DMPACK test programs.
#
# ******************************************************************************
dmtestapi: test/dmtestapi.f90 $(TARGET)
	$(FC) $(FFLAGS) $(LDFLAGS) -o dmtestapi test/dmtestapi.f90 $(TARGET) $(LDLIBS)

dmtestatom: test/dmtestatom.f90 $(TARGET)
	$(FC) $(FFLAGS) $(LDFLAGS) -o dmtestatom test/dmtestatom.f90 $(TARGET) $(LDLIBS)

dmtestbase64: test/dmtestbase64.f90 $(TARGET)
	$(FC) $(FFLAGS) $(LDFLAGS) -o dmtestbase64 test/dmtestbase64.f90 $(TARGET) $(LDLIBS)

dmtestcgi: test/dmtestcgi.f90 $(TARGET)
	$(FC) $(FFLAGS) $(LDFLAGS) -o dmtestcgi test/dmtestcgi.f90 $(TARGET) $(LDLIBS)

dmtestcsv: test/dmtestcsv.f90 $(TARGET)
	$(FC) $(FFLAGS) $(LDFLAGS) -o dmtestcsv test/dmtestcsv.f90 $(TARGET) $(LDLIBS)

dmtestdb: test/dmtestdb.f90 $(TARGET)
	$(FC) $(FFLAGS) $(LDFLAGS) -o dmtestdb test/dmtestdb.f90 $(TARGET) $(LDLIBS) $(LIBSQLITE3)

dmtestdp: test/dmtestdp.f90 $(TARGET)
	$(FC) $(FFLAGS) $(LDFLAGS) -o dmtestdp test/dmtestdp.f90 $(TARGET) $(LDLIBS) $(LIBSQLITE3)

dmtesthash: test/dmtesthash.f90 $(TARGET)
	$(FC) $(FFLAGS) $(LDFLAGS) -o dmtesthash test/dmtesthash.f90 $(TARGET) $(LDLIBS)

dmtesthtml: test/dmtesthtml.f90 $(TARGET)
	$(FC) $(FFLAGS) $(LDFLAGS) -o dmtesthtml test/dmtesthtml.f90 $(TARGET) $(LDLIBS)

dmtestlogger: test/dmtestlogger.f90 $(TARGET)
	$(FC) $(FFLAGS) $(LDFLAGS) -o dmtestlogger test/dmtestlogger.f90 $(TARGET) $(LDLIBS) $(LIBRT)

dmtestlua: test/dmtestlua.f90 $(TARGET)
	$(FC) $(FFLAGS) $(LDFLAGS) -o dmtestlua test/dmtestlua.f90 $(TARGET) $(LDLIBS) $(LIBLUA54)

dmtestjob: test/dmtestjob.f90 $(TARGET)
	$(FC) $(FFLAGS) $(LDFLAGS) -o dmtestjob test/dmtestjob.f90 $(TARGET) $(LDLIBS)

dmtestjson: test/dmtestjson.f90 $(TARGET)
	$(FC) $(FFLAGS) $(LDFLAGS) -o dmtestjson test/dmtestjson.f90 $(TARGET) $(LDLIBS)

dmtestmail: test/dmtestmail.f90 $(TARGET)
	$(FC) $(FFLAGS) $(LDFLAGS) -o dmtestmail test/dmtestmail.f90 $(TARGET) $(LDLIBS) $(LIBCURL)

dmtestmqtt: test/dmtestmqtt.f90 $(TARGET)
	$(FC) $(FFLAGS) $(LDFLAGS) -o dmtestmqtt test/dmtestmqtt.f90 $(TARGET) $(LDLIBS) $(LIBCURL)

dmtestmqueue: test/dmtestmqueue.f90 $(TARGET)
	$(FC) $(FFLAGS) $(LDFLAGS) -o dmtestmqueue test/dmtestmqueue.f90 $(TARGET) $(LDLIBS) $(LIBRT)

dmtestnml: test/dmtestnml.f90 $(TARGET)
	$(FC) $(FFLAGS) $(LDFLAGS) -o dmtestnml test/dmtestnml.f90 $(TARGET) $(LDLIBS)

dmtestobserv: test/dmtestobserv.f90 $(TARGET)
	$(FC) $(FFLAGS) $(LDFLAGS) -o dmtestobserv test/dmtestobserv.f90 $(TARGET) $(LDLIBS)

dmtestpath: test/dmtestpath.f90 $(TARGET)
	$(FC) $(FFLAGS) $(LDFLAGS) -o dmtestpath test/dmtestpath.f90 $(TARGET) $(LDLIBS)

dmtestpipe: test/dmtestpipe.f90 $(TARGET)
	$(FC) $(FFLAGS) $(LDFLAGS) -o dmtestpipe test/dmtestpipe.f90 $(TARGET) $(LDLIBS)

dmtestplot: test/dmtestplot.f90 $(TARGET)
	$(FC) $(FFLAGS) $(LDFLAGS) -o dmtestplot test/dmtestplot.f90 $(TARGET) $(LDLIBS)

dmtestregex: test/dmtestregex.f90 $(TARGET)
	$(FC) $(FFLAGS) $(LDFLAGS) -o dmtestregex test/dmtestregex.f90 $(TARGET) $(LDLIBS) $(LIBPCRE2)

dmtestrouter: test/dmtestrouter.f90 $(TARGET)
	$(FC) $(FFLAGS) $(LDFLAGS) -o dmtestrouter test/dmtestrouter.f90 $(TARGET) $(LDLIBS)

dmtestrpc: test/dmtestrpc.f90 $(TARGET)
	$(FC) $(FFLAGS) $(LDFLAGS) -o dmtestrpc test/dmtestrpc.f90 $(TARGET) $(LDLIBS) $(LIBCURL) $(LIBZ)

dmteststring: test/dmteststring.f90 $(TARGET)
	$(FC) $(FFLAGS) $(LDFLAGS) -o dmteststring test/dmteststring.f90 $(TARGET) $(LDLIBS)

dmtesttime: test/dmtesttime.f90 $(TARGET)
	$(FC) $(FFLAGS) $(LDFLAGS) -o dmtesttime test/dmtesttime.f90 $(TARGET) $(LDLIBS)

dmtesttty: test/dmtesttty.f90 $(TARGET)
	$(FC) $(FFLAGS) $(LDFLAGS) -o dmtesttty test/dmtesttty.f90 $(TARGET) $(LDLIBS)

dmtestunit: test/dmtestunit.f90 $(TARGET)
	$(FC) $(FFLAGS) $(LDFLAGS) -o dmtestunit test/dmtestunit.f90 $(TARGET) $(LDLIBS)

dmtestutil: test/dmtestutil.f90 $(TARGET)
	$(FC) $(FFLAGS) $(LDFLAGS) -o dmtestutil test/dmtestutil.f90 $(TARGET) $(LDLIBS)

dmtestuuid: test/dmtestuuid.f90 $(TARGET)
	$(FC) $(FFLAGS) $(LDFLAGS) -o dmtestuuid test/dmtestuuid.f90 $(TARGET) $(LDLIBS)

dmtestz: test/dmtestz.f90 $(TARGET)
	$(FC) $(FFLAGS) $(LDFLAGS) -o dmtestz test/dmtestz.f90 $(TARGET) $(LDLIBS) $(LIBZ)

# ******************************************************************************
#
# DMPACK programs.
#
# ******************************************************************************
$(DMAPI): app/dmapi.f90 $(TARGET)
	$(FC) $(FFLAGS) $(LDFLAGS) -o $(DMAPI) app/dmapi.f90 $(TARGET) $(LDLIBS) $(LIBSQLITE3) $(LIBZ) $(LIBFASTCGI)

$(DMBACKUP): app/dmbackup.f90 $(TARGET)
	$(FC) $(FFLAGS) $(LDFLAGS) -o $(DMBACKUP) app/dmbackup.f90 $(TARGET) $(LDLIBS) $(LIBSQLITE3)

$(DMBEAT): app/dmbeat.f90 $(TARGET)
	$(FC) $(FFLAGS) $(LDFLAGS) -o $(DMBEAT) app/dmbeat.f90 $(TARGET) $(LDLIBS) $(LIBCURL) $(LIBLUA54) $(LIBZ) $(LIBRT)

$(DMDB): app/dmdb.f90 $(TARGET)
	$(FC) $(FFLAGS) $(LDFLAGS) -o $(DMDB) app/dmdb.f90 $(TARGET) $(LDLIBS) $(LIBLUA54) $(LIBSQLITE3) $(LIBPTHREAD) $(LIBRT)

$(DMDBCLI): app/dmdbctl.f90 $(TARGET)
	$(FC) $(FFLAGS) $(LDFLAGS) -o $(DMDBCLI) app/dmdbctl.f90 $(TARGET) $(LDLIBS) $(LIBSQLITE3)

$(DMEXPORT): app/dmexport.f90 $(TARGET)
	$(FC) $(FFLAGS) $(LDFLAGS) -o $(DMEXPORT) app/dmexport.f90 $(TARGET) $(LDLIBS) $(LIBSQLITE3)

$(DMFEED): app/dmfeed.f90 $(TARGET)
	$(FC) $(FFLAGS) $(LDFLAGS) -o $(DMFEED) app/dmfeed.f90 $(TARGET) $(LDLIBS) $(LIBLUA54) $(LIBSQLITE3)

$(DMFS): app/dmfs.f90 $(TARGET)
	$(FC) $(FFLAGS) $(LDFLAGS) -o $(DMFS) app/dmfs.f90 $(TARGET) $(LDLIBS) $(LIBLUA54) $(LIBPCRE2) $(LIBRT)

$(DMGRAPH): app/dmgraph.f90 $(TARGET)
	$(FC) $(FFLAGS) $(LDFLAGS) -o $(DMGRAPH) app/dmgraph.f90 $(TARGET) $(LDLIBS) $(LIBLUA54) $(LIBSQLITE3)

$(DMIMPORT): app/dmimport.f90 $(TARGET)
	$(FC) $(FFLAGS) $(LDFLAGS) -o $(DMIMPORT) app/dmimport.f90 $(TARGET) $(LDLIBS) $(LIBSQLITE3)

$(DMINFO): app/dminfo.f90 $(TARGET)
	$(FC) $(FFLAGS) $(LDFLAGS) -o $(DMINFO) app/dminfo.f90 $(TARGET) $(LDLIBS) $(LIBSQLITE3)

$(DMINIT): app/dminit.f90 $(TARGET)
	$(FC) $(FFLAGS) $(LDFLAGS) -o $(DMINIT) app/dminit.f90 $(TARGET) $(LDLIBS) $(LIBSQLITE3)

$(DMLOG): app/dmlog.f90 $(TARGET)
	$(FC) $(FFLAGS) $(LDFLAGS) -o $(DMLOG) app/dmlog.f90 $(TARGET) $(LDLIBS) $(LIBRT)

$(DMLOGGER): app/dmlogger.f90 $(TARGET)
	$(FC) $(FFLAGS) $(LDFLAGS) -o $(DMLOGGER) app/dmlogger.f90 $(TARGET) $(LDLIBS) $(LIBLUA54) $(LIBSQLITE3) $(LIBPTHREAD) $(LIBRT)

$(DMLUA): app/dmlua.f90 $(TARGET)
	$(FC) $(FFLAGS) $(LDFLAGS) -o $(DMLUA) app/dmlua.f90 $(TARGET) $(LDLIBS) $(LIBLUA54) $(LIBRT)

$(DMPIPE): app/dmpipe.f90 $(TARGET)
	$(FC) $(FFLAGS) $(LDFLAGS) -o $(DMPIPE) app/dmpipe.f90 $(TARGET) $(LDLIBS) $(LIBLUA54) $(LIBPCRE2) $(LIBRT)

$(DMRECV): app/dmrecv.f90 $(TARGET)
	$(FC) $(FFLAGS) $(LDFLAGS) -o $(DMRECV) app/dmrecv.f90 $(TARGET) $(LDLIBS) $(LIBLUA54) $(LIBRT)

$(DMREPORT): app/dmreport.f90 $(TARGET)
	$(FC) $(FFLAGS) $(LDFLAGS) -o $(DMREPORT) app/dmreport.f90 $(TARGET) $(LDLIBS) $(LIBLUA54) $(LIBSQLITE3)

$(DMSEND): app/dmsend.f90 $(TARGET)
	$(FC) $(FFLAGS) $(LDFLAGS) -o $(DMSEND) app/dmsend.f90 $(TARGET) $(LDLIBS) $(LIBLUA54) $(LIBRT)

$(DMSERIAL): app/dmserial.f90 $(TARGET)
	$(FC) $(FFLAGS) $(LDFLAGS) -o $(DMSERIAL) app/dmserial.f90 $(TARGET) $(LDLIBS) $(LIBLUA54) $(LIBPCRE2) $(LIBRT)

$(DMSYNC): app/dmsync.f90 $(TARGET)
	$(FC) $(FFLAGS) $(LDFLAGS) -o $(DMSYNC) app/dmsync.f90 $(TARGET) $(LDLIBS) $(LIBCURL) $(LIBLUA54) $(LIBSQLITE3) $(LIBZ) $(LIBPTHREAD) $(LIBRT)

$(DMUUID): app/dmuuid.f90 $(TARGET)
	$(FC) $(FFLAGS) $(LDFLAGS) -o $(DMUUID) app/dmuuid.f90 $(TARGET) $(LDLIBS)

$(DMWEB): app/dmweb.f90 $(TARGET)
	$(FC) $(FFLAGS) $(LDFLAGS) -o $(DMWEB) app/dmweb.f90 $(TARGET) $(LDLIBS) $(LIBSQLITE3)

# ******************************************************************************
#
# Source code documentation, manual pages, and User's Guide.
#
# ******************************************************************************
doc:
	$(FORD) ford.md -d ./src

man:
	cd $(ADOCDIR) && $(MAKE) man

html:
	cd $(ADOCDIR) && $(MAKE) html

pdf:
	cd $(ADOCDIR) && $(MAKE) pdf

guide:
	cd $(GUIDEDIR) && $(MAKE)

# ******************************************************************************
#
# Installation and deinstallation.
#
# ******************************************************************************
install:
	install -d $(IBINDIR)
	install -d $(IETCDIR)
	install -d $(IINCDIR)
	install -d $(ILIBDIR)
	install -d $(ISHRDIR)
	install -m 755 $(DISTDIR)/dmapi $(IBINDIR)
	install -m 755 $(DISTDIR)/dmbackup $(IBINDIR)
	install -m 755 $(DISTDIR)/dmbeat $(IBINDIR)
	install -m 755 $(DISTDIR)/dmdb $(IBINDIR)
	install -m 755 $(DISTDIR)/dmdbctl $(IBINDIR)
	install -m 755 $(DISTDIR)/dmexport $(IBINDIR)
	install -m 755 $(DISTDIR)/dmfeed $(IBINDIR)
	install -m 755 $(DISTDIR)/dmfs $(IBINDIR)
	install -m 755 $(DISTDIR)/dmgraph $(IBINDIR)
	install -m 755 $(DISTDIR)/dmimport $(IBINDIR)
	install -m 755 $(DISTDIR)/dminfo $(IBINDIR)
	install -m 755 $(DISTDIR)/dminit $(IBINDIR)
	install -m 755 $(DISTDIR)/dmlog $(IBINDIR)
	install -m 755 $(DISTDIR)/dmlogger $(IBINDIR)
	install -m 755 $(DISTDIR)/dmlua $(IBINDIR)
	install -m 755 $(DISTDIR)/dmpipe $(IBINDIR)
	install -m 755 $(DISTDIR)/dmrecv $(IBINDIR)
	install -m 755 $(DISTDIR)/dmreport $(IBINDIR)
	install -m 755 $(DISTDIR)/dmsend $(IBINDIR)
	install -m 755 $(DISTDIR)/dmserial $(IBINDIR)
	install -m 755 $(DISTDIR)/dmsync $(IBINDIR)
	install -m 755 $(DISTDIR)/dmuuid $(IBINDIR)
	install -m 755 $(DISTDIR)/dmweb $(IBINDIR)
	install -m 644 $(INCDIR)/*.mod $(IINCDIR)
	install -m 644 $(TARGET) $(ILIBDIR)
	install -m 644 $(SHARED) $(ILIBDIR)
	install -m 644 $(CONFDIR)/*.conf.sample $(IETCDIR)
	install -m 644 $(SHRDIR)/dmpack.css $(ISHRDIR)
	install -m 644 $(SHRDIR)/dmpack.min.css $(ISHRDIR)
	install -m 644 $(SHRDIR)/dmreport.css $(ISHRDIR)
	install -m 644 $(SHRDIR)/dmreport.min.css $(ISHRDIR)
	install -m 644 $(SHRDIR)/dmlua.lua $(ISHRDIR)
	install -m 644 $(SHRDIR)/feed.xsl $(ISHRDIR)
	install -m 755 $(SHRDIR)/mkreport.sh $(ISHRDIR)

deinstall:
	$(RM) -rf $(PREFIX)/include/dmpack
	$(RM) -rf $(PREFIX)/share/dmpack
	$(RM) -f $(PREFIX)/etc/*.conf.sample
	$(RM) -f $(PREFIX)/lib/libdmpack.a
	$(RM) -f $(PREFIX)/lib/libdmpack.so
	$(RM) -f $(PREFIX)/dmapi
	$(RM) -f $(PREFIX)/dmbackup
	$(RM) -f $(PREFIX)/dmbeat
	$(RM) -f $(PREFIX)/dmdb
	$(RM) -f $(PREFIX)/dmdbctl
	$(RM) -f $(PREFIX)/dmexport
	$(RM) -f $(PREFIX)/dmfeed
	$(RM) -f $(PREFIX)/dmfs
	$(RM) -f $(PREFIX)/dmgraph
	$(RM) -f $(PREFIX)/dmimport
	$(RM) -f $(PREFIX)/dminfo
	$(RM) -f $(PREFIX)/dminit
	$(RM) -f $(PREFIX)/dmlog
	$(RM) -f $(PREFIX)/dmlogger
	$(RM) -f $(PREFIX)/dmlua
	$(RM) -f $(PREFIX)/dmpipe
	$(RM) -f $(PREFIX)/dmrecv
	$(RM) -f $(PREFIX)/dmreport
	$(RM) -f $(PREFIX)/dmsend
	$(RM) -f $(PREFIX)/dmserial
	$(RM) -f $(PREFIX)/dmsync
	$(RM) -f $(PREFIX)/dmuuid
	$(RM) -f $(PREFIX)/dmweb
	@echo
	@echo "You may need to manually remove $(PREFIX)/etc/dmpack/ if it is no longer needed."
	@echo

install_freebsd:
	$(MAKE) install PREFIX=/usr/local

install_linux:
	$(MAKE) install PREFIX=/usr

# ******************************************************************************
#
# Remove binaries, libraries, modules and object files in, clear "dist" and
# "man" directories.
#
# ******************************************************************************
clean:
	@echo "--- deleting libraries ..."
	if [ -e $(THIN) ];   then $(RM) $(THIN); fi
	if [ -e $(TARGET) ]; then $(RM) $(TARGET); fi
	if [ -e $(SHARED) ]; then $(RM) $(SHARED); fi
	@echo "--- deleting build files ..."
	if [ `ls -1 *.mod 2>/dev/null | wc -l` -gt 0 ]; then $(RM) *.mod; fi
	if [ `ls -1 *.a   2>/dev/null | wc -l` -gt 0 ]; then $(RM) *.a; fi
	if [ `ls -1 *.so  2>/dev/null | wc -l` -gt 0 ]; then $(RM) *.so; fi
	if [ `ls -1 *.o   2>/dev/null | wc -l` -gt 0 ]; then $(RM) *.o; fi
	@echo "--- deleting test programs ..."
	if [ `ls -1 dmtest* 2>/dev/null | wc -l` -gt 0 ]; then $(RM) dmtest*; fi
	@echo "--- deleting applications ..."
	if [ `ls -1 $(DISTDIR) 2>/dev/null | wc -l` -gt 0 ]; then $(RM) $(DISTDIR)/*; fi
	@echo "--- cleaning man pages ..."
	cd $(ADOCDIR)  && $(MAKE) clean
	@echo "--- cleaning guide ..."
	cd $(GUIDEDIR) && $(MAKE) clean

# ******************************************************************************
#
# Additionally, clean all dependencies and remove FORD output.
#
# ******************************************************************************
purge: clean
	@echo "--- cleaning fortran-curl ..."
	cd vendor/fortran-curl/ && make clean TARGET="../../$(LIBFCURL)"
	@echo "--- cleaning fortran-lua54 ..."
	cd vendor/fortran-lua54/ && make clean TARGET="../../$(LIBFLUA54)"
	@echo "--- cleaning fortran-pcre2 ..."
	cd vendor/fortran-pcre2/ && make clean TARGET="../../$(LIBFPCRE2)"
	@echo "--- cleaning fortran-sqlite3 ..."
	cd vendor/fortran-sqlite3/ && make clean TARGET="../../$(LIBFSQLITE3)"
	@echo "--- cleaning fortran-unix ..."
	cd vendor/fortran-unix/ && make clean TARGET="../../$(LIBFUNIX)"
	@echo "--- cleaning fortran-zlib ..."
	cd vendor/fortran-zlib/ && make clean TARGET="../../$(LIBFZ)"
	@echo "--- deleting module files ..."
	if [ -e $(INCDIR) ]; then $(RM) -r $(INCDIR); fi
	@echo "--- deleting source code documentation ..."
	if [ -e $(DOCDIR) ]; then $(RM) -r $(DOCDIR); fi
