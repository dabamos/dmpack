# ******************************************************************************
#
#                          POSIX Makefile for DMPACK
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
# Display the build options:
#
#   $ make options
#
# Show build target descriptions:
#
#   $ make help
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
# DMPACK build options:
#
#   OS      - The operating system, either `FreeBSD` or `linux` (for GCC only).
#   PREFIX  - Path prefix, `/usr/local` on FreeBSD, `/usr` on Linux.
#
#   FC      - Fortran 2018 compiler (`gfortran`, `ifx`).
#   CC      - ANSI C compiler (`gcc`, `icx`).
#   AR      - Archiver.
#   MAKE    - Either: `make`, `bmake`, or `gmake`.
#   STRIP   - Strip utility.
#   RM      - Remove command.
#   SH      - Shell.
#   FORD    - FORD documentation generator.
#
#   FFLAGS  - Fortran compiler options.
#   CLAGS   - C compiler options.
#   PPFLAGS - Pre-processor options (must be empty for Intel oneAPI).
#   ARFLAGS - Archiver options.
#   LDFLAGS - Linker options.
#   LDLIBS  - Linker libraries.
#
#   DEBUG   - Debug options.
#   RELEASE - Release options.
#
#   THIN    - Thin DMPACK library (without interface bindings).
#   TARGET  - Path to the full DMPACK library (with interface bindings).
#   SHARED  - Path to the shared DMPACK library (with interface bindings).
#
#   CONFDIR - Directory of configuration files.
#   DISTDIR - Directory of distribution files (libraries and programs).
#   LIBDIR  - Directory of static libraries.
#   INCDIR  - Directory of Fortran module files.
#   SHRDIR  - Directory of shared files.
#   SRCDIR  - Directory of source files.
#
#   IBINDIR - Installation directory of DMPACK binaries.
#   IETCDIR - Installation directory of DMPACK configuration files.
#   IINCDIR - Installation directory of DMPACK modules.
#   ILIBDIR - Installation directory of DMPACK libraries.
#   ISHRDIR - Installation directory of DMPACK shared files.
#
# ******************************************************************************

.POSIX:
.SUFFIXES:

# Platform.
OS      = FreeBSD
PREFIX  = /usr/local

# Compilers and build utilities.
FC      = gfortran
CC      = gcc
AR      = ar
MAKE    = make
STRIP   = strip
RM      = /bin/rm
SH      = /bin/sh
FORD    = ford
GZIP    = gzip

# Workspace directories.
CONFDIR = ./config
DISTDIR = ./dist

INCDIR  = ./include
LIBDIR  = ./lib
SHRDIR  = ./share
SRCDIR  = ./src

ADOCDIR = ./adoc
DOCDIR  = ./doc
MANDIR  = ./man
GUIDDIR = ./guide

# Installation directories.
IBINDIR = $(PREFIX)/bin
IETCDIR = $(PREFIX)/etc/dmpack
IINCDIR = $(PREFIX)/include/dmpack
ILIBDIR = $(PREFIX)/lib
IMANDIR = $(PREFIX)/man/man1
ISHRDIR = $(PREFIX)/share/dmpack

# DMPACK libraries.
THIN    = $(LIBDIR)/libdm.a
TARGET  = $(DISTDIR)/libdmpack.a
SHARED  = $(DISTDIR)/libdmpack.so

# Debug and release options.
#DEBUG  = -g -O0 -Wall -fcheck=all -fmax-errors=1 -fPIE -ffpe-trap=invalid,zero,overflow -fno-omit-frame-pointer
DEBUG   = -g -O0 -Wall -fcheck=all -fmax-errors=1
RELEASE = -mtune=native -O2

# Common build options.
FFLAGS  = $(RELEASE) -ffree-line-length-0
CFLAGS  = $(RELEASE)
PPFLAGS = -cpp -D__$(OS)__
ARFLAGS = -rcs
LDFLAGS = -I$(INCDIR) -J$(INCDIR) -L$(PREFIX)/lib -z execstack -z now
#LDLIBS = -pie -static-libasan -fsanitize=address -fno-omit-frame-pointer
LDLIBS  =

# Additional include search directories.
INCHDF5 = `pkg-config --cflags hdf5`

# Shared libraries to link.
LIBCURL    = `curl-config --libs`
LIBFASTCGI = -lfcgi
LIBHDF5    = `pkg-config --libs hdf5` -lhdf5_fortran
LIBLAPACK  = -llapack -lblas
LIBLUA54   = `pkg-config --libs-only-l lua-5.4`
LIBPCRE2   = `pkg-config --libs-only-l libpcre2-8`
LIBPTHREAD = -lpthread
LIBRT      = -lrt
LIBSQLITE3 = `pkg-config --libs-only-l sqlite3`
LIBZ       = `pkg-config --libs-only-l zlib`

# All shared libraries (for `libdmpack.so`).
LIBSHARED  = $(LIBCURL) $(LIBFASTCGI) $(LIBHDF5) $(LIBLAPACK) $(LIBLUA54) \
             $(LIBPCRE2) $(LIBPTHREAD) $(LIBRT) $(LIBSQLITE3) $(LIBZ)

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
DMDBCTL  = $(DISTDIR)/dmdbctl
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

# Library source files.
SRC = src/dm_version.f90 src/dm_kind.f90 src/dm_platform.f90 src/dm_ascii.f90 \
      src/dm_const.f90 src/dm_error.f90 src/dm_string.f90 src/dm_type.f90 \
      src/dm_format.f90 src/dm_ansi.f90 src/dm_env.f90 src/dm_util.f90 \
      src/dm_time.f90 src/dm_timer.f90 src/dm_base64.f90 src/dm_path.f90 \
      src/dm_file.f90 src/dm_hash.f90 src/dm_hash_table.f90 src/dm_hdf5.f90 \
      src/dm_unit.f90 src/dm_id.f90 src/dm_uuid.f90 src/dm_arg.f90 \
      src/dm_signal.f90 src/dm_system.f90 src/dm_pipe.f90 src/dm_tty.f90 \
      src/dm_sem.f90 src/dm_mutex.f90 src/dm_dp.f90 src/dm_fifo.f90 \
      src/dm_node.f90 src/dm_sensor.f90 src/dm_target.f90 src/dm_response.f90 \
      src/dm_request.f90 src/dm_observ.f90 src/dm_log.f90 src/dm_job.f90 \
      src/dm_plot.f90 src/dm_report.f90 src/dm_regex.f90  src/dm_sync.f90 \
      src/dm_beat.f90 src/dm_mqueue.f90 src/dm_logger.f90 src/dm_test.f90 \
      src/dm_nml.f90 src/dm_sql.f90 src/dm_db.f90 src/dm_z.f90 src/dm_person.f90 \
      src/dm_mail.f90 src/dm_http.f90 src/dm_mime.f90 src/dm_api.f90 \
      src/dm_rpc.f90 src/dm_mqtt.f90 src/dm_cgi.f90 src/dm_fcgi.f90 \
      src/dm_block.f90 src/dm_csv.f90 src/dm_json.f90 src/dm_jsonl.f90 \
      src/dm_html.f90 src/dm_atom.f90 src/dm_cgi_router.f90 src/dm_la.f90 \
      src/dm_transform.f90 src/dm_geocom_error.f90 src/dm_geocom_api.f90 \
      src/dm_geocom.f90 src/dm_lua.f90 src/dm_lua_api.f90 src/dm_lua_geocom.f90 \
      src/dm_lua_lib.f90 src/dm_config.f90 src/dm_rts.f90 src/dm_mqueue_util.f90 \
      src/dmpack.f90

# Library object files.
OBJ = dm_version.o dm_kind.o dm_platform.o dm_ascii.o dm_const.o dm_error.o \
      dm_string.o dm_type.o dm_format.o dm_ansi.o dm_env.o dm_util.o dm_time.o \
      dm_timer.o dm_base64.o dm_path.o dm_file.o dm_hash.o dm_hash_table.o \
      dm_hdf5.o dm_unit.o dm_id.o dm_uuid.o dm_arg.o dm_signal.o dm_system.o \
      dm_pipe.o dm_tty.o dm_sem.o dm_mutex.o dm_dp.o dm_fifo.o dm_node.o \
      dm_sensor.o dm_target.o dm_response.o dm_request.o dm_observ.o dm_log.o \
      dm_job.o dm_plot.o dm_report.o dm_regex.o dm_sync.o dm_beat.o dm_mqueue.o \
      dm_logger.o dm_test.o dm_nml.o dm_sql.o dm_db.o dm_z.o dm_person.o dm_mail.o \
      dm_http.o dm_mime.o dm_api.o dm_rpc.o dm_mqtt.o dm_cgi.o dm_fcgi.o dm_block.o \
      dm_csv.o dm_json.o dm_jsonl.o dm_html.o dm_atom.o dm_cgi_router.o dm_la.o \
      dm_transform.o dm_geocom_error.o dm_geocom_api.o dm_geocom.o dm_lua.o \
      dm_lua_api.o dm_lua_geocom.o dm_lua_lib.o dm_config.o dm_rts.o dm_mqueue_util.o \
      dmpack.o

# ******************************************************************************
#
# Build targets.
#
# ******************************************************************************

# Named build targets.
.PHONY: all app clean deinstall doc freebsd freebsd_debug freebsd_release guide \
        help html install install_freebsd install_linux linux linux_debug \
        linux_release man options pdf purge setup test

# Library target.
all: $(TARGET) $(SHARED) test app

# Apps target.
app: $(DMAPI) $(DMBACKUP) $(DMBEAT) $(DMDB) $(DMDBCTL) $(DMEXPORT) $(DMFEED) \
     $(DMFS) $(DMGRAPH) $(DMINFO) $(DMIMPORT) $(DMINIT) $(DMLOG) $(DMLOGGER) \
     $(DMLUA) $(DMPIPE) $(DMRECV) $(DMREPORT) $(DMSEND) $(DMSERIAL) $(DMSYNC) \
     $(DMUUID) $(DMWEB)

# Tests target.
test: dmtestapi dmtestatom dmtestbase64 dmtestcgi dmtestconfig dmtestcsv \
      dmtestdb dmtestdp dmtesthash dmtesthdf5 dmtesthtml dmtestlogger dmtestlua \
      dmtestjob dmtestjson dmtestmail dmtestmqtt dmtestmqueue dmtestnml \
      dmtestobserv dmtestpath dmtestpipe dmtestplot dmtestregex dmtestrpc \
      dmtestrts dmteststring dmtesttime dmtesttty dmtestunit dmtestutil \
      dmtestuuid dmtestz

# ******************************************************************************
#
# Setup.
#
# ******************************************************************************

setup:
	mkdir -p $(INCDIR)
	mkdir -p $(LIBDIR)
	mkdir -p $(DISTDIR)

# ******************************************************************************
#
# FreeBSD target.
#
# ******************************************************************************

freebsd_debug:
	$(MAKE) all OS=FreeBSD PREFIX=/usr/local RELEASE="$(DEBUG)"

freebsd_release:
	$(MAKE) all OS=FreeBSD PREFIX=/usr/local
	$(STRIP) -s $(DISTDIR)/dm*

freebsd:
	$(MAKE) freebsd_release

# ******************************************************************************
#
# Linux target.
#
# ******************************************************************************

linux_debug:
	$(MAKE) all OS=linux PREFIX=/usr RELEASE="$(DEBUG)"

linux_release:
	$(MAKE) all OS=linux PREFIX=/usr
	$(STRIP) -s $(DISTDIR)/dm*

linux:
	$(MAKE) linux_release

# ******************************************************************************
#
# 3rd party interface libraries.
#
# ******************************************************************************

$(LIBFCURL): setup
	cd vendor/fortran-curl/ && make CFLAGS="-fPIC $(CFLAGS)" FFLAGS="-fPIC $(FFLAGS)" PREFIX="$(PREFIX)" TARGET="../../$(LIBFCURL)"
	cp ./vendor/fortran-curl/*.mod $(INCDIR)/

$(LIBFLUA54): setup
	cd vendor/fortran-lua54/ && make CFLAGS="-fPIC $(CFLAGS)" FFLAGS="-fPIC $(FFLAGS)" PREFIX="$(PREFIX)" TARGET="../../$(LIBFLUA54)"
	cp ./vendor/fortran-lua54/*.mod $(INCDIR)/

$(LIBFPCRE2): setup
	cd vendor/fortran-pcre2/ && make CFLAGS="-fPIC $(CFLAGS)" FFLAGS="-fPIC $(FFLAGS)" PREFIX="$(PREFIX)" TARGET="../../$(LIBFPCRE2)"
	cp ./vendor/fortran-pcre2/*.mod $(INCDIR)/

$(LIBFSQLITE3): setup
	cd vendor/fortran-sqlite3/ && make CFLAGS="-fPIC $(CFLAGS)" FFLAGS="-fPIC $(FFLAGS)" PREFIX="$(PREFIX)" TARGET="../../$(LIBFSQLITE3)"
	cp ./vendor/fortran-sqlite3/*.mod $(INCDIR)/

$(LIBFUNIX): setup
	@echo "---"
	@echo "--- Building for $(OS) ..."
	@echo "---"
	cd vendor/fortran-unix/ && make CFLAGS="-fPIC $(CFLAGS)" FFLAGS="-fPIC $(FFLAGS)" PREFIX="$(PREFIX)" PPFLAGS="$(PPFLAGS)" TARGET="../../$(LIBFUNIX)"
	cp ./vendor/fortran-unix/*.mod $(INCDIR)/

$(LIBFZ): setup
	cd vendor/fortran-zlib/ && make CFLAGS="-fPIC $(CFLAGS)" FFLAGS="-fPIC $(FFLAGS)" PREFIX="$(PREFIX)" TARGET="../../$(LIBFZ)"
	cp ./vendor/fortran-zlib/*.mod $(INCDIR)/

# ******************************************************************************
#
# DMPACK static libraries.
#
# ******************************************************************************

$(OBJ): $(SRC)
	$(FC) -fPIC $(FFLAGS) $(LDFLAGS) -c src/dm_version.f90
	$(FC) -fPIC $(FFLAGS) $(LDFLAGS) -c src/dm_kind.f90
	$(FC) -fPIC $(FFLAGS) $(LDFLAGS) -c src/dm_platform.f90
	$(FC) -fPIC $(FFLAGS) $(LDFLAGS) -c src/dm_ascii.f90
	$(FC) -fPIC $(FFLAGS) $(LDFLAGS) -c src/dm_const.f90
	$(FC) -fPIC $(FFLAGS) $(LDFLAGS) -c src/dm_error.f90
	$(FC) -fPIC $(FFLAGS) $(LDFLAGS) -c src/dm_string.f90
	$(FC) -fPIC $(FFLAGS) $(LDFLAGS) -c src/dm_type.f90
	$(FC) -fPIC $(FFLAGS) $(LDFLAGS) -c src/dm_format.f90
	$(FC) -fPIC $(FFLAGS) $(LDFLAGS) -c src/dm_ansi.f90
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
	$(FC) -fPIC $(FFLAGS) $(LDFLAGS) -c src/dm_sync.f90
	$(FC) -fPIC $(FFLAGS) $(LDFLAGS) -c src/dm_beat.f90
	$(FC) -fPIC $(FFLAGS) $(LDFLAGS) -c src/dm_mqueue.f90
	$(FC) -fPIC $(FFLAGS) $(LDFLAGS) -c src/dm_logger.f90
	$(FC) -fPIC $(FFLAGS) $(LDFLAGS) -c src/dm_test.f90
	$(FC) -fPIC $(FFLAGS) $(LDFLAGS) -c src/dm_nml.f90
	$(FC) -fPIC $(FFLAGS) $(LDFLAGS) $(INCHDF5) -c src/dm_hdf5.f90
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
	$(FC) -fPIC $(FFLAGS) $(LDFLAGS) -c src/dm_cgi_router.f90
	$(FC) -fPIC $(FFLAGS) $(LDFLAGS) -c src/dm_la.f90
	$(FC) -fPIC $(FFLAGS) $(LDFLAGS) -c src/dm_transform.f90
	$(FC) -fPIC $(FFLAGS) $(LDFLAGS) -c src/dm_geocom_error.f90
	$(FC) -fPIC $(FFLAGS) $(LDFLAGS) -c src/dm_geocom_api.f90
	$(FC) -fPIC $(FFLAGS) $(LDFLAGS) -c src/dm_geocom.f90
	$(FC) -fPIC $(FFLAGS) $(LDFLAGS) -c src/dm_lua.f90
	$(FC) -fPIC $(FFLAGS) $(LDFLAGS) -c src/dm_lua_api.f90
	$(FC) -fPIC $(FFLAGS) $(LDFLAGS) -c src/dm_lua_geocom.f90
	$(FC) -fPIC $(FFLAGS) $(LDFLAGS) -c src/dm_lua_lib.f90
	$(FC) -fPIC $(FFLAGS) $(LDFLAGS) -c src/dm_config.f90
	$(FC) -fPIC $(FFLAGS) $(LDFLAGS) -c src/dm_rts.f90
	$(FC) -fPIC $(FFLAGS) $(LDFLAGS) -c src/dm_mqueue_util.f90
	$(FC) -fPIC $(FFLAGS) $(LDFLAGS) -c src/dmpack.f90

# Static library `libdmpack.a`.
$(TARGET): $(LIBF) $(OBJ)
	$(AR) $(ARFLAGS) $(THIN) $(OBJ)
	$(SH) makelib.sh $(TARGET) $(THIN)

# ******************************************************************************
#
# DMPACK shared library (includes Lua API).
#
# ******************************************************************************

# Shared library `libdmpack.so`.
$(SHARED): $(TARGET)
	$(FC) -fPIC -shared $(FFLAGS) $(LDFLAGS) -o $(SHARED) -Wl,--whole-archive $(TARGET) -Wl,--no-whole-archive $(LIBSHARED) $(LDLIBS)

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

dmtestconfig: test/dmtestconfig.f90 $(TARGET)
	$(FC) $(FFLAGS) $(LDFLAGS) -o dmtestconfig test/dmtestconfig.f90 $(TARGET) $(LDLIBS) $(LIBLUA54)

dmtestcsv: test/dmtestcsv.f90 $(TARGET)
	$(FC) $(FFLAGS) $(LDFLAGS) -o dmtestcsv test/dmtestcsv.f90 $(TARGET) $(LDLIBS)

dmtestdb: test/dmtestdb.f90 $(TARGET)
	$(FC) $(FFLAGS) $(LDFLAGS) -o dmtestdb test/dmtestdb.f90 $(TARGET) $(LDLIBS) $(LIBSQLITE3)

dmtestdp: test/dmtestdp.f90 $(TARGET)
	$(FC) $(FFLAGS) $(LDFLAGS) -o dmtestdp test/dmtestdp.f90 $(TARGET) $(LDLIBS)

dmtesthash: test/dmtesthash.f90 $(TARGET)
	$(FC) $(FFLAGS) $(LDFLAGS) -o dmtesthash test/dmtesthash.f90 $(TARGET) $(LDLIBS)

dmtesthdf5: test/dmtesthdf5.f90 $(TARGET)
	$(FC) $(FFLAGS) $(LDFLAGS) $(INCHDF5) -o dmtesthdf5 test/dmtesthdf5.f90 $(TARGET) $(LDLIBS) $(LIBHDF5)

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

dmtestrts: test/dmtestrts.f90 $(TARGET)
	$(FC) $(FFLAGS) $(LDFLAGS) -o dmtestrts test/dmtestrts.f90 $(TARGET) $(LDLIBS)

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

$(DMDBCTL): app/dmdbctl.f90 $(TARGET)
	$(FC) $(FFLAGS) $(LDFLAGS) -o $(DMDBCTL) app/dmdbctl.f90 $(TARGET) $(LDLIBS) $(LIBSQLITE3)

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
# Documentation.
#
# ******************************************************************************

# Documentation from source code.
doc:
	$(FORD) ford.md

# AsciiDoc to man pages.
man:
	cd $(ADOCDIR) && $(MAKE) man

# Man pages to HTML format.
html:
	cd $(ADOCDIR) && $(MAKE) html

# Man pages to PDF format.
pdf:
	cd $(ADOCDIR) && $(MAKE) pdf

# User's Guide to HTML format.
guide:
	cd $(GUIDDIR) && $(MAKE)

# ******************************************************************************
#
# Installation and deinstallation.
#
# ******************************************************************************

install:
	@echo "--- Installing DMPACK to $(PREFIX) ..."
	install -d $(IBINDIR)
	install -d $(IETCDIR)
	install -d $(IINCDIR)
	install -d $(ILIBDIR)
	install -d $(IMANDIR)
	install -d $(ISHRDIR)
	install -m 755 $(DMAPI)    $(IBINDIR)/
	install -m 755 $(DMBACKUP) $(IBINDIR)/
	install -m 755 $(DMBEAT)   $(IBINDIR)/
	install -m 755 $(DMDB)     $(IBINDIR)/
	install -m 755 $(DMDBCTL)  $(IBINDIR)/
	install -m 755 $(DMEXPORT) $(IBINDIR)/
	install -m 755 $(DMFEED)   $(IBINDIR)/
	install -m 755 $(DMFS)     $(IBINDIR)/
	install -m 755 $(DMGRAPH)  $(IBINDIR)/
	install -m 755 $(DMIMPORT) $(IBINDIR)/
	install -m 755 $(DMINFO)   $(IBINDIR)/
	install -m 755 $(DMINIT)   $(IBINDIR)/
	install -m 755 $(DMLOG)    $(IBINDIR)/
	install -m 755 $(DMLOGGER) $(IBINDIR)/
	install -m 755 $(DMLUA)    $(IBINDIR)/
	install -m 755 $(DMPIPE)   $(IBINDIR)/
	install -m 755 $(DMRECV)   $(IBINDIR)/
	install -m 755 $(DMREPORT) $(IBINDIR)/
	install -m 755 $(DMSEND)   $(IBINDIR)/
	install -m 755 $(DMSERIAL) $(IBINDIR)/
	install -m 755 $(DMSYNC)   $(IBINDIR)/
	install -m 755 $(DMUUID)   $(IBINDIR)/
	install -m 755 $(DMWEB)    $(IBINDIR)/
	install -m 644 $(INCDIR)/*.mod $(IINCDIR)/
	install -m 644 $(TARGET) $(ILIBDIR)/
	install -m 644 $(SHARED) $(ILIBDIR)/
	install -m 644 $(CONFDIR)/*.conf.sample $(IETCDIR)/
	install -m 644 $(SHRDIR)/dmpack.css       $(ISHRDIR)/
	install -m 644 $(SHRDIR)/dmpack.min.css   $(ISHRDIR)/
	install -m 644 $(SHRDIR)/dmreport.css     $(ISHRDIR)/
	install -m 644 $(SHRDIR)/dmreport.min.css $(ISHRDIR)/
	install -m 644 $(SHRDIR)/dmlua.lua        $(ISHRDIR)/
	install -m 644 $(SHRDIR)/feed.xsl         $(ISHRDIR)/
	install -m 755 $(SHRDIR)/diskfree.sh      $(ISHRDIR)/
	install -m 755 $(SHRDIR)/mkreport.sh      $(ISHRDIR)/
	$(GZIP) -9 < $(MANDIR)/dmapi.1    > $(IMANDIR)/dmapi.1.gz
	$(GZIP) -9 < $(MANDIR)/dmbackup.1 > $(IMANDIR)/dmbackup.1.gz
	$(GZIP) -9 < $(MANDIR)/dmbeat.1   > $(IMANDIR)/dmbeat.1.gz
	$(GZIP) -9 < $(MANDIR)/dmdb.1     > $(IMANDIR)/dmdb.1.gz
	$(GZIP) -9 < $(MANDIR)/dmdbctl.1  > $(IMANDIR)/dmdbctl.1.gz
	$(GZIP) -9 < $(MANDIR)/dmexport.1 > $(IMANDIR)/dmexport.1.gz
	$(GZIP) -9 < $(MANDIR)/dmfeed.1   > $(IMANDIR)/dmfeed.1.gz
	$(GZIP) -9 < $(MANDIR)/dmfs.1     > $(IMANDIR)/dmfs.1.gz
	$(GZIP) -9 < $(MANDIR)/dmgraph.1  > $(IMANDIR)/dmgraph.1.gz
	$(GZIP) -9 < $(MANDIR)/dmimport.1 > $(IMANDIR)/dmimport.1.gz
	$(GZIP) -9 < $(MANDIR)/dminfo.1   > $(IMANDIR)/dminfo.1.gz
	$(GZIP) -9 < $(MANDIR)/dminit.1   > $(IMANDIR)/dminit.1.gz
	$(GZIP) -9 < $(MANDIR)/dmlog.1    > $(IMANDIR)/dmlog.1.gz
	$(GZIP) -9 < $(MANDIR)/dmlogger.1 > $(IMANDIR)/dmlogger.1.gz
	$(GZIP) -9 < $(MANDIR)/dmlua.1    > $(IMANDIR)/dmlua.1.gz
	$(GZIP) -9 < $(MANDIR)/dmpipe.1   > $(IMANDIR)/dmpipe.1.gz
	$(GZIP) -9 < $(MANDIR)/dmrecv.1   > $(IMANDIR)/dmrecv.1.gz
	$(GZIP) -9 < $(MANDIR)/dmreport.1 > $(IMANDIR)/dmreport.1.gz
	$(GZIP) -9 < $(MANDIR)/dmsend.1   > $(IMANDIR)/dmsend.1.gz
	$(GZIP) -9 < $(MANDIR)/dmserial.1 > $(IMANDIR)/dmserial.1.gz
	$(GZIP) -9 < $(MANDIR)/dmsync.1   > $(IMANDIR)/dmsync.1.gz
	$(GZIP) -9 < $(MANDIR)/dmuuid.1   > $(IMANDIR)/dmuuid.1.gz
	$(GZIP) -9 < $(MANDIR)/dmweb.1    > $(IMANDIR)/dmweb.1.gz

install_freebsd:
	$(MAKE) install PREFIX=/usr/local

install_linux:
	$(MAKE) install PREFIX=/usr

deinstall:
	@echo "--- Deleting DMPACK from $(PREFIX) ..."
	$(RM) -r $(IINCDIR)
	$(RM) -r $(ISHRDIR)
	$(RM) -f $(IETCDIR)/*.conf.sample
	$(RM) -f $(ILIBDIR)/libdmpack.a
	$(RM) -f $(ILIBDIR)/libdmpack.so
	$(RM) -f $(IBINDIR)/dmapi
	$(RM) -f $(IBINDIR)/dmbackup
	$(RM) -f $(IBINDIR)/dmbeat
	$(RM) -f $(IBINDIR)/dmdb
	$(RM) -f $(IBINDIR)/dmdbctl
	$(RM) -f $(IBINDIR)/dmexport
	$(RM) -f $(IBINDIR)/dmfeed
	$(RM) -f $(IBINDIR)/dmfs
	$(RM) -f $(IBINDIR)/dmgraph
	$(RM) -f $(IBINDIR)/dmimport
	$(RM) -f $(IBINDIR)/dminfo
	$(RM) -f $(IBINDIR)/dminit
	$(RM) -f $(IBINDIR)/dmlog
	$(RM) -f $(IBINDIR)/dmlogger
	$(RM) -f $(IBINDIR)/dmlua
	$(RM) -f $(IBINDIR)/dmpipe
	$(RM) -f $(IBINDIR)/dmrecv
	$(RM) -f $(IBINDIR)/dmreport
	$(RM) -f $(IBINDIR)/dmsend
	$(RM) -f $(IBINDIR)/dmserial
	$(RM) -f $(IBINDIR)/dmsync
	$(RM) -f $(IBINDIR)/dmuuid
	$(RM) -f $(IBINDIR)/dmweb
	$(RM) -f $(IMANDIR)/dmapi.1.gz
	$(RM) -f $(IMANDIR)/dmbackup.1.gz
	$(RM) -f $(IMANDIR)/dmbeat.1.gz
	$(RM) -f $(IMANDIR)/dmdb.1.gz
	$(RM) -f $(IMANDIR)/dmdbctl.1.gz
	$(RM) -f $(IMANDIR)/dmexport.1.gz
	$(RM) -f $(IMANDIR)/dmfeed.1.gz
	$(RM) -f $(IMANDIR)/dmfs.1.gz
	$(RM) -f $(IMANDIR)/dmgraph.1.gz
	$(RM) -f $(IMANDIR)/dmimport.1.gz
	$(RM) -f $(IMANDIR)/dminfo.1.gz
	$(RM) -f $(IMANDIR)/dminit.1.gz
	$(RM) -f $(IMANDIR)/dmlog.1.gz
	$(RM) -f $(IMANDIR)/dmlogger.1.gz
	$(RM) -f $(IMANDIR)/dmlua.1.gz
	$(RM) -f $(IMANDIR)/dmpipe.1.gz
	$(RM) -f $(IMANDIR)/dmrecv.1.gz
	$(RM) -f $(IMANDIR)/dmreport.1.gz
	$(RM) -f $(IMANDIR)/dmsend.1.gz
	$(RM) -f $(IMANDIR)/dmserial.1.gz
	$(RM) -f $(IMANDIR)/dmsync.1.gz
	$(RM) -f $(IMANDIR)/dmuuid.1.gz
	$(RM) -f $(IMANDIR)/dmweb.1.gz
	@echo
	@echo "You may need to manually remove $(IETCDIR) if it is no longer needed."
	@echo

# ******************************************************************************
#
# Remove binaries, libraries, modules and object files in, clear "dist" and
# "man" directories.
#
# ******************************************************************************

clean:
	@echo "--- Deleting libraries ..."
	if [ -e $(THIN) ];   then $(RM) $(THIN); fi
	if [ -e $(TARGET) ]; then $(RM) $(TARGET); fi
	if [ -e $(SHARED) ]; then $(RM) $(SHARED); fi
	@echo "--- Deleting build files ..."
	if [ `ls -1 *.mod 2>/dev/null | wc -l` -gt 0 ]; then $(RM) *.mod; fi
	if [ `ls -1 *.a   2>/dev/null | wc -l` -gt 0 ]; then $(RM) *.a; fi
	if [ `ls -1 *.so  2>/dev/null | wc -l` -gt 0 ]; then $(RM) *.so; fi
	if [ `ls -1 *.o   2>/dev/null | wc -l` -gt 0 ]; then $(RM) *.o; fi
	@echo "--- Deleting tests ..."
	if [ `ls -1 dmtest* 2>/dev/null | wc -l` -gt 0 ]; then $(RM) dmtest*; fi
	@echo "--- Deleting programs ..."
	if [ `ls -1 $(DISTDIR) 2>/dev/null | wc -l` -gt 0 ]; then $(RM) $(DISTDIR)/*; fi
	@echo "--- Cleaning guide ..."
	cd $(GUIDDIR) && $(MAKE) clean

# ******************************************************************************
#
# Additionally, clean dependencies, stale files, and FORD output.
#
# ******************************************************************************

purge: clean
	@echo "--- Cleaning fortran-curl ..."
	cd vendor/fortran-curl/ && make clean TARGET="../../$(LIBFCURL)"
	@echo "--- Cleaning fortran-lua54 ..."
	cd vendor/fortran-lua54/ && make clean TARGET="../../$(LIBFLUA54)"
	@echo "--- Cleaning fortran-pcre2 ..."
	cd vendor/fortran-pcre2/ && make clean TARGET="../../$(LIBFPCRE2)"
	@echo "--- Cleaning fortran-sqlite3 ..."
	cd vendor/fortran-sqlite3/ && make clean TARGET="../../$(LIBFSQLITE3)"
	@echo "--- Cleaning fortran-unix ..."
	cd vendor/fortran-unix/ && make clean TARGET="../../$(LIBFUNIX)"
	@echo "--- Cleaning fortran-zlib ..."
	cd vendor/fortran-zlib/ && make clean TARGET="../../$(LIBFZ)"
	@echo "--- Deleting module files ..."
	if [ -e $(INCDIR) ]; then $(RM) -r $(INCDIR); fi
	@echo "--- Deleting source code documentation ..."
	if [ -e $(DOCDIR) ]; then $(RM) -r $(DOCDIR); fi
	@echo "--- Deleting stale test files ..."
	if [ -e testobserv.hdf5 ]; then $(RM) testobserv.hdf5; fi
	if [ -e testbeat.sqlite ]; then $(RM) testbeat.sqlite; fi
	if [ -e testlog.sqlite ]; then $(RM) testlog.sqlite; fi
	if [ -e testobserv.sqlite ]; then $(RM) testobserv.sqlite; fi
	if [ -e testobserv_backup.sqlite ]; then $(RM) testobserv_backup.sqlite; fi
	if [ -e testobserv_vacuum.sqlite ]; then $(RM) testobserv_vacuum.sqlite; fi

# ******************************************************************************
#
# Print build options.
#
# ******************************************************************************

options:
	@echo "OS         = $(OS)"
	@echo "PREFIX     = $(PREFIX)"
	@echo "CC         = $(CC)"
	@echo "FC         = $(FC)"
	@echo "CONFDIR    = $(CONFDIR)"
	@echo "DISTDIR    = $(DISTDIR)"
	@echo "INCDIR     = $(INCDIR)"
	@echo "LIBDIR     = $(LIBDIR)"
	@echo "SHRDIR     = $(SHRDIR)"
	@echo "SRCDIR     = $(SRCDIR)"
	@echo "IBINDIR    = $(IBINDIR)"
	@echo "IETCDIR    = $(IETCDIR)"
	@echo "IINCDIR    = $(IINCDIR)"
	@echo "ILIBDIR    = $(ILIBDIR)"
	@echo "IMANDIR    = $(IMANDIR)"
	@echo "ISHRDIR    = $(ISHRDIR)"
	@echo "THIN       = $(THIN)"
	@echo "TARGET     = $(TARGET)"
	@echo "SHARED     = $(SHARED)"
	@echo "DEBUG      = $(DEBUG)"
	@echo "RELEASE    = $(RELEASE)"
	@echo "FFLAGS     = $(FFLAGS)"
	@echo "CFLAGS     = $(CFLAGS)"
	@echo "PPFLAGS    = $(PPFLAGS)"
	@echo "ARFLAGS    = $(ARFLAGS)"
	@echo "LDFLAGS    = $(LDFLAGS)"
	@echo "LDLIBS     = $(LDLIBS)"
	@echo "INCHDF5    = $(INCHDF5)"
	@echo "LIBCURL    = $(LIBCURL)"
	@echo "LIBFASTCGI = $(LIBFASTCGI)"
	@echo "LIBHDF5    = $(LIBHDF5)"
	@echo "LIBLAPACK  = $(LIBLAPACK)"
	@echo "LIBLUA54   = $(LIBLUA54)"
	@echo "LIBPCRE2   = $(LIBPCRE2)"
	@echo "LIBPTHREAD = $(LIBPTHREAD)"
	@echo "LIBRT      = $(LIBRT)"
	@echo "LIBSQLITE3 = $(LIBSQLITE3)"
	@echo "LIBZ       = $(LIBZ)"

# ******************************************************************************
#
# Print build targets.
#
# ******************************************************************************

help:
	@echo "The following build targets are available:"
	@echo
	@echo "    all             - Build DMPACK libraries, tests, and programs."
	@echo "    app             - Build DMPACK programs."
	@echo "    clean           - Clean DMPACK build environment."
	@echo "    deinstall       - Deinstall DMPACK from PREFIX."
	@echo "    doc             - Create source code documentation (requires FORD)."
	@echo "    freebsd         - Build FreeBSD release version."
	@echo "    freebsd_debug   - Build FreeBSD debug version."
	@echo "    freebsd_release - Build FreeBSD release version."
	@echo "    guide           - Convert User's Guide to HTML (requires AsciiDoctor)."
	@echo "    help            - Show this help."
	@echo "    html            - Convert man pages to HTML (requires mandoc)."
	@echo "    install         - Install DMPACK to PREFIX."
	@echo "    install_freebsd - Install DMPACK to /usr/local/."
	@echo "    install_linux   - Install DMPACK to /usr/."
	@echo "    linux           - Build Linux release version."
	@echo "    linux_debug     - Build Linux debug version."
	@echo "    linux_release   - Build Linux release version."
	@echo "    man             - Convert man pages (requires AsciiDoctor)."
	@echo "    options         - Show build flags and options."
	@echo "    pdf             - Convert man pages to PDF (requires ps2pdf)."
	@echo "    purge           - Purge DMPACK build environment (including dependencies)."
	@echo "    setup           - Create directories."
	@echo "    test            - Build test programs."
	@echo
