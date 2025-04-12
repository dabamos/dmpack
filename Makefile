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
# On Linux (x86-64), run:
#
#   $ make linux
#   $ make install PREFIX=/usr
#
# On Linux (AArch64), instead:
#
#   $ make linux_aarch64
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
#   freebsd_debug   - FreeBSD debug (x86-64, AArch64).
#   freebsd_release - FreeBSD release (x86-64, AArch64).
#   linux           - Alias for target `linux_release`.
#   linux_aarch64   - Linux release (aarch64)
#   linux_debug     - Linux debug (x86-64).
#   linux_release   - Linux release (x86-64).
#   test            - Build test programs.
#
# Targets for system-wide installation:
#
#   install   - Install to `/usr/local` (pass `PREFIX` to overwrite).
#   deinstall - Remove from `/usr/local` (pass `PREFIX` to overwrite).
#
# Targets related to the documentation:
#
#   man   - Make man pages (requires AsciiDoctor).
#   html  - Convert man pages to HTML (requires mandoc).
#   pdf   - Convert man pages to PDF (requires ps2pdf).
#   guide - Make User Guide (requires AsciiDoctor).
#   doc   - Make source code documentation (requires FORD).
#
# ******************************************************************************
#
# DMPACK build options:
#
#   OS       - The operating system, either `FreeBSD` or `linux` (for GCC only).
#   PREFIX   - Path prefix, `/usr/local` on FreeBSD, `/usr` on Linux.
#
#   FC       - Fortran 2018 compiler (`gfortran`, `ifx`).
#   CC       - ANSI C compiler (`gcc`, `icx`).
#
#   DEBUG    - Debug options.
#   RELEASE  - Release options.
#
#   FFLAGS   - Fortran compiler options.
#   CFLAGS   - C compiler options.
#   LIBFLAGS - Extra Fortran and C compiler options.
#   MODFLAGS - Module options.
#   PPFLAGS  - Pre-processor options (must be empty for Intel oneAPI).
#   EXFLAGS  - Extra linker flags for HDF5.
#   ARFLAGS  - Archiver options.
#   LDFLAGS  - Linker options.
#   LDLIBS   - Linker libraries.
#
#   THIN     - Thin DMPACK library (without interface bindings).
#   TARGET   - Path to the full DMPACK library (with interface bindings).
#   SHARED   - Path to the shared DMPACK library (with interface bindings).
#
#   CONFDIR  - Directory of configuration files.
#   DISTDIR  - Directory of distribution files (libraries and programs).
#   LIBDIR   - Directory of static libraries.
#   INCDIR   - Directory of Fortran module files.
#   SHRDIR   - Directory of shared files.
#   SRCDIR   - Directory of source files.
#
#   IBINDIR  - Installation directory of DMPACK binaries.
#   IETCDIR  - Installation directory of DMPACK configuration files.
#   IINCDIR  - Installation directory of DMPACK modules.
#   ILIBDIR  - Installation directory of DMPACK libraries.
#   ISHRDIR  - Installation directory of DMPACK shared files.
#
# ******************************************************************************

.POSIX:
.SUFFIXES:

# Platform.
OS      = FreeBSD
PREFIX  = /usr/local

# Fortran and C compiler.
FC      = gfortran
CC      = gcc

# Build utilities.
AR      = ar
CP      = /bin/cp
INSTALL = install
MAKE    = make
MAKELIB = makelib.sh
MKDIR   = /bin/mkdir
STRIP   = strip
RM      = /bin/rm
SH      = /bin/sh
FORD    = ford
GZIP    = gzip

# Workspace directories.
CONFDIR = config
DISTDIR = dist

INCDIR  = include
LIBDIR  = lib
SHRDIR  = share
SRCDIR  = src

ADOCDIR = adoc
DOCDIR  = doc
MANDIR  = man
GUIDDIR = guide

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
DEBUG   = -g -O0 -Wall -pedantic -fcheck=all -fmax-errors=1
RELEASE = -O2 -mtune=native

# Additional include search directories.
INCHDF5 = `pkg-config --cflags hdf5`

# Common build options.
FFLAGS   = $(RELEASE) $(INCHDF5) -ffree-line-length-0 -std=f2018
CFLAGS   = $(RELEASE) -I$(PREFIX)/include
LIBFLAGS = -fPIC
MODFLAGS = -I$(INCDIR) -J$(INCDIR)
PPFLAGS  = -cpp -D__$(OS)__
EXFLAGS  = -Wl,-z,execstack
ARFLAGS  = -rcs
LDFLAGS  = -L$(PREFIX)/lib -Wl,-z,execstack -Wl,-z,now
LDLIBS   =

# Shared libraries to link.
LIBCURL    = `pkg-config --libs-only-l libcurl`
LIBCRYPTO  = -lcrypto
LIBFASTCGI = -lfcgi
LIBHDF5    = `pkg-config --libs hdf5` -lhdf5_fortran
LIBLAPACK  = `pkg-config --libs-only-l lapack blas`
LIBLUA54   = `pkg-config --libs-only-l lua-5.4`
LIBMODBUS  = `pkg-config --libs-only-l libmodbus`
LIBPCRE2   = `pkg-config --libs-only-l libpcre2-8`
LIBPTHREAD = -lpthread
LIBRT      = -lrt
LIBSQLITE3 = `pkg-config --libs-only-l sqlite3`
LIBSTROPHE = `pkg-config --libs-only-l libstrophe expat openssl zlib`
LIBZLIB    = `pkg-config --libs-only-l zlib`
LIBZSTD    = `pkg-config --libs-only-l libzstd`
LIBZ       = $(LIBZLIB) $(LIBZSTD)

# All shared libraries (for `libdmpack.so`).
LIBSHARED = $(LIBCURL) $(LIBCRYPTO) $(LIBFASTCGI) $(LIBHDF5) $(LIBLAPACK) \
            $(LIBLUA54) $(LIBMODBUS) $(LIBPCRE2) $(LIBPTHREAD) $(LIBRT) \
            $(LIBSQLITE3) $(LIBSTROPHE) $(LIBZ) $(LIBZSTD)

# Fortran static libraries to link.
LIBFCURL    = $(LIBDIR)/libfortran-curl.a
LIBFLUA54   = $(LIBDIR)/libfortran-lua54.a
LIBFMODBUS  = $(LIBDIR)/libfortran-modbus.a
LIBFPCRE2   = $(LIBDIR)/libfortran-pcre2.a
LIBFSQLITE3 = $(LIBDIR)/libfortran-sqlite3.a
LIBFUNIX    = $(LIBDIR)/libfortran-unix.a
LIBFXMPP    = $(LIBDIR)/libfortran-xmpp.a
LIBFZLIB    = $(LIBDIR)/libfortran-zlib.a
LIBFZSTD    = $(LIBDIR)/libfortran-zstd.a
LIBF        = $(LIBFCURL) $(LIBFLUA54) $(LIBFMODBUS) $(LIBFPCRE2) $(LIBFSQLITE3) \
              $(LIBFUNIX) $(LIBFXMPP) $(LIBFZLIB) $(LIBFZSTD)

# Programs.
DMAPI    = $(DISTDIR)/dmapi
DMBACKUP = $(DISTDIR)/dmbackup
DMBEAT   = $(DISTDIR)/dmbeat
DMBOT    = $(DISTDIR)/dmbot
DMDB     = $(DISTDIR)/dmdb
DMDBCTL  = $(DISTDIR)/dmdbctl
DMDWD    = $(DISTDIR)/dmdwd
DMEXPORT = $(DISTDIR)/dmexport
DMFEED   = $(DISTDIR)/dmfeed
DMFS     = $(DISTDIR)/dmfs
DMGRC    = $(DISTDIR)/dmgrc
DMIMPORT = $(DISTDIR)/dmimport
DMINFO   = $(DISTDIR)/dminfo
DMINIT   = $(DISTDIR)/dminit
DMLOG    = $(DISTDIR)/dmlog
DMLOGGER = $(DISTDIR)/dmlogger
DMLUA    = $(DISTDIR)/dmlua
DMMB     = $(DISTDIR)/dmmb
DMMBCTL  = $(DISTDIR)/dmmbctl
DMPIPE   = $(DISTDIR)/dmpipe
DMPLOT   = $(DISTDIR)/dmplot
DMRECV   = $(DISTDIR)/dmrecv
DMREPORT = $(DISTDIR)/dmreport
DMSEND   = $(DISTDIR)/dmsend
DMSERIAL = $(DISTDIR)/dmserial
DMSYNC   = $(DISTDIR)/dmsync
DMSYSTEM = $(DISTDIR)/dmsystem
DMUUID   = $(DISTDIR)/dmuuid
DMVED    = $(DISTDIR)/dmved
DMWEB    = $(DISTDIR)/dmweb

# Library source files.
SRC = $(SRCDIR)/dm_ansi.f90 \
      $(SRCDIR)/dm_api_status.f90 \
      $(SRCDIR)/dm_arg.f90 \
      $(SRCDIR)/dm_ascii.f90 \
      $(SRCDIR)/dm_atom.f90 \
      $(SRCDIR)/dm_base64.f90 \
      $(SRCDIR)/dm_beat.f90 \
      $(SRCDIR)/dm_block.f90 \
      $(SRCDIR)/dm_c.f90 \
      $(SRCDIR)/dm_camera.f90 \
      $(SRCDIR)/dm_cgi.f90 \
      $(SRCDIR)/dm_cgi_router.f90 \
      $(SRCDIR)/dm_config.f90 \
      $(SRCDIR)/dm_const.f90 \
      $(SRCDIR)/dm_crypto.f90 \
      $(SRCDIR)/dm_csv.f90 \
      $(SRCDIR)/dm_db.f90 \
      $(SRCDIR)/dm_db_query.f90 \
      $(SRCDIR)/dm_db_stmt.f90 \
      $(SRCDIR)/dm_db_table.f90 \
      $(SRCDIR)/dm_dp.f90 \
      $(SRCDIR)/dm_dwd.f90 \
      $(SRCDIR)/dm_dwd_api.f90 \
      $(SRCDIR)/dm_env.f90 \
      $(SRCDIR)/dm_error.f90 \
      $(SRCDIR)/dm_fcgi.f90 \
      $(SRCDIR)/dm_fifo.f90 \
      $(SRCDIR)/dm_file.f90 \
      $(SRCDIR)/dm_format.f90 \
      $(SRCDIR)/dm_freebsd.f90 \
      $(SRCDIR)/dm_ftp.f90 \
      $(SRCDIR)/dm_geocom.f90 \
      $(SRCDIR)/dm_geocom_api.f90 \
      $(SRCDIR)/dm_geocom_error.f90 \
      $(SRCDIR)/dm_geocom_type.f90 \
      $(SRCDIR)/dm_geojson.f90 \
      $(SRCDIR)/dm_gm.f90 \
      $(SRCDIR)/dm_hash.f90 \
      $(SRCDIR)/dm_hash_table.f90 \
      $(SRCDIR)/dm_hdf5.f90 \
      $(SRCDIR)/dm_html.f90 \
      $(SRCDIR)/dm_http.f90 \
      $(SRCDIR)/dm_id.f90 \
      $(SRCDIR)/dm_im.f90 \
      $(SRCDIR)/dm_image.f90 \
      $(SRCDIR)/dm_job.f90 \
      $(SRCDIR)/dm_json.f90 \
      $(SRCDIR)/dm_jsonl.f90 \
      $(SRCDIR)/dm_kind.f90 \
      $(SRCDIR)/dm_la.f90 \
      $(SRCDIR)/dm_linux.f90 \
      $(SRCDIR)/dm_log.f90 \
      $(SRCDIR)/dm_logger.f90 \
      $(SRCDIR)/dm_lua.f90 \
      $(SRCDIR)/dm_lua_api.f90 \
      $(SRCDIR)/dm_lua_geocom.f90 \
      $(SRCDIR)/dm_lua_lib.f90 \
      $(SRCDIR)/dm_mail.f90 \
      $(SRCDIR)/dm_mime.f90 \
      $(SRCDIR)/dm_modbus.f90 \
      $(SRCDIR)/dm_modbus_register.f90 \
      $(SRCDIR)/dm_modbus_type.f90 \
      $(SRCDIR)/dm_mqtt.f90 \
      $(SRCDIR)/dm_mqueue.f90 \
      $(SRCDIR)/dm_mqueue_util.f90 \
      $(SRCDIR)/dm_mutex.f90 \
      $(SRCDIR)/dm_net.f90 \
      $(SRCDIR)/dm_nml.f90 \
      $(SRCDIR)/dm_node.f90 \
      $(SRCDIR)/dm_observ.f90 \
      $(SRCDIR)/dm_path.f90 \
      $(SRCDIR)/dm_person.f90 \
      $(SRCDIR)/dm_pipe.f90 \
      $(SRCDIR)/dm_platform.F90 \
      $(SRCDIR)/dm_plot.f90 \
      $(SRCDIR)/dm_regex.f90 \
      $(SRCDIR)/dm_report.f90 \
      $(SRCDIR)/dm_request.f90 \
      $(SRCDIR)/dm_response.f90 \
      $(SRCDIR)/dm_roff.f90 \
      $(SRCDIR)/dm_rpc.f90 \
      $(SRCDIR)/dm_rts.f90 \
      $(SRCDIR)/dm_sem.f90 \
      $(SRCDIR)/dm_sensor.f90 \
      $(SRCDIR)/dm_signal.f90 \
      $(SRCDIR)/dm_sql.f90 \
      $(SRCDIR)/dm_string.f90 \
      $(SRCDIR)/dm_sync.f90 \
      $(SRCDIR)/dm_system.f90 \
      $(SRCDIR)/dm_target.f90 \
      $(SRCDIR)/dm_test.f90 \
      $(SRCDIR)/dm_thread.f90 \
      $(SRCDIR)/dm_time.f90 \
      $(SRCDIR)/dm_timer.f90 \
      $(SRCDIR)/dm_transform.f90 \
      $(SRCDIR)/dm_tty.f90 \
      $(SRCDIR)/dm_type.f90 \
      $(SRCDIR)/dm_unit.f90 \
      $(SRCDIR)/dm_util.f90 \
      $(SRCDIR)/dm_uuid.f90 \
      $(SRCDIR)/dm_ve.f90 \
      $(SRCDIR)/dm_version.F90 \
      $(SRCDIR)/dm_z.f90 \
      $(SRCDIR)/dm_zlib.f90 \
      $(SRCDIR)/dm_zstd.f90 \
      $(SRCDIR)/dmpack.f90

# Library object files.
OBJ = dm_ansi.o \
      dm_api_status.o \
      dm_arg.o \
      dm_ascii.o \
      dm_atom.o \
      dm_base64.o \
      dm_beat.o \
      dm_block.o \
      dm_c.o \
      dm_camera.o \
      dm_cgi.o \
      dm_cgi_router.o \
      dm_config.o \
      dm_const.o \
      dm_crypto.o \
      dm_csv.o \
      dm_db.o \
      dm_db_query.o \
      dm_db_stmt.o \
      dm_db_table.o \
      dm_dp.o \
      dm_dwd.o \
      dm_dwd_api.o \
      dm_env.o \
      dm_error.o \
      dm_fcgi.o \
      dm_fifo.o \
      dm_file.o \
      dm_format.o \
      dm_freebsd.o \
      dm_ftp.o \
      dm_geocom.o \
      dm_geocom_api.o \
      dm_geocom_error.o \
      dm_geocom_type.o \
      dm_geojson.o \
      dm_gm.o \
      dm_hash.o \
      dm_hash_table.o \
      dm_hdf5.o \
      dm_html.o \
      dm_http.o \
      dm_id.o \
      dm_im.o \
      dm_image.o \
      dm_job.o \
      dm_json.o \
      dm_jsonl.o \
      dm_kind.o \
      dm_la.o \
      dm_linux.o \
      dm_log.o \
      dm_logger.o \
      dm_lua.o \
      dm_lua_api.o \
      dm_lua_geocom.o \
      dm_lua_lib.o \
      dm_mail.o \
      dm_mime.o \
      dm_modbus.o \
      dm_modbus_register.o \
      dm_modbus_type.o \
      dm_mqtt.o \
      dm_mqueue.o \
      dm_mqueue_util.o \
      dm_mutex.o \
      dm_net.o \
      dm_nml.o \
      dm_node.o \
      dm_observ.o \
      dm_path.o \
      dm_person.o \
      dm_pipe.o \
      dm_platform.o \
      dm_plot.o \
      dm_regex.o \
      dm_report.o \
      dm_request.o \
      dm_response.o \
      dm_roff.o \
      dm_rpc.o \
      dm_rts.o \
      dm_sem.o \
      dm_sensor.o \
      dm_signal.o \
      dm_sql.o \
      dm_string.o \
      dm_sync.o \
      dm_system.o \
      dm_target.o \
      dm_test.o \
      dm_thread.o \
      dm_time.o \
      dm_timer.o \
      dm_transform.o \
      dm_tty.o \
      dm_type.o \
      dm_unit.o \
      dm_util.o \
      dm_uuid.o \
      dm_ve.o \
      dm_version.o \
      dm_z.o \
      dm_zlib.o \
      dm_zstd.o \
      dmpack.o

# ******************************************************************************
#
# Build targets.
#
# ******************************************************************************

# Named build targets.
.PHONY: all app build clean deinstall doc freebsd freebsd_debug freebsd_release \
        guide help html install linux linux_aarch64 linux_debug linux_release \
        man options pdf purge setup test

# Library target.
all:
	@echo "Select one of the following build targets:"
	@echo
	@echo "    freebsd         - FreeBSD release build (x86-64, aarch64)."
	@echo "    freebsd_debug   - FreeBSD debug build (x86-64, aarch64)."
	@echo "    linux           - Linux release build (x86-64)."
	@echo "    linux_aarch64   - Linux release build (aarch64)."
	@echo "    linux_debug     - Linux debug build (x86-64)."
	@echo
	@echo "For an overview of all available targets, select target <help>."

build: $(TARGET) $(SHARED) test app

# Apps target.
app: $(DMAPI) \
     $(DMBACKUP) \
     $(DMBEAT) \
     $(DMBOT) \
     $(DMDB) \
     $(DMDBCTL) \
     $(DMDWD) \
     $(DMEXPORT) \
     $(DMFEED) \
     $(DMFS) \
     $(DMGRC) \
     $(DMINFO) \
     $(DMIMPORT) \
     $(DMINIT) \
     $(DMLOG) \
     $(DMLOGGER) \
     $(DMLUA) \
     $(DMMB) \
     $(DMMBCTL) \
     $(DMPIPE) \
     $(DMPLOT) \
     $(DMRECV) \
     $(DMREPORT) \
     $(DMSEND) \
     $(DMSERIAL) \
     $(DMSYNC) \
     $(DMSYSTEM) \
     $(DMUUID) \
     $(DMVED) \
     $(DMWEB)

# Tests target.
test: dmtestapi \
      dmtestascii \
      dmtestatom \
      dmtestbase64 \
      dmtestc \
      dmtestcgi \
      dmtestconfig \
      dmtestcrypto \
      dmtestcsv \
      dmtestdb \
      dmtestdp \
      dmtestdwd \
      dmtestfile \
      dmtestfreebsd \
      dmtestftp \
      dmtestgm \
      dmtesthash \
      dmtesthdf5 \
      dmtesthtml \
      dmtestid \
      dmtestlinux \
      dmtestlog \
      dmtestlogger \
      dmtestlua \
      dmtestjob \
      dmtestjson \
      dmtestmail \
      dmtestmodbus \
      dmtestmqtt \
      dmtestmqueue \
      dmtestnet \
      dmtestnml \
      dmtestobserv \
      dmtestpath \
      dmtestpipe \
      dmtestplot \
      dmtestregex \
      dmtestroff \
      dmtestrpc \
      dmtestrts \
      dmteststring \
      dmtestsystem \
      dmtestthread \
      dmtesttime \
      dmtesttransform \
      dmtesttty \
      dmtestunit \
      dmtestutil \
      dmtestuuid \
      dmtestve \
      dmtestversion \
      dmtestz \
      dmtestzlib \
      dmtestzstd

# ******************************************************************************
#
# Setup.
#
# ******************************************************************************

setup:
	$(MKDIR) -p $(DISTDIR)
	$(MKDIR) -p $(INCDIR)
	$(MKDIR) -p $(LIBDIR)

# ******************************************************************************
#
# FreeBSD targets.
#
# ******************************************************************************

# AArch64, x86-64
freebsd_debug:
	$(MAKE) build OS=FreeBSD PREFIX=/usr/local RELEASE="$(DEBUG)"

# AArch64, x86-64
freebsd_release:
	$(MAKE) build OS=FreeBSD PREFIX=/usr/local RELEASE="$(RELEASE)"
	$(STRIP) -s $(DISTDIR)/dm*

# AArch64, x86-64
freebsd:
	$(MAKE) freebsd_release

# ******************************************************************************
#
# Linux targets.
#
# ******************************************************************************

# AArch64
linux_aarch64:
	$(MAKE) build OS=linux PREFIX=/usr PPFLAGS="-cpp -D__linux__ -D__aarch64__" RELEASE="$(RELEASE)"
	$(STRIP) -s $(DISTDIR)/dm*

# x86-64
linux_debug:
	$(MAKE) build OS=linux PREFIX=/usr RELEASE="$(DEBUG)"

# x86-64
linux_release:
	$(MAKE) build OS=linux PREFIX=/usr RELEASE="$(RELEASE)"
	$(STRIP) -s $(DISTDIR)/dm*

# x86-64
linux:
	$(MAKE) linux_release

# ******************************************************************************
#
# 3rd party interface libraries.
#
# ******************************************************************************

$(LIBFCURL): setup
	cd vendor/fortran-curl/ && $(MAKE) CC=$(CC) FC=$(FC) CFLAGS="$(CFLAGS) $(LIBFLAGS)" FFLAGS="$(FFLAGS) $(LIBFLAGS)" PREFIX="$(PREFIX)" TARGET="../../$(LIBFCURL)"
	$(CP) vendor/fortran-curl/*.mod $(INCDIR)/

$(LIBFLUA54): setup
	cd vendor/fortran-lua54/ && $(MAKE) CC=$(CC) FC=$(FC) CFLAGS="$(CFLAGS) $(LIBFLAGS)" FFLAGS="$(FFLAGS) $(LIBFLAGS)" PREFIX="$(PREFIX)" TARGET="../../$(LIBFLUA54)"
	$(CP) vendor/fortran-lua54/*.mod $(INCDIR)/

$(LIBFMODBUS): setup
	cd vendor/fortran-modbus/ && $(MAKE) CC=$(CC) FC=$(FC) CFLAGS="$(CFLAGS) $(LIBFLAGS)" FFLAGS="$(FFLAGS) $(LIBFLAGS)" PREFIX="$(PREFIX)" TARGET="../../$(LIBFMODBUS)"
	$(CP) vendor/fortran-modbus/*.mod $(INCDIR)/

$(LIBFPCRE2): setup
	cd vendor/fortran-pcre2/ && $(MAKE) CC=$(CC) FC=$(FC) CFLAGS="$(CFLAGS) $(LIBFLAGS)" FFLAGS="$(FFLAGS) $(LIBFLAGS)" PREFIX="$(PREFIX)" TARGET="../../$(LIBFPCRE2)"
	$(CP) vendor/fortran-pcre2/*.mod $(INCDIR)/

$(LIBFSQLITE3): setup
	cd vendor/fortran-sqlite3/ && $(MAKE) CC=$(CC) FC=$(FC) CFLAGS="$(CFLAGS) $(LIBFLAGS)" FFLAGS="$(FFLAGS) $(LIBFLAGS)" PREFIX="$(PREFIX)" TARGET="../../$(LIBFSQLITE3)"
	$(CP) vendor/fortran-sqlite3/*.mod $(INCDIR)/

$(LIBFUNIX): setup
	@echo "---"
	@echo "--- Building for $(OS) ..."
	@echo "---"
	cd vendor/fortran-unix/ && $(MAKE) CC=$(CC) FC=$(FC) CFLAGS="$(CFLAGS) $(LIBFLAGS)" FFLAGS="$(FFLAGS) $(LIBFLAGS)" PREFIX="$(PREFIX)" PPFLAGS="$(PPFLAGS)" TARGET="../../$(LIBFUNIX)"
	$(CP) vendor/fortran-unix/*.mod $(INCDIR)/

$(LIBFXMPP): setup
	cd vendor/fortran-xmpp/ && $(MAKE) CC=$(CC) FC=$(FC) CFLAGS="$(CFLAGS) $(LIBFLAGS)" FFLAGS="$(FFLAGS) $(LIBFLAGS)" PREFIX="$(PREFIX)" TARGET="../../$(LIBFXMPP)"
	$(CP) vendor/fortran-xmpp/*.mod $(INCDIR)/

$(LIBFZLIB): setup
	cd vendor/fortran-zlib/ && $(MAKE) CC=$(CC) FC=$(FC) CFLAGS="$(CFLAGS) $(LIBFLAGS)" FFLAGS="$(FFLAGS) $(LIBFLAGS)" PREFIX="$(PREFIX)" TARGET="../../$(LIBFZLIB)"
	$(CP) vendor/fortran-zlib/*.mod $(INCDIR)/

$(LIBFZSTD): setup
	cd vendor/fortran-zstd/ && $(MAKE) FC=$(FC) FFLAGS="$(FFLAGS) $(LIBFLAGS)" PREFIX="$(PREFIX)" TARGET="../../$(LIBFZSTD)"
	$(CP) vendor/fortran-zstd/*.mod $(INCDIR)/

# ******************************************************************************
#
# DMPACK static libraries.
#
# ******************************************************************************

$(OBJ): $(SRC)
	$(FC) $(FFLAGS) $(PPFLAGS) $(LIBFLAGS) $(MODFLAGS) -c src/dm_platform.F90
	$(FC) $(FFLAGS) $(LIBFLAGS) $(MODFLAGS) -c src/dm_version.F90
	$(FC) $(FFLAGS) $(LIBFLAGS) $(MODFLAGS) -c src/dm_kind.f90
	$(FC) $(FFLAGS) $(LIBFLAGS) $(MODFLAGS) -c src/dm_c.f90
	$(FC) $(FFLAGS) $(LIBFLAGS) $(MODFLAGS) -c src/dm_ascii.f90
	$(FC) $(FFLAGS) $(LIBFLAGS) $(MODFLAGS) -c src/dm_ansi.f90
	$(FC) $(FFLAGS) $(LIBFLAGS) $(MODFLAGS) -c src/dm_const.f90
	$(FC) $(FFLAGS) $(LIBFLAGS) $(MODFLAGS) -c src/dm_error.f90
	$(FC) $(FFLAGS) $(LIBFLAGS) $(MODFLAGS) -c src/dm_string.f90
	$(FC) $(FFLAGS) $(LIBFLAGS) $(MODFLAGS) -c src/dm_format.f90
	$(FC) $(FFLAGS) $(LIBFLAGS) $(MODFLAGS) -c src/dm_util.f90
	$(FC) $(FFLAGS) $(LIBFLAGS) $(MODFLAGS) -c src/dm_type.f90
	$(FC) $(FFLAGS) $(LIBFLAGS) $(MODFLAGS) -c src/dm_env.f90
	$(FC) $(FFLAGS) $(LIBFLAGS) $(MODFLAGS) -c src/dm_time.f90
	$(FC) $(FFLAGS) $(LIBFLAGS) $(MODFLAGS) -c src/dm_timer.f90
	$(FC) $(FFLAGS) $(LIBFLAGS) $(MODFLAGS) -c src/dm_base64.f90
	$(FC) $(FFLAGS) $(LIBFLAGS) $(MODFLAGS) -c src/dm_path.f90
	$(FC) $(FFLAGS) $(LIBFLAGS) $(MODFLAGS) -c src/dm_file.f90
	$(FC) $(FFLAGS) $(LIBFLAGS) $(MODFLAGS) -c src/dm_hash.f90
	$(FC) $(FFLAGS) $(LIBFLAGS) $(MODFLAGS) -c src/dm_hash_table.f90
	$(FC) $(FFLAGS) $(LIBFLAGS) $(MODFLAGS) -c src/dm_unit.f90
	$(FC) $(FFLAGS) $(LIBFLAGS) $(MODFLAGS) -c src/dm_id.f90
	$(FC) $(FFLAGS) $(LIBFLAGS) $(MODFLAGS) -c src/dm_net.f90
	$(FC) $(FFLAGS) $(LIBFLAGS) $(MODFLAGS) -c src/dm_uuid.f90
	$(FC) $(FFLAGS) $(LIBFLAGS) $(MODFLAGS) -c src/dm_signal.f90
	$(FC) $(FFLAGS) $(LIBFLAGS) $(MODFLAGS) -c src/dm_pipe.f90
	$(FC) $(FFLAGS) $(LIBFLAGS) $(MODFLAGS) -c src/dm_freebsd.f90
	$(FC) $(FFLAGS) $(LIBFLAGS) $(MODFLAGS) -c src/dm_linux.f90
	$(FC) $(FFLAGS) $(LIBFLAGS) $(MODFLAGS) -c src/dm_system.f90
	$(FC) $(FFLAGS) $(LIBFLAGS) $(MODFLAGS) -c src/dm_thread.f90
	$(FC) $(FFLAGS) $(LIBFLAGS) $(MODFLAGS) -c src/dm_sem.f90
	$(FC) $(FFLAGS) $(LIBFLAGS) $(MODFLAGS) -c src/dm_mutex.f90
	$(FC) $(FFLAGS) $(LIBFLAGS) $(MODFLAGS) -c src/dm_dp.f90
	$(FC) $(FFLAGS) $(LIBFLAGS) $(MODFLAGS) -c src/dm_fifo.f90
	$(FC) $(FFLAGS) $(LIBFLAGS) $(MODFLAGS) -c src/dm_node.f90
	$(FC) $(FFLAGS) $(LIBFLAGS) $(MODFLAGS) -c src/dm_sensor.f90
	$(FC) $(FFLAGS) $(LIBFLAGS) $(MODFLAGS) -c src/dm_target.f90
	$(FC) $(FFLAGS) $(LIBFLAGS) $(MODFLAGS) -c src/dm_response.f90
	$(FC) $(FFLAGS) $(LIBFLAGS) $(MODFLAGS) -c src/dm_request.f90
	$(FC) $(FFLAGS) $(LIBFLAGS) $(MODFLAGS) -c src/dm_observ.f90
	$(FC) $(FFLAGS) $(LIBFLAGS) $(MODFLAGS) -c src/dm_log.f90
	$(FC) $(FFLAGS) $(LIBFLAGS) $(MODFLAGS) -c src/dm_arg.f90
	$(FC) $(FFLAGS) $(LIBFLAGS) $(MODFLAGS) -c src/dm_job.f90
	$(FC) $(FFLAGS) $(LIBFLAGS) $(MODFLAGS) -c src/dm_tty.f90
	$(FC) $(FFLAGS) $(LIBFLAGS) $(MODFLAGS) -c src/dm_plot.f90
	$(FC) $(FFLAGS) $(LIBFLAGS) $(MODFLAGS) -c src/dm_report.f90
	$(FC) $(FFLAGS) $(LIBFLAGS) $(MODFLAGS) -c src/dm_regex.f90
	$(FC) $(FFLAGS) $(LIBFLAGS) $(MODFLAGS) -c src/dm_sync.f90
	$(FC) $(FFLAGS) $(LIBFLAGS) $(MODFLAGS) -c src/dm_beat.f90
	$(FC) $(FFLAGS) $(LIBFLAGS) $(MODFLAGS) -c src/dm_mqueue.f90
	$(FC) $(FFLAGS) $(LIBFLAGS) $(MODFLAGS) -c src/dm_logger.f90
	$(FC) $(FFLAGS) $(LIBFLAGS) $(MODFLAGS) -c src/dm_mqueue_util.f90
	$(FC) $(FFLAGS) $(LIBFLAGS) $(MODFLAGS) -c src/dm_test.f90
	$(FC) $(FFLAGS) $(LIBFLAGS) $(MODFLAGS) -c src/dm_nml.f90
	$(FC) $(FFLAGS) $(LIBFLAGS) $(MODFLAGS) $(EXFLAGS) -c src/dm_hdf5.f90
	$(FC) $(FFLAGS) $(LIBFLAGS) $(MODFLAGS) -c src/dm_sql.f90
	$(FC) $(FFLAGS) $(LIBFLAGS) $(MODFLAGS) -c src/dm_db_stmt.f90
	$(FC) $(FFLAGS) $(LIBFLAGS) $(MODFLAGS) -c src/dm_db_query.f90
	$(FC) $(FFLAGS) $(LIBFLAGS) $(MODFLAGS) -c src/dm_db.f90
	$(FC) $(FFLAGS) $(LIBFLAGS) $(MODFLAGS) -c src/dm_db_table.f90
	$(FC) $(FFLAGS) $(LIBFLAGS) $(MODFLAGS) -c src/dm_zlib.f90
	$(FC) $(FFLAGS) $(LIBFLAGS) $(MODFLAGS) -c src/dm_zstd.f90
	$(FC) $(FFLAGS) $(LIBFLAGS) $(MODFLAGS) -c src/dm_z.f90
	$(FC) $(FFLAGS) $(LIBFLAGS) $(MODFLAGS) -c src/dm_person.f90
	$(FC) $(FFLAGS) $(LIBFLAGS) $(MODFLAGS) -c src/dm_mail.f90
	$(FC) $(FFLAGS) $(LIBFLAGS) $(MODFLAGS) -c src/dm_http.f90
	$(FC) $(FFLAGS) $(LIBFLAGS) $(MODFLAGS) -c src/dm_mime.f90
	$(FC) $(FFLAGS) $(LIBFLAGS) $(MODFLAGS) -c src/dm_api_status.f90
	$(FC) $(FFLAGS) $(LIBFLAGS) $(MODFLAGS) -c src/dm_rpc.f90
	$(FC) $(FFLAGS) $(LIBFLAGS) $(MODFLAGS) -c src/dm_mqtt.f90
	$(FC) $(FFLAGS) $(LIBFLAGS) $(MODFLAGS) -c src/dm_cgi.f90
	$(FC) $(FFLAGS) $(LIBFLAGS) $(MODFLAGS) -c src/dm_fcgi.f90
	$(FC) $(FFLAGS) $(LIBFLAGS) $(MODFLAGS) -c src/dm_block.f90
	$(FC) $(FFLAGS) $(LIBFLAGS) $(MODFLAGS) -c src/dm_csv.f90
	$(FC) $(FFLAGS) $(LIBFLAGS) $(MODFLAGS) -c src/dm_json.f90
	$(FC) $(FFLAGS) $(LIBFLAGS) $(MODFLAGS) -c src/dm_jsonl.f90
	$(FC) $(FFLAGS) $(LIBFLAGS) $(MODFLAGS) -c src/dm_geojson.f90
	$(FC) $(FFLAGS) $(LIBFLAGS) $(MODFLAGS) -c src/dm_html.f90
	$(FC) $(FFLAGS) $(LIBFLAGS) $(MODFLAGS) -c src/dm_atom.f90
	$(FC) $(FFLAGS) $(LIBFLAGS) $(MODFLAGS) -c src/dm_cgi_router.f90
	$(FC) $(FFLAGS) $(LIBFLAGS) $(MODFLAGS) -c src/dm_la.f90
	$(FC) $(FFLAGS) $(LIBFLAGS) $(MODFLAGS) -c src/dm_transform.f90
	$(FC) $(FFLAGS) $(LIBFLAGS) $(MODFLAGS) -c src/dm_geocom_error.f90
	$(FC) $(FFLAGS) $(LIBFLAGS) $(MODFLAGS) -c src/dm_geocom_type.f90
	$(FC) $(FFLAGS) $(LIBFLAGS) $(MODFLAGS) -c src/dm_geocom_api.f90
	$(FC) $(FFLAGS) $(LIBFLAGS) $(MODFLAGS) -c src/dm_geocom.f90
	$(FC) $(FFLAGS) $(LIBFLAGS) $(MODFLAGS) -c src/dm_modbus_type.f90
	$(FC) $(FFLAGS) $(LIBFLAGS) $(MODFLAGS) -c src/dm_modbus_register.f90
	$(FC) $(FFLAGS) $(LIBFLAGS) $(MODFLAGS) -c src/dm_modbus.f90
	$(FC) $(FFLAGS) $(LIBFLAGS) $(MODFLAGS) -c src/dm_lua.f90
	$(FC) $(FFLAGS) $(LIBFLAGS) $(MODFLAGS) -c src/dm_lua_api.f90
	$(FC) $(FFLAGS) $(LIBFLAGS) $(MODFLAGS) -c src/dm_lua_geocom.f90
	$(FC) $(FFLAGS) $(LIBFLAGS) $(MODFLAGS) -c src/dm_lua_lib.f90
	$(FC) $(FFLAGS) $(LIBFLAGS) $(MODFLAGS) -c src/dm_config.f90
	$(FC) $(FFLAGS) $(LIBFLAGS) $(MODFLAGS) -c src/dm_rts.f90
	$(FC) $(FFLAGS) $(LIBFLAGS) $(MODFLAGS) -c src/dm_crypto.f90
	$(FC) $(FFLAGS) $(LIBFLAGS) $(MODFLAGS) -c src/dm_image.f90
	$(FC) $(FFLAGS) $(LIBFLAGS) $(MODFLAGS) -c src/dm_gm.f90
	$(FC) $(FFLAGS) $(LIBFLAGS) $(MODFLAGS) -c src/dm_camera.f90
	$(FC) $(FFLAGS) $(LIBFLAGS) $(MODFLAGS) -c src/dm_im.f90
	$(FC) $(FFLAGS) $(LIBFLAGS) $(MODFLAGS) -c src/dm_ve.f90
	$(FC) $(FFLAGS) $(LIBFLAGS) $(MODFLAGS) -c src/dm_dwd.f90
	$(FC) $(FFLAGS) $(LIBFLAGS) $(MODFLAGS) -c src/dm_dwd_api.f90
	$(FC) $(FFLAGS) $(LIBFLAGS) $(MODFLAGS) -c src/dm_ftp.f90
	$(FC) $(FFLAGS) $(LIBFLAGS) $(MODFLAGS) -c src/dm_roff.f90
	$(FC) $(FFLAGS) $(LIBFLAGS) $(MODFLAGS) -c src/dmpack.f90

# Static library `libdmpack.a`.
$(TARGET): $(LIBF) $(OBJ)
	$(AR) $(ARFLAGS) $(THIN) $(OBJ)
	$(SH) $(MAKELIB) $(TARGET) $(LIBDIR)

# ******************************************************************************
#
# DMPACK shared library (includes Lua API).
#
# ******************************************************************************

# Shared library `libdmpack.so`.
$(SHARED): $(TARGET)
	$(FC) $(FFLAGS) $(LDFLAGS) -shared -o $(SHARED) -Wl,--whole-archive $(TARGET) -Wl,--no-whole-archive $(LIBSHARED) $(LDLIBS)

# ******************************************************************************
#
# DMPACK test programs.
#
# ******************************************************************************

dmtestapi: test/dmtestapi.f90 $(TARGET)
	$(FC) $(FFLAGS) $(MODFLAGS) $(LDFLAGS) -o dmtestapi test/dmtestapi.f90 $(TARGET) $(LDLIBS)

dmtestascii: test/dmtestascii.f90 $(TARGET)
	$(FC) $(FFLAGS) $(MODFLAGS) $(LDFLAGS) -o dmtestascii test/dmtestascii.f90 $(TARGET) $(LDLIBS)

dmtestatom: test/dmtestatom.f90 $(TARGET)
	$(FC) $(FFLAGS) $(MODFLAGS) $(LDFLAGS) -o dmtestatom test/dmtestatom.f90 $(TARGET) $(LDLIBS)

dmtestbase64: test/dmtestbase64.f90 $(TARGET)
	$(FC) $(FFLAGS) $(MODFLAGS) $(LDFLAGS) -o dmtestbase64 test/dmtestbase64.f90 $(TARGET) $(LDLIBS)

dmtestc: test/dmtestc.f90 $(TARGET)
	$(FC) $(FFLAGS) $(MODFLAGS) $(LDFLAGS) -o dmtestc test/dmtestc.f90 $(TARGET) $(LDLIBS)

dmtestcgi: test/dmtestcgi.f90 $(TARGET)
	$(FC) $(FFLAGS) $(MODFLAGS) $(LDFLAGS) -o dmtestcgi test/dmtestcgi.f90 $(TARGET) $(LDLIBS)

dmtestconfig: test/dmtestconfig.f90 $(TARGET)
	$(FC) $(FFLAGS) $(MODFLAGS) $(LDFLAGS) -o dmtestconfig test/dmtestconfig.f90 $(TARGET) $(LIBLUA54) $(LDLIBS)

dmtestcrypto: test/dmtestcrypto.f90 $(TARGET)
	$(FC) $(FFLAGS) $(MODFLAGS) $(LDFLAGS) -o dmtestcrypto test/dmtestcrypto.f90 $(TARGET) $(LIBCRYPTO) $(LDLIBS)

dmtestcsv: test/dmtestcsv.f90 $(TARGET)
	$(FC) $(FFLAGS) $(MODFLAGS) $(LDFLAGS) -o dmtestcsv test/dmtestcsv.f90 $(TARGET) $(LDLIBS)

dmtestdb: test/dmtestdb.f90 $(TARGET)
	$(FC) $(FFLAGS) $(MODFLAGS) $(LDFLAGS) -o dmtestdb test/dmtestdb.f90 $(TARGET) $(LIBSQLITE3) $(LDLIBS)

dmtestdp: test/dmtestdp.f90 $(TARGET)
	$(FC) $(FFLAGS) $(MODFLAGS) $(LDFLAGS) -o dmtestdp test/dmtestdp.f90 $(TARGET) $(LDLIBS)

dmtestdwd: test/dmtestdwd.f90 $(TARGET)
	$(FC) $(FFLAGS) $(MODFLAGS) $(LDFLAGS) -o dmtestdwd test/dmtestdwd.f90 $(TARGET) $(LDLIBS) $(LIBCURL) $(LIBZ) $(LDLIBS)

dmtestfile: test/dmtestfile.f90 $(TARGET)
	$(FC) $(FFLAGS) $(MODFLAGS) $(LDFLAGS) -o dmtestfile test/dmtestfile.f90 $(TARGET) $(LDLIBS)

dmtestfreebsd: test/dmtestfreebsd.f90 $(TARGET)
	$(FC) $(FFLAGS) $(MODFLAGS) $(LDFLAGS) -o dmtestfreebsd test/dmtestfreebsd.f90 $(TARGET) $(LDLIBS)

dmtestftp: test/dmtestftp.f90 $(TARGET)
	$(FC) $(FFLAGS) $(MODFLAGS) $(LDFLAGS) -o dmtestftp test/dmtestftp.f90 $(TARGET) $(LDLIBS) $(LIBCURL)

dmtestgm: test/dmtestgm.f90 $(TARGET)
	$(FC) $(FFLAGS) $(MODFLAGS) $(LDFLAGS) -o dmtestgm test/dmtestgm.f90 $(TARGET) $(LDLIBS) $(LIBCRYPTO)

dmtesthash: test/dmtesthash.f90 $(TARGET)
	$(FC) $(FFLAGS) $(MODFLAGS) $(LDFLAGS) -o dmtesthash test/dmtesthash.f90 $(TARGET) $(LDLIBS)

dmtesthdf5: test/dmtesthdf5.f90 $(TARGET)
	$(FC) $(FFLAGS) $(MODFLAGS) -o dmtesthdf5 test/dmtesthdf5.f90 $(TARGET) $(LIBHDF5) $(LDLIBS)

dmtesthtml: test/dmtesthtml.f90 $(TARGET)
	$(FC) $(FFLAGS) $(MODFLAGS) $(LDFLAGS) -o dmtesthtml test/dmtesthtml.f90 $(TARGET) $(LDLIBS)

dmtestid: test/dmtestid.f90 $(TARGET)
	$(FC) $(FFLAGS) $(MODFLAGS) $(LDFLAGS) -o dmtestid test/dmtestid.f90 $(TARGET) $(LDLIBS)

dmtestlinux: test/dmtestlinux.f90 $(TARGET)
	$(FC) $(FFLAGS) $(MODFLAGS) $(LDFLAGS) -o dmtestlinux test/dmtestlinux.f90 $(TARGET) $(LDLIBS)

dmtestlog: test/dmtestlog.f90 $(TARGET)
	$(FC) $(FFLAGS) $(MODFLAGS) $(LDFLAGS) -o dmtestlog test/dmtestlog.f90 $(TARGET) $(LDLIBS)

dmtestlogger: test/dmtestlogger.f90 $(TARGET)
	$(FC) $(FFLAGS) $(MODFLAGS) $(LDFLAGS) -o dmtestlogger test/dmtestlogger.f90 $(TARGET) $(LIBRT) $(LDLIBS)

dmtestlua: test/dmtestlua.f90 $(TARGET)
	$(FC) $(FFLAGS) $(MODFLAGS) $(LDFLAGS) -o dmtestlua test/dmtestlua.f90 $(TARGET) $(LIBLUA54) $(LDLIBS)

dmtestjob: test/dmtestjob.f90 $(TARGET)
	$(FC) $(FFLAGS) $(MODFLAGS) $(LDFLAGS) -o dmtestjob test/dmtestjob.f90 $(TARGET) $(LDLIBS)

dmtestjson: test/dmtestjson.f90 $(TARGET)
	$(FC) $(FFLAGS) $(MODFLAGS) $(LDFLAGS) -o dmtestjson test/dmtestjson.f90 $(TARGET) $(LDLIBS)

dmtestmail: test/dmtestmail.f90 $(TARGET)
	$(FC) $(FFLAGS) $(MODFLAGS) $(LDFLAGS) -o dmtestmail test/dmtestmail.f90 $(TARGET) $(LIBCURL) $(LDLIBS)

dmtestmodbus: test/dmtestmodbus.f90 $(TARGET)
	$(FC) $(FFLAGS) $(MODFLAGS) $(LDFLAGS) -o dmtestmodbus test/dmtestmodbus.f90 $(TARGET) $(LIBMODBUS) $(LDLIBS)

dmtestmqtt: test/dmtestmqtt.f90 $(TARGET)
	$(FC) $(FFLAGS) $(MODFLAGS) $(LDFLAGS) -o dmtestmqtt test/dmtestmqtt.f90 $(TARGET) $(LIBCURL) $(LDLIBS)

dmtestmqueue: test/dmtestmqueue.f90 $(TARGET)
	$(FC) $(FFLAGS) $(MODFLAGS) $(LDFLAGS) -o dmtestmqueue test/dmtestmqueue.f90 $(TARGET) $(LIBRT) $(LDLIBS)

dmtestnet: test/dmtestnet.f90 $(TARGET)
	$(FC) $(FFLAGS) $(MODFLAGS) $(LDFLAGS) -o dmtestnet test/dmtestnet.f90 $(TARGET) $(LDLIBS)

dmtestnml: test/dmtestnml.f90 $(TARGET)
	$(FC) $(FFLAGS) $(MODFLAGS) $(LDFLAGS) -o dmtestnml test/dmtestnml.f90 $(TARGET) $(LDLIBS)

dmtestobserv: test/dmtestobserv.f90 $(TARGET)
	$(FC) $(FFLAGS) $(MODFLAGS) $(LDFLAGS) -o dmtestobserv test/dmtestobserv.f90 $(TARGET) $(LDLIBS)

dmtestpath: test/dmtestpath.f90 $(TARGET)
	$(FC) $(FFLAGS) $(MODFLAGS) $(LDFLAGS) -o dmtestpath test/dmtestpath.f90 $(TARGET) $(LDLIBS)

dmtestpipe: test/dmtestpipe.f90 $(TARGET)
	$(FC) $(FFLAGS) $(MODFLAGS) $(LDFLAGS) -o dmtestpipe test/dmtestpipe.f90 $(TARGET) $(LDLIBS)

dmtestplot: test/dmtestplot.f90 $(TARGET)
	$(FC) $(FFLAGS) $(MODFLAGS) $(LDFLAGS) -o dmtestplot test/dmtestplot.f90 $(TARGET) $(LDLIBS)

dmtestregex: test/dmtestregex.f90 $(TARGET)
	$(FC) $(FFLAGS) $(MODFLAGS) $(LDFLAGS) -o dmtestregex test/dmtestregex.f90 $(TARGET) $(LIBPCRE2) $(LDLIBS)

dmtestroff: test/dmtestroff.f90 $(TARGET)
	$(FC) $(FFLAGS) $(MODFLAGS) $(LDFLAGS) -o dmtestroff test/dmtestroff.f90 $(TARGET) $(LDLIBS)

dmtestrpc: test/dmtestrpc.f90 $(TARGET)
	$(FC) $(FFLAGS) $(MODFLAGS) $(LDFLAGS) -o dmtestrpc test/dmtestrpc.f90 $(TARGET) $(LIBCURL) $(LIBZ) $(LDLIBS)

dmtestrts: test/dmtestrts.f90 $(TARGET)
	$(FC) $(FFLAGS) $(MODFLAGS) $(LDFLAGS) -o dmtestrts test/dmtestrts.f90 $(TARGET) $(LDLIBS)

dmteststring: test/dmteststring.f90 $(TARGET)
	$(FC) $(FFLAGS) $(MODFLAGS) $(LDFLAGS) -o dmteststring test/dmteststring.f90 $(TARGET) $(LDLIBS)

dmtestsystem: test/dmtestsystem.f90 $(TARGET)
	$(FC) $(FFLAGS) $(MODFLAGS) $(LDFLAGS) -o dmtestsystem test/dmtestsystem.f90 $(TARGET) $(LDLIBS)

dmtestthread: test/dmtestthread.f90 $(TARGET)
	$(FC) $(FFLAGS) $(MODFLAGS) $(LDFLAGS) -o dmtestthread test/dmtestthread.f90 $(TARGET) $(LIBPTHREAD) $(LDLIBS)

dmtesttime: test/dmtesttime.f90 $(TARGET)
	$(FC) $(FFLAGS) $(MODFLAGS) $(LDFLAGS) -o dmtesttime test/dmtesttime.f90 $(TARGET) $(LDLIBS)

dmtesttransform: test/dmtesttransform.f90 $(TARGET)
	$(FC) $(FFLAGS) $(MODFLAGS) $(LDFLAGS) -o dmtesttransform test/dmtesttransform.f90 $(TARGET) $(LDLIBS) $(LIBLAPACK)

dmtesttty: test/dmtesttty.f90 $(TARGET)
	$(FC) $(FFLAGS) $(MODFLAGS) $(LDFLAGS) -o dmtesttty test/dmtesttty.f90 $(TARGET) $(LDLIBS)

dmtestunit: test/dmtestunit.f90 $(TARGET)
	$(FC) $(FFLAGS) $(MODFLAGS) $(LDFLAGS) -o dmtestunit test/dmtestunit.f90 $(TARGET) $(LDLIBS)

dmtestutil: test/dmtestutil.f90 $(TARGET)
	$(FC) $(FFLAGS) $(MODFLAGS) $(LDFLAGS) -o dmtestutil test/dmtestutil.f90 $(TARGET) $(LDLIBS)

dmtestuuid: test/dmtestuuid.f90 $(TARGET)
	$(FC) $(FFLAGS) $(MODFLAGS) $(LDFLAGS) -o dmtestuuid test/dmtestuuid.f90 $(TARGET) $(LDLIBS)

dmtestve: test/dmtestve.f90 $(TARGET)
	$(FC) $(FFLAGS) $(MODFLAGS) $(LDFLAGS) -o dmtestve test/dmtestve.f90 $(TARGET) $(LDLIBS)

dmtestversion: test/dmtestversion.f90 $(TARGET)
	$(FC) $(FFLAGS) $(MODFLAGS) $(LDFLAGS) -o dmtestversion test/dmtestversion.f90 $(TARGET) $(LDLIBS)

dmtestz: test/dmtestz.f90 $(TARGET)
	$(FC) $(FFLAGS) $(MODFLAGS) $(LDFLAGS) -o dmtestz test/dmtestz.f90 $(TARGET) $(LIBZ) $(LDLIBS)

dmtestzlib: test/dmtestzlib.f90 $(TARGET)
	$(FC) $(FFLAGS) $(MODFLAGS) $(LDFLAGS) -o dmtestzlib test/dmtestzlib.f90 $(TARGET) $(LIBZLIB) $(LDLIBS)

dmtestzstd: test/dmtestzstd.f90 $(TARGET)
	$(FC) $(FFLAGS) $(MODFLAGS) $(LDFLAGS) -o dmtestzstd test/dmtestzstd.f90 $(TARGET) $(LIBZSTD) $(LDLIBS)

# ******************************************************************************
#
# DMPACK programs.
#
# ******************************************************************************

$(DMAPI): app/dmapi.f90 $(TARGET)
	$(FC) $(FFLAGS) $(MODFLAGS) $(LDFLAGS) -o $(DMAPI) app/dmapi.f90 $(TARGET) $(LIBSQLITE3) $(LIBZ) $(LIBFASTCGI) $(LDLIBS)

$(DMBACKUP): app/dmbackup.f90 $(TARGET)
	$(FC) $(FFLAGS) $(MODFLAGS) $(LDFLAGS) -o $(DMBACKUP) app/dmbackup.f90 $(TARGET) $(LIBSQLITE3) $(LDLIBS)

$(DMBEAT): app/dmbeat.f90 $(TARGET)
	$(FC) $(FFLAGS) $(MODFLAGS) $(LDFLAGS) -o $(DMBEAT) app/dmbeat.f90 $(TARGET) $(LIBCURL) $(LIBLUA54) $(LIBZ) $(LIBRT) $(LDLIBS)

$(DMBOT): app/dmbot.f90 $(TARGET)
	$(FC) $(FFLAGS) $(MODFLAGS) $(LDFLAGS) -o $(DMBOT) app/dmbot.f90 $(TARGET) $(LIBLUA54) $(LIBSQLITE3) $(LIBCURL) $(LIBSTROPHE) $(LIBRT) $(LDLIBS)

$(DMDB): app/dmdb.f90 $(TARGET)
	$(FC) $(FFLAGS) $(MODFLAGS) $(LDFLAGS) -o $(DMDB) app/dmdb.f90 $(TARGET) $(LIBLUA54) $(LIBSQLITE3) $(LIBPTHREAD) $(LIBRT) $(LDLIBS)

$(DMDBCTL): app/dmdbctl.f90 $(TARGET)
	$(FC) $(FFLAGS) $(MODFLAGS) $(LDFLAGS) -o $(DMDBCTL) app/dmdbctl.f90 $(TARGET) $(LIBSQLITE3) $(LDLIBS)

$(DMDWD): app/dmdwd.f90 $(TARGET)
	$(FC) $(FFLAGS) $(MODFLAGS) $(LDFLAGS) -o $(DMDWD) app/dmdwd.f90 $(TARGET) $(LIBCURL) $(LIBLUA54) $(LIBRT) $(LIBZ) $(LDLIBS)

$(DMEXPORT): app/dmexport.f90 $(TARGET)
	$(FC) $(FFLAGS) $(MODFLAGS) $(LDFLAGS) -o $(DMEXPORT) app/dmexport.f90 $(TARGET) $(LIBSQLITE3) $(LDLIBS)

$(DMFEED): app/dmfeed.f90 $(TARGET)
	$(FC) $(FFLAGS) $(MODFLAGS) $(LDFLAGS) -o $(DMFEED) app/dmfeed.f90 $(TARGET) $(LIBLUA54) $(LIBSQLITE3) $(LDLIBS)

$(DMFS): app/dmfs.f90 $(TARGET)
	$(FC) $(FFLAGS) $(MODFLAGS) $(LDFLAGS) -o $(DMFS) app/dmfs.f90 $(TARGET) $(LIBLUA54) $(LIBPCRE2) $(LIBRT) $(LDLIBS)

$(DMGRC): app/dmgrc.f90 $(TARGET)
	$(FC) $(FFLAGS) $(MODFLAGS) $(LDFLAGS) -o $(DMGRC) app/dmgrc.f90 $(TARGET) $(LIBLUA54) $(LIBRT) $(LDLIBS)

$(DMIMPORT): app/dmimport.f90 $(TARGET)
	$(FC) $(FFLAGS) $(MODFLAGS) $(LDFLAGS) -o $(DMIMPORT) app/dmimport.f90 $(TARGET) $(LIBSQLITE3) $(LDLIBS)

$(DMINFO): app/dminfo.f90 $(TARGET)
	$(FC) $(FFLAGS) $(MODFLAGS) $(LDFLAGS) -o $(DMINFO) app/dminfo.f90 $(TARGET) $(LIBSQLITE3) $(LDLIBS)

$(DMINIT): app/dminit.f90 $(TARGET)
	$(FC) $(FFLAGS) $(MODFLAGS) $(LDFLAGS) -o $(DMINIT) app/dminit.f90 $(TARGET) $(LIBSQLITE3) $(LDLIBS)

$(DMLOG): app/dmlog.f90 $(TARGET)
	$(FC) $(FFLAGS) $(MODFLAGS) $(LDFLAGS) -o $(DMLOG) app/dmlog.f90 $(TARGET) $(LIBRT) $(LDLIBS)

$(DMLOGGER): app/dmlogger.f90 $(TARGET)
	$(FC) $(FFLAGS) $(MODFLAGS) $(LDFLAGS) -o $(DMLOGGER) app/dmlogger.f90 $(TARGET) $(LIBLUA54) $(LIBSQLITE3) $(LIBPTHREAD) $(LIBRT) $(LDLIBS)

$(DMLUA): app/dmlua.f90 $(TARGET)
	$(FC) $(FFLAGS) $(MODFLAGS) $(LDFLAGS) -o $(DMLUA) app/dmlua.f90 $(TARGET) $(LIBLUA54) $(LIBRT) $(LDLIBS)

$(DMMB): app/dmmb.f90 $(TARGET)
	$(FC) $(FFLAGS) $(MODFLAGS) $(LDFLAGS) -o $(DMMB) app/dmmb.f90 $(TARGET) $(LIBLUA54) $(LIBMODBUS) $(LIBRT) $(LDLIBS)

$(DMMBCTL): app/dmmbctl.f90 $(TARGET)
	$(FC) $(FFLAGS) $(MODFLAGS) $(LDFLAGS) -o $(DMMBCTL) app/dmmbctl.f90 $(TARGET) $(LIBMODBUS) $(LDLIBS)

$(DMPIPE): app/dmpipe.f90 $(TARGET)
	$(FC) $(FFLAGS) $(MODFLAGS) $(LDFLAGS) -o $(DMPIPE) app/dmpipe.f90 $(TARGET) $(LIBLUA54) $(LIBPCRE2) $(LIBRT) $(LDLIBS)

$(DMPLOT): app/dmplot.f90 $(TARGET)
	$(FC) $(FFLAGS) $(MODFLAGS) $(LDFLAGS) -o $(DMPLOT) app/dmplot.f90 $(TARGET) $(LIBLUA54) $(LIBSQLITE3) $(LDLIBS)

$(DMRECV): app/dmrecv.f90 $(TARGET)
	$(FC) $(FFLAGS) $(MODFLAGS) $(LDFLAGS) -o $(DMRECV) app/dmrecv.f90 $(TARGET) $(LIBLUA54) $(LIBRT) $(LDLIBS)

$(DMREPORT): app/dmreport.f90 $(TARGET)
	$(FC) $(FFLAGS) $(MODFLAGS) $(LDFLAGS) -o $(DMREPORT) app/dmreport.f90 $(TARGET) $(LIBLUA54) $(LIBSQLITE3) $(LDLIBS)

$(DMSEND): app/dmsend.f90 $(TARGET)
	$(FC) $(FFLAGS) $(MODFLAGS) $(LDFLAGS) -o $(DMSEND) app/dmsend.f90 $(TARGET) $(LIBLUA54) $(LIBRT) $(LDLIBS)

$(DMSERIAL): app/dmserial.f90 $(TARGET)
	$(FC) $(FFLAGS) $(MODFLAGS) $(LDFLAGS) -o $(DMSERIAL) app/dmserial.f90 $(TARGET) $(LIBLUA54) $(LIBPCRE2) $(LIBRT) $(LDLIBS)

$(DMSYNC): app/dmsync.f90 $(TARGET)
	$(FC) $(FFLAGS) $(MODFLAGS) $(LDFLAGS) -o $(DMSYNC) app/dmsync.f90 $(TARGET) $(LIBCURL) $(LIBLUA54) $(LIBSQLITE3) $(LIBZ) $(LIBPTHREAD) $(LIBRT) $(LDLIBS)

$(DMSYSTEM): app/dmsystem.f90 $(TARGET)
	$(FC) $(FFLAGS) $(MODFLAGS) $(LDFLAGS) -o $(DMSYSTEM) app/dmsystem.f90 $(TARGET) $(LIBCURL) $(LIBLUA54) $(LIBRT) $(LDLIBS)

$(DMUUID): app/dmuuid.f90 $(TARGET)
	$(FC) $(FFLAGS) $(MODFLAGS) $(LDFLAGS) -o $(DMUUID) app/dmuuid.f90 $(TARGET) $(LDLIBS)

$(DMVED): app/dmved.f90 $(TARGET)
	$(FC) $(FFLAGS) $(MODFLAGS) $(LDFLAGS) -o $(DMVED) app/dmved.f90 $(TARGET) $(LIBLUA54) $(LIBRT) $(LDLIBS)

$(DMWEB): app/dmweb.f90 $(TARGET)
	$(FC) $(FFLAGS) $(MODFLAGS) $(LDFLAGS) -o $(DMWEB) app/dmweb.f90 $(TARGET) $(LIBSQLITE3) $(LDLIBS)

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

# User Guide to HTML format.
guide:
	cd $(GUIDDIR) && $(MAKE)
	cd $(GUIDDIR) && $(MAKE) multi

# ******************************************************************************
#
# Installation and deinstallation.
#
# ******************************************************************************

install:
	@echo "--- Installing DMPACK to $(PREFIX) ..."
	$(INSTALL) -d $(IBINDIR)
	$(INSTALL) -d $(IETCDIR)
	$(INSTALL) -d $(IINCDIR)
	$(INSTALL) -d $(ILIBDIR)
	$(INSTALL) -d $(IMANDIR)
	$(INSTALL) -d $(ISHRDIR)
	$(INSTALL) -d $(ISHRDIR)/dmdwd
	$(INSTALL) -d $(ISHRDIR)/dmfeed
	$(INSTALL) -d $(ISHRDIR)/dmlua
	$(INSTALL) -d $(ISHRDIR)/dmpipe
	$(INSTALL) -d $(ISHRDIR)/dmreport
	$(INSTALL) -d $(ISHRDIR)/dmweb
	$(INSTALL) -m 755 $(DMAPI)    $(IBINDIR)/
	$(INSTALL) -m 755 $(DMBACKUP) $(IBINDIR)/
	$(INSTALL) -m 755 $(DMBEAT)   $(IBINDIR)/
	$(INSTALL) -m 755 $(DMBOT)    $(IBINDIR)/
	$(INSTALL) -m 755 $(DMDB)     $(IBINDIR)/
	$(INSTALL) -m 755 $(DMDBCTL)  $(IBINDIR)/
	$(INSTALL) -m 755 $(DMDWD)    $(IBINDIR)/
	$(INSTALL) -m 755 $(DMEXPORT) $(IBINDIR)/
	$(INSTALL) -m 755 $(DMFEED)   $(IBINDIR)/
	$(INSTALL) -m 755 $(DMFS)     $(IBINDIR)/
	$(INSTALL) -m 755 $(DMGRC)    $(IBINDIR)/
	$(INSTALL) -m 755 $(DMIMPORT) $(IBINDIR)/
	$(INSTALL) -m 755 $(DMINFO)   $(IBINDIR)/
	$(INSTALL) -m 755 $(DMINIT)   $(IBINDIR)/
	$(INSTALL) -m 755 $(DMLOG)    $(IBINDIR)/
	$(INSTALL) -m 755 $(DMLOGGER) $(IBINDIR)/
	$(INSTALL) -m 755 $(DMLUA)    $(IBINDIR)/
	$(INSTALL) -m 755 $(DMMB)     $(IBINDIR)/
	$(INSTALL) -m 755 $(DMMBCTL)  $(IBINDIR)/
	$(INSTALL) -m 755 $(DMPIPE)   $(IBINDIR)/
	$(INSTALL) -m 755 $(DMPLOT)   $(IBINDIR)/
	$(INSTALL) -m 755 $(DMRECV)   $(IBINDIR)/
	$(INSTALL) -m 755 $(DMREPORT) $(IBINDIR)/
	$(INSTALL) -m 755 $(DMSEND)   $(IBINDIR)/
	$(INSTALL) -m 755 $(DMSERIAL) $(IBINDIR)/
	$(INSTALL) -m 755 $(DMSYNC)   $(IBINDIR)/
	$(INSTALL) -m 755 $(DMSYSTEM) $(IBINDIR)/
	$(INSTALL) -m 755 $(DMUUID)   $(IBINDIR)/
	$(INSTALL) -m 755 $(DMVED)    $(IBINDIR)/
	$(INSTALL) -m 755 $(DMWEB)    $(IBINDIR)/
	$(INSTALL) -m 644 $(INCDIR)/*.mod $(IINCDIR)/
	$(INSTALL) -m 644 $(TARGET) $(ILIBDIR)/
	$(INSTALL) -m 644 $(SHARED) $(ILIBDIR)/
	$(INSTALL) -m 644 $(CONFDIR)/*.conf.sample $(IETCDIR)/
	$(INSTALL) -m 644 $(SHRDIR)/dmdwd/catalog.cfg         $(ISHRDIR)/dmdwd/
	$(INSTALL) -m 644 $(SHRDIR)/dmfeed/feed.xsl           $(ISHRDIR)/dmfeed/
	$(INSTALL) -m 644 $(SHRDIR)/dmlua/dmlua.lua           $(ISHRDIR)/dmlua/
	$(INSTALL) -m 755 $(SHRDIR)/dmpipe/diskfree.sh        $(ISHRDIR)/dmpipe/
	$(INSTALL) -m 644 $(SHRDIR)/dmreport/dmreport.css     $(ISHRDIR)/dmreport/
	$(INSTALL) -m 644 $(SHRDIR)/dmreport/dmreport.min.css $(ISHRDIR)/dmreport/
	$(INSTALL) -m 755 $(SHRDIR)/dmreport/mkreport.sh      $(ISHRDIR)/dmreport/
	$(INSTALL) -m 644 $(SHRDIR)/dmweb/dmpack.css          $(ISHRDIR)/dmweb/
	$(INSTALL) -m 644 $(SHRDIR)/dmweb/dmpack.min.css      $(ISHRDIR)/dmweb/
	$(INSTALL) -m 644 $(SHRDIR)/dmweb/leaflet.min.css     $(ISHRDIR)/dmweb/
	$(INSTALL) -m 644 $(SHRDIR)/dmweb/dmpack.js           $(ISHRDIR)/dmweb/
	$(INSTALL) -m 644 $(SHRDIR)/dmweb/leaflet.js          $(ISHRDIR)/dmweb/
	$(GZIP) -9 < $(MANDIR)/dmapi.1    > $(IMANDIR)/dmapi.1.gz
	$(GZIP) -9 < $(MANDIR)/dmbackup.1 > $(IMANDIR)/dmbackup.1.gz
	$(GZIP) -9 < $(MANDIR)/dmbeat.1   > $(IMANDIR)/dmbeat.1.gz
	$(GZIP) -9 < $(MANDIR)/dmbot.1    > $(IMANDIR)/dmbot.1.gz
	$(GZIP) -9 < $(MANDIR)/dmdb.1     > $(IMANDIR)/dmdb.1.gz
	$(GZIP) -9 < $(MANDIR)/dmdbctl.1  > $(IMANDIR)/dmdbctl.1.gz
	$(GZIP) -9 < $(MANDIR)/dmdwd.1    > $(IMANDIR)/dmdwd.1.gz
	$(GZIP) -9 < $(MANDIR)/dmexport.1 > $(IMANDIR)/dmexport.1.gz
	$(GZIP) -9 < $(MANDIR)/dmfeed.1   > $(IMANDIR)/dmfeed.1.gz
	$(GZIP) -9 < $(MANDIR)/dmfs.1     > $(IMANDIR)/dmfs.1.gz
	$(GZIP) -9 < $(MANDIR)/dmgrc.1    > $(IMANDIR)/dmgrc.1.gz
	$(GZIP) -9 < $(MANDIR)/dmimport.1 > $(IMANDIR)/dmimport.1.gz
	$(GZIP) -9 < $(MANDIR)/dminfo.1   > $(IMANDIR)/dminfo.1.gz
	$(GZIP) -9 < $(MANDIR)/dminit.1   > $(IMANDIR)/dminit.1.gz
	$(GZIP) -9 < $(MANDIR)/dmlog.1    > $(IMANDIR)/dmlog.1.gz
	$(GZIP) -9 < $(MANDIR)/dmlogger.1 > $(IMANDIR)/dmlogger.1.gz
	$(GZIP) -9 < $(MANDIR)/dmlua.1    > $(IMANDIR)/dmlua.1.gz
	$(GZIP) -9 < $(MANDIR)/dmmb.1     > $(IMANDIR)/dmmb.1.gz
	$(GZIP) -9 < $(MANDIR)/dmmbctl.1  > $(IMANDIR)/dmmbctl.1.gz
	$(GZIP) -9 < $(MANDIR)/dmpipe.1   > $(IMANDIR)/dmpipe.1.gz
	$(GZIP) -9 < $(MANDIR)/dmplot.1   > $(IMANDIR)/dmplot.1.gz
	$(GZIP) -9 < $(MANDIR)/dmrecv.1   > $(IMANDIR)/dmrecv.1.gz
	$(GZIP) -9 < $(MANDIR)/dmreport.1 > $(IMANDIR)/dmreport.1.gz
	$(GZIP) -9 < $(MANDIR)/dmsend.1   > $(IMANDIR)/dmsend.1.gz
	$(GZIP) -9 < $(MANDIR)/dmserial.1 > $(IMANDIR)/dmserial.1.gz
	$(GZIP) -9 < $(MANDIR)/dmsync.1   > $(IMANDIR)/dmsync.1.gz
	$(GZIP) -9 < $(MANDIR)/dmsystem.1 > $(IMANDIR)/dmsystem.1.gz
	$(GZIP) -9 < $(MANDIR)/dmuuid.1   > $(IMANDIR)/dmuuid.1.gz
	$(GZIP) -9 < $(MANDIR)/dmved.1    > $(IMANDIR)/dmved.1.gz
	$(GZIP) -9 < $(MANDIR)/dmweb.1    > $(IMANDIR)/dmweb.1.gz

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
	$(RM) -f $(IBINDIR)/dmbot
	$(RM) -f $(IBINDIR)/dmdb
	$(RM) -f $(IBINDIR)/dmdbctl
	$(RM) -f $(IBINDIR)/dmdwd
	$(RM) -f $(IBINDIR)/dmexport
	$(RM) -f $(IBINDIR)/dmfeed
	$(RM) -f $(IBINDIR)/dmfs
	$(RM) -f $(IBINDIR)/dmgrc
	$(RM) -f $(IBINDIR)/dmplot
	$(RM) -f $(IBINDIR)/dmimport
	$(RM) -f $(IBINDIR)/dminfo
	$(RM) -f $(IBINDIR)/dminit
	$(RM) -f $(IBINDIR)/dmlog
	$(RM) -f $(IBINDIR)/dmlogger
	$(RM) -f $(IBINDIR)/dmlua
	$(RM) -f $(IBINDIR)/dmmb
	$(RM) -f $(IBINDIR)/dmmbctl
	$(RM) -f $(IBINDIR)/dmpipe
	$(RM) -f $(IBINDIR)/dmrecv
	$(RM) -f $(IBINDIR)/dmreport
	$(RM) -f $(IBINDIR)/dmsend
	$(RM) -f $(IBINDIR)/dmserial
	$(RM) -f $(IBINDIR)/dmsync
	$(RM) -f $(IBINDIR)/dmsystem
	$(RM) -f $(IBINDIR)/dmuuid
	$(RM) -f $(IBINDIR)/dmved
	$(RM) -f $(IBINDIR)/dmweb
	$(RM) -f $(IMANDIR)/dmapi.1.gz
	$(RM) -f $(IMANDIR)/dmbackup.1.gz
	$(RM) -f $(IMANDIR)/dmbeat.1.gz
	$(RM) -f $(IMANDIR)/dmbot.1.gz
	$(RM) -f $(IMANDIR)/dmdb.1.gz
	$(RM) -f $(IMANDIR)/dmdbctl.1.gz
	$(RM) -f $(IMANDIR)/dmdwd.1.gz
	$(RM) -f $(IMANDIR)/dmexport.1.gz
	$(RM) -f $(IMANDIR)/dmfeed.1.gz
	$(RM) -f $(IMANDIR)/dmfs.1.gz
	$(RM) -f $(IMANDIR)/dmgrc.1.gz
	$(RM) -f $(IMANDIR)/dmimport.1.gz
	$(RM) -f $(IMANDIR)/dminfo.1.gz
	$(RM) -f $(IMANDIR)/dminit.1.gz
	$(RM) -f $(IMANDIR)/dmlog.1.gz
	$(RM) -f $(IMANDIR)/dmlogger.1.gz
	$(RM) -f $(IMANDIR)/dmlua.1.gz
	$(RM) -f $(IMANDIR)/dmmb.1.gz
	$(RM) -f $(IMANDIR)/dmmbctl.1.gz
	$(RM) -f $(IMANDIR)/dmpipe.1.gz
	$(RM) -f $(IMANDIR)/dmplot.1.gz
	$(RM) -f $(IMANDIR)/dmrecv.1.gz
	$(RM) -f $(IMANDIR)/dmreport.1.gz
	$(RM) -f $(IMANDIR)/dmsend.1.gz
	$(RM) -f $(IMANDIR)/dmserial.1.gz
	$(RM) -f $(IMANDIR)/dmsync.1.gz
	$(RM) -f $(IMANDIR)/dmsystem.1.gz
	$(RM) -f $(IMANDIR)/dmuuid.1.gz
	$(RM) -f $(IMANDIR)/dmved.1.gz
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
	@echo
	@echo "--- Deleting build files ..."
	if [ `ls -1 *.mod 2>/dev/null | wc -l` -gt 0 ]; then $(RM) *.mod; fi
	if [ `ls -1 *.a   2>/dev/null | wc -l` -gt 0 ]; then $(RM) *.a; fi
	if [ `ls -1 *.so  2>/dev/null | wc -l` -gt 0 ]; then $(RM) *.so; fi
	if [ `ls -1 *.o   2>/dev/null | wc -l` -gt 0 ]; then $(RM) *.o; fi
	@echo
	@echo "--- Deleting tests ..."
	if [ `ls -1 dmtest* 2>/dev/null | wc -l` -gt 0 ]; then $(RM) dmtest*; fi
	@echo
	@echo "--- Deleting programs ..."
	if [ `ls -1 $(DISTDIR) 2>/dev/null | wc -l` -gt 0 ]; then $(RM) $(DISTDIR)/*; fi
	@echo
	@echo "--- Cleaning guide ..."
	cd $(GUIDDIR) && $(MAKE) clean

# ******************************************************************************
#
# Additionally, clean dependencies, stale files, and FORD output.
#
# ******************************************************************************

purge: clean
	@echo
	@echo "--- Cleaning fortran-curl ..."
	cd vendor/fortran-curl/ && $(MAKE) clean TARGET="../../$(LIBFCURL)"
	@echo
	@echo "--- Cleaning fortran-lua54 ..."
	cd vendor/fortran-lua54/ && $(MAKE) clean TARGET="../../$(LIBFLUA54)"
	@echo
	@echo "--- Cleaning fortran-modbus ..."
	cd vendor/fortran-modbus/ && $(MAKE) clean TARGET="../../$(LIBFMODBUS)"
	@echo
	@echo "--- Cleaning fortran-pcre2 ..."
	cd vendor/fortran-pcre2/ && $(MAKE) clean TARGET="../../$(LIBFPCRE2)"
	@echo
	@echo "--- Cleaning fortran-sqlite3 ..."
	cd vendor/fortran-sqlite3/ && $(MAKE) clean TARGET="../../$(LIBFSQLITE3)"
	@echo
	@echo "--- Cleaning fortran-unix ..."
	cd vendor/fortran-unix/ && $(MAKE) clean TARGET="../../$(LIBFUNIX)"
	@echo
	@echo "--- Cleaning fortran-xmpp ..."
	cd vendor/fortran-xmpp/ && $(MAKE) clean TARGET="../../$(LIBFXMPP)"
	@echo
	@echo "--- Cleaning fortran-zlib ..."
	cd vendor/fortran-zlib/ && $(MAKE) clean TARGET="../../$(LIBFZLIB)"
	@echo
	@echo "--- Cleaning fortran-zstd ..."
	cd vendor/fortran-zstd/ && $(MAKE) clean TARGET="../../$(LIBFZSTD)"
	@echo
	@echo "--- Deleting module files ..."
	if [ -e $(INCDIR) ]; then $(RM) -r $(INCDIR); fi
	@echo
	@echo "--- Deleting source code documentation ..."
	if [ -e $(DOCDIR) ]; then $(RM) -r $(DOCDIR); fi
	@echo
	@echo "--- Deleting stale test files ..."
	if [ -e testobserv.hdf5 ];          then $(RM) testobserv.hdf5;          fi
	if [ -e testbeat.sqlite ];          then $(RM) testbeat.sqlite;          fi
	if [ -e testlog.sqlite ];           then $(RM) testlog.sqlite;           fi
	if [ -e testobserv.sqlite ];        then $(RM) testobserv.sqlite;        fi
	if [ -e testobserv_backup.sqlite ]; then $(RM) testobserv_backup.sqlite; fi
	if [ -e testobserv_vacuum.sqlite ]; then $(RM) testobserv_vacuum.sqlite; fi
	if [ -e testgm.png ];               then $(RM) testgm.png;               fi
	if [ -e testroff1.pdf ];            then $(RM) testroff1.pdf;            fi
	if [ -e testroff2.pdf ];            then $(RM) testroff2.pdf;            fi
	if [ -e testroff3.pdf ];            then $(RM) testroff3.pdf;            fi

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
	@echo "LIBFLAGS   = $(LIBFLAGS)"
	@echo "MODFLAGS   = $(MODFLAGS)"
	@echo "PPFLAGS    = $(PPFLAGS)"
	@echo "EXFLAGS    = $(EXFLAGS)"
	@echo "ARFLAGS    = $(ARFLAGS)"
	@echo "LDFLAGS    = $(LDFLAGS)"
	@echo "LDLIBS     = $(LDLIBS)"
	@echo "INCHDF5    = $(INCHDF5)"
	@echo "LIBCRYPTO  = $(LIBCRYPTO)"
	@echo "LIBCURL    = $(LIBCURL)"
	@echo "LIBFASTCGI = $(LIBFASTCGI)"
	@echo "LIBHDF5    = $(LIBHDF5)"
	@echo "LIBLAPACK  = $(LIBLAPACK)"
	@echo "LIBLUA54   = $(LIBLUA54)"
	@echo "LIBMODBUS  = $(LIBMODBUS)"
	@echo "LIBPCRE2   = $(LIBPCRE2)"
	@echo "LIBPTHREAD = $(LIBPTHREAD)"
	@echo "LIBRT      = $(LIBRT)"
	@echo "LIBSQLITE3 = $(LIBSQLITE3)"
	@echo "LIBSTROPHE = $(LIBSTROPHE)"
	@echo "LIBZ       = $(LIBZ)"
	@echo "LIBZLIB    = $(LIBZLIB)"
	@echo "LIBZSTD    = $(LIBZSTD)"

# ******************************************************************************
#
# Print build targets.
#
# ******************************************************************************

help:
	@echo "The following build targets are available:"
	@echo
	@echo "    app             - Build DMPACK programs."
	@echo "    build           - Build DMPACK libraries, tests, and programs."
	@echo "    clean           - Clean DMPACK build environment."
	@echo "    deinstall       - Deinstall DMPACK from PREFIX."
	@echo "    doc             - Create source code documentation (requires FORD)."
	@echo "    freebsd         - Build FreeBSD release version (x86-64, aarch64)."
	@echo "    freebsd_debug   - Build FreeBSD debug version (x86-64, aarch64)."
	@echo "    freebsd_release - Build FreeBSD release version (x86-64, aarch64)."
	@echo "    guide           - Convert User Guide to HTML (requires AsciiDoctor)."
	@echo "    help            - Show this help."
	@echo "    html            - Convert man pages to HTML (requires mandoc)."
	@echo "    install         - Install DMPACK to PREFIX."
	@echo "    linux           - Build Linux release version (x86-64)."
	@echo "    linux_aarch64   - Build Linux release version (aarch64)."
	@echo "    linux_debug     - Build Linux debug version (x86-64)."
	@echo "    linux_release   - Build Linux release version (x86-64)."
	@echo "    man             - Convert man pages (requires AsciiDoctor)."
	@echo "    options         - Show build flags and options."
	@echo "    pdf             - Convert man pages to PDF (requires ps2pdf)."
	@echo "    purge           - Purge DMPACK build environment (including dependencies)."
	@echo "    setup           - Create directories."
	@echo "    test            - Build test programs."
	@echo
