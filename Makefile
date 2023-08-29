.POSIX:
.SUFFIXES:

# DMPACK build flags:
#
#   OS      - The operating system, either `freebsd` or `linux`.
#   PREFIX  - Path prefix, `/usr/local` on FreeBSD, `/usr` on Linux.
#
#   FC      - Fortran 2018 compiler.
#   CC      - C compiler.
#   AR      - Archiver.
#   MAKE    - Either: `make`, `bmake`, or `gmake`.
#   RM      - Remove (`rm`).
#   SH      - Shell (`sh`).
#
#   SRCDIR  - Source directory.
#   INCDIR  - Include directory that contains the Fortran module files.
#   LIBDIR  - Library directory that contains the static libraries.
#   DISTDIR - Distribution file directory (libraries and programs).
#   CONFDIR - Configuration file directory.
#   SHARDIR - Shared data directory.
#
#   THIN    - Thin DMPACK library (without interface bindings).
#   TARGET  - Path to the full DMPACK library (with interface bindings).
#   SHARED  - Path to the shared DMPACK library (with interface bindings).
#
#   DEBUG   - Optional debugging flags.
#   RELEASE - Release flags.
#   FFLAGS  - Fortran compiler flags.
#   CLAGS   - C compiler flags.
#   ARFLAGS - Archiver flags.
#   LPFLAGS - LAPACK95 flags.
#   LDFLAGS - Linker flags.
#   LDLIBS  - Linker libraries.

OS      = freebsd
PREFIX  = /usr/local

FC      = gfortran
CC      = gcc
AR      = ar
MAKE    = make
RM      = rm
SH      = sh

SRCDIR  = ./src
INCDIR  = ./include
LIBDIR  = ./lib
DISTDIR = ./dist
CONFDIR = ./config
SHARDIR = ./share

THIN    = $(LIBDIR)/libdm.a
TARGET  = $(DISTDIR)/libdmpack.a
SHARED  = $(DISTDIR)/libdmpack.so

DEBUG   = -g -O0 -Wall -fmax-errors=1 -fbacktrace #-ffpe-trap=invalid,zero,overflow -fno-omit-frame-pointer
RELEASE = -mtune=native -O2
FFLAGS  = $(RELEASE) -std=f2018
CFLAGS  = -mtune=native -O2
ARFLAGS = -rcs
LDFLAGS = -I$(INCDIR) -J$(INCDIR) -L/usr/local/lib/ -z execstack
LDLIBS  = #-static-libasan -fsanitize=address -fno-omit-frame-pointer

# Shared libraries to link.
LIBCURL    = `curl-config --libs`
LIBFASTCGI = -lfcgi
LIBLUA53   = `pkg-config --libs-only-l lua-5.3`
LIBPCRE2   = `pkg-config --libs-only-l libpcre2-8`
LIBPTHREAD = -lpthread
LIBRT      = -lrt
LIBSQLITE3 = `pkg-config --libs-only-l sqlite3`
LIBZ       = `pkg-config --libs-only-l zlib`

# Fortran static libraries to link.
LIBFCURL    = $(LIBDIR)/libfortran-curl.a
LIBFLUA53   = $(LIBDIR)/libfortran-lua53.a
LIBFPCRE2   = $(LIBDIR)/libfortran-pcre2.a
LIBFSQLITE3 = $(LIBDIR)/libfortran-sqlite3.a
LIBFUNIX    = $(LIBDIR)/libfortran-unix.a
LIBFZ       = $(LIBDIR)/libfortran-zlib.a
LIBF        = $(LIBFCURL) $(LIBFLUA53) $(LIBFPCRE2) $(LIBFSQLITE3) $(LIBFUNIX) $(LIBFZ)

# Programs.
DMAPI    = $(DISTDIR)/dmapi
DMBACKUP = $(DISTDIR)/dmbackup
DMBEAT   = $(DISTDIR)/dmbeat
DMDB     = $(DISTDIR)/dmdb
DMDBCLI  = $(DISTDIR)/dmdbcli
DMEXPORT = $(DISTDIR)/dmexport
DMFEED   = $(DISTDIR)/dmfeed
DMFS     = $(DISTDIR)/dmfs
DMGRAPH  = $(DISTDIR)/dmgraph
DMINFO   = $(DISTDIR)/dminfo
DMINIT   = $(DISTDIR)/dminit
DMLOG    = $(DISTDIR)/dmlog
DMLOGGER = $(DISTDIR)/dmlogger
DMLUA    = $(DISTDIR)/dmlua
DMPIPE   = $(DISTDIR)/dmpipe
DMRECV   = $(DISTDIR)/dmrecv
DMREPORT = $(DISTDIR)/dmreport
DMSERIAL = $(DISTDIR)/dmserial
DMSYNC   = $(DISTDIR)/dmsync
DMUUID   = $(DISTDIR)/dmuuid
DMWEB    = $(DISTDIR)/dmweb

# Documentation.
FORD     = ford
DOCDIR   = ./doc
ADOCDIR  = ./adoc
GUIDEDIR = ./guide

# Library sources.
SRC = src/dm_version.f90 \
      src/dm_ascii.f90 \
      src/dm_string.f90 \
      src/dm_type.f90 \
      src/dm_format.f90 \
      src/dm_const.f90 \
      src/dm_error.f90 \
      src/dm_ansi.f90 \
      src/dm_convert.f90 \
      src/dm_env.f90 \
      src/dm_util.f90 \
      src/dm_time.f90 \
      src/dm_timer.f90 \
      src/dm_base64.f90 \
      src/dm_path.f90 \
      src/dm_file.f90 \
      src/dm_hash.f90 \
      src/dm_hash_table.f90 \
      src/dm_unit.f90 \
      src/dm_id.f90 \
      src/dm_uuid.f90 \
      src/dm_arg.f90 \
      src/dm_system.f90 \
      src/dm_pipe.f90 \
      src/dm_tty.f90 \
      src/dm_sem.f90 \
      src/dm_mutex.f90 \
      src/dm_dp.f90 \
      src/dm_fifo.f90 \
      src/dm_node.f90 \
      src/dm_sensor.f90 \
      src/dm_target.f90 \
      src/dm_observ.f90 \
      src/dm_log.f90 \
      src/dm_job.f90 \
      src/dm_plot.f90 \
      src/dm_report.f90 \
      src/dm_regex.f90 \
      src/dm_lua.f90 \
      src/dm_config.f90 \
      src/dm_sync.f90 \
      src/dm_beat.f90 \
      src/dm_app.f90 \
      src/dm_mqueue.f90 \
      src/dm_logger.f90 \
      src/dm_test.f90 \
      src/dm_dummy.f90 \
      src/dm_nml.f90 \
      src/dm_sql.f90 \
      src/dm_db.f90 \
      src/dm_z.f90 \
      src/dm_person.f90 \
      src/dm_mail.f90 \
      src/dm_http.f90 \
      src/dm_mime.f90 \
      src/dm_api.f90 \
      src/dm_rpc.f90 \
      src/dm_mqtt.f90 \
      src/dm_cgi.f90 \
      src/dm_fcgi.f90 \
      src/dm_block.f90 \
      src/dm_csv.f90 \
      src/dm_json.f90 \
      src/dm_jsonl.f90 \
      src/dm_html.f90 \
      src/dm_atom.f90 \
      src/dm_router.f90 \
      src/dm_la.f90 \
      src/dm_transform.f90 \
      src/dmpack.f90

# Library object files.
OBJ = dm_version.o \
      dm_ascii.o \
      dm_string.o \
      dm_type.o \
      dm_format.o \
      dm_const.o \
      dm_error.o \
      dm_ansi.o \
      dm_convert.o \
      dm_env.o \
      dm_util.o \
      dm_time.o \
      dm_timer.o \
      dm_base64.o \
      dm_path.o \
      dm_file.o \
      dm_hash.o \
      dm_hash_table.o \
      dm_unit.o \
      dm_id.o \
      dm_uuid.o \
      dm_arg.o \
      dm_system.o \
      dm_pipe.o \
      dm_tty.o \
      dm_sem.o \
      dm_mutex.o \
      dm_dp.o \
      dm_fifo.o \
      dm_node.o \
      dm_sensor.o \
      dm_target.o \
      dm_observ.o \
      dm_log.o \
      dm_job.o \
      dm_plot.o \
      dm_report.o \
      dm_regex.o \
      dm_lua.o \
      dm_config.o \
      dm_sync.o \
      dm_beat.o \
      dm_app.o \
      dm_mqueue.o \
      dm_logger.o \
      dm_test.o \
      dm_dummy.o \
      dm_nml.o \
      dm_sql.o \
      dm_db.o \
      dm_z.o \
      dm_person.o \
      dm_mail.o \
      dm_http.o \
      dm_mime.o \
      dm_api.o \
      dm_rpc.o \
      dm_mqtt.o \
      dm_cgi.o \
      dm_fcgi.o \
      dm_block.o \
      dm_csv.o \
      dm_json.o \
      dm_jsonl.o \
      dm_html.o \
      dm_atom.o \
      dm_router.o \
      dm_la.o \
      dm_transform.o \
      dmpack.o

.PHONY: all app clean deinstall doc freebsd guide install install_freebsd \
        install_linux linux man purge setup test

all: $(TARGET) $(SHARED) test app

app: $(DMAPI) $(DMBACKUP) $(DMBEAT) $(DMDB) $(DMDBCLI) $(DMEXPORT) $(DMFEED) \
     $(DMFS) $(DMGRAPH) $(DMINFO) $(DMINIT) $(DMLOG) $(DMLOGGER) $(DMLUA) \
     $(DMPIPE) $(DMRECV) $(DMREPORT) $(DMSERIAL) $(DMSYNC) $(DMUUID) $(DMWEB)

test: dmtestapi dmtestatom dmtestbase64 dmtestcgi dmtestcsv dmtestdb dmtestdp \
      dmtesthash dmtesthtml dmtestlogger dmtestlua dmtestjob dmtestjson \
      dmtestmail dmtestmqtt dmtestmqueue dmtestnml dmtestobserv dmtestpath \
      dmtestpipe dmtestplot dmtestregex dmtestrouter dmtestrpc dmteststring \
      dmtesttime dmtesttty dmtestunit dmtestuuid dmtestz

#
# Output directories.
#
setup:
	mkdir -p $(INCDIR)
	mkdir -p $(LIBDIR)
	mkdir -p $(DISTDIR)

#
# Fortran interface libraries.
#
$(LIBFCURL): setup
	cd vendor/fortran-curl/ && make FC="$(FC) -fPIC" CC="$(CC) -fPIC" DEBUG="$(RELEASE)" PREFIX=$(PREFIX) TARGET=../../$(LIBFCURL)
	cp ./vendor/fortran-curl/*.mod $(INCDIR)/

$(LIBFLUA53): setup
	cd vendor/fortran-lua53/ && make FC="$(FC) -fPIC" CC="$(CC) -fPIC" DEBUG="$(RELEASE)" PREFIX=$(PREFIX) TARGET=../../$(LIBFLUA53)
	cp ./vendor/fortran-lua53/*.mod $(INCDIR)/

$(LIBFPCRE2): setup
	cd vendor/fortran-pcre2/ && make FC="$(FC) -fPIC" CC="$(CC) -fPIC" DEBUG="$(RELEASE)" PREFIX=$(PREFIX) TARGET=../../$(LIBFPCRE2)
	cp ./vendor/fortran-pcre2/*.mod $(INCDIR)/

$(LIBFSQLITE3): setup
	cd vendor/fortran-sqlite3/ && make FC="$(FC) -fPIC" CC="$(CC) -fPIC" DEBUG="$(RELEASE)" PREFIX=$(PREFIX) TARGET=../../$(LIBFSQLITE3)
	cp ./vendor/fortran-sqlite3/*.mod $(INCDIR)/

$(LIBFUNIX): setup
	cd vendor/fortran-unix/ && make $(OS) FC="$(FC) -fPIC" CC="$(CC) -fPIC" DEBUG="$(RELEASE)" PREFIX=$(PREFIX) TARGET=../../$(LIBFUNIX)
	cp ./vendor/fortran-unix/*.mod $(INCDIR)/

$(LIBFZ): setup
	cd vendor/fortran-zlib/ && make FC="$(FC) -fPIC" CC="$(CC) -fPIC" DEBUG="$(RELEASE)" PREFIX=$(PREFIX) TARGET=../../$(LIBFZ)
	cp ./vendor/fortran-zlib/*.mod $(INCDIR)/

#
# DMPACK static library.
#
$(TARGET): $(LIBF) $(OBJ)
	$(AR) $(ARFLAGS) $(THIN) $(OBJ)
	$(SH) ./makelib.sh $(TARGET) $(THIN)

$(OBJ): $(SRC)
	$(FC) $(FFLAGS) $(LDFLAGS) -fPIC -c src/dm_version.f90
	$(FC) $(FFLAGS) $(LDFLAGS) -fPIC -c src/dm_ascii.f90
	$(FC) $(FFLAGS) $(LDFLAGS) -fPIC -c src/dm_string.f90
	$(FC) $(FFLAGS) $(LDFLAGS) -fPIC -c src/dm_type.f90
	$(FC) $(FFLAGS) $(LDFLAGS) -fPIC -c src/dm_format.f90
	$(FC) $(FFLAGS) $(LDFLAGS) -fPIC -c src/dm_const.f90
	$(FC) $(FFLAGS) $(LDFLAGS) -fPIC -c src/dm_error.f90
	$(FC) $(FFLAGS) $(LDFLAGS) -fPIC -c src/dm_ansi.f90
	$(FC) $(FFLAGS) $(LDFLAGS) -fPIC -c src/dm_convert.f90
	$(FC) $(FFLAGS) $(LDFLAGS) -fPIC -c src/dm_env.f90
	$(FC) $(FFLAGS) $(LDFLAGS) -fPIC -c src/dm_util.f90
	$(FC) $(FFLAGS) $(LDFLAGS) -fPIC -c src/dm_time.f90
	$(FC) $(FFLAGS) $(LDFLAGS) -fPIC -c src/dm_timer.f90
	$(FC) $(FFLAGS) $(LDFLAGS) -fPIC -c src/dm_base64.f90
	$(FC) $(FFLAGS) $(LDFLAGS) -fPIC -c src/dm_path.f90
	$(FC) $(FFLAGS) $(LDFLAGS) -fPIC -c src/dm_file.f90
	$(FC) $(FFLAGS) $(LDFLAGS) -fPIC -c src/dm_hash.f90
	$(FC) $(FFLAGS) $(LDFLAGS) -fPIC -c src/dm_hash_table.f90
	$(FC) $(FFLAGS) $(LDFLAGS) -fPIC -c src/dm_unit.f90
	$(FC) $(FFLAGS) $(LDFLAGS) -fPIC -c src/dm_id.f90
	$(FC) $(FFLAGS) $(LDFLAGS) -fPIC -c src/dm_uuid.f90
	$(FC) $(FFLAGS) $(LDFLAGS) -fPIC -c src/dm_arg.f90
	$(FC) $(FFLAGS) $(LDFLAGS) -fPIC -c src/dm_system.f90
	$(FC) $(FFLAGS) $(LDFLAGS) -fPIC -c src/dm_pipe.f90
	$(FC) $(FFLAGS) $(LDFLAGS) -fPIC -c src/dm_tty.f90
	$(FC) $(FFLAGS) $(LDFLAGS) -fPIC -c src/dm_sem.f90
	$(FC) $(FFLAGS) $(LDFLAGS) -fPIC -c src/dm_mutex.f90
	$(FC) $(FFLAGS) $(LDFLAGS) -fPIC -c src/dm_dp.f90
	$(FC) $(FFLAGS) $(LDFLAGS) -fPIC -c src/dm_fifo.f90
	$(FC) $(FFLAGS) $(LDFLAGS) -fPIC -c src/dm_node.f90
	$(FC) $(FFLAGS) $(LDFLAGS) -fPIC -c src/dm_sensor.f90
	$(FC) $(FFLAGS) $(LDFLAGS) -fPIC -c src/dm_target.f90
	$(FC) $(FFLAGS) $(LDFLAGS) -fPIC -c src/dm_observ.f90
	$(FC) $(FFLAGS) $(LDFLAGS) -fPIC -c src/dm_log.f90
	$(FC) $(FFLAGS) $(LDFLAGS) -fPIC -c src/dm_job.f90
	$(FC) $(FFLAGS) $(LDFLAGS) -fPIC -c src/dm_plot.f90
	$(FC) $(FFLAGS) $(LDFLAGS) -fPIC -c src/dm_report.f90
	$(FC) $(FFLAGS) $(LDFLAGS) -fPIC -c src/dm_regex.f90
	$(FC) $(FFLAGS) $(LDFLAGS) -fPIC -c src/dm_lua.f90
	$(FC) $(FFLAGS) $(LDFLAGS) -fPIC -c src/dm_config.f90
	$(FC) $(FFLAGS) $(LDFLAGS) -fPIC -c src/dm_sync.f90
	$(FC) $(FFLAGS) $(LDFLAGS) -fPIC -c src/dm_beat.f90
	$(FC) $(FFLAGS) $(LDFLAGS) -fPIC -c src/dm_app.f90
	$(FC) $(FFLAGS) $(LDFLAGS) -fPIC -c src/dm_mqueue.f90
	$(FC) $(FFLAGS) $(LDFLAGS) -fPIC -c src/dm_logger.f90
	$(FC) $(FFLAGS) $(LDFLAGS) -fPIC -c src/dm_test.f90
	$(FC) $(FFLAGS) $(LDFLAGS) -fPIC -c src/dm_dummy.f90
	$(FC) $(FFLAGS) $(LDFLAGS) -fPIC -c src/dm_nml.f90
	$(FC) $(FFLAGS) $(LDFLAGS) -fPIC -c src/dm_sql.f90
	$(FC) $(FFLAGS) $(LDFLAGS) -fPIC -c src/dm_db.f90
	$(FC) $(FFLAGS) $(LDFLAGS) -fPIC -c src/dm_z.f90
	$(FC) $(FFLAGS) $(LDFLAGS) -fPIC -c src/dm_person.f90
	$(FC) $(FFLAGS) $(LDFLAGS) -fPIC -c src/dm_mail.f90
	$(FC) $(FFLAGS) $(LDFLAGS) -fPIC -c src/dm_http.f90
	$(FC) $(FFLAGS) $(LDFLAGS) -fPIC -c src/dm_mime.f90
	$(FC) $(FFLAGS) $(LDFLAGS) -fPIC -c src/dm_api.f90
	$(FC) $(FFLAGS) $(LDFLAGS) -fPIC -c src/dm_rpc.f90
	$(FC) $(FFLAGS) $(LDFLAGS) -fPIC -c src/dm_mqtt.f90
	$(FC) $(FFLAGS) $(LDFLAGS) -fPIC -c src/dm_cgi.f90
	$(FC) $(FFLAGS) $(LDFLAGS) -fPIC -c src/dm_fcgi.f90
	$(FC) $(FFLAGS) $(LDFLAGS) -fPIC -c src/dm_block.f90
	$(FC) $(FFLAGS) $(LDFLAGS) -fPIC -c src/dm_csv.f90
	$(FC) $(FFLAGS) $(LDFLAGS) -fPIC -c src/dm_json.f90
	$(FC) $(FFLAGS) $(LDFLAGS) -fPIC -c src/dm_jsonl.f90
	$(FC) $(FFLAGS) $(LDFLAGS) -fPIC -c src/dm_html.f90
	$(FC) $(FFLAGS) $(LDFLAGS) -fPIC -c src/dm_atom.f90
	$(FC) $(FFLAGS) $(LDFLAGS) -fPIC -c src/dm_router.f90
	$(FC) $(FFLAGS) $(LDFLAGS) -fPIC -c src/dm_la.f90
	$(FC) $(FFLAGS) $(LDFLAGS) -fPIC -c src/dm_transform.f90
	$(FC) $(FFLAGS) $(LDFLAGS) -fPIC -c src/dmpack.f90

#
# DMPACK shared library.
#
$(SHARED): $(TARGET)
	$(FC) $(FFLAGS) -fPIC -shared -o $(SHARED) -Wl,--whole-archive $(TARGET) -Wl,--no-whole-archive

#
# FreeBSD macro.
#
freebsd:
	$(MAKE) all OS=freebsd PREFIX=/usr/local

#
# Linux macro.
#
linux:
	$(MAKE) all OS=linux PREFIX=/usr

#
# DMPACK test programs.
#
dmtestapi: test/dmtestapi.f90
	$(FC) $(FFLAGS) $(LDFLAGS) -o dmtestapi test/dmtestapi.f90 $(TARGET) $(LDLIBS)

dmtestatom: test/dmtestatom.f90
	$(FC) $(FFLAGS) $(LDFLAGS) -o dmtestatom test/dmtestatom.f90 $(TARGET) $(LDLIBS)

dmtestbase64: test/dmtestbase64.f90
	$(FC) $(FFLAGS) $(LDFLAGS) -o dmtestbase64 test/dmtestbase64.f90 $(TARGET) $(LDLIBS)

dmtestcgi: test/dmtestcgi.f90
	$(FC) $(FFLAGS) $(LDFLAGS) -o dmtestcgi test/dmtestcgi.f90 $(TARGET) $(LDLIBS)

dmtestcsv: test/dmtestcsv.f90
	$(FC) $(FFLAGS) $(LDFLAGS) -o dmtestcsv test/dmtestcsv.f90 $(TARGET) $(LDLIBS)

dmtestdb: test/dmtestdb.f90
	$(FC) $(FFLAGS) $(LDFLAGS) -o dmtestdb test/dmtestdb.f90 $(TARGET) $(LDLIBS) $(LIBSQLITE3)

dmtestdp: test/dmtestdp.f90
	$(FC) $(FFLAGS) $(LDFLAGS) -o dmtestdp test/dmtestdp.f90 $(TARGET) $(LDLIBS) $(LIBSQLITE3)

dmtesthash: test/dmtesthash.f90
	$(FC) $(FFLAGS) $(LDFLAGS) -o dmtesthash test/dmtesthash.f90 $(TARGET) $(LDLIBS)

dmtesthtml: test/dmtesthtml.f90
	$(FC) $(FFLAGS) $(LDFLAGS) -o dmtesthtml test/dmtesthtml.f90 $(TARGET) $(LDLIBS)

dmtestlogger: test/dmtestlogger.f90
	$(FC) $(FFLAGS) $(LDFLAGS) -o dmtestlogger test/dmtestlogger.f90 $(TARGET) $(LDLIBS) $(LIBRT)

dmtestlua: test/dmtestlua.f90
	$(FC) $(FFLAGS) $(LDFLAGS) -o dmtestlua test/dmtestlua.f90 $(TARGET) $(LDLIBS) $(LIBLUA53)

dmtestjob: test/dmtestjob.f90
	$(FC) $(FFLAGS) $(LDFLAGS) -o dmtestjob test/dmtestjob.f90 $(TARGET) $(LDLIBS)

dmtestjson: test/dmtestjson.f90
	$(FC) $(FFLAGS) $(LDFLAGS) -o dmtestjson test/dmtestjson.f90 $(TARGET) $(LDLIBS)

dmtestmail: test/dmtestmail.f90
	$(FC) $(FFLAGS) $(LDFLAGS) -o dmtestmail test/dmtestmail.f90 $(TARGET) $(LDLIBS) $(LIBCURL)

dmtestmqtt: test/dmtestmqtt.f90
	$(FC) $(FFLAGS) $(LDFLAGS) -o dmtestmqtt test/dmtestmqtt.f90 $(TARGET) $(LDLIBS) $(LIBCURL)

dmtestmqueue: test/dmtestmqueue.f90
	$(FC) $(FFLAGS) $(LDFLAGS) -o dmtestmqueue test/dmtestmqueue.f90 $(TARGET) $(LDLIBS) $(LIBRT)

dmtestnml: test/dmtestnml.f90
	$(FC) $(FFLAGS) $(LDFLAGS) -o dmtestnml test/dmtestnml.f90 $(TARGET) $(LDLIBS)

dmtestobserv: test/dmtestobserv.f90
	$(FC) $(FFLAGS) $(LDFLAGS) -o dmtestobserv test/dmtestobserv.f90 $(TARGET) $(LDLIBS)

dmtestpath: test/dmtestpath.f90
	$(FC) $(FFLAGS) $(LDFLAGS) -o dmtestpath test/dmtestpath.f90 $(TARGET) $(LDLIBS)

dmtestpipe: test/dmtestpipe.f90
	$(FC) $(FFLAGS) $(LDFLAGS) -o dmtestpipe test/dmtestpipe.f90 $(TARGET) $(LDLIBS)

dmtestplot: test/dmtestplot.f90
	$(FC) $(FFLAGS) $(LDFLAGS) -o dmtestplot test/dmtestplot.f90 $(TARGET) $(LDLIBS)

dmtestregex: test/dmtestregex.f90
	$(FC) $(FFLAGS) $(LDFLAGS) -o dmtestregex test/dmtestregex.f90 $(TARGET) $(LDLIBS) $(LIBPCRE2)

dmtestrouter: test/dmtestrouter.f90
	$(FC) $(FFLAGS) $(LDFLAGS) -o dmtestrouter test/dmtestrouter.f90 $(TARGET) $(LDLIBS)

dmtestrpc: test/dmtestrpc.f90
	$(FC) $(FFLAGS) $(LDFLAGS) -o dmtestrpc test/dmtestrpc.f90 $(TARGET) $(LDLIBS) $(LIBCURL) $(LIBZ)

dmteststring: test/dmteststring.f90
	$(FC) $(FFLAGS) $(LDFLAGS) -o dmteststring test/dmteststring.f90 $(TARGET) $(LDLIBS)

dmtesttime: test/dmtesttime.f90
	$(FC) $(FFLAGS) $(LDFLAGS) -o dmtesttime test/dmtesttime.f90 $(TARGET) $(LDLIBS)

dmtesttty: test/dmtesttty.f90
	$(FC) $(FFLAGS) $(LDFLAGS) -o dmtesttty test/dmtesttty.f90 $(TARGET) $(LDLIBS)

dmtestunit: test/dmtestunit.f90
	$(FC) $(FFLAGS) $(LDFLAGS) -o dmtestunit test/dmtestunit.f90 $(TARGET) $(LDLIBS)

dmtestuuid: test/dmtestuuid.f90
	$(FC) $(FFLAGS) $(LDFLAGS) -o dmtestuuid test/dmtestuuid.f90 $(TARGET) $(LDLIBS)

dmtestz: test/dmtestz.f90
	$(FC) $(FFLAGS) $(LDFLAGS) -o dmtestz test/dmtestz.f90 $(TARGET) $(LDLIBS) $(LIBZ)

#
# DMPACK programs.
#
$(DMAPI): app/dmapi.f90
	$(FC) $(FFLAGS) $(LDFLAGS) -o $(DMAPI) app/dmapi.f90 $(TARGET) $(LDLIBS) $(LIBSQLITE3) $(LIBZ) $(LIBFASTCGI)

$(DMBACKUP): app/dmbackup.f90
	$(FC) $(FFLAGS) $(LDFLAGS) -o $(DMBACKUP) app/dmbackup.f90 $(TARGET) $(LDLIBS) $(LIBSQLITE3)

$(DMBEAT): app/dmbeat.f90
	$(FC) $(FFLAGS) $(LDFLAGS) -o $(DMBEAT) app/dmbeat.f90 $(TARGET) $(LDLIBS) $(LIBCURL) $(LIBLUA53) $(LIBZ) $(LIBRT)

$(DMDB): app/dmdb.f90
	$(FC) $(FFLAGS) $(LDFLAGS) -o $(DMDB) app/dmdb.f90 $(TARGET) $(LDLIBS) $(LIBLUA53) $(LIBSQLITE3) $(LIBRT)

$(DMDBCLI): app/dmdbcli.f90
	$(FC) $(FFLAGS) $(LDFLAGS) -o $(DMDBCLI) app/dmdbcli.f90 $(TARGET) $(LDLIBS) $(LIBSQLITE3)

$(DMEXPORT): app/dmexport.f90
	$(FC) $(FFLAGS) $(LDFLAGS) -o $(DMEXPORT) app/dmexport.f90 $(TARGET) $(LDLIBS) $(LIBSQLITE3)

$(DMFEED): app/dmfeed.f90
	$(FC) $(FFLAGS) $(LDFLAGS) -o $(DMFEED) app/dmfeed.f90 $(TARGET) $(LDLIBS) $(LIBLUA53) $(LIBSQLITE3)

$(DMFS): app/dmfs.f90
	$(FC) $(FFLAGS) $(LDFLAGS) -o $(DMFS) app/dmfs.f90 $(TARGET) $(LDLIBS) $(LIBLUA53) $(LIBPCRE2) $(LIBRT)

$(DMGRAPH): app/dmgraph.f90
	$(FC) $(FFLAGS) $(LDFLAGS) -o $(DMGRAPH) app/dmgraph.f90 $(TARGET) $(LDLIBS) $(LIBLUA53) $(LIBSQLITE3)

$(DMINFO): app/dminfo.f90
	$(FC) $(FFLAGS) $(LDFLAGS) -o $(DMINFO) app/dminfo.f90 $(TARGET) $(LDLIBS) $(LIBSQLITE3)

$(DMINIT): app/dminit.f90
	$(FC) $(FFLAGS) $(LDFLAGS) -o $(DMINIT) app/dminit.f90 $(TARGET) $(LDLIBS) $(LIBSQLITE3)

$(DMLOG): app/dmlog.f90
	$(FC) $(FFLAGS) $(LDFLAGS) -o $(DMLOG) app/dmlog.f90 $(TARGET) $(LDLIBS) $(LIBRT)

$(DMLOGGER): app/dmlogger.f90
	$(FC) $(FFLAGS) $(LDFLAGS) -o $(DMLOGGER) app/dmlogger.f90 $(TARGET) $(LDLIBS) $(LIBLUA53) $(LIBSQLITE3) $(LIBRT)

$(DMLUA): app/dmlua.f90
	$(FC) $(FFLAGS) $(LDFLAGS) -o $(DMLUA) app/dmlua.f90 $(TARGET) $(LDLIBS) $(LIBLUA53) $(LIBRT)

$(DMPIPE): app/dmpipe.f90
	$(FC) $(FFLAGS) $(LDFLAGS) -o $(DMPIPE) app/dmpipe.f90 $(TARGET) $(LDLIBS) $(LIBLUA53) $(LIBPCRE2) $(LIBRT)

$(DMRECV): app/dmrecv.f90
	$(FC) $(FFLAGS) $(LDFLAGS) -o $(DMRECV) app/dmrecv.f90 $(TARGET) $(LDLIBS) $(LIBLUA53) $(LIBRT)

$(DMREPORT): app/dmreport.f90
	$(FC) $(FFLAGS) $(LDFLAGS) -o $(DMREPORT) app/dmreport.f90 $(TARGET) $(LDLIBS) $(LIBLUA53) $(LIBSQLITE3)

$(DMSERIAL): app/dmserial.f90
	$(FC) $(FFLAGS) $(LDFLAGS) -o $(DMSERIAL) app/dmserial.f90 $(TARGET) $(LDLIBS) $(LIBLUA53) $(LIBPCRE2) $(LIBRT)

$(DMSYNC): app/dmsync.f90
	$(FC) $(FFLAGS) $(LDFLAGS) -o $(DMSYNC) app/dmsync.f90 $(TARGET) $(LDLIBS) $(LIBCURL) $(LIBLUA53) $(LIBSQLITE3) $(LIBZ) $(LIBRT)

$(DMUUID): app/dmuuid.f90
	$(FC) $(FFLAGS) $(LDFLAGS) -o $(DMUUID) app/dmuuid.f90 $(TARGET) $(LDLIBS)

$(DMWEB): app/dmweb.f90
	$(FC) $(FFLAGS) $(LDFLAGS) -o $(DMWEB) app/dmweb.f90 $(TARGET) $(LDLIBS) $(LIBSQLITE3)

#
# Source code documentation, manual pages, and User's Guide.
#
doc:
	$(FORD) ford.md -d ./src

man:
	cd $(ADOCDIR) && $(MAKE) man

#
# User's Guide.
#
guide:
	cd $(GUIDEDIR) && $(MAKE)

#
# Installation.
#
install:
	install -d $(PREFIX)/bin/
	install -d $(PREFIX)/etc/dmpack/
	install -d $(PREFIX)/include/dmpack/
	install -d $(PREFIX)/lib/
	install -d $(PREFIX)/share/dmpack/
	install -m 755 $(DISTDIR)/dmapi $(PREFIX)/bin/
	install -m 755 $(DISTDIR)/dmbackup $(PREFIX)/bin/
	install -m 755 $(DISTDIR)/dmbeat $(PREFIX)/bin/
	install -m 755 $(DISTDIR)/dmdb $(PREFIX)/bin/
	install -m 755 $(DISTDIR)/dmdbcli $(PREFIX)/bin/
	install -m 755 $(DISTDIR)/dmexport $(PREFIX)/bin/
	install -m 755 $(DISTDIR)/dmfeed $(PREFIX)/bin/
	install -m 755 $(DISTDIR)/dmfs $(PREFIX)/bin/
	install -m 755 $(DISTDIR)/dmgraph $(PREFIX)/bin/
	install -m 755 $(DISTDIR)/dminfo $(PREFIX)/bin/
	install -m 755 $(DISTDIR)/dminit $(PREFIX)/bin/
	install -m 755 $(DISTDIR)/dmlog $(PREFIX)/bin/
	install -m 755 $(DISTDIR)/dmlogger $(PREFIX)/bin/
	install -m 755 $(DISTDIR)/dmlua $(PREFIX)/bin/
	install -m 755 $(DISTDIR)/dmpipe $(PREFIX)/bin/
	install -m 755 $(DISTDIR)/dmrecv $(PREFIX)/bin/
	install -m 755 $(DISTDIR)/dmreport $(PREFIX)/bin/
	install -m 755 $(DISTDIR)/dmserial $(PREFIX)/bin/
	install -m 755 $(DISTDIR)/dmsync $(PREFIX)/bin/
	install -m 755 $(DISTDIR)/dmuuid $(PREFIX)/bin/
	install -m 755 $(DISTDIR)/dmweb $(PREFIX)/bin/
	install -m 644 $(INCDIR)/*.mod $(PREFIX)/include/dmpack/
	install -m 644 $(TARGET) $(PREFIX)/lib/
	install -m 644 $(SHARED) $(PREFIX)/lib/
	install -m 644 $(CONFDIR)/*.conf $(PREFIX)/etc/dmpack/
	install -m 644 $(SHARDIR)/dmpack.css $(PREFIX)/share/dmpack/
	install -m 644 $(SHARDIR)/dmpack.min.css $(PREFIX)/share/dmpack/
	install -m 644 $(SHARDIR)/dmlua.lua $(PREFIX)/share/dmpack/

deinstall:
	$(RM) -rf $(PREFIX)/include/dmpack
	$(RM) -rf $(PREFIX)/share/dmpack
	$(RM) -f $(PREFIX)/dmapi
	$(RM) -f $(PREFIX)/dmbackup
	$(RM) -f $(PREFIX)/dmbeat
	$(RM) -f $(PREFIX)/dmdb
	$(RM) -f $(PREFIX)/dmdbcli
	$(RM) -f $(PREFIX)/dmexport
	$(RM) -f $(PREFIX)/dmfeed
	$(RM) -f $(PREFIX)/dmfs
	$(RM) -f $(PREFIX)/dmgraph
	$(RM) -f $(PREFIX)/dminfo
	$(RM) -f $(PREFIX)/dminit
	$(RM) -f $(PREFIX)/dmlog
	$(RM) -f $(PREFIX)/dmlogger
	$(RM) -f $(PREFIX)/dmlua
	$(RM) -f $(PREFIX)/dmpipe
	$(RM) -f $(PREFIX)/dmrecv
	$(RM) -f $(PREFIX)/dmreport
	$(RM) -f $(PREFIX)/dmserial
	$(RM) -f $(PREFIX)/dmsync
	$(RM) -f $(PREFIX)/dmuuid
	$(RM) -f $(PREFIX)/dmweb
	$(RM) -f $(PREFIX)/libdmpack.a
	$(RM) -f $(PREFIX)/libdmpack.so
	@echo "You may need to manually remove $(PREFIX)/etc/dmpack/ if it is no longer needed."

install_freebsd:
	$(MAKE) install PREFIX=/usr/local

install_linux:
	$(MAKE) install PREFIX=/usr

#
# Remove binaries, libraries, modules and object files in
# the root directory, clear "dist" and "man" directories.
#
clean:
	if [ -e $(THIN) ];   then $(RM) $(THIN); fi
	if [ -e $(TARGET) ]; then $(RM) $(TARGET); fi
	if [ -e $(SHARED) ]; then $(RM) $(SHARED); fi
	if [ `ls -1 *.mod      2>/dev/null | wc -l` -gt 0 ]; then $(RM) *.mod; fi
	if [ `ls -1 *.a        2>/dev/null | wc -l` -gt 0 ]; then $(RM) *.a; fi
	if [ `ls -1 *.so       2>/dev/null | wc -l` -gt 0 ]; then $(RM) *.so; fi
	if [ `ls -1 *.o        2>/dev/null | wc -l` -gt 0 ]; then $(RM) *.o; fi
	if [ `ls -1 dmtest*    2>/dev/null | wc -l` -gt 0 ]; then $(RM) dmtest*; fi
	if [ `ls -1 $(DISTDIR) 2>/dev/null | wc -l` -gt 0 ]; then $(RM) $(DISTDIR)/*; fi
	cd $(ADOCDIR)  && $(MAKE) clean
	cd $(GUIDEDIR) && $(MAKE) clean

#
# Additionally, clean all dependencies.
#
purge: clean
	cd vendor/fortran-curl/    && make clean TARGET=../../$(LIBFCURL)
	cd vendor/fortran-lua53/   && make clean TARGET=../../$(LIBFLUA53)
	cd vendor/fortran-pcre2/   && make clean TARGET=../../$(LIBFPCRE2)
	cd vendor/fortran-sqlite3/ && make clean TARGET=../../$(LIBFSQLITE3)
	cd vendor/fortran-unix/    && make clean TARGET=../../$(LIBFUNIX)
	cd vendor/fortran-zlib/    && make clean TARGET=../../$(LIBFZ)
	if [ -e $(INCDIR) ]; then $(RM) -r $(INCDIR); fi
