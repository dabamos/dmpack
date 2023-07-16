.POSIX:
.SUFFIXES:

# DMPACK build flags:
#
#   OS      - The operating system, either `freebsd` or `linux`.
#   PREFIX  - Path prefix, `/usr/local` on FreeBSD, `/usr` on Linux.
#   INCDIR  - Include directory that contains the Fortran module files.
#   LIBDIR  - Library directory that contains the static libraries.
#   DISTDIR - Distribution file directory (libraries and programs).
#   THIN    - Thin DMPACK library (without interface bindings).
#   TARGET  - Path to the full DMPACK library (with interface bindings).
#   SHARED  - Path to the shared DMPACK library (with interface bindings).
#
#   FC      - Fortran 2018 compiler.
#   CC      - C compiler.
#   AR      - Archiver.
#   MAKE    - Either: `make`, `bmake`, or `gmake`.
#
#   DEBUG   - Optional debugging flags.
#   FFLAGS  - Fortran compiler flags.
#   CLAGS   - C compiler flags.
#   ARFLAGS - Archiver flags.
#   LPFLAGS - LAPACK95 flags.
#   LDFLAGS - Linker flags.
#   LDLIBS  - Linker libraries.

OS      = freebsd
PREFIX  = /usr/local
INCDIR  = ./include
LIBDIR  = ./lib
DISTDIR = ./dist
THIN    = $(LIBDIR)/libdm.a
TARGET  = $(DISTDIR)/libdmpack.a
SHARED  = $(DISTDIR)/libdmpack.so

FC      = gfortran
CC      = gcc
AR      = ar
MAKE    = make

DEBUG   = -g -O0 -Wall -fmax-errors=1 -fbacktrace #-ffpe-trap=invalid,zero,overflow -fno-omit-frame-pointer
RELEASE = $(DEBUG) #-mtune=native -O2 -ffast-math
FFLAGS  = $(RELEASE) -std=f2018
CFLAGS  =
ARFLAGS = -rcs
LPFLAGS = -I/usr/local/lib/lapack95_modules/
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
LIBFCURL    = lib/libfortran-curl.a
LIBFLUA53   = lib/libfortran-lua53.a
LIBFPCRE2   = lib/libfortran-pcre2.a
LIBFSQLITE3 = lib/libfortran-sqlite3.a
LIBFUNIX    = lib/libfortran-unix.a
LIBFZ       = lib/libfortran-zlib.a
LIBF        = $(LIBFCURL) $(LIBFLUA53) $(LIBFPCRE2) $(LIBFSQLITE3) $(LIBFUNIX) $(LIBFZ)

# Documentation.
FORD     = ford
DOCDIR   = ./doc
ADOCDIR  = ./adoc
GUIDEDIR = ./guide

# Object files.
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
      dm_html.o \
      dm_atom.o \
      dm_router.o \
      dm_transform.o \
      dmpack.o

.PHONY: all app clean doc freebsd guide linux man purge setup test \
    dmapi dmbackup dmbeat dmdb dmdbcli dmexport dmfeed dmfs dmgraph dminfo \
    dminit dmlog dmlogger dmlua dmpipe dmrecv dmreport dmsync dmuuid dmweb

all: $(TARGET) $(SHARED) test app

app: dmapi dmbackup dmbeat dmdb dmdbcli dmexport dmfeed dmfs dmgraph dminfo \
    dminit dmlog dmlogger dmlua dmpipe dmrecv dmreport dmsync dmuuid dmweb

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
$(TARGET): setup $(LIBF)
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
	$(FC) $(FFLAGS) $(LDFLAGS) -fPIC -c src/dm_html.f90
	$(FC) $(FFLAGS) $(LDFLAGS) -fPIC -c src/dm_atom.f90
	$(FC) $(FFLAGS) $(LDFLAGS) -fPIC -c src/dm_router.f90
	$(FC) $(FFLAGS) $(LDFLAGS) $(LPFLAGS) -fPIC -c src/dm_transform.f90
	$(FC) $(FFLAGS) $(LDFLAGS) -fPIC -c src/dmpack.f90
	$(AR) $(ARFLAGS) $(THIN) $(OBJ)
	sh ./makelib.sh $(TARGET) $(THIN)

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
test:
	$(FC) $(FFLAGS) $(LDFLAGS) -o dmtestapi    test/dmtestapi.f90    $(TARGET) $(LDLIBS)
	$(FC) $(FFLAGS) $(LDFLAGS) -o dmtestatom   test/dmtestatom.f90   $(TARGET) $(LDLIBS)
	$(FC) $(FFLAGS) $(LDFLAGS) -o dmtestbase64 test/dmtestbase64.f90 $(TARGET) $(LDLIBS)
	$(FC) $(FFLAGS) $(LDFLAGS) -o dmtestcgi    test/dmtestcgi.f90    $(TARGET) $(LDLIBS)
	$(FC) $(FFLAGS) $(LDFLAGS) -o dmtestcsv    test/dmtestcsv.f90    $(TARGET) $(LDLIBS)
	$(FC) $(FFLAGS) $(LDFLAGS) -o dmtestdb     test/dmtestdb.f90     $(TARGET) $(LDLIBS) $(LIBSQLITE3)
	$(FC) $(FFLAGS) $(LDFLAGS) -o dmtestdp     test/dmtestdp.f90     $(TARGET) $(LDLIBS) $(LIBSQLITE3)
	$(FC) $(FFLAGS) $(LDFLAGS) -o dmtesthash   test/dmtesthash.f90   $(TARGET) $(LDLIBS)
	$(FC) $(FFLAGS) $(LDFLAGS) -o dmtesthtml   test/dmtesthtml.f90   $(TARGET) $(LDLIBS)
	$(FC) $(FFLAGS) $(LDFLAGS) -o dmtestlogger test/dmtestlogger.f90 $(TARGET) $(LDLIBS) $(LIBRT)
	$(FC) $(FFLAGS) $(LDFLAGS) -o dmtestlua    test/dmtestlua.f90    $(TARGET) $(LDLIBS) $(LIBLUA53)
	$(FC) $(FFLAGS) $(LDFLAGS) -o dmtestjob    test/dmtestjob.f90    $(TARGET) $(LDLIBS)
	$(FC) $(FFLAGS) $(LDFLAGS) -o dmtestjson   test/dmtestjson.f90   $(TARGET) $(LDLIBS)
	$(FC) $(FFLAGS) $(LDFLAGS) -o dmtestmail   test/dmtestmail.f90   $(TARGET) $(LDLIBS) $(LIBCURL)
	$(FC) $(FFLAGS) $(LDFLAGS) -o dmtestmqtt   test/dmtestmqtt.f90   $(TARGET) $(LDLIBS) $(LIBCURL)
	$(FC) $(FFLAGS) $(LDFLAGS) -o dmtestmqueue test/dmtestmqueue.f90 $(TARGET) $(LDLIBS) $(LIBRT)
	$(FC) $(FFLAGS) $(LDFLAGS) -o dmtestnml    test/dmtestnml.f90    $(TARGET) $(LDLIBS)
	$(FC) $(FFLAGS) $(LDFLAGS) -o dmtestobserv test/dmtestobserv.f90 $(TARGET) $(LDLIBS)
	$(FC) $(FFLAGS) $(LDFLAGS) -o dmtestpath   test/dmtestpath.f90   $(TARGET) $(LDLIBS)
	$(FC) $(FFLAGS) $(LDFLAGS) -o dmtestpipe   test/dmtestpipe.f90   $(TARGET) $(LDLIBS)
	$(FC) $(FFLAGS) $(LDFLAGS) -o dmtestplot   test/dmtestplot.f90   $(TARGET) $(LDLIBS)
	$(FC) $(FFLAGS) $(LDFLAGS) -o dmtestregex  test/dmtestregex.f90  $(TARGET) $(LDLIBS) $(LIBPCRE2)
	$(FC) $(FFLAGS) $(LDFLAGS) -o dmtestrouter test/dmtestrouter.f90 $(TARGET) $(LDLIBS)
	$(FC) $(FFLAGS) $(LDFLAGS) -o dmtestrpc    test/dmtestrpc.f90    $(TARGET) $(LDLIBS) $(LIBCURL) $(LIBZ)
	$(FC) $(FFLAGS) $(LDFLAGS) -o dmteststring test/dmteststring.f90 $(TARGET) $(LDLIBS)
	$(FC) $(FFLAGS) $(LDFLAGS) -o dmtesttime   test/dmtesttime.f90   $(TARGET) $(LDLIBS)
	$(FC) $(FFLAGS) $(LDFLAGS) -o dmtesttty    test/dmtesttty.f90    $(TARGET) $(LDLIBS)
	$(FC) $(FFLAGS) $(LDFLAGS) -o dmtestunit   test/dmtestunit.f90   $(TARGET) $(LDLIBS)
	$(FC) $(FFLAGS) $(LDFLAGS) -o dmtestuuid   test/dmtestuuid.f90   $(TARGET) $(LDLIBS)
	$(FC) $(FFLAGS) $(LDFLAGS) -o dmtestz      test/dmtestz.f90      $(TARGET) $(LDLIBS) $(LIBZ)

#
# DMPACK programs.
#
dmapi:
	$(FC) $(FFLAGS) $(LDFLAGS) -o $(DISTDIR)/dmapi app/dmapi.f90 $(TARGET) $(LDLIBS) $(LIBSQLITE3) $(LIBZ) $(LIBFASTCGI)

dmbackup:
	$(FC) $(FFLAGS) $(LDFLAGS) -o $(DISTDIR)/dmbackup app/dmbackup.f90 $(TARGET) $(LDLIBS) $(LIBSQLITE3)

dmbeat:
	$(FC) $(FFLAGS) $(LDFLAGS) -o $(DISTDIR)/dmbeat app/dmbeat.f90 $(TARGET) $(LDLIBS) $(LIBCURL) $(LIBLUA53) $(LIBZ) $(LIBRT)

dmdb:
	$(FC) $(FFLAGS) $(LDFLAGS) -o $(DISTDIR)/dmdb app/dmdb.f90 $(TARGET) $(LDLIBS) $(LIBLUA53) $(LIBSQLITE3) $(LIBRT)

dmdbcli:
	$(FC) $(FFLAGS) $(LDFLAGS) -o $(DISTDIR)/dmdbcli app/dmdbcli.f90 $(TARGET) $(LDLIBS) $(LIBSQLITE3)

dmexport:
	$(FC) $(FFLAGS) $(LDFLAGS) -o $(DISTDIR)/dmexport app/dmexport.f90 $(TARGET) $(LDLIBS) $(LIBSQLITE3)

dmfeed:
	$(FC) $(FFLAGS) $(LDFLAGS) -o $(DISTDIR)/dmfeed app/dmfeed.f90 $(TARGET) $(LDLIBS) $(LIBLUA53) $(LIBSQLITE3)

dmfs:
	$(FC) $(FFLAGS) $(LDFLAGS) -o $(DISTDIR)/dmfs app/dmfs.f90 $(TARGET) $(LDLIBS) $(LIBLUA53) $(LIBPCRE2) $(LIBRT)

dmgraph:
	$(FC) $(FFLAGS) $(LDFLAGS) -o $(DISTDIR)/dmgraph app/dmgraph.f90 $(TARGET) $(LDLIBS) $(LIBLUA53) $(LIBSQLITE3)

dminfo:
	$(FC) $(FFLAGS) $(LDFLAGS) -o $(DISTDIR)/dminfo app/dminfo.f90 $(TARGET) $(LDLIBS) $(LIBSQLITE3)

dminit:
	$(FC) $(FFLAGS) $(LDFLAGS) -o $(DISTDIR)/dminit app/dminit.f90 $(TARGET) $(LDLIBS) $(LIBSQLITE3)

dmlog:
	$(FC) $(FFLAGS) $(LDFLAGS) -o $(DISTDIR)/dmlog app/dmlog.f90 $(TARGET) $(LDLIBS) $(LIBRT)

dmlogger:
	$(FC) $(FFLAGS) $(LDFLAGS) -o $(DISTDIR)/dmlogger app/dmlogger.f90 $(TARGET) $(LDLIBS) $(LIBLUA53) $(LIBSQLITE3) $(LIBRT)

dmlua:
	$(FC) $(FFLAGS) $(LDFLAGS) -o $(DISTDIR)/dmlua app/dmlua.f90 $(TARGET) $(LDLIBS) $(LIBLUA53) $(LIBRT)

dmpipe:
	$(FC) $(FFLAGS) $(LDFLAGS) -o $(DISTDIR)/dmpipe app/dmpipe.f90 $(TARGET) $(LDLIBS) $(LIBLUA53) $(LIBPCRE2) $(LIBRT)

dmrecv:
	$(FC) $(FFLAGS) $(LDFLAGS) -o $(DISTDIR)/dmrecv app/dmrecv.f90 $(TARGET) $(LDLIBS) $(LIBLUA53) $(LIBRT)

dmreport:
	$(FC) $(FFLAGS) $(LDFLAGS) -o $(DISTDIR)/dmreport app/dmreport.f90 $(TARGET) $(LDLIBS) $(LIBLUA53) $(LIBSQLITE3)

dmsync:
	$(FC) $(FFLAGS) $(LDFLAGS) -o $(DISTDIR)/dmsync app/dmsync.f90 $(TARGET) $(LDLIBS) $(LIBCURL) $(LIBLUA53) $(LIBSQLITE3) $(LIBZ) $(LIBRT)

dmuuid:
	$(FC) $(FFLAGS) $(LDFLAGS) -o $(DISTDIR)/dmuuid app/dmuuid.f90 $(TARGET) $(LDLIBS)

dmweb:
	$(FC) $(FFLAGS) $(LDFLAGS) -o $(DISTDIR)/dmweb app/dmweb.f90 $(TARGET) $(LDLIBS) $(LIBSQLITE3)

#
# Source code documentation, manual pages, and User’s Guide.
#
doc:
	$(FORD) project.md -d ./src

man:
	cd $(ADOCDIR) && $(MAKE) man

guide:
	cd $(GUIDEDIR) && $(MAKE)

#
# Remove binaries, libraries, modules and object files in
# the root directory, clear "dist" and "man" directories.
#
clean:
	if [ -e $(THIN) ];   then rm $(THIN); fi
	if [ -e $(TARGET) ]; then rm $(TARGET); fi
	if [ -e $(SHARED) ]; then rm $(SHARED); fi
	if [ `ls -1 *.mod      2>/dev/null | wc -l` -gt 0 ]; then rm *.mod; fi
	if [ `ls -1 *.a        2>/dev/null | wc -l` -gt 0 ]; then rm *.a; fi
	if [ `ls -1 *.so       2>/dev/null | wc -l` -gt 0 ]; then rm *.so; fi
	if [ `ls -1 *.o        2>/dev/null | wc -l` -gt 0 ]; then rm *.o; fi
	if [ `ls -1 dmtest*    2>/dev/null | wc -l` -gt 0 ]; then rm dmtest*; fi
	if [ `ls -1 $(DISTDIR) 2>/dev/null | wc -l` -gt 0 ]; then rm $(DISTDIR)/*; fi
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
	if [ -e $(INCDIR) ]; then rm -r $(INCDIR); fi
