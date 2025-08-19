.POSIX:
.SUFFIXES:

FC      = gfortran
AR      = ar
PREFIX  = /usr/local

DEBUG   = -g -O0 -Wall -fmax-errors=1
RELEASE = -O2

FFLAGS  = $(RELEASE)
LDFLAGS = -I$(PREFIX)/include -L$(PREFIX)/lib
LDLIBS  = -lz
ARFLAGS = rcs
INCDIR  = $(PREFIX)/include/libfortran-zlib
LIBDIR  = $(PREFIX)/lib
MODULE  = zlib.mod
TARGET  = ./libfortran-zlib.a
SHARED  = ./libfortran-zlib.so

.PHONY: all clean install shared test test_shared

all: $(TARGET)

shared: $(SHARED)

$(TARGET): src/zlib.f90
	$(FC) $(FFLAGS) -c src/zlib.f90
	$(AR) $(ARFLAGS) $(TARGET) zlib.o

$(SHARED): src/zlib.f90
	$(FC) $(FFLAGS) -fPIC -shared -o $(SHARED) src/zlib.f90 $(LDLIBS)

test: $(TARGET) test/test_zlib.f90
	$(FC) $(FFLAGS) $(LDFLAGS) -o test_zlib test/test_zlib.f90 $(TARGET) $(LDLIBS)

test_shared: $(SHARED) test/test_zlib.f90
	$(FC) $(FFLAGS) $(LDFLAGS) -o test_zlib_shared test/test_zlib.f90 $(SHARED) $(LDLIBS)

install: $(TARGET)
	@echo "--- Installing libraries to $(LIBDIR)/ ..."
	install -d $(LIBDIR)
	install -m 644 $(TARGET) $(LIBDIR)/
	if [ -e $(SHARED) ]; then install -m 644 $(SHARED) $(LIBDIR)/; fi
	@echo "--- Installing module files to $(INCDIR)/ ..."
	install -d $(INCDIR)
	install -m 644 $(MODULE) $(INCDIR)/

clean:
	if [ `ls -1 *.mod 2>/dev/null | wc -l` -gt 0 ]; then rm *.mod; fi
	if [ `ls -1 *.o 2>/dev/null | wc -l` -gt 0 ]; then rm *.o; fi
	if [ -e $(TARGET) ]; then rm $(TARGET); fi
	if [ -e $(SHARED) ]; then rm $(SHARED); fi
	if [ -e test_zlib ]; then rm test_zlib; fi
	if [ -e test_zlib_shared ]; then rm test_zlib_shared; fi
	if [ -e test2.txt ]; then rm test2.txt; fi
	if [ -e test.txt.z ]; then rm test.txt.z; fi
