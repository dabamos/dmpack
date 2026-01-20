.POSIX:
.SUFFIXES:

FC      = gfortran
AR      = ar
RM      = /bin/rm
MAKE    = make
PREFIX  = /usr/local

DEBUG   = -g -O0 -Wall -fmax-errors=1 -std=f2018
RELEASE = -O2

FFLAGS  = $(RELEASE)
LDFLAGS = -I$(PREFIX)/include -L$(PREFIX)/lib
LDLIBS  = -lzstd
ARFLAGS = rcs
INCDIR  = $(PREFIX)/include/libfortran-zstd
LIBDIR  = $(PREFIX)/lib
SRC     = src/zstd.F90
MODULE  = zstd.mod
TARGET  = ./libfortran-zstd.a
SHARED  = ./libfortran-zstd.so

.PHONY: all clean debug install shared test test_shared

all: $(TARGET)

shared: $(SHARED)

debug:
	$(MAKE) FFLAGS="$(DEBUG)"
	$(MAKE) test FFLAGS="$(DEBUG)"

$(TARGET): $(SRC)
	$(FC) $(FFLAGS) -c $(SRC)
	$(AR) $(ARFLAGS) $(TARGET) zstd.o

$(SHARED): $(SRC)
	$(FC) $(FFLAGS) -fPIC -shared -o $(SHARED) $(SRC) $(LDLIBS)

test: $(TARGET) test/test_zstd.f90
	$(FC) $(FFLAGS) $(LDFLAGS) -o test_zstd test/test_zstd.f90 $(TARGET) $(LDLIBS)

test_shared: $(SHARED) test/test_zstd.f90
	$(FC) $(FFLAGS) $(LDFLAGS) -o test_zstd_shared test/test_zstd.f90 $(SHARED) $(LDLIBS)

install: $(TARGET)
	@echo "--- Installing library to $(LIBDIR)/ ..."
	install -d $(LIBDIR)
	install -m 644 $(TARGET) $(LIBDIR)/
	if [ -e $(SHARED) ]; then install -m 644 $(SHARED) $(LIBDIR)/; fi
	@echo "--- Installing module to $(INCDIR)/ ..."
	install -d $(INCDIR)
	install -m 644 $(MODULE) $(INCDIR)/

clean:
	$(RM) -rf *.mod
	$(RM) -rf *.o
	$(RM) -rf $(TARGET)
	$(RM) -rf $(SHARED)
	$(RM) -rf test_zstd
	$(RM) -rf test_zstd_shared
