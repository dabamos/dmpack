.POSIX:
.SUFFIXES:

CC   = gcc
FC   = gfortran
AR   = ar
RM   = /bin/rm
MAKE = make

DEBUG   = -std=f2008 -g -O0 -Wall -fmax-errors=1
RELEASE = -O2

CFLAGS  = $(RELEASE) `pkg-config --cflags lua-5.4`
FFLAGS  = $(RELEASE) `pkg-config --cflags lua-5.4`
ARFLAGS = rcs
LDFLAGS = `pkg-config --libs-only-L lua-5.4`
LDLIBS  = `pkg-config --libs-only-l lua-5.4`
INCDIR  = $(PREFIX)/include/libfortran-lua54
LIBDIR  = $(PREFIX)/lib
MODULE  = lua.mod
TARGET  = libfortran-lua54.a

.PHONY: all clean debug examples install test

all: $(TARGET)

debug:
	$(MAKE) RELEASE="$(DEBUG)"
	$(MAKE) examples RELEASE="$(DEBUG)"

test: types

examples: fibonacci libfortran.so string table

$(TARGET): src/lua.f90
	$(FC) $(FFLAGS) -fPIC -c src/lua.f90
	$(AR) $(ARFLAGS) $(TARGET) lua.o

types: test/types.c
	$(CC) $(CFLAGS) -o types test/types.c $(LDFLAGS)

fibonacci: $(TARGET) examples/fibonacci.f90
	$(FC) $(FFLAGS) $(LDFLAGS) -o fibonacci examples/fibonacci.f90 $(TARGET) $(LDLIBS)

libfortran.so: $(TARGET) examples/libfortran.f90
	$(FC) $(FFLAGS) $(LDFLAGS) -shared -fPIC -o libfortran.so examples/libfortran.f90 $(TARGET)

string: $(TARGET) examples/string.f90
	$(FC) $(FFLAGS) $(LDFLAGS) -o string examples/string.f90 $(TARGET) $(LDLIBS)

table: $(TARGET) examples/table.f90
	$(FC) $(FFLAGS) $(LDFLAGS) -o table examples/table.f90 $(TARGET) $(LDLIBS)

install: $(TARGET)
	@echo "--- Installing $(TARGET) to $(LIBDIR)/ ..."
	install -d $(LIBDIR)
	install -m 644 $(TARGET) $(LIBDIR)/
	@echo "--- Installing module files to $(INCDIR)/ ..."
	install -d $(INCDIR)
	install -m 644 $(MODULE) $(INCDIR)/

clean:
	$(RM) -rf *.mod
	$(RM) -rf *.o
	$(RM) -rf $(TARGET)
	$(RM) -rf types
	$(RM) -rf fibonacci
	$(RM) -rf libfortran.so
	$(RM) -rf string
	$(RM) -rf table
