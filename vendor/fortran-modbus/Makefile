.POSIX:
.SUFFIXES:

FC      = gfortran
AR      = ar
MAKE    = make
PREFIX  = /usr/local

DEBUG   = -std=f2018 -g -O0 -Wall -fmax-errors=1
RELEASE = -std=f2018 -O2

FFLAGS  = $(RELEASE)
LDFLAGS = -I$(PREFIX)/include -L$(PREFIX)/lib
LDLIBS  = -lmodbus
ARFLAGS = rcs
INCDIR  = $(PREFIX)/include/libfortran-modbus
LIBDIR  = $(PREFIX)/lib
SRC     = src/modbus.F90 src/modbus_rtu.f90 src/modbus_tcp.f90
OBJ     = modbus.o modbus_rtu.o modbus_tcp.o
MOD     = modbus.mod modbus_rtu.mod modbus_tcp.mod
TARGET  = ./libfortran-modbus.a

.PHONY: all clean debug install test

all: $(TARGET)

debug:
	$(MAKE) FFLAGS="$(DEBUG)"
	$(MAKE) test FFLAGS="$(DEBUG)"

$(TARGET): $(SRC)
	$(FC) $(FFLAGS) -c src/modbus.F90
	$(FC) $(FFLAGS) -c src/modbus_rtu.f90
	$(FC) $(FFLAGS) -c src/modbus_tcp.f90
	$(AR) $(ARFLAGS) $(TARGET) $(OBJ)

test: $(TARGET) test/test_modbus.f90
	$(FC) $(FFLAGS) $(LDFLAGS) -o test_modbus test/test_modbus.f90 $(TARGET) $(LDLIBS)

install: $(TARGET)
	@echo "--- Installing library to $(LIBDIR)/ ..."
	install -d $(LIBDIR)
	install -m 644 $(TARGET) $(LIBDIR)/
	if [ -e $(SHARED) ]; then install -m 644 $(SHARED) $(LIBDIR)/; fi
	@echo "--- Installing modules to $(INCDIR)/ ..."
	install -d $(INCDIR)
	install -m 644 $(MOD) $(INCDIR)/

clean:
	if [ `ls -1 *.mod 2>/dev/null | wc -l` -gt 0 ]; then rm *.mod; fi
	if [ `ls -1 *.o 2>/dev/null | wc -l` -gt 0 ]; then rm *.o; fi
	if [ -e $(TARGET) ]; then rm $(TARGET); fi
	if [ -e test_modbus ]; then rm test_modbus; fi
