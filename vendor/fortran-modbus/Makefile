.POSIX:
.SUFFIXES:

FC      = gfortran
AR      = ar
RM      = /bin/rm
MAKE    = make
PREFIX  = /usr/local

DEBUG   = -std=f2018 -g -O0 -Wall -fmax-errors=1
RELEASE = -O2

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
	@echo "--- Installing modules to $(INCDIR)/ ..."
	install -d $(INCDIR)
	install -m 644 $(MOD) $(INCDIR)/

clean:
	$(RM) -rf *.mod
	$(RM) -rf *.o
	$(RM) -rf $(TARGET)
	$(RM) -rf test_modbus
