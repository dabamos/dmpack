.POSIX:
.SUFFIXES:

FC      = gfortran
AR      = ar
RM      = /bin/rm
MAKE    = make
PREFIX  = /usr/local

DEBUG   = -g -O0 -Wall -fmax-errors=1
RELEASE = -O2

FFLAGS  = $(RELEASE)
LDFLAGS = -I$(PREFIX)/include -L$(PREFIX)/lib
LDLIBS  = -lmodbus
ARFLAGS = rcs
INCDIR  = $(PREFIX)/include/libfortran-modbus
LIBDIR  = $(PREFIX)/lib
SRC     = src/fast_float_module.F90
OBJ     = fast_float_module.o
MOD     = fast_float_module.mod
TARGET  = libfortran-fast-float.a

.PHONY: all clean install

all: $(TARGET)

$(TARGET): $(SRC)
	$(FC) $(FFLAGS) -c $(SRC)
	$(AR) $(ARFLAGS) $(TARGET) $(OBJ)

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
