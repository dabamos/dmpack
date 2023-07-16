.POSIX:

FC      = gfortran
AR      = ar
PREFIX  = /usr
DEBUG   = #-Wall -g -O0 -fmax-errors=1
FFLAGS  = $(DEBUG)
LDLAGS  = -I$(PREFIX)/include/ -L$(PREFIX)/lib/
LDLIBS  = -lz
ARFLAGS = rcs
TARGET  = libfortran-zlib.a

.PHONY: all clean test

all: $(TARGET)

$(TARGET):
	$(FC) $(FFLAGS) -c src/zlib.f90
	$(AR) $(ARFLAGS) $(TARGET) zlib.o

test: $(TARGET)
	$(FC) $(FFLAGS) $(LDFLAGS) -o test_zlib test/test_zlib.f90 $(TARGET) $(LDLIBS)

clean:
	if [ `ls -1 *.mod 2>/dev/null | wc -l` -gt 0 ]; then rm *.mod; fi
	if [ `ls -1 *.o 2>/dev/null | wc -l` -gt 0 ]; then rm *.o; fi
	if [ -e test_zlib ]; then rm test_zlib; fi
	if [ -e $(TARGET) ]; then rm $(TARGET); fi
