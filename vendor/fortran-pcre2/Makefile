.POSIX:
.SUFFIXES:

FC      = gfortran
PREFIX  = /usr/local

DEBUG   = -g -O0 -Wall -fmax-errors=1
RELEASE = -O2

FFLAGS  = $(RELEASE)
LDFLAGS = -I$(PREFIX)/include -L$(PREFIX)/lib
LDLIBS  = -lpcre2-8
ARFLAGS = rcs
INCDIR  = $(PREFIX)/include/libfortran-pcre2
LIBDIR  = $(PREFIX)/lib
MODULE  = pcre2.mod
TARGET  = libfortran-pcre2.a
TEST    = test_pcre2

.PHONY: all clean install test

all: $(TARGET)

$(TARGET): src/pcre2.F90
	$(FC) $(FFLAGS) -c src/pcre2.F90
	$(AR) $(ARFLAGS) $(TARGET) pcre2.o

install: $(TARGET)
	@echo "--- Installing $(TARGET) to $(LIBDIR)/ ..."
	install -d $(LIBDIR)
	install -m 644 $(TARGET) $(LIBDIR)/
	@echo "--- Installing module files to $(INCDIR)/ ..."
	install -d $(INCDIR)
	install -m 644 $(MODULE) $(INCDIR)/

test:
	$(FC) $(FFLAGS) $(LDFLAGS) -o $(TEST) test/test_pcre2.f90 $(TARGET) $(LDLIBS)

clean:
	if [ `ls -1 *.mod 2>/dev/null | wc -l` -gt 0 ]; then rm *.mod; fi
	if [ `ls -1 *.o 2>/dev/null | wc -l` -gt 0 ]; then rm *.o; fi
	if [ -e $(TARGET) ]; then rm $(TARGET); fi
	if [ -e $(TEST) ]; then rm $(TEST); fi
