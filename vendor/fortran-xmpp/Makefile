.POSIX:
.SUFFIXES:

FC      = gfortran
CC      = gcc
AR      = ar
RM      = /bin/rm
MAKE    = make
PREFIX  = /usr/local

DEBUG   = -g -O0 -Wall -fmax-errors=1 -std=f2018
RELEASE = -O2

FFLAGS  = $(RELEASE) -I$(PREFIX)/include
CFLAGS  = -O2 -I$(PREFIX)/include
LDFLAGS = -L$(PREFIX)/lib
LDLIBS  = -lstrophe -lexpat -lssl -lcrypto -lz
ARFLAGS = rcs
INCDIR  = $(PREFIX)/include/libfortran-xmpp
LIBDIR  = $(PREFIX)/lib
SRC     = src/xmpp.F90 src/xmpp_macro.c src/xmpp_util.f90
OBJ     = xmpp.o xmpp_macro.o xmpp_util.o
MOD     = xmpp.mod xmpp_util.mod
TARGET  = libfortran-xmpp.a

.PHONY: all clean debug examples install

all: $(TARGET)

examples: basic bot roster uuid

$(TARGET): $(SRC)
	$(CC) $(CFLAGS) -c src/xmpp_macro.c
	$(FC) $(FFLAGS) -c src/xmpp_util.f90
	$(FC) $(FFLAGS) -c src/xmpp.F90
	$(AR) $(ARFLAGS) $(TARGET) $(OBJ)

debug: $(SRC)
	$(MAKE) FFLAGS="$(DEBUG)"

basic: $(TARGET) examples/basic.f90
	$(FC) $(FFLAGS) $(LDFLAGS) -o basic examples/basic.f90 $(TARGET) $(LDLIBS)

bot: $(TARGET) examples/bot.f90
	$(FC) $(FFLAGS) $(LDFLAGS) -o bot examples/bot.f90 $(TARGET) $(LDLIBS)

roster: $(TARGET) examples/roster.f90
	$(FC) $(FFLAGS) $(LDFLAGS) -o roster examples/roster.f90 $(TARGET) $(LDLIBS)

uuid: $(TARGET) examples/uuid.f90
	$(FC) $(FFLAGS) $(LDFLAGS) -o uuid examples/uuid.f90 $(TARGET) $(LDLIBS)

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
	$(RM) -rf basic
	$(RM) -rf bot
	$(RM) -rf roster
	$(RM) -rf uuid
