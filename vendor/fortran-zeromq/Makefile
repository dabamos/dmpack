.POSIX:

PREFIX = /usr/local

CC   = gcc
FC   = gfortran
AR   = ar
RM   = /bin/rm
MAKE = make
FORD = ford

DEBUG   = -g -O0 -Wall -std=f2018 -fmax-errors=1
RELEASE = -O2

CFLAGS  = $(RELEASE)
FFLAGS  = $(RELEASE)
ARFLAGS = rcs
LDFLAGS = -L$(PREFIX)/lib
LIBCZMQ = -lczmq
LIBZMQ  = -lzmq
INCDIR  = $(PREFIX)/include/libfortran-zeromq
LIBDIR  = $(PREFIX)/lib

LIBFCZMQ = libfortran-czmq.a
LIBFZMQ  = libfortran-zmq.a
TARGET   = libfortran-zeromq.a

SRC_ZMQ  = src/zmq.F90 \
           src/zmq_util.F90
SRC_CZMQ = src/czmq.f90 \
           src/czmq_macro.c \
           src/czmq_zclock.f90 \
           src/czmq_zframe.f90 \
           src/czmq_zmsg.f90 \
           src/czmq_zsock.f90 \
           src/czmq_zstr.f90 \
           src/zmq_util.F90
OBJ_ZMQ  = zmq.o \
           zmq_util.o
OBJ_CZMQ = czmq.o \
           czmq_macro.o \
           czmq_zclock.o \
           czmq_zframe.o \
           czmq_zmsg.o \
           czmq_zsock.o \
           czmq_zstr.o \
           zmq_util.o

SRC = $(SRC_CZMQ) src/zmq.F90 src/zeromq.f90
OBJ = $(OBJ_CZMQ) zmq.o zeromq.o

EXAMPLES = mspoller \
           pair \
           rrbroker \
           rrclient \
           rrworker \
           zmsg_client \
           zmsg_server \
           zsock_client \
           zsock_server

.PHONY: all clean czmq doc examples install zmq

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Libraries
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
all: $(TARGET)
zmq: $(LIBFZMQ)
czmq: $(LIBFCZMQ)

$(LIBFZMQ): $(SRC_ZMQ)
	$(MAKE) zmq_util.o
	$(MAKE) zmq.o
	$(AR) $(ARFLAGS) $(LIBFZMQ) $(OBJ_ZMQ)

$(LIBFCZMQ): $(SRC_CZMQ)
	$(MAKE) zmq_util.o
	$(MAKE) czmq_macro.o
	$(MAKE) czmq_zclock.o
	$(MAKE) czmq_zframe.o
	$(MAKE) czmq_zmsg.o
	$(MAKE) czmq_zsock.o
	$(MAKE) czmq_zstr.o
	$(MAKE) czmq.o
	$(AR) $(ARFLAGS) $(LIBFCZMQ) $(OBJ_CZMQ)

$(TARGET): $(SRC)
	$(MAKE) zmq
	$(MAKE) czmq
	$(MAKE) zeromq.o
	$(AR) $(ARFLAGS) $(TARGET) $(OBJ)

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Object Files
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
czmq.o: src/czmq.f90
	$(FC) $(FFLAGS) -c src/czmq.f90

czmq_macro.o: src/czmq_macro.c
	$(CC) $(CFLAGS) -c src/czmq_macro.c

czmq_zclock.o: src/czmq_zclock.f90
	$(FC) $(FFLAGS) -c src/czmq_zclock.f90

czmq_zframe.o: src/czmq_zframe.f90
	$(FC) $(FFLAGS) -c src/czmq_zframe.f90

czmq_zmsg.o: src/czmq_zmsg.f90
	$(FC) $(FFLAGS) -c src/czmq_zmsg.f90

czmq_zsock.o: src/czmq_zsock.f90
	$(FC) $(FFLAGS) -c src/czmq_zsock.f90

czmq_zstr.o: src/czmq_zstr.f90
	$(FC) $(FFLAGS) -c src/czmq_zstr.f90

zeromq.o: src/zeromq.f90
	$(FC) $(FFLAGS) -c src/zeromq.f90

zmq.o: src/zmq.F90
	$(FC) $(FFLAGS) -c src/zmq.F90

zmq_util.o: src/zmq_util.F90
	$(FC) $(FFLAGS) -c src/zmq_util.F90

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Examples
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
examples: $(EXAMPLES)

mspoller: $(LIBFZMQ) examples/mspoller.f90
	$(FC) $(FFLAGS) $(LDFLAGS) -o mspoller examples/mspoller.f90 $(LIBFZMQ) $(LIBZMQ)

pair: $(LIBFZMQ) examples/pair.f90
	$(FC) $(FFLAGS) $(LDFLAGS) -fopenmp -o pair examples/pair.f90 $(LIBFZMQ) $(LIBZMQ)

rrbroker: $(LIBFZMQ) examples/rrbroker.f90
	$(FC) $(FFLAGS) $(LDFLAGS) -o rrbroker examples/rrbroker.f90 $(LIBFZMQ) $(LIBZMQ)

rrclient: $(LIBFZMQ) examples/rrclient.f90
	$(FC) $(FFLAGS) $(LDFLAGS) -o rrclient examples/rrclient.f90 $(LIBFZMQ) $(LIBZMQ)

rrworker: $(LIBFZMQ) examples/rrworker.f90
	$(FC) $(FFLAGS) $(LDFLAGS) -o rrworker examples/rrworker.f90 $(LIBFZMQ) $(LIBZMQ)

zmsg_client: $(TARGET) examples/zmsg_client.f90
	$(FC) $(FFLAGS) $(LDFLAGS) -o zmsg_client examples/zmsg_client.f90 $(TARGET) $(LIBZMQ) $(LIBCZMQ)

zmsg_server: $(TARGET) examples/zmsg_server.f90
	$(FC) $(FFLAGS) $(LDFLAGS) -o zmsg_server examples/zmsg_server.f90 $(TARGET) $(LIBZMQ) $(LIBCZMQ)

zsock_client: $(TARGET) examples/zsock_client.f90
	$(FC) $(FFLAGS) $(LDFLAGS) -o zsock_client examples/zsock_client.f90 $(TARGET) $(LIBZMQ) $(LIBCZMQ)

zsock_server: $(TARGET) examples/zsock_server.f90
	$(FC) $(FFLAGS) $(LDFLAGS) -o zsock_server examples/zsock_server.f90 $(TARGET) $(LIBZMQ) $(LIBCZMQ)

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Documentation
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
doc:
	$(FORD) ford.md

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Installation
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
install: $(TARGET)
	@echo "--- Installing $(TARGET) to $(LIBDIR)/ ..."
	install -d $(LIBDIR)
	install -m 644 $(LIBFZMQ) $(LIBDIR)/
	if [ -f "$(LIBFCZMQ)" ]; then; install -m 644 $(LIBFCZMQ) $(LIBDIR)/; fi
	if [ -f "$(TARGET)"   ]; then; install -m 644 $(TARGET)   $(LIBDIR)/; fi
	@echo "--- Installing module files to $(INCDIR)/ ..."
	install -d $(INCDIR)
	install -m 644 zmq.mod $(INCDIR)/
	install -m 644 zmq_util.mod $(INCDIR)/
	if [ -f "czmq.mod"        ]; then; install -m 644 czmq.mod        $(INCDIR)/
	if [ -f "czmq_zclock.mod" ]; then; install -m 644 czmq_zclock.mod $(INCDIR)/
	if [ -f "czmq_zframe.mod" ]; then; install -m 644 czmq_zframe.mod $(INCDIR)/
	if [ -f "czmq_zmsg.mod"   ]; then; install -m 644 czmq_zmsg.mod   $(INCDIR)/
	if [ -f "czmq_zsock.mod"  ]; then; install -m 644 czmq_zsock.mod  $(INCDIR)/
	if [ -f "czmq_zstr.mod"   ]; then; install -m 644 czmq_zstr.mod   $(INCDIR)/
	if [ -f "zeromq.mod"      ]; then; install -m 644 zeromq.mod      $(INCDIR)/

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Cleaning
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
clean:
	$(RM) -rf *.mod
	$(RM) -rf *.o
	$(RM) -rf $(LIBFCZMQ)
	$(RM) -rf $(LIBFZMQ)
	$(RM) -rf $(TARGET)
	$(RM) -rf pair
	$(RM) -rf rrbroker
	$(RM) -rf rrclient
	$(RM) -rf rrworker
	$(RM) -rf zmsg_client
	$(RM) -rf zmsg_server
	$(RM) -rf zsock_client
	$(RM) -rf zsock_server
	$(RM) -rf mspoller
