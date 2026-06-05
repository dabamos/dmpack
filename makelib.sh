#!/bin/sh
#
# This script packs the "thin" DMPACK library `libdm.a` and the interface
# libraries into a "fat" static library `libdmpack.a`.
#
# To execute the script, run:
#
#   $ sh makelib.sh <target> <path>
#
# For example:
#
#   $ sh makelib.sh ./dist/libdmpack.a ./lib
#
set -e

target=${1-"./dist/libdmpack.a"}
lib=${2-"./lib"}

ar -M <<EOF
CREATE ${target}
ADDLIB ${lib}/libdm.a
ADDLIB ${lib}/libfortran-curl.a
ADDLIB ${lib}/libfortran-fast-float.a
ADDLIB ${lib}/libfortran-modbus.a
ADDLIB ${lib}/libfortran-lua54.a
ADDLIB ${lib}/libfortran-pcre2.a
ADDLIB ${lib}/libfortran-sqlite3.a
ADDLIB ${lib}/libfortran-unix.a
ADDLIB ${lib}/libfortran-xmpp.a
ADDLIB ${lib}/libfortran-zlib.a
ADDLIB ${lib}/libfortran-zmq.a
ADDLIB ${lib}/libfortran-zstd.a
SAVE
END
EOF

ranlib ${target}
