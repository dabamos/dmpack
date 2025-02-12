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

TARGET=${1-"./dist/libdmpack.a"}
LIB=${2-"./lib"}

ar -M <<EOF
CREATE ${TARGET}
ADDLIB ${LIB}/libdm.a
ADDLIB ${LIB}/libfortran-curl.a
ADDLIB ${LIB}/libfortran-modbus.a
ADDLIB ${LIB}/libfortran-lua54.a
ADDLIB ${LIB}/libfortran-pcre2.a
ADDLIB ${LIB}/libfortran-sqlite3.a
ADDLIB ${LIB}/libfortran-unix.a
ADDLIB ${LIB}/libfortran-xmpp.a
ADDLIB ${LIB}/libfortran-zlib.a
ADDLIB ${LIB}/libfortran-zstd.a
SAVE
END
EOF

ranlib ${TARGET}
