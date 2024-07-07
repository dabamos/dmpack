#!/bin/sh
#
# This script packs the "thin" DMPACK library `libdm.a` and the interface
# libraries into a "fat" static library `libdmpack.a`.
#
# To execute the script, run:
#
#   $ sh makelib.sh <fat library> <thin library>
#
# For example:
#
#   $ sh makelib.sh ./dist/libdmpack.a ./lib/libdm.a
#
set -e

TARGET=${1-"./dist/libdmpack.a"}
THIN=${2-"./lib/libdm.a"}

ar -M <<EOF
CREATE ${TARGET}
ADDLIB ${THIN}
ADDLIB ./lib/libfortran-curl.a
ADDLIB ./lib/libfortran-modbus.a
ADDLIB ./lib/libfortran-lua54.a
ADDLIB ./lib/libfortran-pcre2.a
ADDLIB ./lib/libfortran-sqlite3.a
ADDLIB ./lib/libfortran-unix.a
ADDLIB ./lib/libfortran-zlib.a
ADDLIB ./lib/libfortran-zstd.a
SAVE
END
EOF

ranlib ${TARGET}
