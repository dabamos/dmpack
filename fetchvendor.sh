#!/bin/sh
#
# Auxiliary script that downloads the 3rd-party libraries into directory
# `vendor/`, if the source code repository has not been cloned recursively with
# Git. The following libraries are fetched and unpacked:
#
# * fortran-curl
# * fortran-lua54
# * fortran-pcre2
# * fortran-sqlite3
# * fortran-unix
# * fortran-zlib
#
# The curl(1) and unzip(1) command-line programs are required.
#
set -e

UNZIP=unzip
VENDOR=./vendor

LIBS="curl lua54 pcre2 sqlite3 unix zlib"

for LIB in ${LIBS}; do
    LIBPATH="${VENDOR}/fortran-${LIB}"
    echo "Searching for ${LIBPATH}/ ..."
    [ -d ${LIBPATH} ] && echo "Error: directory exists" && exit 1
    LIBURL="https://codeload.github.com/interkosmos/fortran-${LIB}/zip/refs/heads/master"
    LIBZIP="fortran-${LIB}-master.zip"
    LIBFILE="${VENDOR}/${LIBZIP}"
    LIBDIR="${VENDOR}/fortran-${LIB}"
    echo "Fetching ${LIBZIP} ..."
    curl ${LIBURL} -s -o ${LIBFILE}
    echo "Unpacking ${LIBZIP} ..."
    ${UNZIP} -q -d ${VENDOR} ${LIBFILE}
    mv "${VENDOR}/fortran-${LIB}-master" ${LIBDIR}
    echo "Deleting ${LIBZIP} ..."
    rm ${LIBFILE}
done
