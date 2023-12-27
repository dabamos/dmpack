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
VENDOR=./vendor2

LIBS="curl lua54 pcre2 sqlite3 unix zlib"

mkdir -p ${VENDOR}

for LIB in ${LIBS}; do
    LIBNAME="fortran-${LIB}"
    LIBPATH="${VENDOR}/${LIBNAME}"
    echo "Searching for ${LIBNAME} ..."
    [ -d "${LIBPATH}/Makefile" ] && echo "Error: ${LIBNAME} exists" && exit 1
    LIBURL="https://codeload.github.com/interkosmos/${LIBNAME}/zip/refs/heads/master"
    LIBMASTER="${LIBNAME}-master"
    LIBZIP="${LIBMASTER}.zip"
    LIBFILE="${VENDOR}/${LIBZIP}"
    LIBDIR="${VENDOR}/${LIBNAME}"
    echo "Fetching ${LIBZIP} ..."
    curl ${LIBURL} -s -o ${LIBFILE}
    echo "Unpacking ${LIBZIP} ..."
    ${UNZIP} -q -d ${VENDOR} ${LIBFILE}
    echo "Deleting ${LIBZIP} ..."
    rm ${LIBFILE}
    echo "Renaming directory ..."
    mv "${VENDOR}/${LIBMASTER}" ${LIBDIR}
done
