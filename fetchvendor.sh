#!/bin/sh
#
# Auxiliary script that downloads the 3rd-party libraries into directory
# `vendor/`, if the source code repository has not been cloned recursively with
# Git. The following libraries are fetched and unpacked:
#
# * fortran-curl
# * fortran-lua54
# * fortran-modbus
# * fortran-pcre2
# * fortran-sqlite3
# * fortran-unix
# * fortran-xmpp
# * fortran-zlib
# * fortran-zstd
#
# The curl(1) and unzip(1) command-line programs are required.
#
set -e

VENDOR=${1-"./vendor"}
CURL=curl
UNZIP=unzip
LIBS="curl lua54 modbus pcre2 sqlite3 unix xmpp zlib zstd"

echo "Creating ${VENDOR} ..."
mkdir -p ${VENDOR}

for LIB in ${LIBS}; do
    LIBNAME="fortran-${LIB}"
    LIBPATH="${VENDOR}/${LIBNAME}"
    echo "Searching for ${LIBNAME} ..."
    [ -e "${LIBPATH}/Makefile" ] && echo "Error: ${LIBNAME} exists" && exit 1
    [ -d ${LIBPATH} ] && rm -r ${LIBPATH} && echo "Deleted ${LIBPATH}"
    LIBURL="https://codeload.github.com/interkosmos/${LIBNAME}/zip/refs/heads/master"
    LIBMASTER="${LIBNAME}-master"
    LIBZIP="${LIBMASTER}.zip"
    LIBFILE="${VENDOR}/${LIBZIP}"
    LIBDIR="${VENDOR}/${LIBNAME}"
    echo "Fetching ${LIBZIP} ..."
    ${CURL} -L -s -o ${LIBFILE} ${LIBURL}
    echo "Unpacking ${LIBZIP} ..."
    ${UNZIP} -q -d ${VENDOR} ${LIBFILE}
    echo "Deleting ${LIBZIP} ..."
    rm ${LIBFILE}
    echo "Renaming directory ..."
    mv "${VENDOR}/${LIBMASTER}" ${LIBDIR}
done
