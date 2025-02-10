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
LIBS="curl lua54 modbus pcre2 sqlite3 unix xmpp zlib zstd"
PROFILE="interkosmos"

CURL=curl
MKDIR=/bin/mkdir
MV=/bin/mv
RM=/bin/rm
UNZIP=unzip

echo "Creating ${VENDOR} ..."
${MKDIR} -p ${VENDOR}

for LIB in ${LIBS}; do
    LIBNAME="fortran-${LIB}"
    LIBPATH="${VENDOR}/${LIBNAME}"

    echo "Searching for ${LIBNAME} ..."

    [ -e "${LIBPATH}/Makefile" ] && echo "Warning: ${LIBNAME} exists"
    [ -d "${LIBPATH}" ]          && ${RM} -r "${LIBPATH}" && echo "Deleted ${LIBPATH}"

    LIBURL="https://codeload.github.com/${PROFILE}/${LIBNAME}/zip/refs/heads/master"
    LIBMASTER="${LIBNAME}-master"
    LIBZIP="${LIBMASTER}.zip"
    LIBFILE="${VENDOR}/${LIBZIP}"
    LIBDIR="${VENDOR}/${LIBNAME}"

    echo "Fetching ${LIBZIP} ..."
    ${CURL} -L -s -o ${LIBFILE} ${LIBURL}

    echo "Unpacking ${LIBZIP} ..."
    ${UNZIP} -q -d ${VENDOR} ${LIBFILE}

    echo "Deleting ${LIBZIP} ..."
    ${RM} ${LIBFILE}

    echo "Renaming directory ..."
    ${MV} "${VENDOR}/${LIBMASTER}" ${LIBDIR}
done
