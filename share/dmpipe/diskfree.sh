#!/bin/sh

# diskfree.sh - pre-formats disk usage
#
# Prints available and used disk space in bytes, as well as the capacity in
# percent to standard output, by default of file system `/`.
#
# Pass the desired file system as the first argument:
#
#   $ sh diskfree.sh /var
#   435790304 25466624 6
#
# File system `/var` has a maximum size of 208 GiB, of which 12 GiB or 6 % are
# currently used.
#
# The script may be executed by dmpipe(1) to read the file system statistics
# periodically as observations into DMPACK.

set -e

FS=${1-"/"}

df ${FS} | sed "1d" | awk "{ print $4, $3, $5 }" | sed "s/%//"
