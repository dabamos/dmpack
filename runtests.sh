#!/bin/sh
#
# This script runs all DMPACK test programs and prints the results to standard
# output. Simply run:
#
#   $ sh runtests.sh
#
# You may want to uncomment and set the following environment variables.
#
# HTTP-RPC API host and credentials.
#
#   export DM_API_HOST=localhost
#   export DM_API_USERNAME=dummy-node
#   export DM_API_PASSWORD=secret
#
# MQTT server settings:
#
#   export DM_MQTT_HOST=localhost
#   export DM_MQTT_PORT=1883
#
# E-mail and SMTP settings:
#
#   export DM_MAIL_FROM=alice@example.com
#   export DM_MAIL_TO=bob@example.com
#   export DM_MAIL_HOST=example.com
#   export DM_MAIL_USERNAME=alice
#   export DM_MAIL_PASSWORD=secret
#
# Enable DWD API tests:
#
#   export DM_DWD_API=1
#
# Skipping POSIX message queue tests:
#
#   export DM_MQUEUE_SKIP=1
#
# Skipping pipe tests:
#
#   export DM_PIPE_SKIP=1
#
# Skipping GraphicsMagick tests:
#
#   export DM_GM_SKIP=1
#
TESTS=`/bin/ls | grep "^dmtest"`
NTEST=`echo ${TESTS} | wc -w`

FAILS=""
NFAIL=0

for TEST in ${TESTS}; do
    ./${TEST}
    if [ $? -ne 0 ]; then
        NFAIL=`expr ${NFAIL} + 1`
        FAILS="${FAILS} ${TEST}"
        echo "++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++"
        printf "TEST %s FAILED!\n" ${TEST}
        echo "++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++"
    fi
done


if [ ${NFAIL} -eq 0 ]; then
    echo "------------------------------------------------------------------------"
    printf "ALL %s TEST PROGRAMS FINISHED SUCCESSFULLY!\n" ${NTEST}
    echo "------------------------------------------------------------------------"
else
    echo "++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++"
    printf "%s OF %s TEST PROGRAMS FAILED:\n" ${NFAIL} ${NTEST}
    for FAIL in ${FAILS}; do
        printf "${FAIL}\n"
    done
    echo "++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++"
fi

echo "User and system times used by this script:"
times
echo "++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++"

if [ ${NFAIL} -gt 0 ]; then
    exit 1
fi
