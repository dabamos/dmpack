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
tests=$(/bin/ls | grep "^dmtest")
ntest=$(echo ${tests} | wc -w)

fails=""
nfail=0

if [ ${ntest} -eq 0 ]; then
    echo "No tests found!"
    exit 1
fi

for test in ${tests}; do
    ./${test}
    if [ $? -ne 0 ]; then
        nfail=$(expr ${nfail} + 1)
        fails="${fails} ${test}"
        echo "++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++"
        printf "TEST %s FAILED!\n" ${test}
        echo "++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++"
    fi
done

if [ ${nfail} -eq 0 ]; then
    echo "------------------------------------------------------------------------"
    printf "ALL %s TEST PROGRAMS FINISHED SUCCESSFULLY!\n" ${ntest}
    echo "------------------------------------------------------------------------"
else
    echo "++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++"
    printf "%s OF %s TEST PROGRAMS failED:\n" ${nfail} ${ntest}
    for fail in ${fails}; do
        printf "${fail}\n"
    done
    echo "++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++"
fi

echo "User and system times used by this script:"
times
echo "++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++"

if [ ${nfail} -gt 0 ]; then
    exit 1
fi
