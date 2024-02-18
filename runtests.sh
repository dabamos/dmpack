#!/bin/sh
#
# This script runs all DMPACK test programs and prints the results to standard
# output. Simply run:
#
#   $ sh runtests.sh
#
# You may want to uncomment and set the following environment variables:
#
# export DM_API_HOST=localhost
# export DM_API_USERNAME=dummy-node
# export DM_API_PASSWORD=secret
#
# export DM_MQTT_HOST=localhost
# export DM_MQTT_PORT=1883
#
# export DM_MAIL_FROM=alice@example.com
# export DM_MAIL_TO=bob@example.com
# export DM_MAIL_HOST=example.com
# export DM_MAIL_USERNAME=alice
# export DM_MAIL_PASSWORD=secret
#
TESTS="dmtestapi dmtestbase64 dmtestcgi dmtestconfig dmtestcsv dmtestdb \
dmtestdp dmtesthash dmtesthdf5 dmtesthtml dmtestlogger dmtestlua dmtestjob \
dmtestjson dmtestmail dmtestmqueue dmtestmqtt dmtestnml dmtestobserv dmtestpath \
dmtestpipe dmtestplot dmtestregex dmtestrpc dmtestrts dmteststring dmtesttime \
dmtesttty dmtestunit dmtestutil dmtestuuid"
NTEST=`echo ${TESTS} | wc -w`
NFAIL=0

for TEST in ${TESTS}; do
    ./${TEST}
    if [ $? -ne 0 ]; then
        NFAIL=`expr ${NFAIL} + 1`
    fi
done

printf "++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++\n"

if [ ${NFAIL} -eq 0 ]; then
    printf "ALL %s TEST PROGRAMS FINISHED SUCCESSFULLY!\n" ${NTEST}
else
    printf "\033[31m%s OF %s TEST PROGRAMS FAILED!\033[39m\n" ${NFAIL} ${NTEST}
fi

printf "++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++\n"
printf "User and system times used by this script:\n"
times
printf "++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++\n\n"

if [ ${NFAIL} -gt 0 ]; then
    exit 1
fi
