#!/bin/sh
#
# This script runs all DMPACK test programs and prints the results to standard
# output. Simply run:
#
#   $ sh ./runtests.sh
#
# You may want to uncomment and set the following environment variables:
#
# export DM_API_HOST=localhost
# export DM_API_USERNAME=alice
# export DM_API_PASSWORD=secret
#
# export DM_MQTT_HOST=localhost
# export DM_MQTT_PORT=1883
#
# export DM_MAIL_FROM=alice@localhost
# export DM_MAIL_TO=bob@localhost
# export DM_MAIL_HOST=localhost
# export DM_MAIL_USERNAME=alice
# export DM_MAIL_PASSWORD=secret
#
TESTS="dmtestapi dmtestbase64 dmtestcgi dmtestcsv dmtestdb dmtestdp dmtesthash \
dmtesthtml dmtestlogger dmtestlua dmtestjob dmtestjson dmtestmail dmtestmqueue \
dmtestmqtt dmtestnml dmtestobserv dmtestpath dmtestpipe dmtestplot dmtestregex \
dmtestrouter dmtestrpc dmteststring dmtesttime dmtestunit dmtestuuid"
NTEST=`echo ${TESTS} | wc -w`
NFAIL=0

for TEST in ${TESTS}; do
    ./${TEST}
    if [ $? -ne 0 ]; then
        NFAIL=`expr ${NFAIL} + 1`
    fi
done

printf "\n************************************************************************\n"

if [ ${NFAIL} -eq 0 ]; then
    printf "ALL %s TEST PROGRAMS FINISHED SUCCESSFULLY!\n" ${NTEST}
else
    printf "\033[31m%s OF %s TEST PROGRAMS FAILED!\033[39m\n" ${NFAIL} ${NTEST}
fi

printf "************************************************************************\n\n"
