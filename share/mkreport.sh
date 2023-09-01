#!/bin/sh

#
#   mkreport.sh - creates monthly reports
#
#   A auxiliary shell script that runs dmreport(1) to create a report of the
#   previous and the current month.
#
#   Example:
#
#   $ sh mkreport.sh
#   --- Writing report from 2023-08-01 to 2023-08-31 to file /var/www/reports/2023-08_report.html ...
#   --- Writing report from 2023-09-01 to 2023-09-30 to file /var/www/reports/2023-09_report.html ...
#

set -e

dmreport="/usr/local/bin/dmreport"
name="dmreport"
config="/usr/local/etc/dmpack/dmreport.conf"
output="/var/www/reports/"

createReport () {
  first=${1}
  last=${2}

  prefix=`echo "${first}" | cut -c 1-7`
  file_name="${prefix}_report.html"
  path="${output}${file_name}"

  from="${first}T00:00:00.000+00:00"
  to="${last}T23:59:59.999+00:00"

  echo "--- Writing report from ${first} to ${last} to file ${path} ..."
  ${dmreport} -n ${name} -c "${config}" -o "${path}" -B "${from}" -E "${to}"
}

last_month_first=`date -v -1m "+%Y-%m-01"`
last_month_last=`date -v -1d "+%Y-%m-%d"`

this_month_first=`date +"%Y-%m-01"`
this_month_last=`date -v +1m -v -1d +"%Y-%m-%d"`

createReport $last_month_first $last_month_last
createReport $this_month_first $this_month_last
