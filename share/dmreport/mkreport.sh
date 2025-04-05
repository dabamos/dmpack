#!/bin/sh

# mkreport.sh - creates monthly reports
#
# An auxiliary shell script that runs dmreport(1) to create a report of the
# the this month and the last month on the first day of the month.
#
# Example:
#
# $ sh mkreport.sh
# --- Writing report of 2023-08 to file /var/www/reports/2023-08_report.html ...
# --- Writing report if 2023-09 to file /var/www/reports/2023-09_report.html ...

set -e

dmreport="/usr/local/bin/dmreport"
name="dmreport"
config="/usr/local/etc/dmpack/dmreport.conf"
output="/var/www/reports/"

create_report () {
  first=${1}
  last=${2}

  prefix=`echo "${first}" | cut -c 1-7`
  file_name="${prefix}_report.html"
  path="${output}${file_name}"

  from="${first}T00:00:00.000000+00:00"
  to="${last}T00:00:00.000000+00:00"

  echo "--- Writing report of ${prefix} to file ${path} ..."
  ${dmreport} -n ${name} -c "${config}" -o "${path}" -B "${from}" -E "${to}"
}

last_month_first=`date -v -1m +"%Y-%m-01"`
this_month_first=`date +"%Y-%m-01"`
next_month_first=`date -v +1m +"%Y-%m-01"`
this_month_day=`date +"%d"`

# create report of the current month
create_report $this_month_first $next_month_first

# create report of the last month
if [ "$this_month_day" == "01" ]; then
  create_report $last_month_first $this_month_first
fi
