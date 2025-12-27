#!/usr/bin/env tclsh8.6
#
# A script to create monitoring report of the current month with dmreport(1).
# If today is the first day of a month, the report of the previous month is
# (re-)created as well in case sensor data arrived late at the server.
#
# The argument --format should start with the format groups "%Y-%m" to add the
# current year and month as a file name prefix, for example "%Y-%m_report.pdf".
# See <https://www.tcl-lang.org/man/tcl8.6/TclCmd/clock.htm> for an overview of
# accepted format groups.
#
# This script requires tcllib.
#
# Available command-line arguments:
#
#   --config <path> - Path to the dmreport configuration file.
#   --format <name> - Format of output file name.
#   --logger <name> - Name of dmlogger instance for log forwarding.
#   --name <name>   - Name of the dmreport configuration table.
#   --output <path> - Output directory of reports.
#
# Create a single report from configuration "dmreport" in dmreport.conf:
#
#   $ ./mkreport.tcl --config dmreport.conf --format pdf --output reports/
#
# Create multiple reports from configuration "dmreport1" (PDF) and "dmreport2"
# (HTML) in file dmreport.conf with GNU Parallel:
#
#   $ parallel --jobs 0 ::: \
#         "./mkreport.tcl --name dmreport1 --config dmreport.conf --output reports/ --format %Y-%m_report.pdf" \
#         "./mkreport.tcl --name dmreport2 --config dmreport.conf --output reports/ --format %Y-%m_report.html"
#
package require cmdline

# Options.
variable config /usr/local/etc/dmpack/dmreport.conf
variable format %Y-%m_report.pdf
variable logger {}
variable name   dmreport
variable output ./

# Executables.
variable dmlog    /usr/local/bin/dmlog
variable dmreport /usr/local/bin/dmreport

# Parse command-line arguments.
#
proc get-opts {} {
    variable argv

    variable config
    variable format
    variable logger
    variable name
    variable output

    set options [list \
        [list config.arg $config "dmreport configuration file"] \
        [list format.arg $format "output file format"] \
        [list logger.arg $logger "logger name"] \
        [list name.arg   $name   "dmreport configuraton table"] \
        [list output.arg $output "output directory"] \
    ]

    set usage ": [file tail [info script]] \[options] ...\noptions:"

    try {
        array set params [::cmdline::getoptions argv $options $usage]
        # Note: argv is modified now. The recognized options are
        # removed from it, leaving the non-option arguments behind.
        set config  $params(config)
        set format  $params(format)
        set logger  $params(logger)
        set name    $params(name)
        set output  $params(output)
    } trap {CMDLINE USAGE} {msg o} {
        # Trap the usage signal, print the message, and exit the application.
        # Note: Other errors are not caught and passed through to higher levels!
        puts $msg
        exit 1
    }

    if {![file exists $config]} {
        log error "configuration file $config not found"
        exit 1
    }

    if {![string length $format]} {
        log error "invalid output file format"
        exit 1
    }

    if {![file isdirectory $output]} {
        log error "invalid output directory"
        exit 1
    }
}

# Send log message to logger.
#
proc log {level msg} {
    variable dmlog
    variable logger

    set src [file rootname [file tail [info script]]]

    if {[string length $logger]} {
        exec -ignorestderr -- $dmlog -D -Z $src -l $logger -L $level -m "$msg"
    } else {
        exec -ignorestderr -- $dmlog -V -Z $src -L $level -m "$msg"
    }
}

# Create report of given time range.
#
proc make {name config output format first last} {
    variable dmreport

    set fn   [clock format [clock scan $first -format {%Y-%m-%d}] -format $format]
    set path [file join $output $fn]

    set iso  "T00:00:00.000000+00:00"
    set from "$first$iso"
    set to   "$last$iso"

    if {[catch {exec -ignorestderr -- $dmreport -n $name -c $config -o $path -B $from -E $to} result]} {
        log error "failed to create report $path"
        return 1
    }

    log debug "report from $first to $last written to $path"
    return 0
}

# Read command-line arguments.
if {[catch {get-opts}]} {puts stderr $::errorInfo; exit 1}

# Calculate dates.
set now [clock seconds]
set day [clock format $now -format {%d}]

set last1 [clock format [clock scan {- 1 month} -base [clock scan [clock format $now -format {%Y-%m-01}]]] -format {%Y-%m-%d}]
set this1 [clock format $now -format {%Y-%m-01}]
set next1 [clock format [clock scan {+ 1 month} -base [clock scan [clock format $now -format {%Y-%m-01}]]] -format {%Y-%m-%d}]

# Create report of the current month.
make $name $config $output $format $this1 $next1

# On the first day of a month, additionally create report of the last month.
if {$day == 01} {
    make $name $config $output $format $last1 $this1
}
