% DMSERIAL(1) Version 2.0.0 | User Commands

# NAME

dmserial -- reads observations from TTY/PTY

# SYNOPSIS

**dmserial** \--help

**dmserial** \--version

**dmserial** \--**config** *file* \[\--**name** *name*\] \[\--**node** *id*\]
\[\--**sensor** *id*\] \[\--**logger** *name*\] \[\--**path** *path*\]
\[\--**baudrate** *n*\] \[\--**bytesize** *n*\] \[\--**stopbits** *n*\]
\[\--**parity** *name*\] \[\--**timeout** *n*\] \[\--**format** *name*\]
\[\--**output** *file*\] \[\--**dtr**\] \[\--**rts**\] \[\--**debug**\]
\[\--**verbose**\]

# DESCRIPTION

The **dmserial** program sends requests to a sensor connected via
USB/RS-232/RS-422/RS-485. Sensor commands and responses are sent/received
through a teletype (TTY) device provided by the operating system. A
pseudo-terminal (PTY) may be used to connect a virtual sensor.

Each request of an observation must contain the raw request intended for the
sensor in attribute `request`. Response values are extracted by group from the
raw response using the given regular expression pattern.  Each group name must
match a response name. Response names are limited to 32 characters.

Observations will be forwarded to the next receiver via POSIX message queue if
any receiver is specified. The program can act as a sole data logger if output
and format are set. If the output path is set to `-`, observations are printed
to *stdout*, else to file.

A configuration file is required to configure the jobs to perform. Each
observation must have a valid target id. The database must contain the specified
node, sensor, and targets. The observation requests may be created by using
GeoCOM API functions.

# OPTIONS

**\--baudrate**, **-B** *n*

:   Number of symbols transmitted per second. The following baud rates are
    supported: 50, 75, 110, 134, 150, 200, 300, 600, 1200, 1800, 2400, 4800,
    9600, 19200, 38400, 57600, 115200, 230400, 460800, 921600.

**\--bytesize**, **-Z** *n*

:   Byte size (5, 6, 7, 8).

**\--config**, **-c** *file*

:   File path to the configuration file.

**\--debug**, **-D**

:   Forward logs messages of level `LL_DEBUG` via IPC (if logger is set).

**\--dtr**, **-Q**

:   Enable Data Terminal Ready (DTR).

**\--format**, **-f** \[csv\|jsonl\]

:   Output format of observations if `--output` is set. Either CSV or JSON
    Lines.

**\--help**, **-h**

:   Print available command-line arguments and quit.

**\--logger**, **-l** *name*

:   Name of logger. If set, sends logs to *dmlogger(1)* process of given name.

**\--name**, **-n** *name*

:   Name of program instance and configuration (default is `dmserial`).

**\--node**, **-N** *id*

:   Node id.

**\--output**, **-o** *file*

:   Output file to append observations to (`-` for *stdout*).

**\--parity**, **-P** \[none\|even\|odd\]

:   Parity bits (`none`, `even`, or `odd`).

**\--rts**, **-R**

:   Enable Request To Send (RTS).

**\--sensor**, **-S** *id*

:   Sensor id.

**\--stopbits**, **-O** *n*

:   Number of stop bits (1, 2).

**\--timeout**, **-T** *n*

:   Connection timeout in seconds.

**\--path**, **-p** *path*

:   Path to TTY/PTY device (for example, `/dev/ttyUSB0`).

**\--verbose**, **-V**

:   Print log messages to *stderr*.

**\--version**, **-v**

:   Print version information and quit.

# EXIT STATUS

**0**

:   Success. Process terminated without errors.

**1**

:   Failure. Process failed.

# ENVIRONMENT

**DM_LOGGER**

:   Name of logger instance to send logs to.

**NO_COLOR**

:   Disable ANSI colour output.

# EXAMPLE

Read the jobs to perform from configuration file and execute them sequentially:

    $ dmserial -n dmserial -c /usr/local/etc/dmpack/dmserial.conf -V
