% DMFS(1) Version 2.0.0 | User Commands

# NAME

dmfs -- reads observations from file or named pipe

# SYNOPSIS

**dmfs** \--help

**dmfs** \--version

**dmfs** \--**config** *file* \[\--**name** *name*\] \[\--**node** *id*\]
\[\--**sensor** *id*\] \[\--**logger** *name*\] \[\--**format** *format*\]
\[\--**output** *file*\] \[\--**debug**\]

# DESCRIPTION

The **dmfs** program reads observations from file system, virtual file, or named
pipe. The program can be used to read sensor values from the 1-Wire File System
(OWFS).

All requests of an observation have to contain the path of the file to be read
in attribute `request`. Response values are extracted by group from the raw
response using the given regular expression pattern. For each group to extract,
a response with the name of the respective group has to be added to the request.

If any receivers are specified, observations are forwarded to the next receiver
via POSIX message queue. The program can act as a sole data logger if output and
format are set. If the output path is set to `-`, observations are printed to
*stdout*.

A configuration file is required to configure the jobs to perform. Each
observation must have a valid target id. The database must contain the node, the
sensor, and the target.

# OPTIONS

**\--config**, **-c** *file*

:   File path to the configuration file.

**\--debug**, **-D**

:   Forward logs messages of level `LL_DEBUG` via IPC (if logger is set).

**\--format**, **-f** \[csv\|jsonl\]

:   Output format of observations if `--output` is set. Either CSV or JSON
    Lines.

**\--help**, **-h**

:   Print available command-line arguments and quit.

**\--logger**, **-l** *name*

:   Name of logger. If set, sends logs to *dmlogger(1)* process of given name.

**\--name**, **-n** *name*

:   Name of program instance and configuration (default is `dmfs`).

**\--node**, **-N** *id*

:   Node id.

**\--output**, **-o** *file*

:   Output file to append observations to (`-` for *stdout*).

**\--sensor**, **-S** *id*

:   Sensor id.

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

    $ dmfs -n dmfs -c /usr/local/etc/dmpack/dmfs.conf -V
