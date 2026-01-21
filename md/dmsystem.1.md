% DMSYSTEM(1) Version 2.0.0 | User Commands

# NAME

dmsystem -- monitors system parameters

# SYNOPSIS

**dmsystem** \--help

**dmsystem** \--version

**dmsystem** \--**node** *id* \--**sensor** *id* \--**target** *id*
\[\--**receiver** *name*\] \[\--**interval** *seconds*\] \[\--**count** *n*\]
\[\--**name** *name*\] \[\--**logger** *name*\] \[\--**debug**\]
\[\--**verbose**\]

**dmsystem** \--**config** *file* \[\--**name** *name*\]

# DESCRIPTION

A system monitor to watch free disk space and disk capacity, database sizes,
load average, CPU temperature, and system uptime. The file system for the free
disk space and disk capacity responses is determined through a given file or
directory path. The paths to the log and observation databases have to be given
in the configuration file.

cpu_temp

:   CPU temperature \[°C\]. Disabled by default.

disk_free

:   Free disk space \[Byte\]. Requires file or directory path.

disk_capacity

:   Disk capacity \[%\]. Requires file or directory path.

load_avg1

:   Load average, last minute. Enabled by default.

load_avg5

:   Load average, last 5 minutes. Enabled by default.

load_avg15

:   Load average, last 15 minutes. Enabled by default.

uptime

:   System uptime \[sec\]. Enabled by default.

# OPTIONS

**\--config**, **-c** *file*

:   File path to the configuration file.

**\--count**, **-C** *n*

:   Number of observations to create. No limit if count is set to 0 (default).

**\--debug**, **-D**

:   Forward logs messages of level `LL_DEBUG` via IPC (if logger is set).

**\--help**, **-h**

:   Print available command-line arguments and quit.

**\--interval**, **-I** *seconds*

:   Interval in seconds in which to read the system parameters.

**\--logger**, **-l** *name*

:   Name of logger. If set, sends logs to *dmlogger(1)* process of given name.

**\--name**, **-n** *name*

:   Name of program instance and configuration (default is `dmsystem`).

**\--node**, **-N** *id*

:   Node id.

**\--receiver**, **-r** *name*

:   Name of the observation receiver, without leading `/`.

**\--sensor**, **-S** *id*

:   Sensor id.

**\--target**, **-T** *id*

:   Target id.

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

Read system parameters in an interval of 60 seconds and forward observations to
`dmrecv`:

    $ dmsystem -N dummy-node -S dummy-sensor -T dummy-target \
      -I 60 -r dmrecv -V

Start `dmrecv` with:

    $ dmrecv -n dmrecv -t observ -f jsonl -o "-" -V
