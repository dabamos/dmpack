% DMLOG(1) Version 2.0.0 | User Commands

# NAME

dmlog -- sends log to POSIX message queue

# SYNOPSIS

**dmlog** \--help

**dmlog** \--version

**dmlog** \--**node** *id* \--**message** *string* \[\--**logger** *name*\]
\[\--**level** *level*\] \[\--**error** *n*\] \[\--**node** *id*\]
\[\--**sensor** *id*\] \[\--**target** *id*\] \[\--**observ** *id*\]
\[\--**source** *source*\] \[\--**debug**\]

# DESCRIPTION

The **dmlog** utility forwards a log message to the POSIX message queue of a
*dmlogger(1)* instance. Argument `--message` is mandatory. Pass the name of the
*dmlogger* or *dmrecv* instance to send the log to through command-line argument
`--logger`. Alternatively, set environment variable `DMLOGGER` to the name of
the logger. The command-line argument overwrites the environment variable.#

The default log level is `info` (2). Valid log levels are `debug` (1), `info`
(2), `warning` (3), `error` (4), `critical` (5), and `user` (6). The program
terminates after log transmission.

# OPTIONS

**\--debug**, **-D**

:   Send log message of level `debug` to logger.

**\--error**, **-e** *n*

:   DMPACK error code (\>= 0).

**\--help**, **-h**

:   Print available command-line arguments and quit.

**\--level**, **-L** *level*

:   Log level, from `debug` (1) to `user` (6) The default level is `info` (2).
    The argument may be an integer or log level name string.

**\--logger**, **-l** *name*

:   Name of logger instance and POSIX message queue. If the command-line
    argument is not passed and the environment variable `DMLOGGER` is not set,
    the log messages will not be transmitted via POSIX message queue.

**\--message**, **-m** *string*

:   Log message (max. 512 characters).

**\--node**, **-N** *id*

:   Node id.

**\--observ**, **-O** *id*

:   Observation id (UUIDv4).

**\--sensor**, **-S** *id*

:   Sensor id.

**\--source**, **-Z** *source*

:   Name of log message source (sender).

**\--target**, **-T** *id*

:   Target id.

**\--verbose**, **-V**

:   Print log to *stderr*.

**\--version**, **-v**

:   Print version information and quit.

# EXIT STATUS

**0**

:   Success. Log has been sent.

**1**

:   Failure. Program execution failed.

# ENVIRONMENT

**DM_LOGGER**

:   Name of logger instance to send logs to.

**NO_COLOR**

:   Disable ANSI colour output.

# EXAMPLE

Send a log message to the POSIX message queue of logger `dmlogger`:

    $ export DMLOGGER=dmlogger
    $ dmlog -N dummy-node -L warning -m "low battery" -Z test -V
    2022-12-09T22:50:44.161000+01:00 [WARNING ] test - low battery

The logger will receive the log message and insert it into the log database (if
the log level is greater or equal the minimum log level):

    $ dmlogger -N dummy-node -d log.db -V
    2022-12-09T22:50:44.161000+01:00 [WARNING] test - low battery

# SEE ALSO

*dmlogger(1)*
