% DMLOGGER(1) Version 2.0.0 | User Commands

# NAME

dmlogger -- inserts collected log messages into database

# SYNOPSIS

**dmlogger** \--help

**dmlogger** \--version

**dmlogger** \--**node** *id* \[\--**database** *file*\] \[\--**output**
*file*\] \[\--**minlevel** *n*\] \[\--**ipc**\] \[\--**verbose**\]

**dmlogger** \--**config** *file* \[\--**name** *name*\]

# DESCRIPTION

The **dmlogger** program collects log messages from a POSIX message queue and
inserts them into a SQLite database. The name of the message queue matches the
given **dmlogger** name, by default `/dmlogger`.

If a minimum log level is selected, only logs of a level greater equal the
minimum are written to log file or log database. Log messages with lower level
are discarded.

The option `--ipc` enables process synchronisation via POSIX semaphores.  The
semaphore value is incremented from 0 to 1 whenever a new log has been
delivered. The name of the semaphore equals the **dmlogger** name with leading
`/`. Only a single process shall wait for the semaphore unless round-robin IPC
is desired. This option may be used to synchronise incoming log messages
automatically with a remote RPC API server. For example, pass the **dmlogger**
name to *dmsync(1)* through command-line argument `--wait`. The semaphore will
then trigger the log synchronisation.

# OPTIONS

**\--config**, **-c** *file*

:   Path to configuration file.

**\--database**, **-d** *file*

:   Path to SQLite log database.

**\--help**, **-h**

:   Print available command-line arguments and quit.

**\--ipc**, **-Q**

:   Use POSIX semaphore for process synchronisation. The name of the semaphore
    matches the instance name (with leading slash). The semaphore is set to 1
    each time a log message has been received.  Only a single process shall wait
    for this semaphore.

**\--minlevel**, **-L** *level*

:   Minimum level for a log to be stored in the database, from 1 (`LL_DEBUG`) to
    5 (`LL_CRITICAL`). The argument may be an integer or name string.

**\--name**, **-n** *name*

:   Name of logger instance, configuration, POSIX message queue, and POSIX
    semaphore (default is `dmlogger`).

**\--node**, **-N** *id*

:   Node id.

**\--output**, **-o** *file*

:   Path of log file. If the file does not exist, a new log file will be
    created. Logs are appended to the end of the file. If set to `-`, logs are
    printed to *stderr* (equals argument `--verbose`).

**\--verbose**, **-V**

:   Print received logs to *stderr*.

**\--version**, **-v**

:   Print version information and quit.

# EXIT STATUS

**0**

:   Success. Program executed without errors.

**1**

:   Failure. Program execution failed.

# ENVIRONMENT

**NO_COLOR**

:   Disable ANSI colour output.

# EXAMPLE

Create a POSIX message queue `/dmlogger` and wait for incoming logs:

    $ dmlogger -N dummy-node -d /var/dmpack/log.db -L info

Received logs are inserted only if they are of level *info* or higher.

# SEE ALSO

*dmlog(1)*, *dmsync(1)*
