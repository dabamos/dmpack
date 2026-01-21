% DMDB(1) Version 2.0.0 | User Commands

# NAME

dmdb -- stores observations received from message queue in database

# SYNOPSIS

**dmdb** \--help

**dmdb** \--version

**dmdb** \--**database** *file* \--**node** *id* \[\--**ipc**\] \[\--**logger**
*name*\] \[\--**debug**\] \[\--**verbose**\]

**dmdb** \--**config** *file* \[\--**name** *name*\]

# DESCRIPTION

The **dmdb** program collects observations from a POSIX message queue and stores
them in an SQLite database. The name of the message queue matches the given
**dmdb** name, by default `dmdb`. The IPC option enables process synchronisation
via POSIX semaphores. The semaphore value is increased from 0 to 1 for every
observation that has been received. The name of the semaphore equals the
**dmdb** name. Only a single process shall wait for the semaphore.

# OPTIONS

**\--config**, **-c** *file*

:   Path to configuration file.

**\--database**, **-d** *file*

:   Path to SQLite observation database.

**\--debug**, **-D**

:   Forward logs messages of level `LL_DEBUG` via IPC (if logger is set).

**\--help**, **-h**

:   Print available command-line arguments and quit.

**\--ipc**, **-Q**

:   Uses a POSIX semaphore for process synchronisation. The name of the
    semaphore matches the instance name (with leading `/`). The semaphore is set
    to 1 each time an observation has been received.  Only a single process
    shall wait for this semaphore, otherwise, reading occurs in round-robin
    fashion.

**\--logger**, **-l** *name*

:   Name of logger. If set, sends logs to *dmlogger(1)* process of given name.

**\--name**, **-n** *name*

:   Name of program instance, configuration, POSIX message queue, and POSIX
    semaphore (default is `dmdb`).

**\--node**, **-N** *id*

:   Node id.

**\--verbose**, **-V**

:   Print log messages to *stderr*.

**\--version**, **-v**

:   Print version information and quit.

# EXIT STATUS

**0**

:   Success. Program executed without errors.

**1**

:   Failure. Program execution failed.

# ENVIRONMENT

**DM_LOGGER**

:   Name of logger instance to send logs to.

**NO_COLOR**

:   Disable ANSI colour output.

# EXAMPLE

Create a message queue `/dmdb`, wait for incoming observations, and store them
in the given database:

    $ dmdb -n dmdb -N dummy-node -d /var/dmpack/observ.db -V

# SEE ALSO

*dmdbctl(1)*, *dminit(1)*, *dmsync(1)*
