% DMGRC(1) Version 2.0.0 | User Commands

# NAME

dmgrc -- creates log messages from Leica GeoCOM return codes

# SYNOPSIS

**dmgrc** \--help

**dmgrc** \--version

**dmgrc** \[\--**node** *id*\] \[\--**level** *level*\] \[\--**response**
*name*\] \[\--**name** *name*\] \[\--**logger** *name*\] \[\--**debug**\]
\[\--**verbose**\]

**dmgrc** \--**config** *file* \[\--**name** *name*\]

# DESCRIPTION

The **dmgrc** program creates log messages from Leica GeoCOM return codes.
Observations received by POSIX message queue are searched for a GeoCOM return
code (GRC) response. If the code does not equal `GRC_OK`, a log message is sent
to the configured logger instance.

By default, observation responses of name `grc` are verified. For each GeoCOM
error code, a custom log level may be specified in the configuration file.
Otherwise, the default log level is used instead.

# OPTIONS

**\--config**, **-c** *file*

:   File path to the optional configuration file.

**\--debug**, **-D**

:   Forward logs messages of level `LL_DEBUG` via IPC (if logger is set).

**\--help**, **-h**

:   Print available command-line arguments and quit.

**\--level**, **-L** *level*

:   Default log level of return codes other than `GRC_OK` (`LL_WARNING` by
    default). The argument may be an integer or name string.

**\--logger**, **-l** *name*

:   Name of *dmlogger(1)* process to send logs to.

**\--name**, **-n** *name*

:   Name of program instance and configuration (`dmgrc` by default).

**\--node**, **-N** *id*

:   Node id.

**\--response**, **-R** *name*

:   Response name of the GeoCOM return code (`grc` by default).

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

A configuration file is not required, but allows to specifiy the log level of
certain GeoCOM return codes. In the following example configuration, the default
log level for all return codes other than `GRC_OK` is set to `LL_WARNING`. The
level is further refined for specific GeoCOM codes:

    -- dmgrc.conf
    dmgrc = {
      logger = "dmlogger",
      node = "dummy-node",
      response = "grc",
      level = LL_WARNING,
      levels = {
        debug = { GRC_ABORT, GRC_SHUT_DOWN, GRC_NO_EVENT },
        info = { GRC_SLEEP_NODE, GRC_NA, GRC_STOPPED },
        warning = { GRC_TMC_ACCURACY_GUARANTEE, GRC_AUT_NO_TARGET },
        error = { GRC_FATAL },
        critical =
      },
      debug = false,
      verbose = true
    }

Pass the path of the configuration file through the command-line argument:

    $ dmgrc -n dmgrc -c /usr/local/etc/dmpack/dmgrc.conf

The name argument must match the name of the configuration table.
