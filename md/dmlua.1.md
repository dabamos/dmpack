% DMLUA(1) Version 2.0.0 | User Commands

# NAME

dmlua -- passes received observations to Lua function

# SYNOPSIS

**dmlua** \--help

**dmlua** \--version

**dmlua** \--**node** *id* \--**script** *file* \[\--**procedure** *name*\]
\[\--**name** *name*\] \[\--**logger** *name*\] \[\--**debug**\]
\[\--**verbose**\]

**dmlua** \--**config** *file* \[\--**name** *name*\]

# DESCRIPTION

The **dmlua** program runs a custom Lua script to process observations received
from POSIX message queue. Each observation is passed as a Lua table to the Lua
function of the name given in option `procedure`. The function name `process` is
assumed by default. The function must return the (modified) observation table on
exit.

The observation returned from the Lua function will be forwarded to the next
specified receiver or discarded if no receivers are left.

The Lua script file will be executed after loading. Any code outside of the Lua
function will be evaluated once to allow an (optional) initialisation of the
script.

# OPTIONS

**\--config**, **-c** *file*

:   File path to the optional configuration file.

**\--debug**, **-D**

:   Forward logs messages of level `LL_DEBUG` via IPC (if logger is set).

**\--help**, **-h**

:   Print available command-line arguments and quit.

**\--logger**, **-l** *name*

:   Name of logger. If set, sends logs to *dmlogger(1)* process of given name.

**\--name**, **-n** *name*

:   Name of program instance and configuration (default is `dmlua`).

**\--node**, **-N** *id*

:   Node id.

**\--procedure**, **-p** *name*

:   Lua function name (default is `process`).

**\--script**, **-s** *file*

:   Path to Lua script file.

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

Wait for an observation on POSIX message queue `/dmlua`, pass it as Lua table
`observ` to function `process(observ)` in `script.lua`, then forward the result
to the next receiver:

    $ dmlua -n dmlua -N dummy-node -s script.lua
