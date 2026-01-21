% DMPIPE(1) Version 2.0.0 | User Commands

# NAME

dmpipe -- reads observations from sub-process

# SYNOPSIS

**dmpipe** \--help

**dmpipe** \--version

**dmpipe** \--**config** *file* \[\--**name** *name*\] \[\--**node**
*id*\] \[\--**sensor** *id*\] \[\--**logger** *name*\] \[\--**debug**\]
\[\--**verbose**\]

# DESCRIPTION

The **dmpipe** program reads responses from a process connected via anonymous
pipe.

Each request of an observation must contain the process to execute in attribute
`request`. Response values are extracted by group from the raw response using
the provided regular expression pattern. A response has to be added for each
group in the pattern. The response name must equal the group name.

If any receivers are specified, observations are forwarded to the next receiver
via POSIX message queue. The program can act as a sole data logger if output and
format are set. If the output path is set to `-`, observations are printed to
*stdout*.

A configuration file is required to configure the jobs to perform. Each
observation must have a valid target id. The database must contain node, sensor,
and target of all observations configured.

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

:   Name of program instance and configuration (default is `dmpipe`).

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

Read the jobs from configuration file and execute them sequentially:

    $ dmpipe -n dmpipe -c /usr/local/etc/dmpack/dmpipe.conf -V
