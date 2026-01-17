% DMSEND(1) Version 2.0.0 | User Commands

# NAME

dmsend -- sends observations and logs to POSIX message queues

# SYNOPSIS

**dmsend** \--help

**dmsend** \--version

**dmsend** \--**type** \[log\|observ\] \--**format** \[csv\|nml\] \--**input**
*file* \[\--**receiver** *name*\] \[\--**name** *name*\] \[\--**node** *id*\]
\[\--**logger** *name*\] \[\--**debug**\] \[\--**forward**\] \[\--**verbose**\]

**dmsend** \--**config** *file* \[\--**name** *name*\]

# DESCRIPTION

The **dmsend** program reads observations or logs in CSV or Fortran 95 Namelist
format, and sends them sequentially to the POSIX message queue of the given
receiver. The data is either read from file or from standard input. If the input
data is of type `observ` and the argument `--forward` is passed, each
observation will be sent to its next specified receiver in the receivers list.
If no receivers are declared, or if the end of the receivers list is reached,
the observation will not be forwarded.

The program settings are passed through command-line arguments or an optional
configuration file. The arguments overwrite settings from file.

# OPTIONS

**\--config**, **-c** *file*

:   Path to configuration file.

**\--debug**, **-D**

:   Forward logs messages of level `LL_DEBUG` via IPC (if logger is set).

**\--format**, **-f** \[csv\|nml\]

:   Input format, either CSV or Fortran 95 Namelist.

**\--forward**, **-F**

:   Forward observations to the next specified receiver in the receivers list.
    Not allowed in combination with argument `--receiver`.

**\--logger**, **-l** *name*

:   Name of logger. If set, sends logs to *dmlogger(1)* process of given name.

**\--help**, **-h**

:   Print available command-line arguments and quit.

**\--input**, **-i**

:   Path to input file, either in CSV or Namelist format. Reads records from
    *stdin* if not passed or set to `-`.

**\--name**, **-n** *name*

:   Name of program instance and configuration (default is `dmsend`).

**\--node**, **-N** *id*

:   Optional node id.

**\--receiver**, **-r** *name*

:   Name of the observation or log receiver, without leading `/`. Not allowed in
    combination with argument `--forward`.

**\--type**, **-t** \[log\|observ\]

:   Data type to send, either log or observation.

**\--verbose**, **-V**

:   Print log messages to *stderr*.

**\--version**, **-v**

:   Print version information and quit.

# EXIT STATUS

**0**

:   Success. Program terminated normally.

**1**

:   Failure. Program execution failed.

# ENVIRONMENT

**DM_LOGGER**

:   Name of logger instance to send logs to.

**NO_COLOR**

:   Disable ANSI colour output.

# EXAMPLE

Read observation from Namelist file `observ.nml` and send it to the next
specified receiver:

    $ dmsend -t observ -f nml -i observ.nml -F

Send logs in CSV file `logs.csv` sequentially to process `dmrecv`:

    $ dmsend -r dmrecv -t log -f csv -i logs.csv

# SEE ALSO

*dmrecv(1)*
