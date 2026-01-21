% DMRECV(1) Version 2.0.0 | User Commands

# NAME

dmrecv -- receives observations and logs from POSIX message queue

# SYNOPSIS

**dmrecv** \--help

**dmrecv** \--version

**dmrecv** \--**type** \[log\|observ\] \--**format** \[block\|csv\|jsonl\|nml\]
\[\--**name** *name*\] \[\--**output** *file*\] \[\--**response** *name*\]
\[\--**node** *id*\] \[\--**logger** *name*\] \[\--**forward**\]
\[\--**replace**\] \[\--**verbose**\]

**dmrecv** \--**config** *file* \[\--**name** *name*\]

# DESCRIPTION

The **dmrecv** program listens to the POSIX message queue of its name and writes
received observations and logs to *stdout*, file, or named pipe; in ASCII block,
CSV, JSON, or Fortran 95 Namelist format. By default, the serialised data is
appended to the end of the output file.  If argument `--replace` is passed, the
file will be replaced consecutively.

Received observations are not forwarded to the next specified receiver, unless
argument `--forward` is set. If no receivers are defined or left, the
observation will be discarded after output.

The output format `block` is available for observation data only, and requires a
valid response name. Received observations will be searched for this response
and converted to data point type if found. The data point is printed in ASCII
block format.

If output format JSON Lines is selected, observations and logs are written
sequentially as JSON objects to file or *stdout*, separated by new line. Use
*jq(1)* to convert a file `input.jsonl` of JSON objects to a JSON array in
`output.json`:

    $ jq -s '.' input.jsonl > output.json

The program settings are passed through command-line arguments or an optional
configuration file. The arguments overwrite settings from file.

# OPTIONS

**\--config**, **-c** *file*

:   Path to configuration file.

**\--debug**, **-D**

:   Forward logs messages of level `LL_DEBUG` via IPC (if logger is set).

**\--format**, **-f** \[block\|csv\|jsonl\|nml\]

:   Output format, either ASCII block, CSV, JSON Lines, or Namelist.  Format
    `block` is allowed for type `observ` only.

**\--forward**, **-F**

:   Forward each observation to its next specified receiver.

**\--help**, **-h**

:   Print available command-line arguments and quit.

**\--logger**, **-l** *name*

:   Name of logger. If set, sends logs to *dmlogger(1)* process of given name.

**\--name**, **-n** *name*

:   Name of configuration and POSIX message queue (default is `dmrecv`).

**\--node**, **-N** *id*

:   Optional node id.

**\--output**, **-o** *file*

:   Path of output file. Prints serialised data to *stdout* if `-` or not set.

**\--replace**, **-r**

:   Replace output file instead of appending data.

**\--response**, **-R** *name*

:   Name of the observation response to output (required for format `block`).

**\--type**, **-t** \[log\|observ\]

:   Data type to receive, either log or observation.

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

Write received log messages in CSV format to file `/tmp/logs.csv`:

    $ dmrecv -n dmrecv -t log -f csv -o /tmp/logs.csv

Output observations in JSON format to *stdout*:

    $ dmrecv -n dmrecv -t observ -f json

Output observation responses of name `x` in ASCII block format to *stdout*:

    $ dmrecv -n dmrecv -t observ -f block -R x

# SEE ALSO

*dmsend(1)*
