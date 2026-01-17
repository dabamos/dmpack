% DMBEAT(1) Version 2.0.0 | User Commands

# NAME

dmbeat -- sends status messages to an RPC server

# SYNOPSIS

**dmbeat** \--help

**dmbeat** \--version

**dmbeat** \--**node** *id* \--**host** *host* \[\--**port** *port*\]
\[\--**tls**\] \[\--**username** *user*\] \[\--**password** *password*\]
\[\--**count** *n*\] \[\--**interval** *seconds*\] \[\--**compression** *name*\]
\[\--**logger** *name*\] \[\--**debug**\] \[\--**verbose**\]

**dmbeat** \--**config** *file* \[\--**name** *name*\]

# DESCRIPTION

This program is a heartbeat emitter that sends status messages to a remote
DMPACK HTTP-RPC API. The status messages include timestamp, system uptime, and
last connection error. The server may inspect this data to check if a client is
still running and has network access. The RPC endpoint is expected at
`[http|https]://<host>:<port>/api/v1/beat`.  The heartbeat is transmitted in
Zstandard-compressed Fortran 95 Namelist format.

Passing the server credentials via the command-line arguments `--username` and
`--password` is insecure on multi-user operating systems and only recommended
for testing.

# OPTIONS

**\--compression**, **-x** *name*

:   Compression library to use for payload data (`none`, `zlib`, `zstd`). The
    default is `zstd` (Z standard). Select `zlib` for deflate compression.

**\--config**, **-c** *file*

:   Path to Lua-based configuration file.

**\--count**, **-C** *n*

:   Maximum number of heartbeats to send. The default is 0 (unlimited).

**\--debug**, **-D**

:   Forward logs messages of level `LL_DEBUG` via IPC (if logger is set).

**\--help**, **-h**

:   Print available command-line arguments and quit.

**\--host**, **-H** *host*

:   IP or FQDN of RPC host (for example, `127.0.0.1` or `example.com`).

**\--interval**, **-I** *seconds*

:   Emit interval in seconds (default is 60 seconds).

**\--logger**, **-l** *name*

:   Name of logger. If set, sends logs to *dmlogger(1)* process of given name.

**\--name**, **-n** *name*

:   Name of instance and table in given configuration file (default is
    `dmbeat`).

**\--node**, **-N** *id*

:   Sensor node id.

**\--password**, **-P** *password*

:   RPC API password. Be aware that passing the credentials via command-line
    arguments is insecure and only recommended for testing.

**\--port**, **-q** *port*

:   Port of RPC API server. The default port is 0 (automatic selection depending
    on protocol).

**\--tls**, **-E**

:   Use TLS encryption.

**\--username**, **-U** *user*

:   RPC API user name. If set, implies HTTP Basic Auth.

**\--verbose**, **-V**

:   Print log messages to *stderr*.

**\--version**, **-v**

:   Print version information and quit.

# EXIT STATUS

**0**

:   Success. Beat has been transmitted.

**1**

:   Failure. Beat transmission failed.

# ENVIRONMENT

**DM_LOGGER**

:   Name of logger instance to send logs to.

**NO_COLOR**

:   Disable ANSI colour output.

# EXAMPLE

Send a single heartbeat to a *dmapi(1)* process on localhost:

    $ dmbeat -N dummy-node -H 127.0.0.1 -C 1 -V

# SEE ALSO

*dmapi(1)*
