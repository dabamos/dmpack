% DMSYNC(1) Version 2.0.0 | User Commands

# NAME

dmsync -- synchronises databases between client and server

# SYNOPSIS

**dmsync** \--help

**dmsync** \--version

**dmsync** \--**database** *file* \--**type** *type* \--**host** *host*
\[\--**tls**\] \[\--**port** *port*\] \[\--**username** *user*\]
\[\--**password** *password*\] \[\--**node** *id*\] \[\--**interval**
*seconds*\] \[\--**wait** *name*\] \[\--**compression** *name*\] \[\--**logger**
*name*\] \[\--**create**\] \[\--**debug**\]

**dmsync** \--**config** *file* \[\--**name** *name*\]

# DESCRIPTION

The **dmsync** program synchronises logs, nodes, observations, sensors, and
targets of a local database concurrently with an HTTP-RPC server.  The
synchronisation may be started only once if no interval is set (to transfer
nodes, sensors, and targets from client to server), periodically as a cron job,
or by waiting for a POSIX semaphore.

The nodes, sensors, and targets referenced by observations in the local database
must also exist in the remote server database. They can be created either with
*dmdbcli(1)* or *dmweb(1)*, but also synchronised with **dmsync**. Logs and
targets do not require any additional database entries on server-side.

The client databases must contain synchronisation tables. The tables are created
automatically by *dminit(1)* if command-line argument `--sync` is passed.
Alternatively, start **dmsync** with argument `--create` once.

Database records are sent Zstandard-compressed in Fortran 95 Namelist format via
HTTP to the server. The program uses libcurl for data transfer. The called
HTTP-RPC API endpoints are expected under base URI
`[http|https]://<host>:<port>/api/v2/`.

The result of each synchronisation attempt is stored in the local database.
Records are marked as synchronised only if the server returns HTTP 201
(Created).

Passing the server credentials via the command-line arguments `--username` and
`--password` is insecure on multi-user operating systems and only recommended
for testing.

# OPTIONS

**\--compression**, **-x** \[none\|zlib\|zstd\]

:   Compression library to use for payload data. The default is `zstd`
    (Zstandard). Select `zlib` for deflate compression or `none` to disable
    compression.

**\--config**, **-c** *file*

:   Path to configuration file.

**\--create**, **-C**

:   Create database synchronisation tables.

**\--database**, **-d** *file*

:   Path to SQLite log or observation database.

**\--debug**, **-D**

:   Forward logs messages of level `LL_DEBUG` via IPC (if logger is set).

**\--help**, **-h**

:   Print available command-line arguments and quit.

**\--host**, **-H** *host*

:   IP address or FQDN of RPC API server.

**\--interval**, **-I** *seconds*

:   Synchronisation interval in seconds. If set to `0`, only a single attempt is
    made (default).

**\--logger**, **-l** *name*

:   Name of logger. If set, sends logs to *dmlogger(1)* process of given name.

**\--name**, **-n** *name*

:   Name of program instance and configuration.

**\--node**, **-N** *id*

:   Node id, required for types `sensor` and `observ`.

**\--password**, **-P** *password*

:   RPC API password. Be aware that passing the credentials via command-line
    arguments is insecure and only recommended for testing.

**\--port**, **-q** *port*

:   Port of RPC API server. The default is `0`, which selects the port
    automatically depending on the protocol.

**\--tls**, **-E**

:   Use TLS-encrypted connection.

**\--type**, **-t** \[log\|node\|observ\|sensor\|target\]

:   Type of data to synchronise, either log, node, observation, sensor, or
    target. Type log requires a log database, all others an observation
    database.

**\--username**, **-U** *user*

:   RPC API user name. If set, implies HTTP Basic Auth.

**\--verbose**, **-V**

:   Print log messages to *stderr*.

**\--version**, **-v**

:   Print version information and quit.

**\--wait**, **-w**

:   Name of program to wait for. Starts to synchronise once the named semaphore
    of the program is greater 0.

# EXIT STATUS

**0**

:   Success. Synchronisation finished without errors.

**1**

:   Failure. Synchronisation failed.

# ENVIRONMENT

**DM_LOGGER**

:   Name of logger instance to send logs to.

**NO_COLOR**

:   Disable ANSI colour output.

# EXAMPLE

Synchronise nodes, sensors, and targets with a remote HTTP-RPC API:

    $ dmsync -d observ.db -t node -H example.com
    $ dmsync -d observ.db -t sensor -N dummy-node -H example.com
    $ dmsync -d observ.db -t target -H example.com

Synchronise observations:

    $ dmsync -d observ.db --t observ -H example.com

Synchronise log messages:

    $ dmsync -d log.db -t log -H example.com

# SEE ALSO

*dmapi(1)*
