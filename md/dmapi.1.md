% DMAPI(1) Version 2.0.0 | User Commands

# NAME

dmapi -- FastCGI-based HTTP-RPC service for database access

# SYNOPSIS

**dmapi**

# DESCRIPTION

**dmapi** is an HTTP-RPC service for remote DMPACK database access. The web
application has to be executed by a FastCGI-compatible web server or a FastCGI
spawner. It is recommended to run **dmapi** in *lighttpd(1)*.

The **dmapi** service offers endpoints for clients to insert nodes, sensors,
targets, observations, logs, and beats into the server SQLite databases, and to
retrieve the data in CSV, JSON, JSON Lines, or Fortran 95 Namelist format.
Authentication and encryption are independent from **dmapi** and have to be
provided by the environment.

The POST data sent by clients must be serialised in Namelist format, with
optional deflate or zstd compression. The content type and the content encoding
have to be passed in the HTTP header of the request.

If HTTP Basic Auth is enabled, the sensor id of each beat, image, log, node,
sensor, and observation sent to the RPC service must match the name of the
authenticated user, otherwise, the data will be rejected (HTTP 401).

# ENVIRONMENT

Add the following environment variables to the configuration of the web server
or spawner to pass them to **dmapi**:

**DM_BEAT_DB**

:   Path to heartbeat database.

**DM_IMAGE_DB**

:   Path to image database.

**DM_IMAGE_DIR**

:   Path to image directory.

**DM_LOG_DB**

:   Path to log database.

**DM_OBSERV_DB**

:   Path to observation database.

**DM_READ_ONLY**

:   Set to 1 to open databases in read-only mode.

# SEE ALSO

*dmbeat(1)*, *dmsync(1)*
