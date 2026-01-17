% DMWEB(1) Version 2.0.0 | User Commands

# NAME

dmweb -- CGI-based web user interface

# SYNOPSIS

**dmweb**

# DESCRIPTION

**dmweb** is a CGI-based web user interface for DMPACK database access on client
and server. The web application has to be executed through a CGI-compatible web
server. The web server must be able to pass environment variables to the CGI
program. It is recommended to use *lighttpd(1)*. Transport security and
authentication have to be provided by the web server.

This web application allows users to:

- add and view nodes, sensors, targets;

- view observations, logs, beats, and uploaded images;

- create plots in SVG format;

- show interactive map of nodes, sensors, and targets.

Gnuplot is used as the plotting backend and must be present on the host.
Responses are returned in HTML5.

The map view requires a URL to the tile server in environment variable
`DM_TILE_URL`. For example, set the variable to
[`https://tile.openstreetmap.org/{z}/{x}/{y}.png`](https://tile.openstreetmap.org/{z}/{x}/{y}.png)
to use OpenStreetMap as the backend.

Copy the directory `/usr/local/share/dmpack/dmweb` to the WWW root or create a
symlink.

# ENVIRONMENT

Add the following environment variables to the configuration of the web server
to pass them to **dmweb**:

**DM_BEAT_DB**

:   Path to beat database.

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

**DM_TILE_URL**

:   URL of map tile server.

# SEE ALSO

*dmapi(1)*, *dminit(1)*
