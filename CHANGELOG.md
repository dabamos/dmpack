# Change Log

All notable changes to the project will be documented in this file.

## [Unreleased]

### Library

* `Added` module `dm_roff` for GNU roff abstraction
* `Added` terminals `gpic`, `postscript`, and `sixeltek` to `dm_plot`
* `Added` HTTP request and response headers to `dm_rpc`
* `Added` HTTP response headers to `dm_fcgi`
* `Added` JavaScript module `dm_js`
* `Changed` structure of database abstraction layer, added modules `dm_db_api`,
  `dm_db_count`, `dm_db_json`, `dm_db_pragma`, and `dm_db_row`
* `Fixed` reading from pipe in `dm_pipe`
* `Fixed` unsigned type conversion in `dm_c`
* `Fixed` return code handling of database select functions

### Programs

* `Added` scale factor for response values to `dmreport`
* `Added` PDF and PostScript output to `dmreport`
* `Added` message queue reading to `dmmb`
* `Changed` configuration of `dmgrc`
* `Changed` environment variables in `dmapi` and `dmweb` to `DM_BEAT_DB`,
  `DM_LOG_DB`, `DM_OBSERV_DB`
* `Changed` database access in `dmexport`

## [0.9.7] – 2025-04-05

### Library

* `Added` module `dm_c` for C interoperability
* `Added` module `dm_camera` for webcam and RTSP access (FFmpeg)
* `Added` module `dm_crypto` of cryptographic hash functions
* `Added` module `dm_db_query` for SQL query building
* `Added` module `dm_dwd` for DWD API format parsing
* `Added` module `dm_dwd_api` for DWD API access (libcurl)
* `Added` module `dm_ftp` for FTP transfer (libcurl)
* `Added` module `dm_freebsd` for system status access on FreeBSD
* `Added` module `dm_geojson` for GeoJSON serialisation
* `Added` module `dm_gm` for GraphicsMagick access
* `Added` module `dm_im` for XMPP connectivity (libstrophe)
* `Added` module `dm_linux` for system status access on Linux
* `Added` module `dm_modbus` for Modbus RTU/TCP connectivity (libmodbus)
* `Added` module `dm_ve` for Victron Energy VE.Direct protocol support
* `Added` library build date and system type parameters (CPP, FPP)
* `Added` Linux and FreeBSD abstractions in module `dm_system`
* `Changed` refactored database abstraction layer
* `Changed` refactored database schema
* `Changed` refactored command-line argument parsing
* `Changed` refactored logical function names
* `Fixed` wrong baud rate in GeoCOM class

### Programs

* `Added` program `dmbot` for remote control of clients through XMPP
* `Added` program `dmdwd` for Deutscher Wetterdienst (DWD) API monitoring
* `Added` program `dmmb` for sensor monitoring through Modbus RTU/TCP
* `Added` program `dmmbctl` for Modbus RTU/TCP command-line control
* `Added` program `dmsystem` for system monitoring
* `Added` program `dmved` for MPPT and battery monitoring (VE.Direct)
* `Added` interactive map view to `dmweb` (Leaflet)

### Documentation

* `Added` multi-page HTML output
* `Added` sensor control tutorials to user guide
