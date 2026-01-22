# Change Log

All notable changes to the project will be documented in this file.

## [2.0.0] – Unreleased

## Library

* `Added` module `dm_arg_parser` (outsourced from `dm_arg`)
* `Added` module `dm_group` for observation groups
* `Added` module `dm_ipc` for NNG socket access
* `Added` module `dm_ipc_message` for NNG message handling
* `Added` module `dm_ipc_mutex` for NNG mutex access
* `Added` module `dm_ipc_thread` for NNG thread access
* `Added` module `dm_job_list` (outsourced from `dm_job`)
* `Added` interface bindings to NNG
* `Added` error codes
* `Changed` names of modules `dm_mqueue`, `dm_mqueue_util`, `dm_sem`,
  `dm_signal`, `dm_thread` to include `posix` prefix
* `Changed` model of observation data structure `dm_observ`
* `Changed` default database suffix from `.sqlite` to `.db`
* `Changed` database schema version
* `Deleted` model of request data strucutre `dm_request`

## Programs

* `Changed` response value units in `dmved`

## Documentation

* Converted user guide from AsciiDoc to Markdown
* Converted man pages from AsciiDoc to Markdown

## [1.0.0] – Unreleased

## Library

* `Added` Bessel, Butterworth, Chebyshev filters to `dm_filter`

## [0.9.9] – 2026-01-11

### Library

* `Added` module `dm_coord` for coordinate transformation
* `Added` module `dm_filter` for filtering
* `Added` module `dm_ghostscript` for Ghostscript access
* `Added` module `dm_netstring` for netstring parsing
* `Added` module `dm_statistics` of statistics functions
* `Added` file tree size function to module `dm_file`
* `Added` file modification date/time to function `dm_file_touch()` in `dm_file`
* `Added` format `FORMAT_TSV` for tab-separated values to `dm_format`
* `Added` swap routines to `dm_util`
* `Changed` error code order for future additions
* `Changed` default database cache size of `dm_db_open()` to improve performance

### Programs

* `Added` author option to `dmreport`
* `Added` meta data to PDF output of `dmreport`
* `Fixed` reading of options in `dmdbctl`
* `Changed` last modified date/time to last log date/time in output of `dmfeed`

## [0.9.8] – 2025-08-30

### Library

* `Added` module `dm_image` for image handling
* `Added` module `dm_js` for JavaScript generation
* `Added` module `dm_roff` for GNU roff abstraction
* `Added` module `dm_serial` for derived type serialisation
* `Added` module `dm_transfer` for HTTP-RPC API file transfers
* `Added` GeoJSON Feature Collection procedures to `dm_geojson`
* `Added` image upload to `dm_rpc`
* `Added` HTTP request and response headers to `dm_rpc`
* `Added` HTTP response headers to `dm_fcgi`
* `Added` terminals `gpic`, `postscript`, and `sixeltek` to `dm_plot`
* `Added` image and transfer functions to `dm_db_api`
* `Changed` structure of database abstraction layer, added modules `dm_db_api`, `dm_db_count`, `dm_db_json`, `dm_db_pragma`, and `dm_db_row`
* `Changed` API of modules `dm_arg` and `dm_config` to object-oriented
* `Changed` query parameter API in `dm_cgi`
* `Changed` Git submodules to copies of third-party dependencies
* `Fixed` reading from pipe in `dm_pipe`
* `Fixed` unsigned type conversion in `dm_c`
* `Fixed` return code handling of database select functions

### Programs

* `Added` program `dmcamera` for capturing of camera images
* `Added` program `dmupload` for image upload to HTTP-RPC API
* `Added` scale factor for response values to `dmreport`
* `Added` PDF and PostScript output to `dmreport`
* `Added` message queue reading to `dmmb`
* `Added` image upload to `dmapi`
* `Added` image viewer to `dmweb`
* `Added` environment variables `DM_IMAGE_DB`, `DM_IMAGE_DIR` to `dmapi` and `dmweb`
* `Added` log file output to `dmlogger`
* `Changed` environment variables in `dmapi` and `dmweb` to `DM_BEAT_DB`, `DM_LOG_DB`, `DM_OBSERV_DB`
* `Changed` derived type serialisation in `dmapi` and `dmexport`
* `Changed` database access in `dmexport`
* `Changed` configuration of `dmgrc`

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
