# Change Log

All notable changes to the project will be documented in this file.

## [2.0.0] – Unreleased

## Library

* `Added` more error codes to `dm_error`
* `Added` more log levels to `dm_log`
* `Added` _fortran-fast-float_ module
* `Added` _fortran-zeromq_ interface bindings
* `Added` function `dm_net_ipv6_is_valid()` to module `dm_net`
* `Added` functions `dm_db_json_select_observ()` and `dm_db_json_select_observs()` function to `dm_db_json`
* `Added` type `posix_signal_type` and self-pipe procedures to module `dm_posix_signal`
* `Added` decode routine to `dm_base64`
* `Added` module `dm_arg_parser` (outsourced from `dm_arg`)
* `Added` module `dm_buffer` of byte buffer
* `Added` module `dm_gantner` for eGateHighSpeedPort API access
* `Added` module `dm_group` for observation groups
* `Added` module `dm_ipc` for IPC message header
* `Added` module `dm_job_list` (outsourced from `dm_job`)
* `Added` module `dm_msgpack` for MessagePack serialisation/deserialisation
* `Added` module `dm_ods` for OpenDocument Spreadsheet (ODS) export
* `Added` module `dm_posix_regex` for POSIX regular expression matching (BRE, ERE)
* `Added` module `dm_process` for POSIX process handling
* `Added` module `dm_random` for pseudo-random number generation
* `Added` module `dm_xml` for XML abstraction
* `Added` module `dm_zip` for file compression
* `Added` modules `dm_ipc`, `dm_ipc_message`, and `dm_ipc_thread` for ZeroMQ connectivity
* `Changed` name of parameter `RESPONSE_TYPE_STRING` to `RESPONSE_TYPE_BYTES`
* `Changed` high-level HDF5 API in `dm_hdf5`
* `Changed` location of sleep routines (moved to `dm_posix`)
* `Changed` functions in `dm_gm` to subroutines
* `Changed` procedure names in `dm_uuid`
* `Changed` name of type `lua_state_type` in `dm_lua` to `lua_type`
* `Changed` name of module `dm_sql` to `dm_db_sql`
* `Changed` name of module `dm_system` to `dm_posix`
* `Changed` names of modules `dm_fifo`, `dm_mqueue`, `dm_pipe`, `dm_sem`, `dm_signal`, `dm_thread`, `dm_tty` to include `posix` prefix
* `Changed` names of ANSI colour parameters in `dm_ansi`
* `Changed` database schema due to updated data model
* `Changed` default database suffix from `.sqlite` to `.db`
* `Changed` GeoCOM API for Lua to accept prototype observation as first argument
* `Changed` GeoCOM API response identifiers to more memorable names
* `Changed` slash handling of function `dm_path_join` in `dm_path`
* `Changed` model of observation data structure in `dm_observ`, removed priority, requests, and receivers
* `Changed` model of sensor data structure in `dm_sensor`
* `Changed` modules `dm_arg` and `dm_arg_parser` to accept logical command-line argument values
* `Deleted` module `dm_mqueue_util`
* `Deleted` module `dm_request`
* `Fixed` GeoCOM API response identifiers in `dm_geocom` and `dm_geocom_api`
* `Fixed` paper format in `dm_roff` (now DIN A4)

## Programs

* `Changed` response value units in `dmved`
* `Changed` signal handlers to use self-pipe for POSIX conformance

## Documentation

* Converted user guide from AsciiDoc to Markdown
* Converted man pages from AsciiDoc to Markdown

## [1.0.1] – 2026-04-28

### Library

* `Added` example filter program to `share/dmfilter/`

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
