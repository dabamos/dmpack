! Author:  Philipp Engel
! Licence: ISC
module dm_sql
    !! Predefined SQL statements as Fortran parameter strings.
    use :: dm_ascii, only: NL => ASCII_LF
    implicit none (type, external)
    private

    integer, parameter, public :: SQL_TABLE_NODES        = 1  !! Nodes table.
    integer, parameter, public :: SQL_TABLE_SENSORS      = 2  !! Sensors table.
    integer, parameter, public :: SQL_TABLE_TARGETS      = 3  !! Targets table.
    integer, parameter, public :: SQL_TABLE_OBSERVS      = 4  !! Observations table.
    integer, parameter, public :: SQL_TABLE_RECEIVERS    = 5  !! Receivers table.
    integer, parameter, public :: SQL_TABLE_REQUESTS     = 6  !! Requests table.
    integer, parameter, public :: SQL_TABLE_RESPONSES    = 7  !! Responses table.
    integer, parameter, public :: SQL_TABLE_LOGS         = 8  !! Logs table.
    integer, parameter, public :: SQL_TABLE_BEATS        = 9  !! Heartbeats table.
    integer, parameter, public :: SQL_TABLE_TRANSFERS    = 10 !! Transfers table.
    integer, parameter, public :: SQL_TABLE_IMAGES       = 11 !! Images table.
    integer, parameter, public :: SQL_TABLE_SYNC_NODES   = 12 !! Sync nodes table.
    integer, parameter, public :: SQL_TABLE_SYNC_SENSORS = 13 !! Sync sensors table.
    integer, parameter, public :: SQL_TABLE_SYNC_TARGETS = 14 !! Sync targets table.
    integer, parameter, public :: SQL_TABLE_SYNC_OBSERVS = 15 !! Sync observations table.
    integer, parameter, public :: SQL_TABLE_SYNC_LOGS    = 16 !! Sync logs table.
    integer, parameter, public :: SQL_TABLE_LAST         = 16 !! Never use this.

    integer, parameter, public :: SQL_TABLE_NAME_LEN = 12 !! Max. length of table names.

    character(len=*), parameter, public :: SQL_TABLE_NAMES(SQL_TABLE_LAST) = [ &
        character(len=SQL_TABLE_NAME_LEN) :: &
        'nodes',        &
        'sensors',      &
        'targets',      &
        'observs',      &
        'receivers',    &
        'requests',     &
        'responses',    &
        'logs',         &
        'beats',        &
        'transfers',    &
        'images',       &
        'sync_nodes',   &
        'sync_sensors', &
        'sync_targets', &
        'sync_observs', &
        'sync_logs'     &
    ] !! SQL table names.

    ! **************************************************************************
    ! UTILITY QUERIES.
    ! **************************************************************************
    ! Drop table.
    character(len=*), parameter, public :: SQL_DROP_TABLE = "DROP TABLE IF EXISTS ?"

    ! Select all tables.
    character(len=*), parameter, public :: SQL_SELECT_TABLES = &
        "SELECT "                                                                                  // &
        "(SELECT COUNT(*) FROM sqlite_schema WHERE type = 'table' AND name NOT LIKE 'sqlite_%'), " // &
        "name "                                                                                    // &
        "FROM sqlite_schema "                                                                      // &
        "WHERE type = 'table' AND name NOT LIKE 'sqlite_%'"

    ! Select the table of given name.
    character(len=*), parameter, public :: SQL_SELECT_TABLE = &
        "SELECT name FROM sqlite_master WHERE type = 'table' AND name = ?"

    ! **************************************************************************
    ! TABLE CREATION QUERIES.
    ! **************************************************************************
    ! Beats schema.
    character(len=*), parameter, public :: SQL_CREATE_BEATS = &
        "CREATE TABLE IF NOT EXISTS beats("                                      // NL // &
        "row_id    INTEGER PRIMARY KEY,"                                         // NL // & ! Explicit alias for rowid.
        "node_id   TEXT    NOT NULL UNIQUE,"                                     // NL // &
        "address   TEXT,"                                                        // NL // &
        "client    TEXT,"                                                        // NL // &
        "time_sent TEXT    NOT NULL DEFAULT '1970-01-01T00:00:00.000000+00:00'," // NL // &
        "time_recv TEXT    NOT NULL DEFAULT (strftime('%FT%R:%f000+00:00')),"    // NL // &
        "error     INTEGER NOT NULL DEFAULT 0,"                                  // NL // &
        "interval  INTEGER NOT NULL DEFAULT 0,"                                  // NL // &
        "uptime    INTEGER NOT NULL DEFAULT 0) STRICT"

    ! Images schema.
    character(len=*), parameter, public :: SQL_CREATE_IMAGES = &
        "CREATE TABLE IF NOT EXISTS images("                                     // NL // &
        "row_id    INTEGER PRIMARY KEY,"                                         // NL // & ! Explicit alias for rowid.
        "id        TEXT    NOT NULL UNIQUE,"                                     // NL // &
        "node_id   TEXT    NOT NULL,"                                            // NL // &
        "sensor_id TEXT    NOT NULL,"                                            // NL // &
        "target_id TEXT    NOT NULL,"                                            // NL // &
        "timestamp TEXT    NOT NULL DEFAULT '1970-01-01T00:00:00.000000+00:00'," // NL // &
        "mime      TEXT,"                                                        // NL // &
        "width     INTEGER NOT NULL DEFAULT 0,"                                  // NL // &
        "height    INTEGER NOT NULL DEFAULT 0,"                                  // NL // &
        "size      INTEGER NOT NULL DEFAULT 0) STRICT"

    ! Logs schema
    character(len=*), parameter, public :: SQL_CREATE_LOGS = &
        "CREATE TABLE IF NOT EXISTS logs("                                    // NL // &
        "row_id    INTEGER PRIMARY KEY,"                                      // NL // & ! Explicit alias for rowid.
        "id        TEXT    NOT NULL UNIQUE,"                                  // NL // &
        "level     INTEGER NOT NULL DEFAULT 0,"                               // NL // &
        "error     INTEGER NOT NULL DEFAULT 0,"                               // NL // &
        "timestamp TEXT    NOT NULL DEFAULT (strftime('%FT%R:%f000+00:00'))," // NL // &
        "node_id   TEXT,"                                                     // NL // &
        "sensor_id TEXT,"                                                     // NL // &
        "target_id TEXT,"                                                     // NL // &
        "observ_id TEXT,"                                                     // NL // &
        "source    TEXT,"                                                     // NL // &
        "message   TEXT) STRICT"

    ! Sensor nodes schema.
    character(len=*), parameter, public :: SQL_CREATE_NODES = &
        "CREATE TABLE IF NOT EXISTS nodes("    // NL // &
        "row_id    INTEGER PRIMARY KEY,"       // NL // & ! Explicit alias for rowid.
        "id        TEXT NOT NULL UNIQUE,"      // NL // &
        "name      TEXT NOT NULL,"             // NL // &
        "meta      TEXT,"                      // NL // &
        "x         REAL NOT NULL DEFAULT 0.0," // NL // &
        "y         REAL NOT NULL DEFAULT 0.0," // NL // &
        "z         REAL NOT NULL DEFAULT 0.0," // NL // &
        "longitude REAL NOT NULL DEFAULT 0.0," // NL // &
        "latitude  REAL NOT NULL DEFAULT 0.0," // NL // &
        "elevation REAL NOT NULL DEFAULT 0.0) STRICT"

    ! Sensors schema.
    character(len=*), parameter, public :: SQL_CREATE_SENSORS = &
        "CREATE TABLE IF NOT EXISTS sensors("     // NL // &
        "row_id    INTEGER PRIMARY KEY,"          // NL // & ! Explicit alias for rowid.
        "node_id   INTEGER NOT NULL,"             // NL // &
        "id        TEXT    NOT NULL UNIQUE,"      // NL // &
        "type      INTEGER NOT NULL DEFAULT 0,"   // NL // &
        "name      TEXT    NOT NULL,"             // NL // &
        "sn        TEXT,"                         // NL // &
        "meta      TEXT,"                         // NL // &
        "x         REAL    NOT NULL DEFAULT 0.0," // NL // &
        "y         REAL    NOT NULL DEFAULT 0.0," // NL // &
        "z         REAL    NOT NULL DEFAULT 0.0," // NL // &
        "longitude REAL    NOT NULL DEFAULT 0.0," // NL // &
        "latitude  REAL    NOT NULL DEFAULT 0.0," // NL // &
        "elevation REAL    NOT NULL DEFAULT 0.0," // NL // &
        "FOREIGN KEY (node_id) REFERENCES nodes(row_id)) STRICT"

    ! Targets schema.
    character(len=*), parameter, public :: SQL_CREATE_TARGETS = &
        "CREATE TABLE IF NOT EXISTS targets("      // NL // &
        "row_id    INTEGER PRIMARY KEY,"           // NL // & ! Explicit alias for rowid.
        "id        TEXT    NOT NULL UNIQUE,"       // NL // &
        "name      TEXT,"                          // NL // &
        "meta      TEXT,"                          // NL // &
        "state     INTEGER NOT NULL DEFAULT 0,"    // NL // &
        "x         REAL    NOT NULL DEFAULT 0.0,"  // NL // &
        "y         REAL    NOT NULL DEFAULT 0.0,"  // NL // &
        "z         REAL    NOT NULL DEFAULT 0.0,"  // NL // &
        "longitude REAL    NOT NULL DEFAULT 0.0,"  // NL // &
        "latitude  REAL    NOT NULL DEFAULT 0.0,"  // NL // &
        "elevation REAL    NOT NULL DEFAULT 0.0) STRICT"

    ! Observations schema.
    character(len=*), parameter, public :: SQL_CREATE_OBSERVS = &
        "CREATE TABLE IF NOT EXISTS observs("                                  // NL // &
        "row_id     INTEGER PRIMARY KEY,"                                      // NL // & ! Explicit alias for rowid.
        "node_id    INTEGER NOT NULL,"                                         // NL // &
        "sensor_id  INTEGER NOT NULL,"                                         // NL // &
        "target_id  INTEGER NOT NULL,"                                         // NL // &
        "id         TEXT    NOT NULL UNIQUE,"                                  // NL // &
        "name       TEXT    NOT NULL,"                                         // NL // &
        "timestamp  TEXT    NOT NULL DEFAULT (strftime('%FT%R:%f000+00:00'))," // NL // &
        "source     TEXT,"                                                     // NL // &
        "device     TEXT,"                                                     // NL // &
        "priority   INTEGER NOT NULL DEFAULT 0,"                               // NL // &
        "error      INTEGER NOT NULL DEFAULT 0,"                               // NL // &
        "next       INTEGER NOT NULL DEFAULT 0,"                               // NL // &
        "nreceivers INTEGER NOT NULL DEFAULT 0,"                               // NL // &
        "nrequests  INTEGER NOT NULL DEFAULT 0,"                               // NL // &
        "FOREIGN KEY (node_id)   REFERENCES nodes(row_id),"                    // NL // &
        "FOREIGN KEY (sensor_id) REFERENCES sensors(row_id),"                  // NL // &
        "FOREIGN KEY (target_id) REFERENCES targets(row_id)) STRICT"

    ! Receivers schema.
    character(len=*), parameter, public :: SQL_CREATE_RECEIVERS = &
        "CREATE TABLE IF NOT EXISTS receivers("               // NL // &
        "row_id    INTEGER PRIMARY KEY,"                      // NL // & ! Explicit alias for rowid.
        "observ_id INTEGER NOT NULL,"                         // NL // &
        "idx       INTEGER NOT NULL,"                         // NL // &
        "name      TEXT    NOT NULL,"                         // NL // &
        "FOREIGN KEY (observ_id) REFERENCES observs(row_id)," // NL // &
        "UNIQUE      (observ_id, idx) ON CONFLICT REPLACE) STRICT"

    ! Requests schema.
    character(len=*), parameter, public :: SQL_CREATE_REQUESTS = &
        "CREATE TABLE IF NOT EXISTS requests("                                 // NL // &
        "row_id     INTEGER PRIMARY KEY,"                                      // NL // & ! Explicit alias for rowid.
        "observ_id  INTEGER NOT NULL,"                                         // NL // &
        "idx        INTEGER NOT NULL,"                                         // NL // &
        "name       TEXT    NOT NULL,"                                         // NL // &
        "timestamp  TEXT    NOT NULL DEFAULT (strftime('%FT%R:%f000+00:00'))," // NL // &
        "request    TEXT,"                                                     // NL // &
        "response   TEXT,"                                                     // NL // &
        "delimiter  TEXT,"                                                     // NL // &
        "pattern    TEXT,"                                                     // NL // &
        "delay      INTEGER NOT NULL DEFAULT 0,"                               // NL // &
        "error      INTEGER NOT NULL DEFAULT 0,"                               // NL // &
        "mode       INTEGER NOT NULL DEFAULT 0,"                               // NL // &
        "retries    INTEGER NOT NULL DEFAULT 0,"                               // NL // &
        "state      INTEGER NOT NULL DEFAULT 0,"                               // NL // &
        "timeout    INTEGER NOT NULL DEFAULT 0,"                               // NL // &
        "nresponses INTEGER NOT NULL DEFAULT 0,"                               // NL // &
        "FOREIGN KEY (observ_id) REFERENCES observs(row_id),"                  // NL // &
        "UNIQUE      (observ_id, idx) ON CONFLICT REPLACE) STRICT"

    ! Responses schema.
    character(len=*), parameter, public :: SQL_CREATE_RESPONSES = &
        "CREATE TABLE IF NOT EXISTS responses("                 // NL // &
        "row_id     INTEGER PRIMARY KEY,"                       // NL // & ! Explicit alias for rowid.
        "request_id INTEGER NOT NULL,"                          // NL // &
        "idx        INTEGER NOT NULL,"                          // NL // &
        "name       TEXT,"                                      // NL // &
        "unit       TEXT,"                                      // NL // &
        "type       INTEGER NOT NULL DEFAULT 0,"                // NL // &
        "error      INTEGER NOT NULL DEFAULT 0,"                // NL // &
        "value      REAL    NOT NULL DEFAULT 0.0,"              // NL // &
        "FOREIGN KEY (request_id) REFERENCES requests(row_id)," // NL // &
        "UNIQUE      (request_id, idx) ON CONFLICT REPLACE) STRICT"

    ! Transfers schema.
    character(len=*), parameter, public :: SQL_CREATE_TRANSFERS = &
        "CREATE TABLE IF NOT EXISTS transfers("                               // NL // &
        "row_id    INTEGER PRIMARY KEY,"                                      // NL // & ! Explicit alias for rowid.
        "type      INTEGER NOT NULL DEFAULT 0,"                               // NL // &
        "node_id   TEXT    NOT NULL,"                                         // NL // &
        "type_id   TEXT    NOT NULL UNIQUE,"                                  // NL // &
        "id        TEXT    NOT NULL UNIQUE,"                                  // NL // &
        "timestamp TEXT    NOT NULL DEFAULT (strftime('%FT%R:%f000+00:00'))," // NL // &
        "address   TEXT,"                                                     // NL // &
        "error     INTEGER NOT NULL DEFAULT 0,"                               // NL // &
        "state     INTEGER NOT NULL DEFAULT 0,"                               // NL // &
        "size      INTEGER NOT NULL DEFAULT 0) STRICT"

    ! **************************************************************************
    ! SYNC TABLE CREATION QUERIES.
    ! **************************************************************************
    ! Synchronised logs schema.
    character(len=*), parameter, public :: SQL_CREATE_SYNC_LOGS = &
        "CREATE TABLE IF NOT EXISTS sync_logs("                               // NL // &
        "row_id    INTEGER PRIMARY KEY,"                                      // NL // & ! Explicit alias for rowid.
        "log_id    INTEGER NOT NULL UNIQUE,"                                  // NL // &
        "timestamp TEXT    NOT NULL DEFAULT (strftime('%FT%R:%f000+00:00'))," // NL // &
        "code      INTEGER NOT NULL DEFAULT 0,"                               // NL // &
        "attempts  INTEGER NOT NULL DEFAULT 0,"                               // NL // &
        "FOREIGN KEY (log_id) REFERENCES logs(row_id)) STRICT"

    ! Synchronised nodes schema.
    character(len=*), parameter, public :: SQL_CREATE_SYNC_NODES = &
        "CREATE TABLE IF NOT EXISTS sync_nodes("                              // NL // &
        "row_id    INTEGER PRIMARY KEY,"                                      // NL // & ! Explicit alias for rowid.
        "node_id   INTEGER NOT NULL UNIQUE,"                                  // NL // &
        "timestamp TEXT    NOT NULL DEFAULT (strftime('%FT%R:%f000+00:00'))," // NL // &
        "code      INTEGER NOT NULL DEFAULT 0,"                               // NL // &
        "attempts  INTEGER NOT NULL DEFAULT 0,"                               // NL // &
        "FOREIGN KEY (node_id) REFERENCES nodes(row_id)) STRICT"

    ! Synchronised observations schema.
    character(len=*), parameter, public :: SQL_CREATE_SYNC_OBSERVS = &
        "CREATE TABLE IF NOT EXISTS sync_observs("                            // NL // &
        "row_id    INTEGER PRIMARY KEY,"                                      // NL // & ! Explicit alias for rowid.
        "observ_id INTEGER NOT NULL UNIQUE,"                                  // NL // &
        "timestamp TEXT    NOT NULL DEFAULT (strftime('%FT%R:%f000+00:00'))," // NL // &
        "code      INTEGER NOT NULL DEFAULT 0,"                               // NL // &
        "attempts  INTEGER NOT NULL DEFAULT 0,"                               // NL // &
        "FOREIGN KEY (observ_id) REFERENCES observs(row_id)) STRICT"

    ! Synchronised sensors schema.
    character(len=*), parameter, public :: SQL_CREATE_SYNC_SENSORS = &
        "CREATE TABLE IF NOT EXISTS sync_sensors("                            // NL // &
        "row_id    INTEGER PRIMARY KEY,"                                      // NL // & ! Explicit alias for rowid.
        "sensor_id INTEGER NOT NULL UNIQUE,"                                  // NL // &
        "timestamp TEXT    NOT NULL DEFAULT (strftime('%FT%R:%f000+00:00'))," // NL // &
        "code      INTEGER NOT NULL DEFAULT 0,"                               // NL // &
        "attempts  INTEGER NOT NULL DEFAULT 0,"                               // NL // &
        "FOREIGN KEY (sensor_id) REFERENCES sensors(row_id)) STRICT"

    ! Synchronised targets schema.
    character(len=*), parameter, public :: SQL_CREATE_SYNC_TARGETS = &
        "CREATE TABLE IF NOT EXISTS sync_targets("                            // NL // &
        "row_id    INTEGER PRIMARY KEY,"                                      // NL // & ! Explicit alias for rowid.
        "target_id INTEGER NOT NULL UNIQUE,"                                  // NL // &
        "timestamp TEXT    NOT NULL DEFAULT (strftime('%FT%R:%f000+00:00'))," // NL // &
        "code      INTEGER NOT NULL DEFAULT 0,"                               // NL // &
        "attempts  INTEGER NOT NULL DEFAULT 0,"                               // NL // &
        "FOREIGN KEY (target_id) REFERENCES targets(row_id)) STRICT"

    ! **************************************************************************
    ! CREATE INDEX QUERIES.
    ! **************************************************************************
    character(len=*), parameter, public :: SQL_CREATE_BEAT_INDICES(1) = [ character(len=64) :: &
        "CREATE INDEX IF NOT EXISTS idx_node_id ON beats(node_id)" &
    ]

    character(len=*), parameter, public :: SQL_CREATE_LOG_INDICES(8) = [ character(len=64) :: &
        "CREATE INDEX IF NOT EXISTS idx_timestamp ON logs(timestamp)", &
        "CREATE INDEX IF NOT EXISTS idx_level     ON logs(level)",     &
        "CREATE INDEX IF NOT EXISTS idx_error     ON logs(error)",     &
        "CREATE INDEX IF NOT EXISTS idx_node_id   ON logs(node_id)",   &
        "CREATE INDEX IF NOT EXISTS idx_sensor_id ON logs(sensor_id)", &
        "CREATE INDEX IF NOT EXISTS idx_target_id ON logs(target_id)", &
        "CREATE INDEX IF NOT EXISTS idx_observ_id ON logs(observ_id)", &
        "CREATE INDEX IF NOT EXISTS idx_source    ON logs(source)"     &
    ]

    character(len=*), parameter, public :: SQL_CREATE_OBSERV_INDICES(12) = [ character(len=128) :: &
        "CREATE INDEX IF NOT EXISTS idx_nodes_id             ON nodes(id)",                        &
        "CREATE INDEX IF NOT EXISTS idx_sensors_id           ON sensors(id)",                      &
        "CREATE INDEX IF NOT EXISTS idx_targets_id           ON targets(id)",                      &
        "CREATE INDEX IF NOT EXISTS idx_observs              ON observs(name, timestamp, error)",  &
        "CREATE INDEX IF NOT EXISTS idx_observs_timestamp    ON observs(timestamp)",               &
        "CREATE INDEX IF NOT EXISTS idx_receivers_idx        ON receivers(idx)",                   &
        "CREATE INDEX IF NOT EXISTS idx_requests_idx         ON requests(idx)",                    &
        "CREATE INDEX IF NOT EXISTS idx_requests_name        ON requests(name)",                   &
        "CREATE INDEX IF NOT EXISTS idx_requests_timestamp   ON requests(timestamp)",              &
        "CREATE INDEX IF NOT EXISTS idx_responses            ON responses(request_id, idx, name, unit, type, error, value)", &
        "CREATE INDEX IF NOT EXISTS idx_responses_request_id ON responses(request_id)",            &
        "CREATE INDEX IF NOT EXISTS idx_responses_name       ON responses(name)"                   &
    ]

    ! **************************************************************************
    ! TRIGGERS.
    ! **************************************************************************
    ! SQL trigger that removes any receivers, requests, and responses
    ! associated with an observation.
    character(len=*), parameter, public :: SQL_DELETE_OBSERV_TRIGGER = &
        "CREATE TRIGGER IF NOT EXISTS delete_observ_trigger"                    // NL // &
        "    BEFORE DELETE"                                                     // NL // &
        "    ON observs"                                                        // NL // &
        "BEGIN"                                                                 // NL // &
        "    DELETE FROM receivers WHERE observ_id = old.row_id;"               // NL // &
        "    DELETE FROM responses WHERE request_id IN"                         // NL // &
        "        ("                                                             // NL // &
        "            SELECT"                                                    // NL // &
        "                requests.row_id"                                       // NL // &
        "            FROM"                                                      // NL // &
        "                requests"                                              // NL // &
        "            INNER JOIN observs ON observs.row_id = requests.observ_id" // NL // &
        "            WHERE observs.row_id = old.row_id"                         // NL // &
        "        );"                                                            // NL // &
        "    DELETE FROM requests WHERE observ_id = old.row_id;"                // NL // &
        "END"

    ! **************************************************************************
    ! DELETE QUERIES.
    ! **************************************************************************
    ! Query to delete beats.
    ! Arguments: beats.node_id
    character(len=*), parameter, public :: SQL_DELETE_BEAT = &
        "DELETE FROM beats WHERE node_id = ?"

    ! Query to delete log.
    ! Arguments: logs.id
    character(len=*), parameter, public :: SQL_DELETE_LOG = &
        "DELETE FROM logs WHERE id = ?"

    ! Query to delete node.
    ! Arguments: nodes.id
    character(len=*), parameter, public :: SQL_DELETE_NODE = &
        "DELETE FROM nodes WHERE id = ?"

    ! Query to delete sensor.
    ! Arguments: sensors.id
    character(len=*), parameter, public :: SQL_DELETE_SENSOR = &
        "DELETE FROM sensors WHERE id = ?"

    ! Query to delete target.
    ! Arguments: targets.id
    character(len=*), parameter, public :: SQL_DELETE_TARGET = &
        "DELETE FROM targets WHERE id = ?"

    ! Query to delete observation.
    ! Arguments: targets.id
    character(len=*), parameter, public :: SQL_DELETE_OBSERV = &
        "DELETE FROM observs WHERE id = ?"

    ! Query to delete all receivers of an observation.
    ! Arguments: targets.id
    character(len=*), parameter, public :: SQL_DELETE_RECEIVERS = &
        "DELETE FROM receivers WHERE observ_id IN (SELECT row_id FROM observs WHERE id = ?)"

    ! Query to delete all requests of an observation.
    ! Arguments: observ.id
    character(len=*), parameter, public :: SQL_DELETE_REQUESTS = &
        "DELETE FROM requests WHERE observ_id IN (SELECT row_id FROM observs WHERE id = ?)"

    ! Query to delete all responses of a request.
    ! Arguments: observ.id, requests.idx
    character(len=*), parameter, public :: SQL_DELETE_REQUEST_RESPONSES = &
        "DELETE FROM responses WHERE request_id IN "                 // &
        "(SELECT row_id FROM requests "                              // &
        "INNER JOIN observs ON observs.row_id = requests.observ_id " // &
        "WHERE observs.id = ? AND requests.idx = ?)"

    ! Query to delete all responses of an observation.
    ! Arguments: observ.id
    character(len=*), parameter, public :: SQL_DELETE_OBSERV_RESPONSES = &
        "DELETE FROM responses WHERE request_id IN "                 // &
        "(SELECT request_id FROM requests "                          // &
        "INNER JOIN observs ON observs.row_id = requests.observ_id " // &
        "WHERE observs.id = ?)"

    ! **************************************************************************
    ! INSERT QUERIES.
    ! **************************************************************************
    ! Query to upsert beat.
    ! Arguments: beats.node_id, beats.address, beats.time_sent,
    !            beats.time_recv, beats.interval, beats.error
    character(len=*), parameter, public :: SQL_INSERT_BEAT = &
        "INSERT INTO "                     // &
        "beats(node_id, address, client, time_sent, time_recv, error, interval, uptime) " // &
        "VALUES (?, ?, ?, ?, ?, ?, ?, ?) " // &
        "ON CONFLICT DO UPDATE SET "       // &
        "node_id = excluded.node_id, "     // &
        "address = excluded.address, "     // &
        "client = excluded.client, "       // &
        "time_sent = excluded.time_sent, " // &
        "time_recv = excluded.time_recv, " // &
        "error = excluded.error, "         // &
        "interval = excluded.interval, "   // &
        "uptime = excluded.uptime"

    ! Query to insert log.
    ! Arguments: logs.level, logs.error, logs.timestamp, logs.node_id,
    !            logs.sensor_id, logs.target_id, logs.observ_id, logs.message
    character(len=*), parameter, public :: SQL_INSERT_LOG = &
        "INSERT OR FAIL INTO "                                                                          // &
        "logs(id, level, error, timestamp, node_id, sensor_id, target_id, observ_id, source, message) " // &
        "VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"

    ! Query to insert node.
    ! Arguments: nodes.id, nodes.name, nodes.meta, nodes.x, nodes.y, nodes.z,
    !            nodes.longitude, nodes.latitude, nodes.elevation
    character(len=*), parameter, public :: SQL_INSERT_NODE = &
        "INSERT OR FAIL INTO "                                            // &
        "nodes(id, name, meta, x, y, z, longitude, latitude, elevation) " // &
        "VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)"

    ! Query to insert sensor.
    ! Arguments: sensors.id, nodes.id, sensors.type, sensors.id, sensors.name,
    !            sensors.sn, sensors.meta, sensors.x, sensors.y, sensors.z,
    !            sensors.longitude, sensors.latitude, sensors.elevation
    character(len=*), parameter, public :: SQL_INSERT_SENSOR = &
        "INSERT OR FAIL INTO "                                                                 // &
        "sensors(id, node_id, type, name, sn, meta, x, y, z, longitude, latitude, elevation) " // &
        "VALUES (?, (SELECT row_id FROM nodes WHERE id = ?), ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"

    ! Query to insert target.
    ! Arguments: targets.id, targets.name, targets.meta, targets.state,
    !            targets.x, targets.y, targets.z, targets.longitude,
    !            targets.latitude, targets.elevation
    character(len=*), parameter, public :: SQL_INSERT_TARGET = &
        "INSERT OR FAIL INTO "                                                     // &
        "targets(id, name, meta, state, x, y, z, longitude, latitude, elevation) " // &
        "VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"

    ! Query to insert observation.
    ! Arguments: nodes.id, sensors.id, targets.id, observs.id, observs.name,
    !            observs.timestamp, observs.source, observs.device, observs.priority,
    !            observs.error, observs.next, observs.nreceivers, observs.nrequests
    character(len=*), parameter, public :: SQL_INSERT_OBSERV = &
        "INSERT OR FAIL INTO "                                                         // &
        "observs(id, node_id, sensor_id, target_id, name, timestamp, source, device, " // &
        "priority, error, next, nreceivers, nrequests) "                               // &
        "VALUES (?, "                                                                  // &
        "(SELECT row_id FROM nodes WHERE id = ?), "                                    // &
        "(SELECT row_id FROM sensors WHERE id = ?), "                                  // &
        "(SELECT row_id FROM targets WHERE id = ?), "                                  // &
        "?, ?, ?, ?, ?, ?, ?, ?, ?)"

    ! Query to insert receiver.
    ! Arguments: observs.id, receivers.idx, receivers.name
    character(len=*), parameter, public :: SQL_INSERT_RECEIVER = &
        "INSERT OR FAIL INTO "             // &
        "receivers(observ_id, idx, name) " // &
        "VALUES ((SELECT row_id FROM observs WHERE id = ?), ?, ?)"

    ! Query to insert request.
    ! Arguments: observs.id, requests.idx, requests.name, requests.timestamp,
    !            requests.request, requests.response, requests.delimiter,
    !            requests.pattern, requests.delay, requests.error, requests.mode,
    !            requests.retries, requests.state, requests.timeout,
    !            requests.nresponses
    character(len=*), parameter, public :: SQL_INSERT_REQUEST = &
        "INSERT OR FAIL INTO "                                                              // &
        "requests(observ_id, idx, name, timestamp, request, response, delimiter, pattern, " // &
        "delay, error, mode, retries, state, timeout, nresponses) "                         // &
        "VALUES ((SELECT row_id FROM observs WHERE id = ?), ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"

    ! Query to insert response that references observation, request index.
    ! Arguments: responses.request_id, responses.idx, responses.name,
    !            responses.unit, response.type, responses.error, responses.value
    character(len=*), parameter, public :: SQL_INSERT_RESPONSE = &
        "INSERT OR FAIL INTO "                                        // &
        "responses(request_id, idx, name, unit, type, error, value) " // &
        "VALUES ("                                                    // &
        "(SELECT requests.row_id FROM requests INNER JOIN observs ON observs.row_id = requests.observ_id WHERE observs.id = ? AND requests.idx = ?), " // &
        "?, ?, ?, ?, ?, ?)"

    ! **************************************************************************
    ! UPDATE QUERIES.
    ! **************************************************************************
    ! Query to update node.
    ! Arguments: nodes.name, nodes.meta, nodes.x, nodes.y, nodes.z
    !            nodes.longitude, nodes.latitude, nodes.elevation, nodes.id
    character(len=*), parameter, public :: SQL_UPDATE_NODE = &
        "UPDATE OR FAIL nodes SET name = ?, meta = ?, x = ?, y = ?, z = ?, longitude = ?, latitude = ?, elevation = ? WHERE id = ?"

    ! Query to update sensor.
    ! Arguments: nodes.id, sensors.type, sensors.id, sensors.name, sensors.sn,
    !            sensors.meta, sensors.x, sensors.y, sensors.z, sensors.longitude,
    !            sensors.latitude, sensors.elevation, sensors.id
    character(len=*), parameter, public :: SQL_UPDATE_SENSOR = &
        "UPDATE OR FAIL sensors SET node_id = (SELECT row_id FROM nodes WHERE id = ?), " // &
        "type = ?, name = ?, sn = ?, meta = ?, x = ?, y = ?, z = ?, longitude = ?, latitude = ?, elevation = ? WHERE id = ?"

    ! Query to update target.
    ! Arguments: targets.name, targets.meta, targets.state, targets.x, targets.y,
    !            targets.z, targets.longitude, targets.latitude, targets.elevation,
    !            targets.id
    character(len=*), parameter, public :: SQL_UPDATE_TARGET = &
        "UPDATE OR FAIL targets SET name = ?, meta = ?, state = ?, x = ?, y = ?, z = ?, " // &
        "longitude = ?, latitude = ?, elevation = ? WHERE id = ?"

    ! **************************************************************************
    ! SELECT EXISTS QUERIES.
    ! **************************************************************************
    ! Query to check if log exists.
    ! Arguments: logs.id
    character(len=*), parameter, public :: SQL_HAS_LOG = &
        "SELECT EXISTS(SELECT 1 FROM logs WHERE logs.id = ? LIMIT 1)"

    ! Query to check if node exists.
    ! Arguments: nodes.id
    character(len=*), parameter, public :: SQL_HAS_NODE = &
        "SELECT EXISTS(SELECT 1 FROM nodes WHERE nodes.id = ? LIMIT 1)"

    ! Query to check if observation exists.
    ! Arguments: observs.id
    character(len=*), parameter, public :: SQL_HAS_OBSERV = &
        "SELECT EXISTS(SELECT 1 FROM observs WHERE observs.id = ? LIMIT 1)"

    ! Query to check if sensor exists.
    ! Arguments: sensors.id
    character(len=*), parameter, public :: SQL_HAS_SENSOR = &
        "SELECT EXISTS(SELECT 1 FROM sensors WHERE sensors.id = ? LIMIT 1)"

    ! Query to check if target exists.
    ! Arguments: targets.id
    character(len=*), parameter, public :: SQL_HAS_TARGET = &
        "SELECT EXISTS(SELECT 1 FROM targets WHERE targets.id = ? LIMIT 1)"

    ! **************************************************************************
    ! SELECT COUNT QUERIES.
    ! **************************************************************************
    character(len=*), parameter, public :: SQL_SELECT_NBEATS = &
        "SELECT COUNT(row_id) FROM beats"

    ! Query to select number of time series by time range.
    character(len=*), parameter, public :: SQL_SELECT_NDATA_POINTS = &
        "SELECT COUNT(observs.row_id) FROM observs "                  // &
        "INNER JOIN nodes ON nodes.row_id = observs.node_id "         // &
        "INNER JOIN sensors ON sensors.row_id = observs.sensor_id "   // &
        "INNER JOIN targets ON targets.row_id = observs.target_id "   // &
        "INNER JOIN requests ON requests.observ_id = observs.row_id " // &
        "INNER JOIN responses ON responses.request_id = requests.row_id"

    character(len=*), parameter, public :: SQL_SELECT_NLOGS = "SELECT COUNT(row_id) FROM logs"

    ! Query to select the number of observations.
    character(len=*), parameter, public :: SQL_SELECT_NOBSERVS = &
        "SELECT COUNT(observs.row_id) FROM observs "                // &
        "INNER JOIN nodes ON nodes.row_id = observs.node_id "       // &
        "INNER JOIN sensors ON sensors.row_id = observs.sensor_id " // &
        "INNER JOIN targets ON targets.row_id = observs.target_id"

    ! Query to select number of observation views.
    character(len=*), parameter, public :: SQL_SELECT_NOBSERV_VIEWS = &
        "SELECT COUNT(observs.row_id) FROM observs "                  // &
        "INNER JOIN nodes ON nodes.row_id = observs.node_id "         // &
        "INNER JOIN sensors ON sensors.row_id = observs.sensor_id "   // &
        "INNER JOIN targets ON targets.row_id = observs.target_id "   // &
        "INNER JOIN requests ON requests.observ_id = observs.row_id " // &
        "INNER JOIN responses ON responses.request_id = requests.row_id"

    ! Query to select number of sensors.
    character(len=*), parameter, public :: SQL_SELECT_NSENSORS = &
        "SELECT COUNT(sensors.row_id) FROM sensors " // &
        "INNER JOIN nodes ON nodes.row_id = sensors.node_id"

    ! **************************************************************************
    ! SELECT QUERIES.
    ! **************************************************************************
    ! Query to select beats.
    character(len=*), parameter, public :: SQL_SELECT_BEATS = &
        "SELECT "     // &
        "node_id, "   // &
        "address, "   // &
        "client, "    // &
        "time_sent, " // &
        "time_recv, " // &
        "error, "     // &
        "interval, "  // &
        "uptime "     // &
        "FROM beats"

    ! Query to select data points (time series).
    character(len=*), parameter, public :: SQL_SELECT_DATA_POINTS = &
        "SELECT "                                                     // &
        "requests.timestamp, "                                        // &
        "responses.value "                                            // &
        "FROM observs "                                               // &
        "INNER JOIN nodes ON nodes.row_id = observs.node_id "         // &
        "INNER JOIN sensors ON sensors.row_id = observs.sensor_id "   // &
        "INNER JOIN targets ON targets.row_id = observs.target_id "   // &
        "INNER JOIN requests ON requests.observ_id = observs.row_id " // &
        "INNER JOIN responses ON responses.request_id = requests.row_id"

    ! Query to select logs.
    character(len=*), parameter, public :: SQL_SELECT_LOGS = &
        "SELECT "     // &
        "id, "        // &
        "level, "     // &
        "error, "     // &
        "timestamp, " // &
        "node_id, "   // &
        "sensor_id, " // &
        "target_id, " // &
        "observ_id, " // &
        "source, "    // &
        "message "    // &
        "FROM logs"

    ! Query to select nodes.
    character(len=*), parameter, public :: SQL_SELECT_NODES = &
        "SELECT "           // &
        "nodes.id, "        // &
        "nodes.name, "      // &
        "nodes.meta, "      // &
        "nodes.x, "         // &
        "nodes.y, "         // &
        "nodes.z, "         // &
        "nodes.longitude, " // &
        "nodes.latitude, "  // &
        "nodes.elevation "  // &
        "FROM nodes"

    ! Query to select observation ids.
    character(len=*), parameter, public :: SQL_SELECT_OBSERV_IDS = &
        "SELECT observs.id FROM observs "                           // &
        "INNER JOIN nodes ON nodes.row_id = observs.node_id "       // &
        "INNER JOIN sensors ON sensors.row_id = observs.sensor_id " // &
        "INNER JOIN targets ON targets.row_id = observs.target_id"

    ! Query to select observations.
    character(len=*), parameter, public :: SQL_SELECT_OBSERVS = &
        "SELECT "                                                   // &
        "observs.id, "                                              // &
        "nodes.id, "                                                // &
        "sensors.id, "                                              // &
        "targets.id, "                                              // &
        "observs.name, "                                            // &
        "observs.timestamp, "                                       // &
        "observs.source, "                                          // &
        "observs.device, "                                          // &
        "observs.priority, "                                        // &
        "observs.error, "                                           // &
        "observs.next, "                                            // &
        "observs.nreceivers, "                                      // &
        "observs.nrequests "                                        // &
        "FROM observs "                                             // &
        "INNER JOIN nodes ON nodes.row_id = observs.node_id "       // &
        "INNER JOIN sensors ON sensors.row_id = observs.sensor_id " // &
        "INNER JOIN targets ON targets.row_id = observs.target_id"

    ! Query to select observation views.
    character(len=*), parameter, public :: SQL_SELECT_OBSERV_VIEWS = &
        "SELECT "                                                     // &
        "observs.id, "                                                // &
        "nodes.id, "                                                  // &
        "sensors.id, "                                                // &
        "targets.id, "                                                // &
        "observs.name, "                                              // &
        "observs.error, "                                             // &
        "requests.name, "                                             // &
        "requests.timestamp, "                                        // &
        "requests.error, "                                            // &
        "responses.name, "                                            // &
        "responses.unit, "                                            // &
        "responses.type, "                                            // &
        "responses.error, "                                           // &
        "responses.value "                                            // &
        "FROM observs "                                               // &
        "INNER JOIN nodes ON nodes.row_id = observs.node_id "         // &
        "INNER JOIN sensors ON sensors.row_id = observs.sensor_id "   // &
        "INNER JOIN targets ON targets.row_id = observs.target_id "   // &
        "INNER JOIN requests ON requests.observ_id = observs.row_id " // &
        "INNER JOIN responses ON responses.request_id = requests.row_id"

    ! Query to select a single receiver of an observation by index.
    ! Arguments: observs.id, receivers.idx
    character(len=*), parameter, public :: SQL_SELECT_RECEIVER = &
        "SELECT receivers.name FROM receivers "                       // &
        "INNER JOIN observs ON receivers.observ_id = observs.row_id " // &
        "WHERE observs.id = ? AND receivers.idx = ?"

    ! Query to select the observation receivers.
    ! Arguments: observs.id
    character(len=*), parameter, public :: SQL_SELECT_RECEIVERS = &
        "SELECT receivers.name FROM receivers "                       // &
        "INNER JOIN observs ON receivers.observ_id = observs.row_id " // &
        "WHERE observs.id = ? ORDER BY receivers.idx ASC"

    ! Query to select a request of an observation by index.
    ! Arguments: observs.id, requests.idx
    character(len=*), parameter, public :: SQL_SELECT_REQUEST = &
        "SELECT "                                                    // &
        "requests.name, "                                            // &
        "requests.timestamp, "                                       // &
        "requests.request, "                                         // &
        "requests.response, "                                        // &
        "requests.delimiter, "                                       // &
        "requests.pattern, "                                         // &
        "requests.delay, "                                           // &
        "requests.error, "                                           // &
        "requests.mode, "                                            // &
        "requests.retries, "                                         // &
        "requests.state, "                                           // &
        "requests.timeout, "                                         // &
        "requests.nresponses "                                       // &
        "FROM requests "                                             // &
        "INNER JOIN observs ON observs.row_id = requests.observ_id " // &
        "WHERE observs.id = ? AND requests.idx = ?"

    ! Query to select the requests of an observation.
    ! Arguments: observs.id
    character(len=*), parameter, public :: SQL_SELECT_REQUESTS = &
        "SELECT "                                                    // &
        "requests.name, "                                            // &
        "requests.timestamp, "                                       // &
        "requests.request, "                                         // &
        "requests.response, "                                        // &
        "requests.delimiter, "                                       // &
        "requests.pattern, "                                         // &
        "requests.delay, "                                           // &
        "requests.error, "                                           // &
        "requests.mode, "                                            // &
        "requests.retries, "                                         // &
        "requests.state, "                                           // &
        "requests.timeout, "                                         // &
        "requests.nresponses "                                       // &
        "FROM requests "                                             // &
        "INNER JOIN observs ON observs.row_id = requests.observ_id " // &
        "WHERE observs.id = ? ORDER BY requests.idx ASC"

    ! Query to select a single response of a request.
    ! Arguments: observs.id, requests.idx, response.idx
    character(len=*), parameter, public :: SQL_SELECT_RESPONSE = &
        "SELECT "                                                        // &
        "responses.name, "                                               // &
        "responses.unit, "                                               // &
        "responses.type, "                                               // &
        "responses.error, "                                              // &
        "responses.value "                                               // &
        "FROM responses "                                                // &
        "INNER JOIN requests ON requests.row_id = responses.request_id " // &
        "INNER JOIN observs ON observs.row_id = requests.observ_id "     // &
        "WHERE observs.id = ? AND requests.idx = ? AND responses.idx = ?"

    ! Query to select the responses of a request.
    ! Arguments: observs.id, requests.idx
    character(len=*), parameter, public :: SQL_SELECT_RESPONSES = &
        "SELECT "                                                        // &
        "responses.name, "                                               // &
        "responses.unit, "                                               // &
        "responses.type, "                                               // &
        "responses.error, "                                              // &
        "responses.value  "                                              // &
        "FROM responses "                                                // &
        "INNER JOIN requests ON requests.row_id = responses.request_id " // &
        "INNER JOIN observs ON observs.row_id = requests.observ_id "     // &
        "WHERE observs.id = ? AND requests.idx = ? "                     // &
        "ORDER BY responses.idx ASC"

    ! Query to select sensors.
    character(len=*), parameter, public :: SQL_SELECT_SENSORS = &
        "SELECT "             // &
        "sensors.id, "        // &
        "nodes.id, "          // &
        "sensors.type, "      // &
        "sensors.name, "      // &
        "sensors.sn, "        // &
        "sensors.meta, "      // &
        "sensors.x, "         // &
        "sensors.y, "         // &
        "sensors.z, "         // &
        "sensors.longitude, " // &
        "sensors.latitude, "  // &
        "sensors.elevation "  // &
        "FROM sensors "       // &
        "INNER JOIN nodes ON nodes.row_id = sensors.node_id"

    ! Query to select targets.
    character(len=*), parameter, public :: SQL_SELECT_TARGETS = &
        "SELECT "             // &
        "targets.id, "        // &
        "targets.name, "      // &
        "targets.meta, "      // &
        "targets.state, "     // &
        "targets.x, "         // &
        "targets.y, "         // &
        "targets.z, "         // &
        "targets.longitude, " // &
        "targets.latitude, "  // &
        "targets.elevation "  // &
        "FROM targets"

    ! **************************************************************************
    ! SYNC QUERIES.
    ! **************************************************************************
    ! Query to upsert sync_logs data.
    ! Arguments: logs.id, sync_observs.timestamp, sync_observs.code, sync_observs.attempts
    character(len=*), parameter, public :: SQL_INSERT_SYNC_LOG = &
        "INSERT INTO sync_logs(log_id, timestamp, code, attempts) " // &
        "VALUES ((SELECT row_id FROM logs WHERE id = ?), ?, ?, ?) " // &
        "ON CONFLICT DO UPDATE SET "                                // &
        "log_id = excluded.log_id, "                                // &
        "timestamp = excluded.timestamp, "                          // &
        "code = excluded.code, "                                    // &
        "attempts = excluded.attempts"

    ! Query to upsert sync_nodes data.
    ! Arguments: nodes.id, sync_nodes.timestamp, sync_nodes.code, sync_nodes.attempts
    character(len=*), parameter, public :: SQL_INSERT_SYNC_NODE = &
        "INSERT INTO sync_nodes(node_id, timestamp, code, attempts) " // &
        "VALUES ((SELECT row_id FROM nodes WHERE id = ?), ?, ?, ?) "  // &
        "ON CONFLICT DO UPDATE SET "                                  // &
        "node_id = excluded.node_id, "                                // &
        "timestamp = excluded.timestamp, "                            // &
        "code = excluded.code, "                                      // &
        "attempts = excluded.attempts"

    ! Query to upsert sync_observs data.
    ! Arguments: observs.id, sync_observs.timestamp, sync_observs.code, sync_observs.attempts
    character(len=*), parameter, public :: SQL_INSERT_SYNC_OBSERV = &
        "INSERT INTO sync_observs(observ_id, timestamp, code, attempts) " // &
        "VALUES ((SELECT row_id FROM observs WHERE id = ?), ?, ?, ?) "    // &
        "ON CONFLICT DO UPDATE SET "                                      // &
        "observ_id = excluded.observ_id, "                                // &
        "timestamp = excluded.timestamp, "                                // &
        "code = excluded.code, "                                          // &
        "attempts = excluded.attempts"

    ! Query to upsert sync_sensors data.
    ! Arguments: sensors.id, sync_sensors.timestamp, sync_sensors.code, sync_sensors.attempts
    character(len=*), parameter, public :: SQL_INSERT_SYNC_SENSOR = &
        "INSERT INTO sync_sensors(sensor_id, timestamp, code, attempts) " // &
        "VALUES ((SELECT row_id FROM sensors WHERE id = ?), ?, ?, ?) "    // &
        "ON CONFLICT DO UPDATE SET "                                      // &
        "sensor_id = excluded.sensor_id, "                                // &
        "timestamp = excluded.timestamp, "                                // &
        "code = excluded.code, "                                          // &
        "attempts = excluded.attempts"

    ! Query to upsert sync_targets data.
    ! Arguments: targets.id, sync_targets.timestamp, sync_targets.code, sync_targets.attempts
    character(len=*), parameter, public :: SQL_INSERT_SYNC_TARGET = &
        "INSERT INTO sync_targets(target_id, timestamp, code, attempts) " // &
        "VALUES ((SELECT row_id FROM targets WHERE id = ?), ?, ?, ?) "    // &
        "ON CONFLICT DO UPDATE SET "                                      // &
        "target_id = excluded.target_id, "                                // &
        "timestamp = excluded.timestamp, "                                // &
        "code = excluded.code, "                                          // &
        "attempts = excluded.attempts"

    ! **************************************************************************
    ! SELECT SYNC COUNT QUERIES.
    ! **************************************************************************
    ! Query to select the number of logs to synchronise.
    character(len=*), parameter, public :: SQL_SELECT_NSYNC_LOGS = &
        "SELECT COUNT(logs.row_id) FROM logs "                   // &
        "LEFT JOIN sync_logs ON sync_logs.log_id = logs.row_id " // &
        "WHERE sync_logs.log_id IS NULL OR sync_logs.code NOT IN (201, 409)"

    ! Query to select the number of nodes to synchronise.
    character(len=*), parameter, public :: SQL_SELECT_NSYNC_NODES = &
        "SELECT COUNT(nodes.row_id) FROM nodes "                     // &
        "LEFT JOIN sync_nodes ON sync_nodes.node_id = nodes.row_id " // &
        "WHERE sync_nodes.node_id IS NULL OR sync_nodes.code NOT IN (201, 409)"

    ! Query to select the number of observations to synchronise.
    character(len=*), parameter, public :: SQL_SELECT_NSYNC_OBSERVS = &
        "SELECT COUNT(observs.row_id) FROM observs "                         // &
        "LEFT JOIN sync_observs ON sync_observs.observ_id = observs.row_id " // &
        "WHERE sync_observs.observ_id IS NULL OR sync_observs.code NOT IN (201, 409)"

    ! Query to select the number of sensors to synchronise.
    character(len=*), parameter, public :: SQL_SELECT_NSYNC_SENSORS = &
        "SELECT COUNT(sensors.row_id) FROM sensors "                         // &
        "LEFT JOIN sync_sensors ON sync_sensors.sensor_id = sensors.row_id " // &
        "WHERE sync_sensors.sensor_id IS NULL OR sync_sensors.code NOT IN (201, 409)"

    ! Query to select the number of targets to synchronise.
    character(len=*), parameter, public :: SQL_SELECT_NSYNC_TARGETS = &
        "SELECT COUNT(targets.row_id) FROM targets "                         // &
        "LEFT JOIN sync_targets ON sync_targets.target_id = targets.row_id " // &
        "WHERE sync_targets.target_id IS NULL OR sync_targets.code NOT IN (201, 409)"

    ! **************************************************************************
    ! SELECT SYNC QUERIES.
    ! **************************************************************************
    ! Query to select logs.id and sync_logs meta data of unsynchronised logs.
    character(len=*), parameter, public :: SQL_SELECT_SYNC_LOGS = &
        "SELECT "                                                             // &
        "logs.id, "                                                           // &
        "sync_logs.timestamp, "                                               // &
        "sync_logs.code, "                                                    // &
        "sync_logs.attempts "                                                 // &
        "FROM logs "                                                          // &
        "LEFT JOIN sync_logs ON sync_logs.log_id = logs.row_id "              // &
        "WHERE sync_logs.log_id IS NULL OR sync_logs.code NOT IN (201, 409) " // &
        "ORDER BY logs.timestamp ASC"

    ! Query to select node.id and sync_nodes meta data of
    ! unsynchronised nodes.
    character(len=*), parameter, public :: SQL_SELECT_SYNC_NODES = &
        "SELECT "                                                    // &
        "nodes.id, "                                                 // &
        "sync_nodes.timestamp, "                                     // &
        "sync_nodes.code, "                                          // &
        "sync_nodes.attempts "                                       // &
        "FROM nodes "                                                // &
        "LEFT JOIN sync_nodes ON sync_nodes.node_id = nodes.row_id " // &
        "WHERE sync_nodes.node_id IS NULL OR sync_nodes.code NOT IN (201, 409)"

    ! Query to select observ.id and sync_observs meta data of
    ! unsynchronised observations.
    character(len=*), parameter, public :: SQL_SELECT_SYNC_OBSERVS = &
        "SELECT "                                                                      // &
        "observs.id, "                                                                 // &
        "sync_observs.timestamp, "                                                     // &
        "sync_observs.code, "                                                          // &
        "sync_observs.attempts "                                                       // &
        "FROM observs "                                                                // &
        "LEFT JOIN sync_observs ON sync_observs.observ_id = observs.row_id "           // &
        "WHERE sync_observs.observ_id IS NULL OR sync_observs.code NOT IN (201, 409) " // &
        "ORDER BY observs.timestamp ASC"

    ! Query to select sensor.id and sync_sensors meta data of
    ! unsynchronised sensors.
    character(len=*), parameter, public :: SQL_SELECT_SYNC_SENSORS = &
        "SELECT "                                                            // &
        "sensors.id, "                                                       // &
        "sync_sensors.timestamp, "                                           // &
        "sync_sensors.code, "                                                // &
        "sync_sensors.attempts "                                             // &
        "FROM sensors "                                                      // &
        "LEFT JOIN sync_sensors ON sync_sensors.sensor_id = sensors.row_id " // &
        "WHERE sync_sensors.sensor_id IS NULL OR sync_sensors.code NOT IN (201, 409)"

    ! Query to select target.id and sync_targets meta data of
    ! unsynchronised targets.
    character(len=*), parameter, public :: SQL_SELECT_SYNC_TARGETS = &
        "SELECT "                                                            // &
        "targets.id, "                                                       // &
        "sync_targets.timestamp, "                                           // &
        "sync_targets.code, "                                                // &
        "sync_targets.attempts "                                             // &
        "FROM targets "                                                      // &
        "LEFT JOIN sync_targets ON sync_targets.target_id = targets.row_id " // &
        "WHERE sync_targets.target_id IS NULL OR sync_targets.code NOT IN (201, 409)"

    ! **************************************************************************
    ! JSON SELECT QUERIES.
    ! **************************************************************************
    ! Query to select beats in JSON format.
    character(len=*), parameter, public :: SQL_SELECT_JSON_BEATS = &
        "SELECT " // &
        "json_object('node_id', node_id, 'address', address, 'client', client, " // &
        "'time_sent', time_sent, 'time_recv', time_recv, 'error', error, "       // &
        "'interval', interval, 'uptime', uptime) "                               // &
        "FROM beats"

    ! Query to select logs in JSON format.
    character(len=*), parameter, public :: SQL_SELECT_JSON_LOGS = &
        "SELECT " // &
        "json_object('id', id, 'level', level, 'error', error, 'timestamp', timestamp, "               // &
        "'node_id', node_id, 'sensor_id', sensor_id, 'target_id', target_id, 'observ_id', observ_id, " // &
        "'source', source, 'message', message) "                                                       // &
        "FROM logs"

    ! Query to select nodes in JSON format.
    character(len=*), parameter, public :: SQL_SELECT_JSON_NODES = &
        "SELECT " // &
        "json_object('id', id, 'name', name, 'meta', meta, 'x', x, 'y', y, 'z', z, " // &
        "'longitude', longitude, 'latitude', latitude, 'elevation', elevation) "     // &
        "FROM nodes"

    ! Query to select sensors in JSON format.
    character(len=*), parameter, public :: SQL_SELECT_JSON_SENSORS = &
        "SELECT " // &
        "json_object('id', sensors.id, 'node_id', nodes.id, 'type', sensors.type, 'name', 'sensors.name, " // &
        "'sn', sensors.sn, 'meta', sensors.meta, 'x', sensors.x, 'y', sensors.y, 'z', sensors.z, "         // &
        "'longitude', sensors.longitude, 'latitude', sensors.latitude, 'elevation', sensors.elevation) "   // &
        "FROM sensors " // &
        "INNER JOIN nodes ON nodes.row_id = sensors.node_id"

    ! Query to select targets in JSON format.
    character(len=*), parameter, public :: SQL_SELECT_JSON_TARGETS = &
        "SELECT " // &
        "json_object('id', id, 'name', name, 'meta', meta, 'state', state, 'x', x, 'y', y, 'z', z, " // &
        "'longitude', longitude, 'latitude', latitude, 'elevation', elevation) "                     // &
        "FROM targets"
end module dm_sql
