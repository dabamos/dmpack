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
    integer, parameter, public :: SQL_TABLE_IMAGES       = 9  !! Images table.
    integer, parameter, public :: SQL_TABLE_BEATS        = 10 !! Heartbeats table.
    integer, parameter, public :: SQL_TABLE_SYNC_NODES   = 11 !! Sync nodes table.
    integer, parameter, public :: SQL_TABLE_SYNC_SENSORS = 12 !! Sync sensors table.
    integer, parameter, public :: SQL_TABLE_SYNC_TARGETS = 13 !! Sync targets table.
    integer, parameter, public :: SQL_TABLE_SYNC_OBSERVS = 14 !! Sync observations table.
    integer, parameter, public :: SQL_TABLE_SYNC_LOGS    = 15 !! Sync logs table.
    integer, parameter, public :: SQL_TABLE_SYNC_IMAGES  = 16 !! Sync images table.
    integer, parameter, public :: SQL_TABLE_LAST         = 16 !! Never use this.

    integer, parameter, public :: SQL_TABLE_NAME_LEN = 12 !! Max. length of table names.

    character(len=*), parameter, public :: SQL_TABLE_NAMES(SQL_TABLE_LAST) = [ &
        character(len=SQL_TABLE_NAME_LEN) :: 'nodes', 'sensors', 'targets', 'observs', &
        'receivers', 'requests', 'responses', 'logs', 'images', 'beats', 'sync_nodes', &
        'sync_sensors', 'sync_targets', 'sync_observs', 'sync_logs', 'sync_images' &
    ] !! SQL table names.

    ! **************************************************************************
    ! UTILITY QUERIES.
    ! **************************************************************************
    ! Drop table.
    character(len=*), parameter, public :: SQL_DROP_TABLE = 'DROP TABLE IF EXISTS ?'

    ! Select all tables.
    character(len=*), parameter, public :: SQL_SELECT_TABLES = &
        'SELECT ' // &
        '(SELECT COUNT(*) FROM sqlite_schema WHERE type = ''table'' AND name NOT LIKE ''sqlite_%''), ' // &
        'name ' // &
        'FROM sqlite_schema ' // &
        'WHERE type = ''table'' AND name NOT LIKE ''sqlite_%'''

    ! Select the table of given name.
    character(len=*), parameter, public :: SQL_SELECT_TABLE = &
        'SELECT name FROM sqlite_master WHERE type = ''table'' AND name = ?'

    ! **************************************************************************
    ! TABLE CREATION QUERIES.
    ! **************************************************************************
    ! Beats schema.
    character(len=*), parameter, public :: SQL_CREATE_BEATS = &
        'CREATE TABLE IF NOT EXISTS beats(' // NL // &
        'beat_id   INTEGER PRIMARY KEY,' // NL // &
        'node_id   TEXT    NOT NULL UNIQUE,' // NL // &
        'address   TEXT,' // NL // &
        'client    TEXT,' // NL // &
        'time_sent TEXT    NOT NULL DEFAULT ''1970-01-01T00:00:00.000000+00:00'',' // NL // &
        'time_recv TEXT    NOT NULL DEFAULT (strftime(''%FT%R:%f000+00:00'')),' // NL // &
        'error     INTEGER NOT NULL DEFAULT 0,' // NL // &
        'interval  INTEGER NOT NULL DEFAULT 0,' // NL // &
        'uptime    INTEGER NOT NULL DEFAULT 0) STRICT'

    ! Logs schema
    character(len=*), parameter, public :: SQL_CREATE_LOGS = &
        'CREATE TABLE IF NOT EXISTS logs(' // NL // &
        'log_id    INTEGER PRIMARY KEY,' // NL // &
        'id        TEXT    NOT NULL UNIQUE,' // NL // &
        'level     INTEGER NOT NULL DEFAULT 0,' // NL // &
        'error     INTEGER NOT NULL DEFAULT 0,' // NL // &
        'timestamp TEXT    NOT NULL DEFAULT (strftime(''%FT%R:%f000+00:00'')),' // NL // &
        'node_id   TEXT,' // NL // &
        'sensor_id TEXT,' // NL // &
        'target_id TEXT,' // NL // &
        'observ_id TEXT,' // NL // &
        'source    TEXT,' // NL // &
        'message   TEXT) STRICT'

    ! Sensor nodes schema.
    character(len=*), parameter, public :: SQL_CREATE_NODES = &
        'CREATE TABLE IF NOT EXISTS nodes(' // NL // &
        'node_id INTEGER PRIMARY KEY,' // NL // &
        'id      TEXT NOT NULL UNIQUE,' // NL // &
        'name    TEXT NOT NULL,' // NL // &
        'meta    TEXT,' // NL // &
        'x       REAL NOT NULL DEFAULT 0.0,' // NL // &
        'y       REAL NOT NULL DEFAULT 0.0,' // NL // &
        'z       REAL NOT NULL DEFAULT 0.0,' // NL // &
        'lon     REAL NOT NULL DEFAULT 0.0,' // NL // &
        'lat     REAL NOT NULL DEFAULT 0.0,' // NL // &
        'alt     REAL NOT NULL DEFAULT 0.0) STRICT'

    ! Sensors schema.
    character(len=*), parameter, public :: SQL_CREATE_SENSORS = &
        'CREATE TABLE IF NOT EXISTS sensors(' // NL // &
        'sensor_id INTEGER PRIMARY KEY,' // NL // &
        'node_id   INTEGER NOT NULL,' // NL // &
        'id        TEXT    NOT NULL UNIQUE,' // NL // &
        'type      INTEGER NOT NULL DEFAULT 0,' // NL // &
        'name      TEXT    NOT NULL,' // NL // &
        'sn        TEXT,' // NL // &
        'meta      TEXT,' // NL // &
        'x         REAL    NOT NULL DEFAULT 0.0,' // NL // &
        'y         REAL    NOT NULL DEFAULT 0.0,' // NL // &
        'z         REAL    NOT NULL DEFAULT 0.0,' // NL // &
        'lon       REAL    NOT NULL DEFAULT 0.0,' // NL // &
        'lat       REAL    NOT NULL DEFAULT 0.0,' // NL // &
        'alt       REAL    NOT NULL DEFAULT 0.0,' // NL // &
        'FOREIGN KEY (node_id) REFERENCES nodes(node_id)) STRICT'

    ! Targets schema.
    character(len=*), parameter, public :: SQL_CREATE_TARGETS = &
        'CREATE TABLE IF NOT EXISTS targets(' // NL // &
        'target_id INTEGER PRIMARY KEY,' // NL // &
        'id        TEXT    NOT NULL UNIQUE,' // NL // &
        'name      TEXT,' // NL // &
        'meta      TEXT,' // NL // &
        'state     INTEGER NOT NULL DEFAULT 0,' // NL // &
        'x         REAL    NOT NULL DEFAULT 0.0,' // NL // &
        'y         REAL    NOT NULL DEFAULT 0.0,' // NL // &
        'z         REAL    NOT NULL DEFAULT 0.0,' // NL // &
        'lon       REAL    NOT NULL DEFAULT 0.0,' // NL // &
        'lat       REAL    NOT NULL DEFAULT 0.0,' // NL // &
        'alt       REAL    NOT NULL DEFAULT 0.0) STRICT'

    ! Observations schema.
    character(len=*), parameter, public :: SQL_CREATE_OBSERVS = &
        'CREATE TABLE IF NOT EXISTS observs(' // NL // &
        'observ_id  INTEGER PRIMARY KEY,' // NL // &
        'node_id    INTEGER NOT NULL,' // NL // &
        'sensor_id  INTEGER NOT NULL,' // NL // &
        'target_id  INTEGER NOT NULL,' // NL // &
        'id         TEXT    NOT NULL UNIQUE,' // NL // &
        'name       TEXT    NOT NULL,' // NL // &
        'timestamp  TEXT    NOT NULL DEFAULT (strftime(''%FT%R:%f000+00:00'')),' // NL // &
        'source     TEXT,' // NL // &
        'device     TEXT,' // NL // &
        'priority   INTEGER NOT NULL DEFAULT 0,' // NL // &
        'error      INTEGER NOT NULL DEFAULT 0,' // NL // &
        'next       INTEGER NOT NULL DEFAULT 0,' // NL // &
        'nreceivers INTEGER NOT NULL DEFAULT 0,' // NL // &
        'nrequests  INTEGER NOT NULL DEFAULT 0,' // NL // &
        'FOREIGN KEY (node_id)   REFERENCES nodes(node_id),' // NL // &
        'FOREIGN KEY (sensor_id) REFERENCES sensors(sensor_id),' // NL // &
        'FOREIGN KEY (target_id) REFERENCES targets(target_id)) STRICT'

    ! Receivers schema.
    character(len=*), parameter, public :: SQL_CREATE_RECEIVERS = &
        'CREATE TABLE IF NOT EXISTS receivers(' // NL // &
        'receiver_id INTEGER PRIMARY KEY,' // NL // &
        'observ_id   INTEGER NOT NULL,' // NL // &
        'idx         INTEGER NOT NULL,' // NL // &
        'name        TEXT    NOT NULL,' // NL // &
        'FOREIGN KEY (observ_id) REFERENCES observs(observ_id),' // NL // &
        'UNIQUE      (observ_id, idx) ON CONFLICT REPLACE) STRICT'

    ! Requests schema.
    character(len=*), parameter, public :: SQL_CREATE_REQUESTS = &
        'CREATE TABLE IF NOT EXISTS requests(' // NL // &
        'request_id INTEGER PRIMARY KEY,' // NL // &
        'observ_id  INTEGER NOT NULL,' // NL // &
        'idx        INTEGER NOT NULL,' // NL // &
        'name       TEXT    NOT NULL,' // NL // &
        'timestamp  TEXT    NOT NULL DEFAULT (strftime(''%FT%R:%f000+00:00'')),' // NL // &
        'request    TEXT,' // NL // &
        'response   TEXT,' // NL // &
        'delimiter  TEXT,' // NL // &
        'pattern    TEXT,' // NL // &
        'delay      INTEGER NOT NULL DEFAULT 0,' // NL // &
        'error      INTEGER NOT NULL DEFAULT 0,' // NL // &
        'mode       INTEGER NOT NULL DEFAULT 0,' // NL // &
        'retries    INTEGER NOT NULL DEFAULT 0,' // NL // &
        'state      INTEGER NOT NULL DEFAULT 0,' // NL // &
        'timeout    INTEGER NOT NULL DEFAULT 0,' // NL // &
        'nresponses INTEGER NOT NULL DEFAULT 0,' // NL // &
        'FOREIGN KEY (observ_id) REFERENCES observs(observ_id),' // NL // &
        'UNIQUE      (observ_id, idx) ON CONFLICT REPLACE) STRICT'

    ! Responses schema.
    character(len=*), parameter, public :: SQL_CREATE_RESPONSES = &
        'CREATE TABLE IF NOT EXISTS responses(' // NL // &
        'response_id INTEGER PRIMARY KEY,' // NL // &
        'request_id  INTEGER NOT NULL,' // NL // &
        'idx         INTEGER NOT NULL,' // NL // &
        'name        TEXT,' // NL // &
        'unit        TEXT,' // NL // &
        'type        INTEGER NOT NULL DEFAULT 0,' // NL // &
        'error       INTEGER NOT NULL DEFAULT 0,' // NL // &
        'value       REAL    NOT NULL DEFAULT 0.0,' // NL // &
        'FOREIGN KEY (request_id) REFERENCES requests(request_id),' // NL // &
        'UNIQUE      (request_id, idx) ON CONFLICT REPLACE) STRICT'

    ! **************************************************************************
    ! SYNC TABLE CREATION QUERIES.
    ! **************************************************************************
    ! Synchronised logs schema.
    character(len=*), parameter, public :: SQL_CREATE_SYNC_LOGS = &
        'CREATE TABLE IF NOT EXISTS sync_logs(' // NL // &
        'sync_log_id INTEGER PRIMARY KEY,' // NL // &
        'log_id      INTEGER NOT NULL UNIQUE,' // NL // &
        'timestamp   TEXT    NOT NULL DEFAULT (strftime(''%FT%R:%f000+00:00'')),' // NL // &
        'code        INTEGER NOT NULL DEFAULT 0,' // NL // &
        'attempts    INTEGER NOT NULL DEFAULT 0,' // NL // &
        'FOREIGN KEY (log_id) REFERENCES logs(log_id)) STRICT'

    ! Synchronised nodes schema.
    character(len=*), parameter, public :: SQL_CREATE_SYNC_NODES = &
        'CREATE TABLE IF NOT EXISTS sync_nodes(' // NL // &
        'sync_node_id INTEGER PRIMARY KEY,' // NL // &
        'node_id      INTEGER NOT NULL UNIQUE,' // NL // &
        'timestamp    TEXT    NOT NULL DEFAULT (strftime(''%FT%R:%f000+00:00'')),' // NL // &
        'code         INTEGER NOT NULL DEFAULT 0,' // NL // &
        'attempts     INTEGER NOT NULL DEFAULT 0,' // NL // &
        'FOREIGN KEY (node_id) REFERENCES nodes(node_id)) STRICT'

    ! Synchronised observations schema.
    character(len=*), parameter, public :: SQL_CREATE_SYNC_OBSERVS = &
        'CREATE TABLE IF NOT EXISTS sync_observs(' // NL // &
        'sync_observ_id INTEGER PRIMARY KEY,' // NL // &
        'observ_id      INTEGER NOT NULL UNIQUE,' // NL // &
        'timestamp      TEXT    NOT NULL DEFAULT (strftime(''%FT%R:%f000+00:00'')),' // NL // &
        'code           INTEGER NOT NULL DEFAULT 0,' // NL // &
        'attempts       INTEGER NOT NULL DEFAULT 0,' // NL // &
        'FOREIGN KEY (observ_id) REFERENCES observs(observ_id)) STRICT'

    ! Synchronised sensors schema.
    character(len=*), parameter, public :: SQL_CREATE_SYNC_SENSORS = &
        'CREATE TABLE IF NOT EXISTS sync_sensors(' // NL // &
        'sync_sensor_id INTEGER PRIMARY KEY,' // NL // &
        'sensor_id      INTEGER NOT NULL UNIQUE,' // NL // &
        'timestamp      TEXT    NOT NULL DEFAULT (strftime(''%FT%R:%f000+00:00'')),' // NL // &
        'code           INTEGER NOT NULL DEFAULT 0,' // NL // &
        'attempts       INTEGER NOT NULL DEFAULT 0,' // NL // &
        'FOREIGN KEY (sensor_id) REFERENCES sensors(sensor_id)) STRICT'

    ! Synchronised targets schema.
    character(len=*), parameter, public :: SQL_CREATE_SYNC_TARGETS = &
        'CREATE TABLE IF NOT EXISTS sync_targets(' // NL // &
        'sync_target_id INTEGER PRIMARY KEY,' // NL // &
        'target_id      INTEGER NOT NULL UNIQUE,' // NL // &
        'timestamp      TEXT    NOT NULL DEFAULT (strftime(''%FT%R:%f000+00:00'')),' // NL // &
        'code           INTEGER NOT NULL DEFAULT 0,' // NL // &
        'attempts       INTEGER NOT NULL DEFAULT 0,' // NL // &
        'FOREIGN KEY (target_id) REFERENCES targets(target_id)) STRICT'

    ! **************************************************************************
    ! CREATE INDEX QUERIES.
    ! **************************************************************************
    character(len=*), parameter, public :: SQL_CREATE_BEATS_INDICES(1) = [ character(len=64) :: &
        'CREATE INDEX IF NOT EXISTS idx_node_id ON beats(node_id)' &
    ]

    character(len=*), parameter, public :: SQL_CREATE_LOGS_INDICES(8) = [ character(len=64) :: &
        'CREATE INDEX IF NOT EXISTS idx_timestamp ON logs(timestamp)', &
        'CREATE INDEX IF NOT EXISTS idx_level     ON logs(level)', &
        'CREATE INDEX IF NOT EXISTS idx_error     ON logs(error)', &
        'CREATE INDEX IF NOT EXISTS idx_node_id   ON logs(node_id)', &
        'CREATE INDEX IF NOT EXISTS idx_sensor_id ON logs(sensor_id)', &
        'CREATE INDEX IF NOT EXISTS idx_target_id ON logs(target_id)', &
        'CREATE INDEX IF NOT EXISTS idx_observ_id ON logs(observ_id)', &
        'CREATE INDEX IF NOT EXISTS idx_source    ON logs(source)' &
    ]

    character(len=*), parameter, public :: SQL_CREATE_OBSERVS_INDICES(12) = [ character(len=128) :: &
        'CREATE INDEX IF NOT EXISTS idx_nodes_id             ON nodes(id)', &
        'CREATE INDEX IF NOT EXISTS idx_sensors_id           ON sensors(id)', &
        'CREATE INDEX IF NOT EXISTS idx_targets_id           ON targets(id)', &
        'CREATE INDEX IF NOT EXISTS idx_observs              ON observs(name, timestamp, error)', &
        'CREATE INDEX IF NOT EXISTS idx_observs_timestamp    ON observs(timestamp)', &
        'CREATE INDEX IF NOT EXISTS idx_receivers_idx        ON receivers(idx)', &
        'CREATE INDEX IF NOT EXISTS idx_requests_idx         ON requests(idx)', &
        'CREATE INDEX IF NOT EXISTS idx_requests_name        ON requests(name)', &
        'CREATE INDEX IF NOT EXISTS idx_requests_timestamp   ON requests(timestamp)', &
        'CREATE INDEX IF NOT EXISTS idx_responses            ON responses(request_id, idx, name, unit, type, error, value)', &
        'CREATE INDEX IF NOT EXISTS idx_responses_request_id ON responses(request_id)', &
        'CREATE INDEX IF NOT EXISTS idx_responses_name       ON responses(name)' &
    ]

    ! **************************************************************************
    ! TRIGGERS.
    ! **************************************************************************
    ! SQL trigger that removes any receivers, requests, and responses
    ! associated with an observation.
    character(len=*), parameter, public :: SQL_DELETE_OBSERV_TRIGGER = &
        'CREATE TRIGGER IF NOT EXISTS delete_observ_trigger' // NL // &
        '    BEFORE DELETE' // NL // &
        '    ON observs' // NL // &
        'BEGIN' // NL // &
        '    DELETE FROM receivers WHERE observ_id = OLD.observ_id;' // NL // &
        '    DELETE FROM responses WHERE request_id IN' // NL // &
        '        (' // NL // &
        '            SELECT' // NL // &
        '                request_id' // NL // &
        '            FROM' // NL // &
        '                requests' // NL // &
        '            INNER JOIN observs ON observs.observ_id = requests.observ_id' // NL // &
        '            WHERE observs.observ_id = OLD.observ_id' // NL // &
        '        );' // NL // &
        '    DELETE FROM requests WHERE observ_id = OLD.observ_id;' // NL // &
        'END'

    ! **************************************************************************
    ! DELETE QUERIES.
    ! **************************************************************************
    ! Query to delete beats.
    ! Arguments: beats.node_id
    character(len=*), parameter, public :: SQL_DELETE_BEAT = &
        'DELETE FROM beats WHERE node_id = ?'

    ! Query to delete log.
    ! Arguments: logs.log_id
    character(len=*), parameter, public :: SQL_DELETE_LOG = &
        'DELETE FROM logs WHERE log_id = ?'

    ! Query to delete node.
    ! Arguments: nodes.id
    character(len=*), parameter, public :: SQL_DELETE_NODE = &
        'DELETE FROM nodes WHERE id = ?'

    ! Query to delete sensor.
    ! Arguments: sensors.id
    character(len=*), parameter, public :: SQL_DELETE_SENSOR = &
        'DELETE FROM sensors WHERE id = ?'

    ! Query to delete target.
    ! Arguments: targets.id
    character(len=*), parameter, public :: SQL_DELETE_TARGET = &
        'DELETE FROM targets WHERE id = ?'

    ! Query to delete observation.
    ! Arguments: targets.id
    character(len=*), parameter, public :: SQL_DELETE_OBSERV = &
        'DELETE FROM observs WHERE id = ?'

    ! Query to delete all receivers of an observation.
    ! Arguments: targets.id
    character(len=*), parameter, public :: SQL_DELETE_RECEIVERS = &
        'DELETE FROM receivers WHERE observ_id IN (SELECT observ_id FROM observs WHERE id = ?)'

    ! Query to delete all requests of an observation.
    ! Arguments: observ.id
    character(len=*), parameter, public :: SQL_DELETE_REQUESTS = &
        'DELETE FROM requests WHERE observ_id IN (SELECT observ_id FROM observs WHERE id = ?)'

    ! Query to delete all responses of a request.
    ! Arguments: observ.id, requests.idx
    character(len=*), parameter, public :: SQL_DELETE_REQUEST_RESPONSES = &
        'DELETE FROM responses WHERE request_id IN ' // &
        '(SELECT request_id FROM requests ' // &
        'INNER JOIN observs ON observs.observ_id = requests.observ_id ' // &
        'WHERE observs.id = ? AND requests.idx = ?)'

    ! Query to delete all responses of an observation.
    ! Arguments: observ.id
    character(len=*), parameter, public :: SQL_DELETE_OBSERV_RESPONSES = &
        'DELETE FROM responses WHERE request_id IN ' // &
        '(SELECT request_id FROM requests ' // &
        'INNER JOIN observs ON observs.observ_id = requests.observ_id ' // &
        'WHERE observs.id = ?)'

    ! **************************************************************************
    ! INSERT QUERIES.
    ! **************************************************************************
    ! Query to upsert beat.
    ! Arguments: beats.node_id, beats.address, beats.time_sent,
    !            beats.time_recv, beats.interval, beats.error
    character(len=*), parameter, public :: SQL_INSERT_BEAT = &
        'INSERT INTO beats(node_id, address, client, time_sent, time_recv, error, interval, uptime) ' // &
        'VALUES (?, ?, ?, ?, ?, ?, ?, ?) ' // &
        'ON CONFLICT DO UPDATE SET ' // &
        'node_id = excluded.node_id, ' // &
        'address = excluded.address, ' // &
        'client = excluded.client, ' // &
        'time_sent = excluded.time_sent, ' // &
        'time_recv = excluded.time_recv, ' // &
        'error = excluded.error, ' // &
        'interval = excluded.interval, ' // &
        'uptime = excluded.uptime'

    ! Query to insert log.
    ! Arguments: logs.level, logs.error, logs.timestamp, logs.node_id,
    !            logs.sensor_id, logs.target_id, logs.observ_id, logs.message
    character(len=*), parameter, public :: SQL_INSERT_LOG = &
        'INSERT OR FAIL INTO ' // &
        'logs(id, level, error, timestamp, node_id, sensor_id, target_id, observ_id, source, message) ' // &
        'VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)'

    ! Query to insert node.
    ! Arguments: nodes.id, nodes.name, nodes.meta, nodes.x, nodes.y, nodes.z,
    !            nodes.lon, nodes.lat, nodes.alt
    character(len=*), parameter, public :: SQL_INSERT_NODE = &
        'INSERT OR FAIL INTO nodes(id, name, meta, x, y, z, lon, lat, alt) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)'

    ! Query to insert sensor.
    ! Arguments: sensors.id, nodes.id, sensors.type, sensors.id, sensors.name,
    !            sensors.sn, sensors.meta, sensors.x, sensors.y, sensors.z,
    !            sensors.lon, sensors.lat, sensors.alt
    character(len=*), parameter, public :: SQL_INSERT_SENSOR = &
        'INSERT OR FAIL INTO sensors(id, node_id, type, name, sn, meta, x, y, z, lon, lat, alt) VALUES (' // &
        '?, (SELECT node_id FROM nodes WHERE id = ?), ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)'

    ! Query to insert target.
    ! Arguments: targets.id, targets.name, targets.meta, targets.state,
    !            targets.x, targets.y, targets.z, targets.lon, targets.lat,
    !            targets.alt
    character(len=*), parameter, public :: SQL_INSERT_TARGET = &
        'INSERT OR FAIL INTO targets(id, name, meta, state, x, y, z, lon, lat, alt) VALUES (' // &
        '?, ?, ?, ?, ?, ?, ?, ?, ?, ?)'

    ! Query to insert observation.
    ! Arguments: nodes.id, sensors.id, targets.id, observs.id, observs.name,
    !            observs.timestamp, observs.source, observs.device, observs.priority,
    !            observs.error, observs.next, observs.nreceivers, observs.nrequests
    character(len=*), parameter, public :: SQL_INSERT_OBSERV = &
        'INSERT OR FAIL INTO observs ' // &
        '(id, node_id, sensor_id, target_id, name, timestamp, source, device, ' // &
        'priority, error, next, nreceivers, nrequests) ' // &
        'VALUES (' // &
        '?, ' // &
        '(SELECT node_id FROM nodes WHERE id = ?), ' // &
        '(SELECT sensor_id FROM sensors WHERE id = ?), ' // &
        '(SELECT target_id FROM targets WHERE id = ?), ' // &
        '?, ?, ?, ?, ?, ?, ?, ?, ?)'

    ! Query to insert receiver.
    ! Arguments: observs.id, receivers.idx, receivers.name
    character(len=*), parameter, public :: SQL_INSERT_RECEIVER = &
        'INSERT OR FAIL INTO receivers(observ_id, idx, name) VALUES (' // &
        '(SELECT observ_id FROM observs WHERE id = ?), ?, ?)'

    ! Query to insert request.
    ! Arguments: observs.id, requests.idx, requests.name, requests.timestamp,
    !            requests.request, requests.response, requests.delimiter,
    !            requests.pattern, requests.delay, requests.error, requests.mode,
    !            requests.retries, requests.state, requests.timeout,
    !            requests.nresponses
    character(len=*), parameter, public :: SQL_INSERT_REQUEST = &
        'INSERT OR FAIL INTO requests(observ_id, idx, name, timestamp, request, response, ' // &
        'delimiter, pattern, delay, error, mode, retries, state, timeout, nresponses) VALUES (' // &
        '(SELECT observ_id FROM observs WHERE id = ?), ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)'

    ! Query to insert response that references observation, request index.
    ! Arguments: responses.request_id, responses.idx, responses.name,
    !            responses.unit, response.type, responses.error, responses.value
    character(len=*), parameter, public :: SQL_INSERT_RESPONSE = &
        'INSERT OR FAIL INTO responses(request_id, idx, name, unit, type, error, value) VALUES (' // &
        '(SELECT requests.request_id FROM requests ' // &
        'INNER JOIN observs ON observs.observ_id = requests.observ_id ' // &
        'WHERE observs.id = ? AND requests.idx = ?), ?, ?, ?, ?, ?, ?)'

    ! **************************************************************************
    ! UPDATE QUERIES.
    ! **************************************************************************
    ! Query to update node.
    ! Arguments: nodes.name, nodes.meta, nodes.x, nodes.y, nodes.z
    !            nodes.lon, nodes.lat, nodes.alt, nodes.id
    character(len=*), parameter, public :: SQL_UPDATE_NODE = &
        'UPDATE OR FAIL nodes SET name = ?, meta = ?, x = ?, y = ?, z = ?, lon = ?, lat = ?, alt = ? WHERE id = ?'

    ! Query to update sensor.
    ! Arguments: nodes.id, sensors.type, sensors.id, sensors.name, sensors.sn,
    !            sensors.meta, sensors.x, sensors.y, sensors.z, sensors.lon,
    !            sensors.lat, sensors.alt, sensors.id
    character(len=*), parameter, public :: SQL_UPDATE_SENSOR = &
        'UPDATE OR FAIL sensors SET node_id = (SELECT node_id FROM nodes WHERE id = ?), ' // &
        'type = ?, name = ?, sn = ?, meta = ?, x = ?, y = ?, z = ?, lon = ?, lat = ?, alt = ? WHERE id = ?'

    ! Query to update target.
    ! Arguments: targets.name, targets.meta, targets.state, targets.x, targets.y,
    !            targets.z, targets.lon, targets.lat, targets.alt, targets.id
    character(len=*), parameter, public :: SQL_UPDATE_TARGET = &
        'UPDATE OR FAIL targets SET name = ?, meta = ?, state = ?, x = ?, y = ?, z = ?, ' // &
        'lon = ?, lat = ?, alt = ? WHERE id = ?'

    ! **************************************************************************
    ! SELECT EXISTS QUERIES.
    ! **************************************************************************
    ! Query to check if log exists.
    ! Arguments: logs.id
    character(len=*), parameter, public :: SQL_EXISTS_LOG = &
        'SELECT EXISTS(SELECT 1 FROM logs WHERE logs.id = ? LIMIT 1)'

    ! Query to check if node exists.
    ! Arguments: nodes.id
    character(len=*), parameter, public :: SQL_EXISTS_NODE = &
        'SELECT EXISTS(SELECT 1 FROM nodes WHERE nodes.id = ? LIMIT 1)'

    ! Query to check if observation exists.
    ! Arguments: observs.id
    character(len=*), parameter, public :: SQL_EXISTS_OBSERV = &
        'SELECT EXISTS(SELECT 1 FROM observs WHERE observs.id = ? LIMIT 1)'

    ! Query to check if sensor exists.
    ! Arguments: sensors.id
    character(len=*), parameter, public :: SQL_EXISTS_SENSOR = &
        'SELECT EXISTS(SELECT 1 FROM sensors WHERE sensors.id = ? LIMIT 1)'

    ! Query to check if target exists.
    ! Arguments: targets.id
    character(len=*), parameter, public :: SQL_EXISTS_TARGET = &
        'SELECT EXISTS(SELECT 1 FROM targets WHERE targets.id = ? LIMIT 1)'

    ! **************************************************************************
    ! SELECT QUERIES.
    ! **************************************************************************
    ! Query to select beat by node id.
    ! Arguments: beats.node_id
    character(len=*), parameter, public :: SQL_SELECT_BEAT = &
        'SELECT node_id, address, client, time_sent, time_recv, error, interval, uptime ' // &
        'FROM beats WHERE node_id = ?'

    ! Query to select all beats.
    character(len=*), parameter, public :: SQL_SELECT_BEATS = &
        'SELECT node_id, address, client, time_sent, time_recv, error, interval, uptime ' // &
        'FROM beats ORDER BY node_id ASC'

    ! Query to select data points (time series) by response name and time range.
    ! Arguments: nodes.id, sensors.id, targets.id, responses.name, responses.error,
    !            observs.timestamp (start), observs.timestamp (end)
    character(len=*), parameter, public :: SQL_SELECT_DATA_POINTS = &
        'SELECT ' // &
        'requests.timestamp, ' // &
        'responses.value ' // &
        'FROM observs ' // &
        'INNER JOIN nodes ON nodes.node_id = observs.node_id ' // &
        'INNER JOIN sensors ON sensors.sensor_id = observs.sensor_id ' // &
        'INNER JOIN targets ON targets.target_id = observs.target_id ' // &
        'INNER JOIN requests ON requests.observ_id = observs.observ_id ' // &
        'INNER JOIN responses ON responses.request_id = requests.request_id ' // &
        'WHERE ' // &
        'nodes.id = ? AND ' // &
        'sensors.id = ? AND ' // &
        'targets.id = ? AND ' // &
        'responses.name = ? AND ' // &
        'responses.error = ? AND ' // &
        'requests.timestamp >= ? AND ' // &
        'requests.timestamp < ? ' // &
        'ORDER BY requests.timestamp ASC'

    ! Query to select log by id.
    ! Arguments: logs.id
    character(len=*), parameter, public :: SQL_SELECT_LOG = &
        'SELECT id, level, error, timestamp, node_id, sensor_id, target_id, observ_id, source, message ' // &
        'FROM logs WHERE id = ?'

    ! Query to select all logs.
    character(len=*), parameter, public :: SQL_SELECT_LOGS = &
        'SELECT id, level, error, timestamp, node_id, sensor_id, target_id, observ_id, source, message ' // &
        'FROM logs'

    ! Query to select logs by node id.
    ! Arguments: logs.node_id
    character(len=*), parameter, public :: SQL_SELECT_LOGS_BY_NODE = &
        'SELECT id, level, error, timestamp, node_id, sensor_id, target_id, observ_id, source, message ' // &
        'FROM logs WHERE node_id = ? ORDER BY timestamp ASC'

    ! Query to select logs by node id and time range.
    ! Arguments: logs.node_id, logs.timestamp (start), logs.timestamp (end)
    character(len=*), parameter, public :: SQL_SELECT_LOGS_BY_NODE_TIME = &
        'SELECT id, level, error, timestamp, node_id, sensor_id, target_id, observ_id, source, message ' // &
        'FROM logs WHERE node_id = ? AND timestamp >= ? AND timestamp < ? ORDER BY timestamp ASC'

    ! Query to select logs by observ_id.
    ! Arguments: logs.observ_id
    character(len=*), parameter, public :: SQL_SELECT_LOGS_BY_OBSERV = &
        'SELECT id, level, error, timestamp, node_id, sensor_id, target_id, observ_id, source, message ' // &
        'FROM logs WHERE observ_id = ? ORDER BY timestamp ASC'

    ! Query to select logs by time range.
    ! Arguments: logs.timestamp (start), logs.timestamp (end)
    character(len=*), parameter, public :: SQL_SELECT_LOGS_BY_TIME = &
        'SELECT id, level, error, timestamp, node_id, sensor_id, target_id, observ_id, source, message ' // &
        'FROM logs WHERE timestamp >= ? AND timestamp < ? ORDER BY timestamp ASC'

    character(len=*), parameter, public :: SQL_SELECT_NBEATS = 'SELECT COUNT(*) FROM beats'

    character(len=*), parameter, public :: SQL_SELECT_NLOGS = 'SELECT COUNT(*) FROM logs'

    ! Query to select number of logs by node id.
    ! Arguments: logs.node_id
    character(len=*), parameter, public :: SQL_SELECT_NLOGS_BY_NODE = &
        'SELECT COUNT(*) FROM logs WHERE node_id = ?'

    ! Query to select number of logs by observation id.
    ! Arguments: logs.observ_id
    character(len=*), parameter, public :: SQL_SELECT_NLOGS_BY_OBSERV = &
        'SELECT COUNT(*) FROM logs WHERE observ_id = ?'

    ! Query to select number of sensors by node id.
    ! Arguments: nodes.id
    character(len=*), parameter, public :: SQL_SELECT_NSENSORS_BY_NODE = &
        'SELECT COUNT(*) FROM sensors ' // &
        'INNER JOIN nodes ON nodes.node_id = sensors.node_id ' // &
        'WHERE nodes.id = ?'

    ! Query to select node by id.
    ! Arguments: nodes.id
    character(len=*), parameter, public :: SQL_SELECT_NODE = &
        'SELECT ' // &
        'nodes.id, ' // &
        'nodes.name, ' // &
        'nodes.meta, ' // &
        'nodes.x, ' // &
        'nodes.y, ' // &
        'nodes.z, ' // &
        'nodes.lon, ' // &
        'nodes.lat, ' // &
        'nodes.alt ' // &
        'FROM nodes WHERE nodes.id = ?'

    ! Query to select all nodes.
    character(len=*), parameter, public :: SQL_SELECT_NODES = &
        'SELECT ' // &
        'nodes.id, ' // &
        'nodes.name, ' // &
        'nodes.meta, ' // &
        'nodes.x, ' // &
        'nodes.y, ' // &
        'nodes.z, ' // &
        'nodes.lon, ' // &
        'nodes.lat, ' // &
        'nodes.alt ' // &
        'FROM nodes ORDER BY nodes.id ASC'

    ! Query to select number of time series by time range.
    ! Arguments: nodes.id, sensors.id, targets.id, responses.name, responses.error,
    !            observs.timestamp (start), observs.timestamp (end)
    character(len=*), parameter, public :: SQL_SELECT_NDATA_POINTS = &
        'SELECT COUNT(*) FROM observs ' // &
        'INNER JOIN nodes ON nodes.node_id = observs.node_id ' // &
        'INNER JOIN sensors ON sensors.sensor_id = observs.sensor_id ' // &
        'INNER JOIN targets ON targets.target_id = observs.target_id ' // &
        'INNER JOIN requests ON requests.observ_id = observs.observ_id ' // &
        'INNER JOIN responses ON responses.request_id = requests.request_id ' // &
        'WHERE ' // &
        'nodes.id = ? AND ' // &
        'sensors.id = ? AND ' // &
        'targets.id = ? AND ' // &
        'responses.name = ? AND ' // &
        'responses.error = ? AND ' // &
        'requests.timestamp >= ? AND ' // &
        'requests.timestamp < ?'

    ! Query to select the number of observations.
    ! Arguments: nodes.id, sensors.id, targets.id
    character(len=*), parameter, public :: SQL_SELECT_NOBSERVS = &
        'SELECT COUNT(*) FROM observs ' // &
        'INNER JOIN nodes ON nodes.node_id = observs.node_id ' // &
        'INNER JOIN sensors ON sensors.sensor_id = observs.sensor_id ' // &
        'INNER JOIN targets ON targets.target_id = observs.target_id'

    ! Query to select the number of observations by id range.
    ! Arguments: nodes.id, sensors.id, targets.id, observs.id (start), observs.id (end)
    character(len=*), parameter, public :: SQL_SELECT_NOBSERVS_BY_ID = &
        SQL_SELECT_NOBSERVS // ' WHERE nodes.id = ? AND sensors.id = ? AND targets.id = ? ' // &
        'AND observs.id <> ? ' // &
        'AND observs.timestamp >= (SELECT timestamp FROM observs WHERE id = ?) ' // &
        'AND observs.timestamp < (SELECT timestamp FROM observs WHERE id = ?)'

    ! Query to select the number of observations by time range.
    ! Arguments: nodes.id, sensors.id, targets.id,
    !            observs.timestamp (start), observs.timestamp (end)
    character(len=*), parameter, public :: SQL_SELECT_NOBSERVS_BY_TIME = &
        SQL_SELECT_NOBSERVS // ' WHERE nodes.id = ? AND sensors.id = ? AND targets.id = ? ' // &
        'AND observs.timestamp >= ? AND observs.timestamp < ?'

    ! Query to select number of request responses by timestamp range.
    ! Arguments: nodes.id, sensors.id, targets.id, responses.name,
    !            observs.timestamp (start), observs.timestamp (end)
    character(len=*), parameter, public :: SQL_SELECT_NOBSERV_VIEWS = &
        'SELECT COUNT(*) FROM observs ' // &
        'INNER JOIN nodes ON nodes.node_id = observs.node_id ' // &
        'INNER JOIN sensors ON sensors.sensor_id = observs.sensor_id ' // &
        'INNER JOIN targets ON targets.target_id = observs.target_id ' // &
        'INNER JOIN requests ON requests.observ_id = observs.observ_id ' // &
        'INNER JOIN responses ON responses.request_id = requests.request_id ' // &
        'WHERE ' // &
        'nodes.id = ? AND ' // &
        'sensors.id = ? AND ' // &
        'targets.id = ? AND ' // &
        'responses.name = ? AND ' // &
        'requests.timestamp >= ? AND ' // &
        'requests.timestamp < ?'

    ! Query to select single observation by id.
    ! Arguments: observs.id
    character(len=*), parameter, public :: SQL_SELECT_OBSERV = &
        'SELECT ' // &
        'observs.id, ' // &
        'nodes.id, ' // &
        'sensors.id, ' // &
        'targets.id, ' // &
        'observs.name, ' // &
        'observs.timestamp, ' // &
        'observs.source, ' // &
        'observs.device, ' // &
        'observs.priority, ' // &
        'observs.error, ' // &
        'observs.next, ' // &
        'observs.nreceivers, ' // &
        'observs.nrequests ' // &
        'FROM observs ' // &
        'INNER JOIN nodes ON nodes.node_id = observs.node_id ' // &
        'INNER JOIN sensors ON sensors.sensor_id = observs.sensor_id ' // &
        'INNER JOIN targets ON targets.target_id = observs.target_id ' // &
        'WHERE observs.id = ?'

    ! Query to select of observation ids.
    ! Arguments: nodes.id, sensors.id, targets.id
    character(len=*), parameter, public :: SQL_SELECT_OBSERV_IDS = &
        'SELECT observs.id FROM observs ' // &
        'INNER JOIN nodes ON nodes.node_id = observs.node_id ' // &
        'INNER JOIN sensors ON sensors.sensor_id = observs.sensor_id ' // &
        'INNER JOIN targets ON targets.target_id = observs.target_id'

    ! Query to select observations.
    ! Arguments: nodes.id, sensors.id, targets.id
    character(len=*), parameter, public :: SQL_SELECT_OBSERVS = &
        'SELECT ' // &
        'observs.id, ' // &
        'nodes.id, ' // &
        'sensors.id, ' // &
        'targets.id, ' // &
        'observs.name, ' // &
        'observs.timestamp, ' // &
        'observs.source, ' // &
        'observs.device, ' // &
        'observs.priority, ' // &
        'observs.error, ' // &
        'observs.next, ' // &
        'observs.nreceivers, ' // &
        'observs.nrequests ' // &
        'FROM observs ' // &
        'INNER JOIN nodes ON nodes.node_id = observs.node_id ' // &
        'INNER JOIN sensors ON sensors.sensor_id = observs.sensor_id ' // &
        'INNER JOIN targets ON targets.target_id = observs.target_id'

    ! Query to select observations by id range.
    ! Arguments: nodes.id, sensors.id, targets.id, observs.id (after), observs.id (before)
    character(len=*), parameter, public :: SQL_SELECT_OBSERVS_BY_ID = &
        SQL_SELECT_OBSERVS // ' WHERE nodes.id = ? AND sensors.id = ? AND targets.id = ? ' // &
        'AND observs.id <> ? ' // &
        'AND observs.timestamp >= (SELECT timestamp FROM observs WHERE id = ?) ' // &
        'AND observs.timestamp < (SELECT timestamp FROM observs WHERE id = ?) ' // &
        'ORDER BY observs.timestamp ASC'

    ! Query to select observations by time range.
    ! Arguments: nodes.id, sensors.id, targets.id,
    !            observs.timestamp (start), observs.timestamp (end)
    character(len=*), parameter, public :: SQL_SELECT_OBSERVS_BY_TIME = &
        SQL_SELECT_OBSERVS // ' WHERE nodes.id = ? AND sensors.id = ? AND targets.id = ? ' // &
        'AND observs.timestamp >= ? AND observs.timestamp < ? ' // &
        'ORDER BY observs.timestamp ASC'

    ! Query to select request responses by timestamp range.
    ! Arguments: nodes.id, sensors.id, targets.id, responses.name,
    !            observs.timestamp (start), observs.timestamp (end)
    character(len=*), parameter, public :: SQL_SELECT_OBSERV_VIEWS = &
        'SELECT ' // &
        'observs.id, ' // &
        'nodes.id, ' // &
        'sensors.id, ' // &
        'targets.id, ' // &
        'observs.name, ' // &
        'observs.error, ' // &
        'requests.name, ' // &
        'requests.timestamp, ' // &
        'requests.error, ' // &
        'responses.name, ' // &
        'responses.unit, ' // &
        'responses.type, ' // &
        'responses.error, ' // &
        'responses.value ' // &
        'FROM observs ' // &
        'INNER JOIN nodes ON nodes.node_id = observs.node_id ' // &
        'INNER JOIN sensors ON sensors.sensor_id = observs.sensor_id ' // &
        'INNER JOIN targets ON targets.target_id = observs.target_id ' // &
        'INNER JOIN requests ON requests.observ_id = observs.observ_id ' // &
        'INNER JOIN responses ON responses.request_id = requests.request_id ' // &
        'WHERE ' // &
        'nodes.id = ? AND ' // &
        'sensors.id = ? AND ' // &
        'targets.id = ? AND ' // &
        'responses.name = ? AND ' // &
        'requests.timestamp >= ? AND ' // &
        'requests.timestamp < ? ' // &
        'ORDER BY requests.timestamp ASC'

    ! Query to select observation receiver by index.
    ! Arguments: observs.id, receivers.idx
    character(len=*), parameter, public :: SQL_SELECT_RECEIVER = &
        'SELECT receivers.name FROM receivers ' // &
        'INNER JOIN observs ON receivers.observ_id = observs.observ_id ' // &
        'WHERE observs.id = ? AND receivers.idx = ?'

    ! Query to select observation receivers.
    ! Arguments: observs.id
    character(len=*), parameter, public :: SQL_SELECT_RECEIVERS = &
        'SELECT receivers.name FROM receivers ' // &
        'INNER JOIN observs ON receivers.observ_id = observs.observ_id ' // &
        'WHERE observs.id = ? ORDER BY receivers.idx ASC'

    ! Query to select request by index.
    ! Arguments: observs.id, requests.idx
    character(len=*), parameter, public :: SQL_SELECT_REQUEST = &
        'SELECT ' // &
        'requests.name, ' // &
        'requests.timestamp, ' // &
        'requests.request, ' // &
        'requests.response, ' // &
        'requests.delimiter, ' // &
        'requests.pattern, ' // &
        'requests.delay, ' // &
        'requests.error, ' // &
        'requests.mode, ' // &
        'requests.retries, ' // &
        'requests.state, ' // &
        'requests.timeout, ' // &
        'requests.nresponses ' // &
        'FROM requests ' // &
        'INNER JOIN observs ON requests.observ_id = observs.observ_id ' // &
        'WHERE observs.id = ? AND requests.idx = ?'

    ! Query to select all requests of an observation.
    ! Arguments: observs.id
    character(len=*), parameter, public :: SQL_SELECT_REQUESTS = &
        'SELECT ' // &
        'requests.name, ' // &
        'requests.timestamp, ' // &
        'requests.request, ' // &
        'requests.response, ' // &
        'requests.delimiter, ' // &
        'requests.pattern, ' // &
        'requests.delay, ' // &
        'requests.error, ' // &
        'requests.mode, ' // &
        'requests.retries, ' // &
        'requests.state, ' // &
        'requests.timeout, ' // &
        'requests.nresponses ' // &
        'FROM requests ' // &
        'INNER JOIN observs ON requests.observ_id = observs.observ_id ' // &
        'WHERE observs.id = ? ORDER BY requests.idx ASC'

    ! Query to select responses by observs.id, requests.idx, and responses.idx.
    ! Arguments: observs.id, requests.idx, response.idx
    character(len=*), parameter, public :: SQL_SELECT_RESPONSE = &
        'SELECT ' // &
        'responses.name, ' // &
        'responses.unit, ' // &
        'responses.type, ' // &
        'responses.error, ' // &
        'responses.value ' // &
        'FROM responses ' // &
        'INNER JOIN requests ON requests.request_id = responses.request_id ' // &
        'INNER JOIN observs ON observs.observ_id = requests.observ_id ' // &
        'WHERE observs.id = ? AND requests.idx = ? AND responses.idx = ?'

    ! Query to select all responses of a request.
    ! Arguments: observs.id, requests.idx
    character(len=*), parameter, public :: SQL_SELECT_RESPONSES = &
        'SELECT ' // &
        'responses.name, ' // &
        'responses.unit, ' // &
        'responses.type, ' // &
        'responses.error, ' // &
        'responses.value  ' // &
        'FROM responses ' // &
        'INNER JOIN requests ON requests.request_id = responses.request_id ' // &
        'INNER JOIN observs ON observs.observ_id = requests.observ_id ' // &
        'WHERE observs.id = ? AND requests.idx = ? ORDER BY responses.idx ASC'

    ! Query to select sensor by id.
    ! Arguments: sensors.id
    character(len=*), parameter, public :: SQL_SELECT_SENSOR = &
        'SELECT ' // &
        'sensors.id, ' // &
        'nodes.id, ' // &
        'sensors.type, ' // &
        'sensors.name, ' // &
        'sensors.sn, ' // &
        'sensors.meta, ' // &
        'sensors.x, ' // &
        'sensors.y, ' // &
        'sensors.z, ' // &
        'sensors.lon, ' // &
        'sensors.lat, ' // &
        'sensors.alt ' // &
        'FROM sensors ' // &
        'INNER JOIN nodes ON nodes.node_id = sensors.node_id ' // &
        'WHERE sensors.id = ?'

    ! Query to select all sensors.
    character(len=*), parameter, public :: SQL_SELECT_SENSORS = &
        'SELECT ' // &
        'sensors.id, ' // &
        'nodes.id, ' // &
        'sensors.type, ' // &
        'sensors.name, ' // &
        'sensors.sn, ' // &
        'sensors.meta, ' // &
        'sensors.x, ' // &
        'sensors.y, ' // &
        'sensors.z, ' // &
        'sensors.lon, ' // &
        'sensors.lat, ' // &
        'sensors.alt ' // &
        'FROM sensors ' // &
        'INNER JOIN nodes ON nodes.node_id = sensors.node_id ' // &
        'ORDER BY sensors.id ASC'

    ! Query to select sensors by node.
    ! Arguments: nodes.id
    character(len=*), parameter, public :: SQL_SELECT_SENSORS_BY_NODE = &
        'SELECT ' // &
        'sensors.id, ' // &
        'nodes.id, ' // &
        'sensors.type, ' // &
        'sensors.name, ' // &
        'sensors.sn, ' // &
        'sensors.meta, ' // &
        'sensors.x, ' // &
        'sensors.y, ' // &
        'sensors.z, ' // &
        'sensors.lon, ' // &
        'sensors.lat, ' // &
        'sensors.alt ' // &
        'FROM sensors ' // &
        'INNER JOIN nodes ON nodes.node_id = sensors.node_id ' // &
        'WHERE nodes.id = ?'

    ! Query to select a target.
    ! Arguments: targets.id
    character(len=*), parameter, public :: SQL_SELECT_TARGET = &
        'SELECT ' // &
        'targets.id, ' // &
        'targets.name, ' // &
        'targets.meta, ' // &
        'targets.state, ' // &
        'targets.x, ' // &
        'targets.y, ' // &
        'targets.z, ' // &
        'targets.lon, ' // &
        'targets.lat, ' // &
        'targets.alt ' // &
        'FROM targets WHERE targets.id = ?'

    ! Query to select all targets.
    character(len=*), parameter, public :: SQL_SELECT_TARGETS = &
        'SELECT ' // &
        'targets.id, ' // &
        'targets.name, ' // &
        'targets.meta, ' // &
        'targets.state, ' // &
        'targets.x, ' // &
        'targets.y, ' // &
        'targets.z, ' // &
        'targets.lon, ' // &
        'targets.lat, ' // &
        'targets.alt ' // &
        'FROM targets ORDER BY targets.id ASC'

    ! **************************************************************************
    ! SYNC QUERIES.
    ! **************************************************************************
    ! Query to upsert sync_logs data.
    ! Arguments: logs.id, sync_observs.timestamp, sync_observs.code, sync_observs.attempts
    character(len=*), parameter, public :: SQL_INSERT_SYNC_LOG = &
        'INSERT INTO sync_logs(log_id, timestamp, code, attempts) ' // &
        'VALUES ((SELECT log_id FROM logs WHERE id = ?), ?, ?, ?) ' // &
        'ON CONFLICT DO UPDATE SET ' // &
        'log_id = excluded.log_id, ' // &
        'timestamp = excluded.timestamp, ' // &
        'code = excluded.code, ' // &
        'attempts = excluded.attempts'

    ! Query to upsert sync_nodes data.
    ! Arguments: nodes.id, sync_nodes.timestamp, sync_nodes.code, sync_nodes.attempts
    character(len=*), parameter, public :: SQL_INSERT_SYNC_NODE = &
        'INSERT INTO sync_nodes(node_id, timestamp, code, attempts) ' // &
        'VALUES ((SELECT node_id FROM nodes WHERE id = ?), ?, ?, ?) ' // &
        'ON CONFLICT DO UPDATE SET ' // &
        'node_id = excluded.node_id, ' // &
        'timestamp = excluded.timestamp, ' // &
        'code = excluded.code, ' // &
        'attempts = excluded.attempts'

    ! Query to upsert sync_observs data.
    ! Arguments: observs.id, sync_observs.timestamp, sync_observs.code, sync_observs.attempts
    character(len=*), parameter, public :: SQL_INSERT_SYNC_OBSERV = &
        'INSERT INTO sync_observs(observ_id, timestamp, code, attempts) ' // &
        'VALUES ((SELECT observ_id FROM observs WHERE id = ?), ?, ?, ?) ' // &
        'ON CONFLICT DO UPDATE SET ' // &
        'observ_id = excluded.observ_id, ' // &
        'timestamp = excluded.timestamp, ' // &
        'code = excluded.code, ' // &
        'attempts = excluded.attempts'

    ! Query to upsert sync_sensors data.
    ! Arguments: sensors.id, sync_sensors.timestamp, sync_sensors.code, sync_sensors.attempts
    character(len=*), parameter, public :: SQL_INSERT_SYNC_SENSOR = &
        'INSERT INTO sync_sensors(sensor_id, timestamp, code, attempts) ' // &
        'VALUES ((SELECT sensor_id FROM sensors WHERE id = ?), ?, ?, ?) ' // &
        'ON CONFLICT DO UPDATE SET ' // &
        'sensor_id = excluded.sensor_id, ' // &
        'timestamp = excluded.timestamp, ' // &
        'code = excluded.code, ' // &
        'attempts = excluded.attempts'

    ! Query to upsert sync_targets data.
    ! Arguments: targets.id, sync_targets.timestamp, sync_targets.code, sync_targets.attempts
    character(len=*), parameter, public :: SQL_INSERT_SYNC_TARGET = &
        'INSERT INTO sync_targets(target_id, timestamp, code, attempts) ' // &
        'VALUES ((SELECT target_id FROM targets WHERE id = ?), ?, ?, ?) ' // &
        'ON CONFLICT DO UPDATE SET ' // &
        'target_id = excluded.target_id, ' // &
        'timestamp = excluded.timestamp, ' // &
        'code = excluded.code, ' // &
        'attempts = excluded.attempts'

    character(len=*), parameter, public :: SQL_SELECT_NSYNC_LOGS = &
        'SELECT COUNT(*) FROM logs ' // &
        'LEFT JOIN sync_logs ON sync_logs.log_id = logs.log_id ' // &
        'WHERE sync_logs.log_id IS NULL OR sync_logs.code NOT IN (201, 409)'

    character(len=*), parameter, public :: SQL_SELECT_NSYNC_NODES = &
        'SELECT COUNT(*) FROM nodes ' // &
        'LEFT JOIN sync_nodes ON sync_nodes.node_id = nodes.node_id ' // &
        'WHERE sync_nodes.node_id IS NULL OR sync_nodes.code NOT IN (201, 409)'

    character(len=*), parameter, public :: SQL_SELECT_NSYNC_OBSERVS = &
        'SELECT COUNT(*) FROM observs ' // &
        'LEFT JOIN sync_observs ON sync_observs.observ_id = observs.observ_id ' // &
        'WHERE sync_observs.observ_id IS NULL OR sync_observs.code NOT IN (201, 409)'

    character(len=*), parameter, public :: SQL_SELECT_NSYNC_SENSORS = &
        'SELECT COUNT(*) FROM sensors ' // &
        'LEFT JOIN sync_sensors ON sync_sensors.sensor_id = sensors.sensor_id ' // &
        'WHERE sync_sensors.sensor_id IS NULL OR sync_sensors.code NOT IN (201, 409)'

    character(len=*), parameter, public :: SQL_SELECT_NSYNC_TARGETS = &
        'SELECT COUNT(*) FROM targets ' // &
        'LEFT JOIN sync_targets ON sync_targets.target_id = targets.target_id ' // &
        'WHERE sync_targets.target_id IS NULL OR sync_targets.code NOT IN (201, 409)'

    ! Query to select logs.id and sync_logs meta data of unsynchronised logs.
    character(len=*), parameter, public :: SQL_SELECT_SYNC_LOGS = &
        'SELECT ' // &
        'logs.id, ' // &
        'sync_logs.timestamp, ' // &
        'sync_logs.code, ' // &
        'sync_logs.attempts ' // &
        'FROM logs ' // &
        'LEFT JOIN sync_logs ON sync_logs.log_id = logs.log_id ' // &
        'WHERE sync_logs.log_id IS NULL OR sync_logs.code NOT IN (201, 409) ' // &
        'ORDER BY logs.timestamp ASC'

    ! Query to select node.id and sync_nodes meta data of
    ! unsynchronised nodes.
    character(len=*), parameter, public :: SQL_SELECT_SYNC_NODES = &
        'SELECT ' // &
        'nodes.id, ' // &
        'sync_nodes.timestamp, ' // &
        'sync_nodes.code, ' // &
        'sync_nodes.attempts ' // &
        'FROM nodes ' // &
        'LEFT JOIN sync_nodes ON sync_nodes.node_id = nodes.node_id ' // &
        'WHERE sync_nodes.node_id IS NULL OR sync_nodes.code NOT IN (201, 409)'

    ! Query to select observ.id and sync_observs meta data of
    ! unsynchronised observations.
    character(len=*), parameter, public :: SQL_SELECT_SYNC_OBSERVS = &
        'SELECT ' // &
        'observs.id, ' // &
        'sync_observs.timestamp, ' // &
        'sync_observs.code, ' // &
        'sync_observs.attempts ' // &
        'FROM observs ' // &
        'LEFT JOIN sync_observs ON sync_observs.observ_id = observs.observ_id ' // &
        'WHERE sync_observs.observ_id IS NULL OR sync_observs.code NOT IN (201, 409) ' // &
        'ORDER BY observs.timestamp ASC'

    ! Query to select sensor.id and sync_sensors meta data of
    ! unsynchronised sensors.
    character(len=*), parameter, public :: SQL_SELECT_SYNC_SENSORS = &
        'SELECT ' // &
        'sensors.id, ' // &
        'sync_sensors.timestamp, ' // &
        'sync_sensors.code, ' // &
        'sync_sensors.attempts ' // &
        'FROM sensors ' // &
        'LEFT JOIN sync_sensors ON sync_sensors.sensor_id = sensors.sensor_id ' // &
        'WHERE sync_sensors.sensor_id IS NULL OR sync_sensors.code NOT IN (201, 409)'

    ! Query to select target.id and sync_targets meta data of
    ! unsynchronised targets.
    character(len=*), parameter, public :: SQL_SELECT_SYNC_TARGETS = &
        'SELECT ' // &
        'targets.id, ' // &
        'sync_targets.timestamp, ' // &
        'sync_targets.code, ' // &
        'sync_targets.attempts ' // &
        'FROM targets ' // &
        'LEFT JOIN sync_targets ON sync_targets.target_id = targets.target_id ' // &
        'WHERE sync_targets.target_id IS NULL OR sync_targets.code NOT IN (201, 409)'

    ! **************************************************************************
    ! JSON SELECT QUERIES.
    ! **************************************************************************
    ! Query to select all beats in JSON format.
    character(len=*), parameter, public :: SQL_SELECT_JSON_BEATS = &
        'SELECT ' // &
        'json_object(''node_id'', node_id, ''address'', address, ''client'', client, ' // &
        '''time_sent'', time_sent, ''time_recv'', time_recv, ''error'', error, ' // &
        '''interval'', interval, ''uptime'', uptime) ' // &
        'FROM beats'

    ! Query to select all logs in JSON format.
    character(len=*), parameter, public :: SQL_SELECT_JSON_LOGS = &
        'SELECT ' // &
        'json_object(''id'', id, ''level'', level, ''error'', error, ''timestamp'', timestamp, ' // &
        '''node_id'', node_id, ''sensor_id'', sensor_id, ''target_id'', target_id, ''observ_id'', observ_id, ' // &
        '''source'', source, ''message'', message) ' // &
        'FROM logs'

    ! Query to select all nodes in JSON format.
    character(len=*), parameter, public :: SQL_SELECT_JSON_NODES = &
        'SELECT ' // &
        'json_object(''id'', id, ''name'', name, ''meta'', meta, ''x'', x, ''y'', y, ''z'', z, ' // &
        '''lon'', lon, ''lat'', lat, ''alt'', alt) ' // &
        'FROM nodes'

    ! Query to select all sensors in JSON format.
    character(len=*), parameter, public :: SQL_SELECT_JSON_SENSORS = &
        'SELECT ' // &
        'json_object(''id'', sensors.id, ''node_id'', nodes.id, ''type'', sensors.type, ''name'', ''sensors.name, ' // &
        '''sn'', sensors.sn, ''meta'', sensors.meta, ''x'', sensors.x, ''y'', sensors.y, ''z'', sensors.z, ' // &
        '''lon'', sensors.lon, ''lat'', sensors.lat, ''alt'', sensors.alt) ' // &
        'FROM sensors ' // &
        'INNER JOIN nodes ON nodes.node_id = sensors.node_id'

    ! Query to select all targets in JSON format.
    character(len=*), parameter, public :: SQL_SELECT_JSON_TARGETS = &
        'SELECT ' // &
        'json_object(''id'', id, ''name'', name, ''meta'', meta, ''state'', state, ''x'', x, ''y'', y, ''z'', z, ' // &
        '''lon'', lon, ''lat'', lat, ''alt'', alt) ' // &
        'FROM targets ORDER BY id ASC'
end module dm_sql
