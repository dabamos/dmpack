-- test.lua

dmtest = {
    logger = "dmlogger",
    node = "dummy-node"
}

foo = "bar"
pi = 3.1415926535
value = 420
mylist = { 1, 2, 3 }

observ = {
    id = "00fbf0570bc54ac88041f4db64e38397",
    group_id = "",
    node_id = "dummy-node",
    sensor_id = "dummy-sensor",
    target_id = "dummy-target",
    timestamp = "1970-01-01T00:00:00.000+00:00",
    name = "lua-observ",
    source = "dmlua",
    device = "/dev/ttyU0",
    request = "?",
    response = "123\\r\\n",
    delimiter = "\\n",
    pattern = "(?:[0-9]+)",
    delay = 100,
    error = 1,
    next = 1,
    priority = 1,
    retries = 1,
    state = 1,
    timeout = 1000,
    nreceivers = 1,
    nresponses = 1,
    receivers = {
        "dmdb"
    },
    responses = {
        {
            name = "x",
            unit = "none",
            error = 0,
            value = 123.0
        }
    }
}

observ2 = {
    id = "9273ab62f9a349b6a4da6dd274ee83e7",
    group_id = "",
    node_id = "dummy-node",
    sensor_id = "dummy-sensor",
    target_id = "dummy-target",
    timestamp = "1970-01-01T00:00:00.000+00:00",
    name = "lua-observ-2",
    source = "dmlua",
    device = "/dev/ttyU1",
    request = "?",
    response = "999\\r\\n",
    delimiter = "\\n",
    pattern = "(?:[0-9]+)",
    delay = 0,
    error = 0,
    next = 0,
    priority = 0,
    retries = 0,
    state = 0,
    timeout = 2000,
    nreceivers = 1,
    nresponses = 1,
    receivers = {
        "dmdb", "dmdummy"
    },
    responses = {
        {
            name = "y",
            unit = "m",
            error = 1,
            value = 999.0
        }
    }
}

observs = { observ, observ }

config = {
    logger = "dmlogger",
    node = "dummy-node",
    list = { 1, 2, 3 },
    jobs = {
        {
            delay = 30 * 1000,
            disabled = 0,
            onetime = 0,
            group = { observ, observ2 }
        }
    }
}

dmreport = {
    node = "dummy-node",
    from = "1970-01-01T00:00:00.000+00:00",
    to = "2070-01-01T00:00:00.000+00:00",
    format = "html",
    output = "index.html",
    style = "share/dmpack.min.css",
    title = "Monitoring Report",
    subtitle = "Project",
    meta = "Now is the time for all good men to come to the aid of the party.",
    verbose = true,
    plots = {
        disabled = false,
        database = "testobserv.db",
        title = "Plots",
        meta = "Now is the time for all good men to come to the aid of the party.",
        observations = {
            {
                sensor = "dummy-sensor",
                target = "dummy-target",
                response = "tz0",
                unit = "deg C",
                format = "svg",
                title = "Temperature",
                subtitle = "tz0",
                meta = "Now is the time for all good men to come to the aid of the party.",
            }
        }
    },
    logs = {
        disabled = false,
        database = "testlog.db",
        minlevel = 3,
        maxlevel = 5,
        title = "Logs",
        meta = "Now is the time for all good men to come to the aid of the party.",
    }
}

dmtestconfig = {
    string = "a\\r\\n",
    integer = 420,
    logical = true,
    real = 1.0
}

function process(table)
    local s = dump(table)
    return table
end

function dump(o)
   if type(o) == 'table' then
      local s = '{ '
      for k, v in pairs(o) do
         if type(k) ~= 'number' then k = '"' .. k .. '"' end
         s = s .. '[' .. k .. '] = ' .. dump(v) .. ','
      end
      return s .. '} '
   else
      return tostring(o)
   end
end

function geocom()
    return geocom_beep_alarm()
end
