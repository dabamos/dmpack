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
    node_id = "dummy-node",
    sensor_id = "dummy-sensor",
    target_id = "dummy-target",
    id = "00fbf0570bc54ac88041f4db64e38397",
    name = "lua-observ",
    timestamp = "1970-01-01T00:00:00.000+00:00",
    source = "dmlua",
    tty = "/dev/ttyU0",
    priority = 1,
    error = 1,
    next = 1,
    nreceivers = 1,
    nrequests = 1,
    receivers = { "dmdb" },
    requests = {
        {
            name = "dummy-1",
            timestamp = "1970-01-01T00:00:00.000+00:00",
            request = "?",
            response = "123\\r\\n",
            delimiter = "\\n",
            pattern = "(?:[0-9]+)",
            delay = 100,
            error = 1,
            retries = 1,
            state = 1,
            timeout = 1000,
            nresponses = 1,
            responses = {
                {
                    name = "x",
                    unit = "none",
                    flag = 1,
                    value = 123.0
                }
            }
        }
    }
}

observs = { observ, observ }

config = {
    logger = "dmlogger",
    node = "dummy-node",
    jobs = {
        {
            delay = 30 * 1000,
            disabled = 0,
            onetime = 0,
            observation = observs[1]
        }
    }
}

dmreport = {
    node = "dummy-node",
    from = "1970-01-01T00:00:00.000+00:00",
    to = "2070-01-01T00:00:00.000+00:00",
    output = "./",
    style = "share/dmpack.min.css",
    title = "Monitoring Report",
    subtitle = "Project",
    meta = "Now is the time for all good men to come to the aid of the party.",
    plots = {
        disabled = 0,
        database = "testobserv.sqlite",
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
        disabled = 0,
        database = "testlog.sqlite",
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
    print(dump(table))
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
