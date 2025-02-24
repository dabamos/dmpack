# DMPACK Test Programs

The DMPACK test programs are compiled by default. In order to run the RPC API
program `dmtestrpc`, set the hostname and the credentials of the server first:

```
$ export DM_API_HOST="localhost"
$ export DM_API_USERNAME="dummy-node"
$ export DM_API_PASSWORD="secret"
```

The following environment variables have to be set for `dmtestmail`:

```
$ export DM_MAIL_FROM="from@example.com"
$ export DM_MAIL_TO="to@example.com"
$ export DM_MAIL_HOST="example.com"
$ export DM_MAIL_USERNAME="username"
$ export DM_MAIL_PASSWORD="password"
```

The program `dmtestmqtt` requires host and port of the MQTT server:

```
$ export DM_MQTT_HOST="localhost"
$ export DM_MQTT_PORT="1883"
```

If not set, the affected tests will be skipped. Enable DWD API tests in
`dmtestdwd`:

```
$ export DM_DWD_API=1
```

To skip the gm, pipe, plotting, and message queue tests, set:

```
$ export DM_GM_SKIP=1
$ export DM_MQUEUE_SKIP=1
$ export DM_PIPE_SKIP=1
```

You may want to uncomment the variables in `runtest.sh`. Run all test programs
with:

```
$ sh runtests.sh
```

To write the test protocols to file, disable coloured output first by setting
the environment variable `NO_COLOR`:

```
$ export NO_COLOR=1
$ sh runtests.sh &> tests.log
```
