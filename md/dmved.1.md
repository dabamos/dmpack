% DMVED(1) Version 2.0.0 | User Commands

# NAME

dmved -- reads status of connected MPPT charger or battery shunt

# SYNOPSIS

**dmved** \--help

**dmved** \--version

**dmved** \--**config** *file* \[\--**name** *name*\] \[\--**node** *id*\]
\[\--**sensor** *id*\] \[\--**target** *id*\] \[\--**device** *type*\]
\[\--**path** *path*\] \[\--**receiver** *name*\] \[\--**interval** *seconds*\]
\[\--**dump** *path*\] \[\--**logger** *name*\] \[\--**debug**\]

# DESCRIPTION

This program captures status data received from a connected Maximum Power Point
Tracking (MPPT) solar charge controller or SmartShunt battery monitor by Victron
Energy, using the VE.Direct protocol. Only the BlueSolar/SmartSolar MPPT series
and the SmartShunt monitor are supported. The serial port will be configured to
19200 baud (8N1).  Observations are forwarded via message queue in the given
interval if a receiver is specified.

The following VE.Direct fields are captured, depending on the device:

Alarm

:   Alarm condition active (on/off).

AR

:   Alarm reason.

CE

:   Consumed amp hours (mAh).

CS

:   State of operation.

DM

:   Mid-point deviation of the battery bank (%/10).

ERR

:   Error code.

H1

:   Depth of the deepest discharge (mAh).

H2

:   Depth of the last discharge (mAh).

H3

:   Depth of the average discharge (mAh).

H4

:   Number of charge cycles.

H5

:   Number of full discharges.

H6

:   Cumulative amp hours drawn (mAh).

H7

:   Minimum main (battery) voltage (mV).

H8

:   Maximum main (battery) voltage (mV).

H9

:   Number of seconds since last full charge (sec).

H10

:   Number of automatic synchronisations.

H11

:   Number of low main voltage alarms.

H12

:   Number of high main voltage alarms.

H15

:   Minimum auxiliary (battery) voltage (mV).

H16

:   Maximum auxiliary (battery) voltage (mV).

H17

:   Amount of produced energy (kWh/100).

H18

:   Amount of consumed energy (kWh/100).

H19

:   Yield total (kWh/100).

H20

:   Yield today (kWh/100).

H21

:   Maximum power today (W).

H22

:   Yield yesterday (kWh/100).

H23

:   Maximum power yesterday (W).

HSDS

:   Day sequence number (0 to 364).

I

:   Main or channel 1 battery current (mA).

IL

:   Load current (mA).

LOAD

:   Load output state (on/off).

MON

:   DC monitor mode.

MPPT

:   Tracker operation mode.

OR

:   Off reason.

P

:   Instantaneous power (W).

PPV

:   Panel power (W).

Relay

:   Relay state (on/off).

SOC

:   State-of-charge (%/10).

T

:   Battery temperature (degrees Celsius).

TTG

:   Time-to-go (min).

V

:   Main or channel 1 (battery) voltage (mV).

VM

:   Mid-point voltage of the battery bank (mV).

VPV

:   Panel voltage (mV).

VS

:   Auxiliary (starter) voltage (mV).

The response names equal the field names in lower-case.

# OPTIONS

**\--config**, **-c** *file*

:   File path to the configuration file.

**\--debug**, **-D**

:   Forward logs messages of level `LL_DEBUG` via IPC (if logger is set).

**\--device**, **-d** \[mppt\|shunt\]

:   Connected device, either MPPT or SmartShunt.

**\--dump**, **-o** *path*

:   Path of file or named pipe to dump received raw data to.

**\--help**, **-h**

:   Print available command-line arguments and quit.

**\--interval**, **-I** *seconds*

:   Observation emit interval in seconds (default is 60 seconds).

**\--logger**, **-l** *name*

:   Name of logger. If set, sends logs to *dmlogger(1)* process of given name.

**\--name**, **-n** *name*

:   Name of program instance and configuration (default is `dmved`).

**\--node**, **-N** *id*

:   Node id.

**\--path**, **-p** *path*

:   Path to TTY device (for example, `/dev/ttyUSB0`).

**\--receiver**, **-r** *name*

:   Name of the observation receiver, without leading `/`.

**\--sensor**, **-S** *id*

:   Sensor id.

**\--target**, **-T** *id*

:   Target id.

**\--verbose**, **-V**

:   Print log messages to *stderr*.

**\--version**, **-v**

:   Print version information and quit.

# EXIT STATUS

**0**

:   Success. Process terminated without errors.

**1**

:   Failure. Process failed.

# ENVIRONMENT

**DM_LOGGER**

:   Name of logger instance to send logs to.

**NO_COLOR**

:   Disable ANSI colour output.

# EXAMPLE

Read status data from MPPT connected via TTL:

    $ dmved -n dmved -c /usr/local/etc/dmpack/dmved.conf -V
