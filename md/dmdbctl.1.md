% DMDBCTL(1) Version 2.0.0 | User Commands

# NAME

dmdbctl -- command-line interface to DMPACK observation databases

# SYNOPSIS

**dmdbctl** \--help

**dmdbctl** \--version

**dmdbctl** \--**database** *file* \--**id** *id*
\[\--\*create\*\|\--\*read\*\|\--\*update\*\|\--**delete** *type*\]
\[\--**name** *name*\] \[\--**meta** *meta*\] \[\--**node** *id*\] \[\--**type**
*sensor*\] \[\--**sn** *sn*\] \[\--**state** *n*\] \[\--**x** *x*\] \[\--**y**
*y*\] \[\--**z** *z*\] \[\--**longitude** *lon*\] \[\--**latitude** *lat*\]
\[\--**elevation** *elev*\] \[\--**verbose**\]

# DESCRIPTION

The **dmdbctl** utility program performs create, read, update, or delete
operations on the observation database:

Create

:   Add nodes, sensors, and targets to the database.

Read

:   Read nodes, sensors, and targets from database. Print the records to
    standard output.

Update

:   Update nodes, sensors, and targets in the database.

Delete

:   Delete nodes, sensors, and targets from the database.

Only nodes, sensors, and targets are supported. All data attributes are passed
through command-line arguments.

# OPTIONS

**\--create**, **-C** \[node\|sensor\|target\]

:   Create database record (node, sensor, or target).

**\--database**, **-d** *file*

:   Path to SQLite observation database.

**\--delete**, **-D** \[node\|sensor\|target\]

:   Delete database record (node, sensor, or target).

**\--elevation**, **-E** *elev*

:   Elevation of node, sensor, or target in metres.

**\--help**, **-h**

:   Print available command-line arguments and quit.

**\--id**, **-I** *id*

:   ID of node, sensor, or target.

**\--latitude**, **-L** *lat*

:   Latitude of node, sensor, or target in degrees (decimal).

**\--longitude**, **-G** *lon*

:   Longitude node, sensor, or target in degrees (decimal).

**\--meta**, **-M** *meta*

:   Meta description of node, sensor, or target.

**\--name**, **-n** *name*

:   Name of node, sensor, or target.

**\--node**, **-N** *id*

:   ID of the node the sensor is attached to.

**\--read**, **-R** \[node\|sensor\|target\]

:   Read database record (node, sensor, or target).

**\--sn**, **-Q** *sn*

:   Serial number of sensor.

**\--state**, **-S** *n*

:   Target state.

**\--type**, **-t** *sensor*

:   Sensor type, for example, `none`, `fs`, `process`, or `tps`.

**\--update**, **-U** \[node\|sensor\|target\]

:   Update database record (node, sensor, or target).

**\--verbose**, **-V**

:   Print log messages to *stderr*.

**\--version**, **-v**

:   Print version information and quit.

**\--x**, **-X** *x*

:   Node, sensor, or target x, usually in metres. May be in local or global
    coordinate system.

**\--y**, **-Y** *z*

:   Node, sensor, or target z, usually in metres. May be in local or global
    coordinate system.

**\--z**, **-Z** *z*

:   Node, sensor, or target z, usually in metres. May be in local or global
    coordinate system.

# EXIT STATUS

**0**

:   Success. Database operation finished without error.

**1**

:   Failure. Database operation failed.

# EXAMPLE

Add a new node of id `node-1` to the database:

    $ dmdbctl -d observ.db -C node -I node-1 -n "Node 1"

Delete target `target-1` from database:

    $ dmdbctl -d observ.db -D target -I target-1

Update the meta description of sensor `sensor-1`:

    $ dmdbctl -d observ.db -U sensor -I sensor-1 -M "in service"

# SEE ALSO

*dmdb(1)*, *dminit(1)*
