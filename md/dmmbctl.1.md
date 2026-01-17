% DMMBCTL(1) Version 2.0.0 | User Commands

# NAME

dmmbctl -- read or write value from or to Modbus RTU/TCP register

# SYNOPSIS

**dmmbctl** \--help

**dmmbctl** \--version

**dmmbctl** \--**path** *path* \--**baudrate** *n* \--**bytesize** *n*
\--**parity** *name* \--**stopbits** *n* \--**slave** *n* \--**type** *name*
\[\--**order** *name*\] \[\--**code** *n*\] \[\--**read** *address*\]
\[\--**write** *address*\] \[\--**value** *n*\]

**dmmbctl** \--**address** *ip* \--**port** *port* \--**slave** *n* \--**type**
*name* \[\--**order** *name*\] \[\--**code** *n*\] \[\--**read** *address*\]
\[\--**write** *address*\] \[\--**value** *n*\] \[\--**debug**\]

# DESCRIPTION

The **dmmbctl** command-line program reads a value from or writes a value to a
register of a connected Modbus RTU/TCP device. Modbus RTU requires the
command-line arguments `--path`, `--baudrate`, `--bytesize`, `--parity`, and
`--stopbits`. For Modbus TCP, only `--address` and `--port` must be passed.

The following data types are supported:

`int16`

:   2-byte signed integer.

`int32`

:   4-byte signed integer.

`uint16`

:   2-byte unsigned integer.

`uint32`

:   4-byte unsigned integer.

`float`

:   4-byte float.

In order to read floating-point values, set `--type` to `float` and `--order` to
the byte order used by the Modbus device, either `abcd`, `badc`, `cdab`, or
`dcba`. Only integer values may be written to a register.

# OPTIONS

**\--address**, **-a** *ip*

:   Modbus TCP address (IPv4).

**\--baudrate**, **-B** *n*

:   Modbus RTU baud rate (9600, 19200, ...).

**\--bytesize**, **-Z** *n*

:   Modbus RTU byte size (5, 6, 7, 8).

**\--code**, **-C** *n*

:   Optional Modbus function code. Set to 1 to read coil status (function 0x01)
    and to 5 to write coil status (function 0x05).

**\--debug**, **-V**

:   Print debug messages from *libmodbus*.

**\--help**, **-h**

:   Print available command-line arguments and quit.

**\--order**, **-b** \[abcd\|badc\|cdab\|dcba\]

:   Byte order of float (`abcd`, `badc`, `cdab`, `dcba`).

**\--parity**, **-P** \[none\|even\|odd\]

:   Modbus RTU parity bits (`none`, `even`, `odd`).

**\--path**, **-p** *path*

:   Modbus RTU device path.

**\--port**, **-q** *port*

:   Modbus TCP port.

**\--read**, **-r** *register*

:   Read value from given Modbus register address.

**\--slave**, **-s** *n*

:   Slave id of Modbus device.

**\--stopbits**, **-O** *n*

:   Modbus RTU stop bits (1, 2).

**\--type**, **-t** \[int16\|int32\|uint16\|uint32\|float\]

:   Number type (`int16`, `int32`, `uint16`, `uint32`, `float`).

**\--value**, **-i** *n*

:   Integer value to write.

**\--version**, **-v**

:   Print version information and quit.

**\--write** , **-w** *register*

:   Write value to given Modbus register address.

# EXIT STATUS

**0**

:   Success. Modbus access was successful.

**1**

:   Failure. Program execution failed.

# EXAMPLE

Read the current temperature in degrees Celsius measured by a Pt100 RTD that is
connected to an I/O module with Modbus RTU interface:

    $ dmmbctl -p /dev/ttyUSB0 -B 19200 -Z 8 -P even -O 1 \
      -s 1 -r 50 -t float -b abcd
    21.217552185059

The I/O module is attached through an RS-485 adapter on `/dev/ttyUSB` (19200
baud, 8E1) and configured to use slave id 1. The value is read from register 50
and converted to float in `abcd` byte order.

# SEE ALSO

*dmmb(1)*
