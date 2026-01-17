% DMEXPORT(1) Version 2.0.0 | User Commands

# NAME

dmexport -- exports records from database to file

# SYNOPSIS

**dmexport** \--help

**dmexport** \--version

**dmexport** \--**database** *file* \--**type** *type* \--**format** *format*
\--**node** *id* \[\--**output** *file*\] \[\--**sensor** *id*\] \[\--**target**
*id*\] \[\--**response** *name*\] \[\--**from** *timestamp*\] \[\--**to**
*timestamp*\] \[\--**header**\]

# DESCRIPTION

The **dmexport** program writes logs, nodes, sensors, targets, and observations
from database to file, in ASCII block, CSV, JSON, JSON Lines, Fortran 95
Namelist format. The ASCII block data format is only available for X/Y data
points.

The types data point, log, and observation require a given sensor id, target id,
and time range in ISO 8601 format.

If no output file is given, the data is printed to standard output. The output
file will be overwritten if it already exists. If no records are found, an empty
file will be created.

# OPTIONS

**\--database**, **-d** *file*

:   Path of the SQLite database (required).

**\--format**, **-f** \[block\|csv\|json\|jsonl\|nml\|tsv\]

:   Output file format, either ASCII block, CSV, JSON, JSON Lines, NML, or TSV
    (required). Only data points can be exported in block format.

**\--from**, **-B** *timestamp*

:   Start of time range in ISO 8601 (required for types `db`, `log`, `observ`).

**\--header**, **-H**

:   Enable CSV/TSV header.

**\--help**, **-h**

:   Print available command-line arguments and quit.

**\--node**, **-N** *id*

:   Node id (required).

**\--output**, **-o** *file*

:   Path of output file. Empty or `-` for standard output.

**\--response**, **-R** *name*

:   Response name (required for type `dp`).

**\--sensor**, **-S** *id*

:   Sensor id (required for types `dp` and `observ`).

**\--separator**, **-s** *char*

:   Single character used as separator instead of comma.

**\--target**, **-T** *id*

:   Target id (required for type `dp` and `observ`).

**\--to**, **-E** *timestamp*

:   End of time range in ISO 8601 (required for types `dp`, `log`, and
    `observ`).

**\--type**, **-t** \[beat\|dp\|log\|node\|observ\|sensor\|target\]

:   Type of records to export (required).

**\--version**, **-v**

:   Print version information and quit.

# EXIT STATUS

**0**

:   Success. Records have been exported.

**1**

:   Failure. Export of records failed.

# EXAMPLE

Export log messages from database to JSON file:

    $ dmexport -d log.db -t log -f json -N dummy-node \
      -B 2020-01-01 -E 2025-01-01 -o /tmp/log.json

Export observations from database to CSV file:

    $ dmexport -d observ.db -t observ -f csv -N dummy-node \
      -S dummy-sensor -T dummy-target -B 2020-01-01 -E 2025-01-01 \
      -o /tmp/observ.csv

# SEE ALSO

*dmimport(1)*
