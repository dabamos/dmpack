% DMIMPORT(1) Version 2.0.0 | User Commands

# NAME

dmimport -- imports records from file into database

# SYNOPSIS

**dmimport** \--help

**dmimport** \--version

**dmimport** \--**input** *file* \--**database** *file* \--**type** *type*
\[\--**quote** *char*\] \[\--**separator** *char*\] \[\--**dry**\]
\[\--**verbose**\]

# DESCRIPTION

The **dmimport** program reads logs, nodes, sensors, targets, and observations
in CSV format from file and imports them into the database.  The database
inserts are transaction-based. If an error occurs, the transaction is rolled
back, and no records are written into the database at all.

The database has to be a valid DMPACK database and must contain the tables
required for the input records. The nodes, sensors, and targets referenced by
input observations must exist in the database. The nodes referenced by input
sensors must exist as well.

# OPTIONS

**\--database**, **-d** *file*

:   Path of the SQLite database (required, unless in dry mode).

**\--dry**, **-D**

:   Enable dry mode. Read records from file and validate them without actual
    database import.

**\--help**, **-h**

:   Print available command-line arguments and quit.

**\--input**, **-i** *file*

:   Path of input file in CSV format.

**\--quote**, **-q** *char*

:   CSV quote character. If not passed, quoting is disabled by default.

**\--separator**, **-s** *char*

:   CSV separator character.

**\--type**, **-t** \[log\|node\|observ\|sensor\|target\]

:   Type of records to import (required).

**\--verbose**, **-V**

:   Print progress to *stdout*.

**\--version**, **-v**

:   Print version information and quit.

# EXIT STATUS

**0**

:   Success. Records have been imported.

**1**

:   Failure. Import of records failed.

# EXAMPLE

Import observations from CSV file into database:

    $ dmimport -t observ -i observ.csv -d observ.db -V

# SEE ALSO

*dmexport(1)*
