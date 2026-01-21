% DMBACKUP(1) Version 2.0.0 | User Commands

# NAME

dmbackup -- creates an online backup of a running database

# SYNOPSIS

**dmbackup** \--help

**dmbackup** \--version

**dmbackup** \--**database** *file* \--**backup** *file* \[\--**vacuum**\]
\[\--**wal**\]

# DESCRIPTION

The **dmbackup** utility creates an online backup of a running SQLite database.
By default, the SQLite backup API is used. The program is functional equivalent
to running the *sqlite3(1)* command-line interface:

    $ sqlite3 <database> ".backup '<backup>'"

**dmbackup** does not replace existing backup databases.

# OPTIONS

**\--backup**, **-b** *file*

:   Path of the backup database.

**\--database**, **-d** *file*

:   Path of the SQLite database to backup.

**\--help**, **-h**

:   Print available command-line arguments and quit.

**\--vacuum**, **-U**

:   Use `VACUUM` `INTO` instead of the SQLite backup API.

**\--verbose**, **-V**

:   Print progress to *stdout* (not in vacuum mode).

**\--version**, **-v**

:   Print version information and quit.

**\--wal**, **-W**

:   Enable WAL mode for backup database.

# EXIT STATUS

**0**

:   Success. Backup has been created.

**1**

:   Failure. Backup creation failed.

# EXAMPLE

Create an online backup of an observation database:

    $ dmbackup -d /var/dmpack/observ.db -b /tmp/observ.db

# SEE ALSO

*dminit(1)*
