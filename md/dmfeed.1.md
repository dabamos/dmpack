% DMFEED(1) Version 2.0.0 | User Commands

# NAME

dmfeed -- creates Atom XML feed from log messages

# SYNOPSIS

**dmfeed** \--help

**dmfeed** \--version

**dmfeed** \--**database** *file* \[\--**output** *file*\] \[\--**node** *id*\]
\[\--**minlevel** *level*\] \[\--**maxlevel** *level*\] \[\--**entries**
*count*\] \[\--**author** *name*\] \[\--**email** *address*\] \[\--**id**
*uuid*\] \[\--**title** *title*\] \[\--**subtitle** *title*\] \[\--**url**
*url*\] \[\--**xsl** *file*\]

**dmfeed** \--**config** *file* \[\--**name** *name*\]

# DESCRIPTION

This program creates a web feed from log messages in Atom Syndication Format
(RFC 4287). The log messages are read from database and written as XML to
standard output or file.

The feed id has to be a 36 characters long UUID with hyphens. News aggregators
use the id to identify the feed. Therefore, the id should not be reused among
different feeds. Run *dmuuid(1)* to generate a valid UUID.

The time stamp of the feed in the updated element is set to the date and time of
the last log message. If no logs have been added to the database since the last
file modification of the feed, the output file is not updated, unless argument
`--force` is passed.

# OPTIONS

**\--author**, **-A** *name*

:   Name of feed author or organisation.

**\--config**, **-c** *file*

:   Path to configuration file.

**\--database**, **-d** *file*

:   Path to SQLite log database.

**\--email**, **-M** *address*

:   E-mail address of feed author.

**\--entries**, **-E** *count*

:   Maximum number of entries in feed (default is 50).

**\--force**, **-F**

:   Force writing of output file. If not set, the output file will be updated
    only if new log records are available.

**\--help**, **-h**

:   Print available command-line arguments and quit.

**\--id**, **-I** *uuid*

:   UUID of the feed, 36 characters long with hyphens.

**\--maxlevel**, **-K** *level*

:   Select log messages of the given maximum log level (from 1 to 5).  Must be
    greater or equal the minimum level. The argument may be an integer or name
    string.

**\--minlevel**, **-L** *level*

:   Select log messages of the given minimum log level (from 1 to 5).  The
    argument may be an integer or name string.

**\--name**, **-n** *name*

:   Name of instance and table in given configuration file (default is
    `dmfeed`).

**\--node**, **-N** *id*

:   Select log messages of the given node id.

**\--output**, **-o** *file*

:   Path of the output file. If empty, the Atom feed will be printed to standard
    output.

**\--subtitle**, **-G** *title*

:   Sub-title of feed.

**\--title**, **-C** *title*

:   Title of feed.

**\--url**, **-U** *url*

:   Public URL of the feed.

**\--version**, **-v**

:   Print version information and quit.

**\--xsl**, **-X**

:   URL or path of optional XSLT style sheet. Web browsers may be able to
    display the Atom feed in HTML format if a style sheet is provided.

# EXIT STATUS

**0**

:   Success. Feed has been created.

**1**

:   Failure. Feed creation failed.

# EXAMPLE

Generate a unique feed id, and then write the last 50 log messages in Atom
format to file `feed.xml`:

    $ dmuuid -p
    19c12109-3e1c-422c-ae36-3ba19281f2e
    $ dmfeed -d /var/dmpack/log.db -o feed.xml \
      -I 19c12109-3e1c-422c-ae36-3ba19281f2e

# SEE ALSO

*dmuuid(1)*
