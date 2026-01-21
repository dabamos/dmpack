% DMREPORT(1) Version 2.0.0 | User Commands

# NAME

dmreport -- creates reports containing plots and logs

# SYNOPSIS

**dmreport** \--help

**dmreport** \--version

**dmreport** \--**config** *file* \[\--**name** *name*\] \[\--**node** *id*\]
\[\--**from** *timestamp*\] \[\--**to** *timestamp*\] \[\--**format** *name*\]
\[\--**output** *file*\] \[\--**style** *file*\]

**dmreport** \--**config** *file* \[\--**name** *name*\]

# DESCRIPTION

The **dmreport** program generates reports in HTML, PDF, or PostScript format,
containing plots of observations and/or log messages selected from database.
Plots are created by calling *gnuplot(1)* and inlining the returned image.
Reports in HTML format may contain GIF, PNG, or SVG images as base64-encoded
data URI. Reports in PostScript and PDF format are generated with *groff(1)* and
support only EPS images. Any style sheet file with classless CSS can be included
to alter the presentation of the HTML report. The output of **dmreport** is a
single HTML, PDF, or PostScript file.

A configuration file is mandatory to create reports. Only a few parameters may
be set through command-line arguments. Passed command-line arguments have
priority over settings in the configuration file.

# OPTIONS

**\--config**, **-c** *file*

:   File path to configuration file.

**\--format**, **-F** \[html\|ps\|pdf\]

:   Output file format, either HTML5, PostScript, or PDF.

**\--from**, **-B** *timestamp*

:   Start of the time range in ISO 8601.

**\--help**, **-h**

:   Print available command-line arguments and quit.

**\--name**, **-n** *name*

:   Name of instance and table in given configuration file (default is
    `dmreport`).

**\--node**, **-N** *id*

:   Select observations and log messages of the given node id.

**\--output**, **-o** *file*

:   File path of the generated report. Additional format descriptors are
    replaced with their values to set date and time dynamically (`%Y`, `%M`,
    `%D`, `%h`, `%m`, `%s`).

**\--style**, **-C** *file*

:   File path to the classless CSS file to be included in the report (optional).

**\--to**, **-E** *timestamp*

:   End of the time range in ISO 8601.

**\--version**, **-v**

:   Print version information and quit.

# EXIT STATUS

**0**

:   Success. Report was created.

**1**

:   Failure. Report creation failed.

# EXAMPLE

Write a report to file `report.html` based on settings in `dmreport.conf`:

    $ dmreport -n dmreport -c dmreport.conf -B 2020-01-01 \
      -E 2021-01-01 -f html -o report.html
