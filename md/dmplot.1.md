% DMPLOT(1) Version 2.0.0 | User Commands

# NAME

dmplot -- creates plots from observations in database

# SYNOPSIS

**dmplot** \--help

**dmplot** \--version

**dmplot** \--**database** *file* \--**node** *id* \--**sensor** *id*
\--**target** *id* \--**response** *name* \--**from** *timestamp* \--**to**
*timestamp* \--**terminal** *terminal* \[\--**output** *file*\]
\[\--**background** *color*\] \[\--**foreground** *color*\] \[\--**font**
*name*\] \[\--**title** *title*\] \[\--**width** *n*\]

**dmplot** \--**config** *file* \[\--**name** *name*\]

# DESCRIPTION

The **dmplot** program is a front-end to *gnuplot(1)* that creates plots of
observations read from database. Plots are either written to file or displayed
in terminal or X11 window.

Depending on the selected terminal backend, you may have to set the environment
variable `GDFONTPATH` to the local font directory first:

    $ export GDFONTPATH=/usr/local/share/fonts/webfonts/

Plotting parameters passed via command-line have priority over those from
configuration file.

# OPTIONS

**\--background**, **-G** *color*

:   Optional background color of the plot (for example, `#ff0000` or `red`).

**\--config**, **-c** *file*

:   File path to the configuration file.

**\--database**, **-d** *file*

:   File path to the SQLite observation database.

**\--font**, **-A** *name*

:   Optional font name (for example, `Open` `Sans`, `arial.ttf`, or
    `monospace`).

**\--foreground**, **-P** *color*

:   Optional foreground color of the plot (for example, `#ffffff` or `white`).

**\--from**, **-B** *timestamp*

:   Start of time range in ISO 8601.

**\--height**, **-H** *n*

:   Optional plot height in pixels.

**\--help**, **-h**

:   Print available command-line arguments and quit.

**\--name**, **-n** *name*

:   Name of program instance and configuration (default is `dmplot`).

**\--node**, **-N** *id*

:   Node id.

**\--output**, **-o** *file*

:   File path of the output image. Required for the terminals `gif`, `png`, and
    `pngcairo`. Additional format descriptors are replaced with their values to
    set date and time dynamically (`%Y`, `%M`, `%D`, `%h`, `%m`, `%s`).

**\--response**, **-R** *name*

:   Response name.

**\--sensor**, **-S** *id*

:   Sensor id.

**\--target**, **-T** *id*

:   Target id.

**\--terminal**, **-m** \[ansi\|ascii\|gif\|gpic\|png\|pngcairo\|postscript\|sixelgd\|sixeltek\|svg\|x11\]

:   Output terminal, either ANSI, ASCII, GIF, GPIC (troff), PNG, PNG (libcairo),
    PostScript (EPS), Sixel (libgd), Sixel (bitmap), SVG, or X11. The terminal
    must be supported by *gnuplot(1)*.

**\--title**, **-C** *title*

:   Plot title.

**\--to**, **-E** *timestamp*

:   End of time range in ISO 8601.

**\--version**, **-v**

:   Print version information and quit.

**\--width**, **-W** *n*

:   Optional plot width in pixels.

# EXIT STATUS

**0**

:   Success. Plot has been created.

**1**

:   Failure. Plot creation failed.

# EXAMPLE

Create a plot of observations selected from database `observ.db` in PNG
terminal, and write the file to `/tmp/plot.png`:

    $ dmplot -N dummy-node -S dummy-sensor -T dummy-target \
      -R dummy -B 2020 -E 2021 -database observ.db \
      -m pngcairo -o /tmp/plot.png

Output the plot directly to terminal, with the configuration loaded from file:

    $ dmplot -n dmplot -c dmplot.conf -m sixelgd

Output in `sixelgd` format requires a terminal emulator with Sixel support.
