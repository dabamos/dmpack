% DMCAMERA(1) Version 2.0.0 | User Commands

# NAME

dmcamera -- captures images from webcam or IP camera

# SYNOPSIS

**dmcamera** \--help

**dmcamera** \--version

**dmcamera** \--**node** *id* \--**sensor** *id* \--**target** *id*
\--**directory** *path* \--**device** *name* \--**input** *path*
\[\--**interval** *n*\] \[\--**mime** *name*\] \[\--**width** *n*\]
\[\--**height** *n*\] \[\--**database** *file*\] \[\--**logger** *name*\]
\[\--**font** *name*\] \[\--**fontsize** *n*\] \[\--**debug**\] \[\--**ipc**\]
\[\--**overlay**\]

**dmcamera** \--**config** *file* \[\--**name** *name*\]

# DESCRIPTION

The **dmcamera** program captures images of an attached USB webcam (V4L2) or
remote IP camera (RTSP). The images are written to file in JPEG or PNG format.
If an image database is specified, the image meta data is added to the database.
The database can be created with *dminit(1)*. The program depends on FFmpeg and
GraphicsMagick. The executables *ffmpeg(1)* and *gm(1)* are required.

Optionally, a text overlay may be added to the image, containg the date and time
it was captured in ISO 8601. The overlay font must be known to GraphicsMagick.
For a list of all fonts supported by GraphicsMagick, run:

    $ gm convert -list font

You may have to modify the GraphicsMagick configuration file `type.mgk`
according to the fonts installed on the system.

# OPTIONS

**\--config**, **-c** *file*

:   Path to configuration file.

**\--database**, **-d** *file*

:   Path to SQLite image database.

**\--debug**, **-D**

:   Forward logs messages of level `LL_DEBUG` via IPC (if logger is set).

**\--directory**, **-p** *path*

:   Directory to store the image files in.

**\--device**, **-C** \[rtsp\|v4l2\]

:   Camera device type (RTSP or V4L2).

**\--font**, **-F** *name*

:   Name of font to use for overlay text. The font must be known to
    GraphicsMagick (default: DejaVuSansMono).

**\--fontsize**, **-Z** *n*

:   Font size of overlay text (default: 12).

**\--height**, **-H** *n*

:   Desired image height. May be ignored if unsupported by camera.

**\--help**, **-h**

:   Print available command-line arguments and quit.

**\--input**, **-i** *path*

:   URL of RTSP stream or path of V4L2 device (for example, `rtsp://localhost/`
    or `/dev/video0`).

**\--interval**, **-I** *n*

:   Capture interval in seconds. By default, the interval is 0, which means that
    only a single image is captured.

**\--ipc**, **-Q**

:   Uses a POSIX semaphore for process synchronisation. The name of the
    semaphore matches the instance name (with leading `/`). The semaphore is set
    to 1 each time an image has been captured. Only a single process shall wait
    for this semaphore, otherwise, reading occurs in round-robin fashion.

**\--logger**, **-l** *name*

:   Name of logger. If set, sends logs to *dmlogger(1)* process of given name.

**\--mime**, **-M** *name*

:   MIME type of image format to use, either `image/jpeg` or `image/png`.

**\--name**, **-n** *name*

:   Name of program instance, configuration, and POSIX semaphore (default is
    `dmcamera`).

**\--node**, **-N** *id*

:   Node id.

**\--overlay**, **-O**

:   Add date and time in ISO 8601 as overlay text to image.

**\--sensor**, **-S** *id*

:   Sensor id of camera.

**\--target**, **-T** *id*

:   Target id of camera.

**\--verbose**, **-V**

:   Print log messages to *stderr*.

**\--version**, **-v**

:   Print version information and quit.

**\--width**, **-W** *n*

:   Desired image width. May be ignored if unsupported by camera.

# EXIT STATUS

**0**

:   Success. Program executed without errors.

**1**

:   Failure. Program execution failed.

# ENVIRONMENT

**DM_LOGGER**

:   Name of logger instance to send logs to.

**NO_COLOR**

:   Disable ANSI colour output.

# EXAMPLE

Capture an image from an attached USB webcam (V4L2), write image to directory
`/tmp`, and add a text overlay with date and time in ISO 8601:

    $ dmcamera -N dummy-node -S dummy-sensor -T dummy-target \
      -p /tmp -C v4l2 -i /dev/video0 -O -V

# SEE ALSO

*dminit(1)*, *dmupload(1)*
