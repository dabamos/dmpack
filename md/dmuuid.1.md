% DMUUID(1) Version 2.0.0 | User Commands

# NAME

dmuuid -- generates pseudo-random UUIDs

# SYNOPSIS

**dmuuid** \--help

**dmuuid** \--version

**dmuuid** \[\--**count** *n*\] \[\--**hyphens**\]

**dmuuid** \--**convert** \< *file*

# DESCRIPTION

The **dmuuid** program generates pseudo-random UUIDs. By default, DMPACK uses 32
characters long UUIDv4 identifers in hexadecimal format (without hyphens). If
command-line flag `--hyphens` is set, 36 characters long identifiers with
hyphens are returned. The option `--convert` expects UUIDs to be passed via
standard input. Invalid identifiers will be replaced with the default UUIDv4.

# OPTIONS

**\--convert**, **-c**

:   Add hyphens to hexadecimal UUIDs passed via *stdin*.

**\--count**, **-n** *n*

:   Number of UUIDs to generate (default: 1).

**\--help**, **-h**

:   Print available command-line arguments and quit.

**\--hyphens**, **-p**

:   Return 36 characters long UUIDs with hyphens.

**\--version**, **-v**

:   Print version information and quit.

# EXIT STATUS

**0**

:   Success. UUID generation was successful.

**1**

:   Failure. UUID generation failed.

# EXAMPLE

Create three random identifiers:

    $ dmuuid -n 3
    6827049760c545ad80d4082cc50203e8
    ad488d0b8edd4c6c94582e702a810ada
    3d3eee7ae1fb4259b5df72f854aaa369

Add hyphens to a hexadecimal UUIDv4:

    $ echo "3d3eee7ae1fb4259b5df72f854aaa369" | dmuuid -c
    3d3eee7a-e1fb-4259-b5df-72f854aaa369
