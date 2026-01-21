# DMPACK User Guide

Install Pandoc to convert the [User Guide](guide.md) to HTML format.
On FreeBSD, install the package with:

```
# pkg install textproc/hs-pandoc
```

## HTML

Convert the User Guide from Markdown to HTML:

```
$ make
```

Or, run `make guide` in the parent directory. The HTML files are written to
directory `dist/`.
