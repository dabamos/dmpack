# DMPACK Manual Pages

Install Pandoc to convert the man pages to troff or mandoc format. On FreeBSD,
install the package with:

```
# pkg install textproc/hs-pandoc
```

## troff

Convert the man pages to troff format:

```
$ make
```

Or, run `make man` in the parent directory. The man pages are written to
directory `../man/`.

## mandoc

Convert the man pages to mandoc format:

```
$ make mandoc
```

The man pages are written to directory `../man/`.
