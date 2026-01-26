# DMPACK Manual Pages

Install Pandoc to convert the man pages to troff format. On FreeBSD, install the
package with:

```
# pkg install textproc/hs-pandoc
```

Convert the man pages to troff format:

```
$ make
```

Or, run `make man` in the parent directory. The man pages are written to
directory `../man/`.
