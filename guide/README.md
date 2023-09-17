# DMPACK User’s Guide

AsciiDoctor and Pygments are required to create the [User’s Guide](guide.adoc).
On FreeBSD, install the packages with:

```
# pkg install devel/rubygem-pygments.rb textproc/rubygem-asciidoctor
```

Convert the User’s Guide from AsciiDoc to HTML:

```
$ asciidoctor -a webfonts! -a stylesheet=../share/adoc.css guide.adoc
```

Or, run `make guide` in the parent directory.
