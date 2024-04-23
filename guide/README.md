# DMPACK User Guide

AsciiDoctor and Pygments are required to create the [User Guide](guide.adoc).
On FreeBSD, install the packages with:

```
# pkg install devel/rubygem-pygments.rb textproc/rubygem-asciidoctor
```

## HTML

Convert the User Guide from AsciiDoc to HTML:

```
$ make html
```

Or, run `make guide` in the parent directory.

## PDF

The User Guide may also be compiled to PDF if
[Asciidoctor PDF](https://docs.asciidoctor.org/pdf-converter/latest/) is
installed:

```
# pkg install textproc/rubygem-asciidoctor-pdf
```

Then, run:

```
$ make pdf
```
