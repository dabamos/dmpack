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

## Multi-Page HTML

For multi-page HTML output, install
[asciidoctor-multipage](https://github.com/owenh000/asciidoctor-multipage)
first:

```
$ gem install --user-install asciidoctor-multipage
```

Then, run:

```
$ make multi
```

The HTML files are written to directory `./multi`.

## PDF

The User Guide may also be compiled to PDF if
[asciidoctor-pdf](https://docs.asciidoctor.org/pdf-converter/latest/) is
installed:

```
# pkg install textproc/rubygem-asciidoctor-pdf
```

Then, run:

```
$ make pdf
```
