.POSIX:

ADOC    = asciidoctor
ADOCPDF = asciidoctor-pdf

OUTDIR  = .
SRC     = guide.adoc
HTML    = index.html
PDF     = guide.pdf

THEME   = resources/themes/dmpack-theme.yml
CSS     = resources/css/adoc.css

.PHONY: all clean html pdf

all: $(HTML)

html: $(HTML)

pdf: $(PDF)

$(HTML): $(SRC)
	$(ADOC) -a webfonts! -a stylesheet=$(CSS) -D $(OUTDIR) -o $(HTML) $(SRC)

$(PDF): $(SRC) $(THEME)
	$(ADOCPDF) -a pdf-theme=$(THEME) -a pdf-fontsdir=resources/fonts -D $(OUTDIR) -o $(PDF) $(SRC)

clean:
	if [ `ls -1 $(OUTDIR)/$(HTML) 2>/dev/null | wc -l` -gt 0 ]; then rm $(OUTDIR)/$(HTML); fi
	if [ `ls -1 $(OUTDIR)/$(PDF)  2>/dev/null | wc -l` -gt 0 ]; then rm $(OUTDIR)/$(PDF); fi
