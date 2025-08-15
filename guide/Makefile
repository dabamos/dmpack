.POSIX:

ADOC      = asciidoctor
ADOCMULTI = asciidoctor-multipage
ADOCPDF   = asciidoctor-pdf
TOUCH     = touch

OUTDIR    = .
MULTIDIR  = multi
SRC       = guide.adoc
INDEX     = index.adoc
HTML      = index.html
PDF       = guide.pdf
THEME     = resources/themes/dmpack-theme.yml
CSS       = resources/css/adoc.css

.PHONY: all clean html multi pdf

all: $(HTML)

html: $(HTML)

multi:
	$(TOUCH) $(INDEX)
	$(ADOCMULTI) -a webfonts! -D $(MULTIDIR) $(INDEX)

pdf: $(PDF)

$(HTML): $(SRC)
	$(ADOC) -a webfonts! -a stylesheet=$(CSS) -D $(OUTDIR) -o $(HTML) $(SRC)

$(PDF): $(SRC) $(THEME)
	$(ADOCPDF) -a pdf-theme=$(THEME) -a pdf-fontsdir=resources/fonts -D $(OUTDIR) -o $(PDF) $(SRC)

clean:
	if [ `ls -1 $(OUTDIR)/$(HTML) 2>/dev/null | wc -l` -gt 0 ]; then rm $(OUTDIR)/$(HTML); fi
	if [ `ls -1 $(OUTDIR)/$(PDF)  2>/dev/null | wc -l` -gt 0 ]; then rm $(OUTDIR)/$(PDF); fi
	if [ -d $(MULTIDIR) ]; then rm -r $(MULTIDIR); fi
