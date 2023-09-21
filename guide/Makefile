.POSIX:

ADOC     = asciidoctor
ADOCDIR  = .
CSSDIR   = ../share
GUIDEDIR = .
TARGET   = index.html

.PHONY: all clean guide multi

all: guide

guide:
	$(ADOC) -a webfonts! -a stylesheet=$(CSSDIR)/adoc.css -D $(GUIDEDIR) -o $(TARGET) $(ADOCDIR)/guide.adoc

clean:
	if [ `ls -1 $(GUIDEDIR)/*.html 2>/dev/null | wc -l` -gt 0 ]; then rm $(GUIDEDIR)/*.html; fi
