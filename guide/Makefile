.POSIX:

ADOC     = asciidoctor
ADOCDIR  = .
CSSDIR   = ../share
GUIDEDIR = .
TARGET   = index.html

.PHONY: all clean guide

all: guide

guide:
	$(ADOC) -a webfonts! -a stylesheet=$(CSSDIR)/adoc-rubygems.css -D $(GUIDEDIR) $(ADOCDIR)/guide.adoc -o $(TARGET)

clean:
	if [ `ls -1 $(GUIDEDIR)/$(TARGET) 2>/dev/null | wc -l` -gt 0 ]; then rm $(GUIDEDIR)/$(TARGET); fi
