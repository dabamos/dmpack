.POSIX:

PANDOC = pandoc
RM     = /bin/rm

SRC    = guide.md
HTML   = html
CSS    = resources/css/pandoc.css
HLIGHT = resources/themes/highlight.theme
THEME  = resources/themes/pandoc.html

.PHONY: all clean

all: $(HTML)

$(HTML):
#	$(ADOC) -b docbook $(SRC)
#	$(PANDOC) -f docbook -t markdown -o $(SRC) $(DOCBOOK)
	$(PANDOC) -o $(HTML) -f markdown -t chunkedhtml --toc-depth=2 \
        --chunk-template="%i.html" --syntax-highlight=$(HLIGHT) \
        --template=$(THEME) --css=$(CSS) \
        --embed-resources --standalone \
        --variable document-css \
        --variable toc \
        --variable mainfont="'IBM Plex Sans', sans-serif" \
        --variable monofont="'IBM Plex Mono', monospace" \
        --variable monobackgroundcolor="WhiteSmoke" \
        --variable maxwidth="52em" \
        --variable fontsize="12pt" \
        --variable linestretch="1.5" \
        --variable linkcolor="Blue" \
        $(SRC)
	$(CP) -r resources $(HTML)/

clean:
	$(RM) -rf $(HTML)
