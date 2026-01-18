.POSIX:

PANDOC = pandoc
RM     = /bin/rm

SRC    = guide.md
CSS    = resources/css/pandoc.css
HLIGHT = resources/themes/highlight.theme
THEME  = resources/themes/pandoc.html
HTML   = dist
PDF    = guide.pdf
SRC    = guide.md

.PHONY: all clean html pdf

all: $(HTML)

html: $(HTML)

pdf: $(PDF)

$(HTML): $(SRC)
	$(PANDOC) -o $(HTML) -f markdown -t chunkedhtml --toc-depth=2 \
        --chunk-template="%i.html" --syntax-highlight=$(HLIGHT) \
        --template=$(THEME) --css=$(CSS) --embed-resources --standalone \
        --variable toc --variable toc-title="DMPACK 2.0.0" \
        $(SRC)
	$(CP) -r resources $(HTML)/

$(PDF): $(SRC)
	$(PANDOC) -o $(PDF) -f markdown -t pdf --pdf-engine lualatex --toc --dpi=300 \
        --variable papersize="a4" \
        --variable fontsize="10pt" \
        --variable geometry="left=2cm,right=2cm,top=2.5cm,bottom=3cm" \
        $(SRC)

clean:
	$(RM) -rf $(HTML)
	$(RM) -rf $(PDF)
