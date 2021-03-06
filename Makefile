PANDOC=pandoc
ARGS=-f markdown+fenced_code_attributes+fenced_code_blocks
BOOKNAME=sicpanswers

SOURCES = Chapter1/E1.1.scm \
	Chapter1/E1.10.md \
	Chapter1/E1.11.scm \
	Chapter1/E1.12.scm \
	Chapter1/E1.13.md \
	Chapter1/E1.14.scm \
	Chapter1/E1.15.md \
	Chapter1/E1.16.scm \
	Chapter1/E1.17.scm \
	Chapter1/E1.18.scm \
	Chapter1/E1.19.scm \
	Chapter1/E1.2.scm \
	Chapter1/E1.20.md \
	Chapter1/E1.21.scm \
	Chapter1/E1.22.scm \
	Chapter1/E1.23.scm \
	Chapter1/E1.24.scm \
	Chapter1/E1.25.md \
	Chapter1/E1.26.md \
	Chapter1/E1.27.scm \
	Chapter1/E1.28.scm \
	Chapter1/E1.29.scm \
	Chapter1/E1.3.scm \
	Chapter1/E1.30.scm \
	Chapter1/E1.31.scm \
	Chapter1/E1.32.scm \
	Chapter1/E1.33.scm \
	Chapter1/E1.34.scm \
	Chapter1/E1.35.md \
	Chapter1/E1.36.scm \
	Chapter1/E1.37.scm \
	Chapter1/E1.38.scm \
	Chapter1/E1.39.scm \
	Chapter1/E1.4.md \
	Chapter1/E1.40.scm \
	Chapter1/E1.41.scm \
	Chapter1/E1.42.scm \
	Chapter1/E1.43.scm \
	Chapter1/E1.44.scm \
	Chapter1/E1.45.scm \
	Chapter1/E1.46.scm \
	Chapter1/E1.5.md \
	Chapter1/E1.6.md \
	Chapter1/E1.7.scm \
	Chapter1/E1.8.scm \
	Chapter1/E1.9.md \
	Chapter2/E2.1.scm \
	Chapter2/E2.10.scm \
	Chapter2/E2.11.scm \
	Chapter2/E2.12.scm \
	Chapter2/E2.13.md \
	Chapter2/E2.14.scm \
	Chapter2/E2.15.md \
	Chapter2/E2.17.scm \
	Chapter2/E2.18.scm \
	Chapter2/E2.19.scm \
	Chapter2/E2.2.scm \
	Chapter2/E2.20.scm \
	Chapter2/E2.21.scm \
	Chapter2/E2.22.scm \
	Chapter2/E2.23.scm \
	Chapter2/E2.24.md \
	Chapter2/E2.24.md \
	Chapter2/E2.25.scm \
	Chapter2/E2.26.scm \
	Chapter2/E2.27.scm \
	Chapter2/E2.28.scm \
	Chapter2/E2.29.scm \
	Chapter2/E2.3.scm \
	Chapter2/E2.30.scm \
	Chapter2/E2.31.scm \
	Chapter2/E2.32.scm \
	Chapter2/E2.33.scm \
	Chapter2/E2.34.scm \
	Chapter2/E2.35.scm \
	Chapter2/E2.36.scm \
	Chapter2/E2.37.scm \
	Chapter2/E2.38.scm \
	Chapter2/E2.39.scm \
	Chapter2/E2.4.scm \
	Chapter2/E2.40.scm \
	Chapter2/E2.41.scm \
	Chapter2/E2.42.scm \
	Chapter2/E2.43.md \
	Chapter2/E2.44.scm \
	Chapter2/E2.45.scm \
	Chapter2/E2.46.scm \
	Chapter2/E2.47.scm \
	Chapter2/E2.48.scm \
	Chapter2/E2.49.scm \
	Chapter2/E2.5.scm \
	Chapter2/E2.50.scm \
	Chapter2/E2.51.scm \
	Chapter2/E2.52.scm \
	Chapter2/E2.53.scm \
	Chapter2/E2.54.scm \
	Chapter2/E2.55.md \
	Chapter2/E2.56.scm \
	Chapter2/E2.57.scm \
	Chapter2/E2.58.scm \
	Chapter2/E2.59.scm \
	Chapter2/E2.6.scm \
	Chapter2/E2.60.scm \
	Chapter2/E2.61.scm \
	Chapter2/E2.62.scm \
	Chapter2/E2.63.scm \
	Chapter2/E2.64.md \
	Chapter2/E2.65.scm \
	Chapter2/E2.66.scm \
	Chapter2/E2.67.scm \
	Chapter2/E2.68.scm \
	Chapter2/E2.69.scm \
	Chapter2/E2.7.scm \
	Chapter2/E2.70.scm \
	Chapter2/E2.71.md \
	Chapter2/E2.72.md \
	Chapter2/E2.73.scm \
	Chapter2/E2.74.scm \
	Chapter2/E2.75.scm \
	Chapter2/E2.76.md \
	Chapter2/E2.77.scm \
	Chapter2/E2.78.scm \
	Chapter2/E2.79.scm \
	Chapter2/E2.8.scm \
	Chapter2/E2.80.scm \
	Chapter2/E2.81.scm \
	Chapter2/E2.82.scm \
	Chapter2/E2.9.md \
	Chapter3/E3.1.scm \
	Chapter3/E3.2.scm \
	Chapter3/E3.3.scm \
	Chapter3/E3.4.scm \
	Chapter3/E3.5.scm \
	Chapter3/E3.6.scm \
	Chapter3/E3.7.scm \
	Chapter3/E3.8.scm

PDFSOURCES = Chapter2/images/E2_24_1.pdf \
	Chapter2/images/E2_24_2.pdf \
	Chapter2/images/E2_64_1.pdf \
	Chapter2/images/E2_64_2.pdf \
	Chapter2/images/E2_71_1.pdf \
	Chapter2/images/E2_71_2.pdf \
	Chapter1/images/E1_14_1.pdf

PNGSOURCES = Chapter2/images/E2_24_1.png \
	Chapter2/images/E2_24_2.png \
	Chapter2/images/E2_64_1.png \
	Chapter2/images/E2_64_2.png \
	Chapter2/images/E2_71_1.png \
	Chapter2/images/E2_71_2.png \
	Chapter1/images/E1_14_1.png

all: $(BOOKNAME).pdf $(BOOKNAME).html $(BOOKNAME).epub

Chapter2/images/%.pdf: Chapter2/images/%.tex
	pdflatex -interaction=batchmode -output-directory Chapter2/images $<

Chapter1/images/E1_14_1.pdf: Chapter1/images/E1_14_1.tex
	lualatex -interaction=batchmode -output-directory Chapter1/images $<

Chapter1/images/E1_14_1.tex: Chapter1/images/E1.14
	./Chapter1/images/E1.14 > $@

Chapter1/images/E1.14: Chapter1/E1.14.scm
	mkdir -p Chapter1/images
	csc $< -o $@

Chapter1/images/%.png: Chapter1/images/%.pdf
	convert -density 120 $< $@

Chapter2/images/%.png: Chapter2/images/%.pdf
	convert -density 120 $< $@

$(BOOKNAME).pdf: $(BOOKNAME)_pdf.md $(PDFSOURCES)
	$(PANDOC) $(ARGS) --toc  --top-level-division=chapter -V geometry:margin=1in -s $< -o $@

$(BOOKNAME).html: $(BOOKNAME)_html.md $(PNGSOURCES)
	$(PANDOC) $(ARGS) --mathjax -s $< -o $@

$(BOOKNAME).epub: $(BOOKNAME)_html.md $(PNGSOURCES)
	$(PANDOC) $(ARGS) -t epub3 -s $< -o $@

$(BOOKNAME)_html.md: TOC.json bookc $(SOURCES)
	./bookc TOC.json html $(BOOKNAME)_html.md

$(BOOKNAME)_pdf.md: TOC.json bookc $(SOURCES)
	./bookc TOC.json pdf $(BOOKNAME)_pdf.md

bookc: bookc.scm
	csc $< -o $@

.PHONY: clean
clean:
	rm -f $(BOOKNAME).* $(BOOKNAME)_* bookc
	rm -f Chapter2/images/*.png
	rm -f Chapter2/images/*.aux
	rm -f Chapter2/images/*.gz
	rm -f Chapter2/images/*.log
	rm -f Chapter2/images/*.pdf
	rm -fr Chapter1/images
