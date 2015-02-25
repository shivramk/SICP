PANDOC=pandoc
ARGS=-f markdown+fenced_code_attributes+fenced_code_blocks
BOOKNAME=sicpanswers

all: $(BOOKNAME).pdf $(BOOKNAME).html $(BOOKNAME).epub

$(BOOKNAME).pdf: $(BOOKNAME).md
	$(PANDOC) $(ARGS) -s $< -o $@

$(BOOKNAME).html: $(BOOKNAME).md
	$(PANDOC) $(ARGS) -s $< -o $@

$(BOOKNAME).epub: $(BOOKNAME).md
	$(PANDOC) $(ARGS) -s $< -o $@

$(BOOKNAME).md: TOC.json makemd.py $(SOURCES)
	python makemd.py TOC.json $(BOOKNAME).md

.PHONY: clean
clean:
	rm -f $(BOOKNAME).*
