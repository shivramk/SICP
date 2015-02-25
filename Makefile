PANDOC=pandoc
BOOKNAME=sicpanswers

all: $(BOOKNAME).pdf $(BOOKNAME).html $(BOOKNAME).epub

$(BOOKNAME).pdf: $(BOOKNAME).md
	$(PANDOC) -s $< -o $@

$(BOOKNAME).html: $(BOOKNAME).md
	$(PANDOC) -s $< -o $@

$(BOOKNAME).epub: $(BOOKNAME).md
	$(PANDOC) -s $< -o $@

$(BOOKNAME).md: $(SOURCES)
	python makemd.py $(BOOKNAME).md

.PHONY: clean
clean:
	rm -f $(BOOKNAME).*
