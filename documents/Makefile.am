

# TODO : fetch from https://github.com/snmsts/roswell/wiki and convert all
# wiki pages (mediawiki format or markdown) to html or man

%.1: %.md
	pandoc -f markdown_phpextra -t man $<  > $@

%.html: %.md
	pandoc -f markdown_phpextra -t html5 $< > $@

if MANUAL_GENERATE
  man1_MANS = $(shell ls *.md | sed s/md/1/g) $(shell ls *.md | sed s/md/html/g)
endif

CLEANFILES = $(man1_MANS)

all: $(man1_MANS)
