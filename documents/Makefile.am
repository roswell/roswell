

# TODO : fetch from https://github.com/snmsts/roswell/wiki and convert all
# wiki pages (mediawiki format or markdown) to html or man


# see also:
# http://pandoc.org/README.html#using-variables-in-templates
# https://github.com/jgm/pandoc-templates/blob/master/default.man

# separated by semicolons
PANDOC_AUTHORS="SANO Masatoshi"

%.1: %.md
	pandoc -f markdown_phpextra -t man -V title="$*" -V section=$(subst .,,$(suffix $@)) -V author=$(PANDOC_AUTHORS) --template=default.man $<  > $@

%.html: %.md
	pandoc -f markdown_phpextra -t html5 -V title="$*(1)" -V pagetitle="$*(1)" -V css=ros.css -V author=$(PANDOC_AUTHORS) --template=default.html $< > $@

if MANUAL_GENERATE
  man1_MANS = $(shell ls *.md | sed s/md/1/g ; ls *.md | sed s/md/html/g)
endif

CLEANFILES = $(man1_MANS)

all: $(man1_MANS)
