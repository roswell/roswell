

# TODO : fetch from https://github.com/snmsts/roswell/wiki and convert all
# wiki pages (mediawiki format or markdown) to html or man


# see also:
# http://pandoc.org/README.html#using-variables-in-templates
# https://github.com/jgm/pandoc-templates/blob/master/default.man

# separated by semicolons
PANDOC_AUTHORS="SANO Masatoshi"

if MANUAL_GENERATE
%.1: %.md
	pandoc -f markdown_phpextra+backtick_code_blocks -t man -V title="$*" -V section=$(subst .,,$(suffix $@)) -V author=$(PANDOC_AUTHORS) --template=default.man $<  > $@
endif

%.html: %.md
	pandoc -f markdown_phpextra+backtick_code_blocks -t html5 -V title="$*(1)" -V pagetitle="$*(1)" -V css=ros.css -V author=$(PANDOC_AUTHORS) --template=default.html $< > $@

man1_MANS = $(shell ls *.md | sed s/md/1/g ; ls *.md | sed s/md/html/g)

CLEANFILES = $(man1_MANS)

if MANUAL_GENERATE
all: $(man1_MANS)
else
all:
endif
