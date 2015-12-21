

# TODO : fetch from https://github.com/snmsts/roswell/wiki and convert all
# wiki pages (mediawiki format or markdown) to html or man


# see also:
# http://pandoc.org/README.html#using-variables-in-templates
# https://github.com/jgm/pandoc-templates/blob/master/default.man

MDS = ros.md ros-dump.md ros-init.md ros-install.md ros-list.md ros-setup.md ros-config.md ros-delete.md ros-emacs.md ros-run.md ros-use.md ros-wait.md
man1_MANS = $(MDS:%.md=man/%.1)
HTMLS = $(MDS:%.md=html/%.html)

if MANUAL_GENERATE
# separated by semicolons
PANDOC_AUTHORS="Roswell Project Team"
man/%.1: %.md
	pandoc -f markdown_phpextra+backtick_code_blocks -t man -V title="$*" -V section=$(subst .,,$(suffix $@)) \
	-V author=$(PANDOC_AUTHORS) --template=default.man $<  > $@

html/%.html: %.md
	pandoc -f markdown_phpextra+backtick_code_blocks -t html5 -V title="$*(1)" -V pagetitle="$*(1)" \
	-V css=ros.css -V author=$(PANDOC_AUTHORS) --template=default.html $< > $@

all: $(man1_MANS) $(HTMLS)
CLEANFILES = $(man1_MANS) $(HTMLS)
endif
