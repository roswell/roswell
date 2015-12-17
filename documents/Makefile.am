

# TODO : fetch from https://github.com/snmsts/roswell/wiki and convert all
# wiki pages (mediawiki format or markdown) to html or man


# see also:
# http://pandoc.org/README.html#using-variables-in-templates
# https://github.com/jgm/pandoc-templates/blob/master/default.man

man1_MANS = ros.1 ros-dump.1 ros-init.1 ros-install.1 ros-list.1 ros-setup.1 ros-config.1 ros-delete.1 ros-emacs.1 ros-run.1 ros-use.1 ros-wait.1
HTMLS = $(man1_MANS:.1=.html)

if MANUAL_GENERATE
# separated by semicolons
PANDOC_AUTHORS="Masataro Asai"
%.1: %.md
	pandoc -f markdown_phpextra+backtick_code_blocks -t man -V title="$*" -V section=$(subst .,,$(suffix $@)) \
	-V author=$(PANDOC_AUTHORS) --template=default.man $<  > $@

%.html: %.md
	pandoc -f markdown_phpextra+backtick_code_blocks -t html5 -V title="$*(1)" -V pagetitle="$*(1)" \
	-V css=ros.css -V author=$(PANDOC_AUTHORS) --template=default.html $< > $@

all: $(man1_MANS) $(HTMLS)
CLEANFILES = $(man1_MANS) $(HTMLS)
endif
