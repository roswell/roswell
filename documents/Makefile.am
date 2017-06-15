# TODO : fetch from https://github.com/roswell/roswell/wiki and convert all
# wiki pages (mediawiki format or markdown) to html or man

# see also:
# http://pandoc.org/README.html#using-variables-in-templates
# https://github.com/jgm/pandoc-templates/blob/master/default.man

# separated by semicolons
PANDOC_AUTHORS="Roswell Project Team"
CLEANFILES=roswell.chm

if MANUAL_GENERATE
MDS = $(wildcard *.md)
man1_MANS = $(MDS:%.md=man/%.1)

man/%.1: %.md
	which pandoc
	pandoc -f markdown -t man -V title="$*" -V section=$(subst .,,$(suffix $@)) \
	-V author=$(PANDOC_AUTHORS) --template=default.man $<  > $@
CLEANFILES+=$(man1_MANS)
endif

if HTML_GENERATE
HTMLS = $(MDS:%.md=html/%.html)
html/%.html: %.md
	which pandoc
# awk hack --- check every underlined words for its corresponding md file, replacing it to an appropriate markdown link to html file if it exist (e.g. _ros-use_ -> check ros-use.md -> [ros-use](ros-use.html) )
# special care should be made regarding checking the existence of the file; 
	awk 'BEGIN{RS="_"; ORS="_"} {target=$$0;gsub("`","",target);if(system("[ -e \""target".md\" ]") == 0){print "["$$1"]("$$1".html)"}else{print $$0}}' < $< | \
	pandoc -f markdown -t html5 -V title="$*(1)" -V pagetitle="$*(1)" \
	-V css=../ros.css -V author=$(PANDOC_AUTHORS) --template=default.html > $@

# html/%.css: %.scss
# 	which scss
# 	scss $< $@
CLEANFILES+=$(HTMLS)
endif

all: $(man1_MANS) $(HTMLS)

roswell.chm: $(HTMLS)
	ros ./chm.ros
	/c/Program\ Files\ \(x86\)/HTML\ Help\ Workshop/hhc.exe ./roswell.hhp ||true
	$(RM) roswell.hhp roswell.hhk roswell.hhc
