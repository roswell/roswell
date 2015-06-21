AM_CPPFLAGS = -DLISP_PATH=\"$(roslispdir)\"
roslispdir = $(datadir)/common-lisp/source/$(PACKAGE)
roslisp_DATA = $(wildcard lisp/*.lisp) $(wildcard lisp/*.ros)

noinst_PROGRAMS = gen
gen_SOURCES = generate.c util_list.c util.c util_string.c util_dir.c util_system.c

gend.h: gen$(EXEEXT) FORCE
	which $(CC)&&(($(CC) --version| $(WINE) ./gen$(EXEEXT) 1) > $@.tmp)
	((which git>/dev/null&&[ -e ../.git ]&&(git log -n 1 --oneline))||printf "")|$(WINE) ./gen$(EXEEXT) 2 >> $@.tmp
	((echo `sh -c 'cd $(roslispdir); pwd -W 2>/dev/null'`| $(WINE) ./gen$(EXEEXT) 3) >> $@.tmp)
	cmp -s $@.tmp $@||cp $@.tmp $@
	rm -f $@.tmp
	cat $@
cmd-version.c: gend.h
bin_PROGRAMS = ros
ros_SOURCES = lsp.c opt.c download.c archive.c html.c internal.c \
	util_list.c util.c util_string.c util_dir.c util_system.c \
	cmd-version.c \
	cmd-run.c cmd-run-sbcl.c cmd-run-ccl.c \
	cmd-install.c cmd-install-sbcl-bin.c cmd-install-quicklisp.c \
	cmd-help.c \
	cmd-config.c \
	cmd-setup.c
noinst_HEADERS = util.h opt.h cmd-install.h gend.h
CLEANFILES = gend.h

%.1: %.html ros$(EXEEXT)
	$(WINE) ./ros$(EXEEXT) setup --no-core;./ros$(EXEEXT) ../scripts/man.ros $< $@

man1_MANS = ../documents/ros.1
CLEANFILES = $(man1_MANS)

FORCE:
.PHONY: FORCE
