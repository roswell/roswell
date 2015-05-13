AM_CPPFLAGS = -DLISP_PATH=\"$(roslispdir)\"
roslispdir = $(datadir)/common-lisp/source/$(PACKAGE)
roslisp_DATA = lisp/init.lisp lisp/install.ros
roslispinstalldir = $(roslispdir)/install
roslispinstall_DATA = lisp/install/sbcl-bin.lisp lisp/install/sbcl.lisp lisp/install/quicklisp.lisp lisp/install/msys.lisp lisp/install/7zip.lisp lisp/install/ccl-bin.lisp
roslispsubcmddir = $(roslispdir)/subcmd
roslispsubcmd_DATA = lisp/dump.ros lisp/list.ros lisp/delete.ros lisp/use.ros lisp/init.ros lisp/build.ros

noinst_PROGRAMS = gen
gen_SOURCES = generate.c util_list.c util.c util_string.c util_dir.c

gend.h: gen$(EXEEXT) FORCE
	(which $(CC)&&(./gen$(EXEEXT) $(CC) > $@.tmp;cmp -s $@.tmp $@||cp $@.tmp $@))||echo $(CC) not found
	rm -f $@.tmp
cmd-version.c: gend.h
bin_PROGRAMS = ros
ros_SOURCES = lsp.c opt.c download.c archive.c html.c internal.c \
	util_list.c util.c util_string.c util_dir.c \
	cmd-version.c \
	cmd-run.c cmd-run-sbcl.c cmd-run-ccl.c \
	cmd-install.c cmd-install-sbcl-bin.c cmd-install-quicklisp.c \
	cmd-help.c \
	cmd-config.c \
	cmd-setup.c
noinst_HEADERS = util.h opt.h cmd-install.h gend.h
CLEANFILES = gend.h

.html.1: bin_PROGRAMS
	./ros setup;./ros ../scripts/man.ros $< $@

man1_MANS = ../documents/ros.1

FORCE:
.PHONY: FORCE
