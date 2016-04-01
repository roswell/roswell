AM_CPPFLAGS = -DLISP_PATH=\"$(roslispdir)\" -DMAN_PATH=\"$(mandir)\"
roslispdir = $(datadir)/common-lisp/source/$(PACKAGE)

gend.h: FORCE
	printf '#define ROS_COMPILE_ENVIRONMENT "%s"\n' "`$(CC) --version|head -n 1`" > $@.tmp
	(printf "#define ROS_REVISION \"" && ((which git>/dev/null&&[ -e ../.git ]&& \
	(git log -n 1 --oneline|cut -d' ' -f1| tr -d '\n'| tr -d '\r'))||printf "") && printf "\"\n") >> $@.tmp
	printf '#define WIN_LISP_PATH \"%s\"' "`cd $(roslispdir); pwd -W 2>/dev/null`" >> $@.tmp
	cmp -s $@.tmp $@||cp $@.tmp $@
	rm -f $@.tmp
	cat $@
cmd-internal.c: gend.h
bin_PROGRAMS = ros
ros_SOURCES = ros.c opt.c download.c archive.c html.c \
	util_list.c util.c util_string.c util_dir.c util_file.c util_system.c \
	cmd-internal.c \
	cmd-run.c cmd-run-sbcl.c cmd-run-ccl.c cmd-run-clisp.c cmd-run-ecl.c cmd-run-abcl.c cmd-run-cmu.c cmd-run-acl.c \
	cmd-install.c cmd-install-sbcl-bin.c \
	cmd-help.c \
	cmd-setup.c
noinst_HEADERS = util.h opt.h cmd-install.h gend.h

CLEANFILES = gend.h $(bashcompletion_DATA) $(zshcompletion_DATA)

FORCE:
.PHONY: FORCE
