AM_CPPFLAGS = -DLISP_PATH=\"$(roslispdir)\" -DMAN_PATH=\"$(mandir)\"
roslispdir = $(datadir)/common-lisp/source/$(PACKAGE)
bashcompletiondir = /etc/bash_completion.d
zshcompletiondir  = /etc/zsh/functions

if BASH_COMPLETION_INSTALL
bashcompletion_DATA = ../scripts/completions/ros
../scripts/completions/ros: ../scripts/completions/ros-completion.bash
	cp $< $@
endif

if ZSH_COMPLETION_INSTALL
zshcompletion_DATA = ../scripts/completions/_ros
../scripts/completions/_ros: ../scripts/completions/ros-completion.zsh
	cp $< $@
endif

gend.h: FORCE
	printf "#define ROS_COMPILE_ENVIRONMENT \"%s\"\n" "`$(CC) --version|head -n 1`" > $@.tmp
	(printf "#define ROS_REVISION \"" && ((which git>/dev/null&&[ -e ../.git ]&& \
	(git log -n 1 --oneline|cut -d' ' -f1| tr -d '\n'| tr -d '\r'))||printf "") && printf "\"\n") >> $@.tmp
	(sh -c 'cd $(roslispdir); pwd -W' 2>&1 >/dev/null && (printf "#define WIN_LISP_PATH \"" \
	echo `sh -c 'cd $(roslispdir); pwd -W 2>/dev/null'`| tr -d '\n'| tr -d '\r' |\
	sed /\//\\/ \
	printf "\\\\\"\n")  >> $@.tmp) || printf ""
	cmp -s $@.tmp $@||cp $@.tmp $@
	rm -f $@.tmp
	cat $@
cmd-version.c: gend.h
bin_PROGRAMS = ros
ros_SOURCES = ros.c opt.c download.c archive.c html.c \
	util_list.c util.c util_string.c util_dir.c util_file.c util_system.c \
	cmd-internal.c \
	cmd-version.c \
	cmd-run.c cmd-run-sbcl.c cmd-run-ccl.c cmd-run-clisp.c cmd-run-ecl.c cmd-run-abcl.c cmd-run-cmu.c cmd-run-acl.c \
	cmd-install.c cmd-install-sbcl-bin.c cmd-install-quicklisp.c \
	cmd-help.c \
	cmd-config.c \
	cmd-setup.c
noinst_HEADERS = util.h opt.h cmd-install.h gend.h

CLEANFILES = gend.h $(bashcompletion_DATA) $(zshcompletion_DATA)

FORCE:
.PHONY: FORCE
