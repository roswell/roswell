ros_utils= util.c util_windows.c util-list.c util-string.c util-dir.c util-dir_windows.c \
	util-file.c util-file_windows.c util-system.c util-system_windows.c util-cmdline.c proc-opt.c \
	manual.c manual_windows.c

check_PROGRAMS = html_sbcl
html_sbcl_SOURCES =  html.c html-sbcl-bin.c ros.c opt.c $(ros_utils)
html_sbcl_CFLAGS = "-D ROSWELL_HTML_TEST"

gend.h: FORCE
	make -C .. src/gend.h
cmd-internal.c: gend.h
bin_PROGRAMS = ros
ros_SOURCES = ros.c opt.c download.c download_windows.c archive.c archive_windows.c \
	register-commands.c html.c html-sbcl-bin.c \
	$(ros_utils) \
	cmd-internal.c cmd-script.c \
	cmd-run.c cmd-run-sbcl.c cmd-run-ccl.c cmd-run-clasp.c cmd-run-clisp.c cmd-run-ecl.c cmd-run-abcl.c cmd-run-cmu.c cmd-run-acl.c \
	cmd-install.c install-sbcl-bin.c install-sbcl-bin_windows.c

if WITH_WIN_ICON
#ros_LDADD += resources.o
resources.o: resources.rc
	@WINDRES@ $^ -o $@
endif

noinst_HEADERS = util.h opt.h cmd-install.h gend.h cmd-run.h

CLEANFILES = gend.h

FORCE:
.PHONY: FORCE
