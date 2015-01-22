AUTOMAKE_OPTIONS = subdir-objects
AM_CPPFLAGS = -DLISP_PATH=\"$(roslispdir)\"
roslispdir = $(datadir)/common-lisp/source/$(PACKAGE)
roslisp_DATA = lisp/init.lisp lisp/install.ros
roslispinstalldir = $(roslispdir)/install
roslispinstall_DATA = lisp/install/sbcl-bin.lisp lisp/install/sbcl.lisp lisp/install/quicklisp.lisp lisp/install/msys.lisp lisp/install/7zip.lisp lisp/install/ccl-bin.lisp
roslispsubcmddir = $(roslispdir)/subcmd
roslispsubcmd_DATA = lisp/dump.ros lisp/list.ros lisp/delete.ros lisp/use.ros lisp/init.ros

bin_PROGRAMS = ros
ros_SOURCES = lsp.c opt.c download.c util.c archive.c html.c internal.c \
	cmd-version.c \
	cmd-run.c cmd-run-sbcl.c cmd-run-ccl.c \
	cmd-install.c cmd-install-sbcl-bin.c cmd-install-quicklisp.c \
	cmd-help.c \
	cmd-config.c \
	cmd-setup.c
noinst_HEADERS = util.h opt.h cmd-install.h
