AUTOMAKE_OPTIONS = subdir-objects
AM_CPPFLAGS = -DLISP_PATH=\"$(roslispdir)\"
roslispdir = $(datadir)/common-lisp/source/$(PACKAGE)
roslisp_DATA = lisp/init.lisp lisp/install.ros
roslispinstalldir = $(roslispdir)/install
roslispinstall_DATA = lisp/install/sbcl.lisp lisp/install/quicklisp.lisp
roslispsubcmddir = $(roslispdir)/subcmd
roslispsubcmd_DATA = lisp/output.ros

bin_PROGRAMS = ros
ros_SOURCES = lsp.c opt.c download.c util.c archive.c html.c internal.c \
	cmds/version/cmd.c \
	cmds/run/cmd.c cmds/run/sbcl.c \
	cmds/install/cmd.c cmds/install/sbcl-bin.c cmds/install/quicklisp.c \
	cmds/list/cmd.c \
	cmds/help/cmd.c \
	cmds/config/cmd.c
noinst_HEADERS = util.h opt.h cmds/install/ros_install.h
