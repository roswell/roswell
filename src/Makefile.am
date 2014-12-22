AUTOMAKE_OPTIONS = subdir-objects
AM_CPPFLAGS = -DLISP_PATH=\"$(roslispdir)\"
roslispdir = $(datadir)/common-lisp/source/$(PACKAGE)
roslisp_DATA = lisp/init.lisp lisp/install.ros
roslispinstalldir = $(roslispdir)/install
roslispinstall_DATA = lisp/install/sbcl-bin.lisp lisp/install/sbcl.lisp lisp/install/quicklisp.lisp
if COND_WINDOWS
roslispinstall_DATA += lisp/install/msys.lisp
endif
roslispsubcmddir = $(roslispdir)/subcmd
roslispsubcmd_DATA = lisp/dump.ros lisp/list.ros lisp/delete.ros lisp/use.ros

bin_PROGRAMS = ros
ros_SOURCES = lsp.c opt.c download.c util.c archive.c html.c internal.c \
	cmds/version/cmd.c \
	cmds/run/cmd.c cmds/run/sbcl.c \
	cmds/install/cmd.c cmds/install/sbcl-bin.c cmds/install/quicklisp.c \
	cmds/help/cmd.c \
	cmds/config/cmd.c \
	cmds/setup/cmd.c
noinst_HEADERS = util.h opt.h cmds/install/ros_install.h
