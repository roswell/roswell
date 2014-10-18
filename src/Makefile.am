AUTOMAKE_OPTIONS = subdir-objects
AM_CPPFLAGS = -DLISP_PATH=\"$(roslispdir)\"
roslispdir = $(datadir)/common-lisp/source/$(PACKAGE)/
roslisp_DATA = lisp/init.lisp
bin_PROGRAMS = ros
ros_SOURCES = lsp.c opt.c download.c util.c archive.c html.c\
	cmds/version/cmd.c \
	cmds/run/cmd.c cmds/run/sbcl.c\
	cmds/install/cmd.c cmds/install/sbcl.c cmds/install/sbcl-bin.c \
	cmds/list/cmd.c \
	cmds/help/cmd.c \
	cmds/config/cmd.c
noinst_HEADERS = util.h opt.h cmds/install/ros_install.h
