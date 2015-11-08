#.NOTPARALLEL:

%.1: %.html ../src/ros$(EXEEXT) ../scripts/man.ros
	$(WINE) ../src/ros$(EXEEXT) -L sbcl-bin ../scripts/man.ros $< $@

if MANUAL_GENERATE
  man1_MANS = ros.1 ros-init.1 ros-setup.1 ros-dump.1 ros-list.1
endif

CLEANFILES = $(man1_MANS)

all: $(man1_MANS)
