#.NOTPARALLEL:
../scripts/man$(EXEEXT): ../scripts/man.ros ../src/ros$(EXEEXT)
	if [ $< -nt $@ ]; then $(WINE) ../src/ros$(EXEEXT) build $< ;else touch $@;fi

%.1: %.html ../scripts/man$(EXEEXT)
	if [ $< -nt $@ ]; then $(WINE) ../scripts/man$(EXEEXT) $< $@ ;else touch $@;fi

if MANUAL_GENERATE
  man1_MANS = ros.1 ros-init.1 ros-setup.1 ros-dump.1 ros-list.1 ../scripts/man$(EXEEXT)
endif

CLEANFILES = $(man1_MANS)

all: $(man1_MANS)
