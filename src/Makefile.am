ros_utils= util.c util_windows.c util-list.c util-string.c util-dir.c util-dir_windows.c \
	util-file.c util-file_windows.c util-system.c util-system_windows.c util-cmdline.c proc-opt.c \
	manual.c manual_windows.c

ros_LDADD =
check_PROGRAMS = tsv_sbcl
tsv_sbcl_SOURCES =  tsv-sbcl-bin.c main.c opt.c $(ros_utils)
tsv_sbcl_CFLAGS = "-D ROSWELL_TSV_TEST"

gend.h: FORCE
	printf '#define ROS_COMPILE_ENVIRONMENT "%s"\n' "`$(CC) --version|head -n 1`" > $@.tmp
	printf '#define ROS_REVISION "@git_version@"\n' >> $@.tmp
	printf '#define UNAME_M "@uname_m@"\n' >> $@.tmp
	printf '#define UNAME_S "@uname_s@"\n' >> $@.tmp
if STANDALONE_EXECUTABLE
	printf '#define STANDALONE_EXECUTABLE true\n' >> $@.tmp
	printf 'void init_standalone_dir();\n' >> $@.tmp
	printf 'void write_standalone_file(const char *dir, const char* name, char *val, unsigned int size);\n' >> $@.tmp
	printf 'void init_standalone_lisp_files();\n' >> $@.tmp
	printf 'void init_standalone_patch_files();\n' >> $@.tmp
	printf '#define PATCH_PATH "/tmp/roswell-@git_version@/patch"\n' >> $@.tmp
else
	printf '#define PATCH_PATH "@roslisppatchdir@"\n' >> $@.tmp
endif
if WITH_WINDOWS
	printf '#define LISP_PATH "%s"\n' "`mkdir -p @roslispdir@;cd @roslispdir@; pwd -W 2>/dev/null`" >> $@.tmp
else
if STANDALONE_EXECUTABLE
	printf '#define LISP_PATH "/tmp/roswell-@git_version@/lisp"\n' >> $@.tmp
else
	printf '#define LISP_PATH "@roslispdir@"\n' >> $@.tmp
endif
endif
	cmp -s $@.tmp $@||cp $@.tmp $@
	cat $@
	rm -f $@.tmp
if STANDALONE_EXECUTABLE
	echo "#include \"util.h\"" > standalone-lisp.c
	echo "#include \"gend.h\"" >> standalone-lisp.c
	cd ../lisp && for file in *; do xxd -i $$file >> ../src/standalone-lisp.c; done
	echo "void init_standalone_lisp_files() {" >> standalone-lisp.c
	ls ../lisp | awk '{ s=$$1; gsub(/[-.+]/, "_", s); print "  write_standalone_file(LISP_PATH, \""$$1"\", "s", "s"_len);"; }' >> ../src/standalone-lisp.c
	echo "}" >> standalone-lisp.c
	echo "#include \"util.h\"" > standalone-patch.c
	echo "#include \"gend.h\"" >> standalone-patch.c
	cd ../patch && for file in *; do xxd -i $$file >> ../src/standalone-patch.c; done
	echo "void init_standalone_patch_files() {" >> standalone-patch.c
	ls ../patch | awk '{ s=$$1; gsub(/[-.+]/, "_", s); print "  write_standalone_file(PATCH_PATH, \""$$1"\", "s", "s"_len);"; }' >> ../src/standalone-patch.c
	echo "}" >> standalone-patch.c
endif

cmd-internal.o: gend.h
util.o: gend.h

bin_PROGRAMS = ros
ros_SOURCES = main.c opt.c download.c download_windows.c archive.c archive_windows.c \
	register-commands.c tsv-sbcl-bin.c \
	$(ros_utils) \
	cmd-internal.c cmd-script.c \
	cmd-run.c cmd-run-sbcl.c cmd-run-ccl.c cmd-run-clasp.c cmd-run-clisp.c cmd-run-ecl.c \
	cmd-run-mkcl.c cmd-run-abcl.c cmd-run-cmu.c cmd-run-acl.c cmd-run-lispworks.c \
	cmd-run-npt.c \
	cmd-install.c install-sbcl-bin.c install-sbcl-bin_windows.c

if WITH_WIN_ICON
ros_LDADD += resources.o
resources.o: resources.rc
	@WINDRES@ $^ -o $@
endif

if STANDALONE_EXECUTABLE
ros_SOURCES += util-standalone.c standalone-lisp.c standalone-patch.c
endif

noinst_HEADERS = util.h opt.h cmd-install.h gend.h cmd-run.h

CLEANFILES = gend.h standalone-lisp.c standalone-patch.c

if OPTIMIZE_EXECUTABLE
all-local:
	upx ros
endif

check-syntax:
	$(CC) -Wall -fsyntax-only $(CHK_SOURCES)

FORCE:
.PHONY: FORCE check-syntax
