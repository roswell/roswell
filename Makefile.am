roslispdir = $(sysconfdir)/$(PACKAGE)
proslispdir = $(sysconfdir)/$(PACKAGE)/patch
SUBDIRS = src

if MANUAL_INSTALL
MDS = ros.md ros-dump.md ros-init.md ros-install.md ros-list.md ros-setup.md ros-template.md \
	ros-config.md ros-delete.md ros-emacs.md ros-run.md ros-use.md ros-wait.md ros-asdf.md ros-build.md ros-update.md \
	ros-help.md
man1_MANS = $(MDS:%.md=documents/man/%.1)
endif

if MANUAL_GENERATE
SUBDIRS += documents
endif

LISPS = helper.el \
	init.lisp \
	util.lisp util-main.lisp \
	util-swank.lisp \
	locations.lisp \
	patch-quicklisp.lisp \
	system.lisp \
	\
	build.ros \
	build-ros.lisp build-asd.lisp \
	\
	config.ros \
	util-config.lisp \
	emacs.ros \
	exec.ros \
	fmt.ros \
	get.ros \
	git.ros \
	hg.ros \
	run.ros \
	setup.ros \
	shell.ros \
	version.ros \
	wait.ros \
	\
	delete.ros \
	delete-default.lisp delete-dump.lisp delete-git.lisp delete-asdf.lisp \
	delete-env.lisp \
	\
	dump.ros \
	util-dump.lisp \
	dump-ccl.lisp dump-clisp.lisp \
	dump-cmucl.lisp dump-ecl.lisp dump-sbcl.lisp \
	\
	help.ros \
	help-install.lisp help-options.lisp help-run.lisp \
	\
	init.ros \
	init-default.lisp init-env.lisp \
	\
	install.ros \
	util-install.lisp util-install-quicklisp.lisp \
	install+7zip.lisp install-abcl-bin.lisp install-allegro.lisp \
	install-ccl-bin.lisp install-clasp.lisp install-clisp.lisp \
	install-cmu-bin.lisp install-ecl.lisp install-quicklisp.lisp \
	install-sbcl-bin.lisp install-sbcl.lisp install-slime.lisp \
	install+externals-clasp.lisp install+ffcall.lisp \
	install+msys2.lisp install+sigsegv.lisp install-asdf.lisp \
	\
	update.ros \
	update-git.lisp update-hg.lisp update-quicklisp.lisp \
	\
	list.ros \
	list-default.lisp \
	list-dump.lisp list-git.lisp list-installed.lisp list-versions.lisp \
	list-asdf.lisp list-env.lisp \
	\
	template.ros \
	util-template.lisp \
	\
	use.ros \
	util-use.lisp \
	use-asdf.lisp use-env.lisp \
	\
	which.ros
roslisp_DATA = $(LISPS:%=lisp/%)

#proslisp_DATA = clisp.patch sbcl-1.3.11.patch sbcl-posix-tests.patch

src/gend.h: FORCE
	printf '#define ROS_COMPILE_ENVIRONMENT "%s"\n' "`$(CC) --version|head -n 1`" > $@.tmp
	(printf "#define ROS_REVISION \"" && ((which git>/dev/null&&[ -e .git ]&& \
	(git log -n 1 --oneline|cut -d' ' -f1| tr -d '\n'| tr -d '\r'))||printf "") && printf "\"\n") >> $@.tmp
	printf '#ifdef HAVE_WINDOWS_H\n'  >> $@.tmp
	printf '#define LISP_PATH "%s"' "`mkdir -p $(roslispdir);cd $(roslispdir); pwd -W 2>/dev/null`" >> $@.tmp
	printf '\n#else\n'  >> $@.tmp
	printf '#define LISP_PATH "%s"' "$(roslispdir)" >> $@.tmp
	printf '\n#endif\n'  >> $@.tmp
	cmp -s $@.tmp $@||cp $@.tmp $@
	cat $@
	rm -f $@.tmp

prepare:
	ros scripts/release.ros prepare
release:
	ros scripts/release.ros release
release-2:
	ros scripts/release.ros release-2

winrelease:
	mkdir -p roswell/lisp
	mkdir -p roswell/documents
	cp src/ros.exe roswell
	cp lisp/*.lisp lisp/*.ros lisp/*.el roswell/lisp
	cd documents;MSYSTEM=MINGW64 make roswell.chm
	cp documents/roswell.chm roswell/documents
	7z a Roswell-`if [ $$MSYSTEM = 'MINGW64' ];then echo x86_64 ;else echo i686; fi`.zip roswell/ros.exe roswell/lisp/*.* roswell/documents/roswell.chm
test:
	run-prove roswell-test.asd

FORCE:
.PHONY: prepare release release-2 test winrelease FORCE
