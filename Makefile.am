roslispdir = @roslispdir@
roslisppatchdir = @roslisppatchdir@
SUBDIRS = src

if MANUAL_INSTALL
MDS = ros.md ros-checkout.md ros-dump.md ros-init.md ros-install.md ros-list.md ros-setup.md ros-template.md \
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
	extend-quicklisp.lisp \
	extend-quicklisp-system.lisp \
	system.lisp \
	\
	balus.ros \
	build.ros \
	build-ros.lisp build-asd.lisp \
	\
	checkout.ros \
	config.ros \
	compile-file.ros \
	util-config.lisp \
	emacs.ros \
	exec.ros \
	dist.ros \
	dist-list.lisp dist-clean.lisp dist-enable.lisp dist-disable.lisp dist-add.lisp \
	dist-delete.lisp dist-use.lisp dist-versions.lisp dist-preference.lisp dist-default.lisp \
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
	init-default.lisp init-env.lisp init-dist.lisp \
	\
	install.ros \
	util-install.lisp util-install-quicklisp.lisp \
	install+7zip.lisp install-abcl-bin.lisp install-allegro.lisp \
	install-ccl-bin.lisp install-clasp.lisp install-clisp.lisp \
	install-cmu-bin.lisp install-ecl.lisp install-quicklisp.lisp \
	install-sbcl-bin.lisp install-sbcl.lisp install-slime.lisp \
	install-sly.lisp install-lispworks.lisp install-mkcl.lisp \
	install+externals-clasp.lisp install+ffcall.lisp \
	install+msys2.lisp install+sigsegv.lisp install-asdf.lisp \
	\
	update.ros \
	update-git.lisp update-hg.lisp update-quicklisp.lisp update-default.lisp \
	\
	list.ros \
	list-default.lisp \
	list-dump.lisp list-git.lisp list-installed.lisp list-versions.lisp \
	list-asdf.lisp list-env.lisp \
	\
	make-depends.ros \
	template.ros \
	util-template.lisp \
	\
	use.ros \
	util-use.lisp \
	use-asdf.lisp use-env.lisp \
	\
	which.ros
roslisp_DATA = $(LISPS:%=lisp/%)

roslisppatch_DATA = patch/sbcl-1.3.11.patch patch/sbcl-posix-tests.patch


release-prepare:
	ros scripts/release.ros prepare
release-commit:
	ros scripts/release.ros release
release-push:
	ros scripts/release.ros release-push
release-changelog:
	ros scripts/release.ros changelog
winrelease:
	mkdir -p roswell/lisp
	mkdir -p roswell/documents
	cp src/ros.exe roswell
	cp lisp/*.lisp lisp/*.ros lisp/*.el roswell/lisp
	cd documents;MSYSTEM=MINGW64 make roswell.chm
	cp documents/roswell.chm roswell/documents
	7z a Roswell-`if [ $$MSYSTEM = 'MINGW64' ];then echo x86_64 ;else echo i686; fi`.zip roswell/ros.exe roswell/lisp/*.* roswell/documents/roswell.chm
debian:
	ros debian/debian.ros all
test:
	run-prove roswell-test.asd
brewpr:
	git pull
	if [ `uname` = 'Darwin' ];then brew bump-formula-pr -v --url=https://github.com/roswell/roswell/archive/`git describe --abbrev=0 --tags`.tar.gz roswell; fi

FORCE:
.PHONY: release-prepare release release-push test winrelease debian FORCE
