roslispdir = @roslispdir@
roslisppatchdir = @roslisppatchdir@
SUBDIRS = src

if MANUAL_INSTALL
MDS = ros.md ros-checkout.md ros-dump.md ros-init.md ros-install.md ros-list.md ros-setup.md ros-template.md \
	ros-config.md ros-delete.md ros-emacs.md ros-run.md ros-use.md ros-wait.md ros-asdf.md ros-build.md ros-update.md \
	ros-help.md ros-client.md ros-serve.md
dist_man1_MANS = $(MDS:%.md=documents/man/%.1)
endif

if MANUAL_GENERATE
SUBDIRS += documents
endif

ROSS = 	balus.ros \
	build.ros \
	checkout.ros \
	client.ros \
	config.ros \
	compile-file.ros \
	ecl-config.ros \
	emacs.ros \
	exec.ros \
	dist.ros \
	fmt.ros \
	get.ros \
	git.ros \
	hg.ros \
	run.ros \
	setup.ros \
	shell.ros \
	version.ros \
	wait.ros \
	delete.ros \
	dump.ros \
	help.ros \
	init.ros \
	install.ros \
	serve.ros \
	update.ros \
	list.ros \
	make-depends.ros \
	template.ros \
	use.ros \
	which.ros

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
	build-ros.lisp build-asd.lisp \
	\
	client-swank.lisp \
	util-config.lisp \
	dist-list.lisp dist-clean.lisp dist-enable.lisp dist-disable.lisp dist-add.lisp \
	dist-delete.lisp dist-use.lisp dist-versions.lisp dist-preference.lisp dist-default.lisp \
	\
	delete-default.lisp delete-dump.lisp delete-git.lisp delete-asdf.lisp \
	delete-env.lisp \
	\
	util-dump.lisp \
	dump-ccl.lisp dump-clisp.lisp \
	dump-cmucl.lisp dump-ecl.lisp dump-sbcl.lisp \
	\
	help-install.lisp help-options.lisp help-run.lisp \
	\
	init-default.lisp init-env.lisp init-dist.lisp \
	\
	util-install.lisp util-install-quicklisp.lisp \
	install+7zip.lisp install-abcl-bin.lisp install-allegro.lisp \
	install-ccl-bin.lisp install-clasp-bin.lisp install-clasp.lisp \
	install-clisp.lisp install-clisp-head.lisp \
	install-cmu-bin.lisp install-ecl.lisp install-quicklisp.lisp install-ql.lisp \
	install-sbcl-bin.lisp install-sbcl.lisp install-sbcl-source.lisp \
	install-sbcl-head.lisp \
	install-slime.lisp install-sly.lisp \
	install-lispworks.lisp install-mkcl.lisp \
	install-npt.lisp \
	install+externals-clasp.lisp install+ffcall.lisp \
	install+msys2.lisp install+sigsegv.lisp install-asdf.lisp \
	\
	serve-default.lisp serve-swank.lisp \
	update-git.lisp update-hg.lisp update-quicklisp.lisp update-default.lisp \
	\
	list-default.lisp \
	list-dump.lisp list-git.lisp list-installed.lisp list-versions.lisp \
	list-asdf.lisp list-env.lisp \
	\
	util-template.lisp \
	\
	util-use.lisp \
	use-asdf.lisp use-env.lisp

dist_roslisp_DATA = $(LISPS:%=lisp/%)
dist_roslisp_SCRIPTS = $(ROSS:%=lisp/%)

dist_roslisppatch_DATA = patch/sbcl-1.3.11.patch patch/sbcl-posix-tests.patch


release-prepare:
	ros scripts/release.ros prepare
release-commit:
	ros scripts/release.ros release
release-push:
	ros scripts/release.ros release-push
release-changelog:
	grep not-yet-released ChangeLog 2>&1 > /dev/null || ros scripts/release.ros changelog
release-ci:
	ros scripts/release.ros release-ci

winrelease: roswell/Makefile
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

PACK = $(PACKAGE)-$(VERSION)-$(shell uname -s | tr '[A-Z]' '[a-z]' | sed -E 's/([^-]*).*/\1/')-$(shell uname -m)$(SBCL_BIN_VARIANT)

roswell/Makefile:
	rm -rf roswell
	$(MKDIR_P) roswell
	echo "prefix = /usr/local" > $@
	echo "mandir = $$""(prefix)/share/man" >> $@
	echo "all: ros$(EXEEXT)" >> $@
	echo "install:" >> $@
	echo "	mkdir -p $$""(prefix)/bin" >> $@
	echo "	mkdir -p $$""(prefix)/etc/roswell/patch" >> $@
	echo "	mkdir -p $$""(mandir)" >> $@
	echo "	install -c ros$(EXEEXT) $$""(prefix)/bin" >> $@
	echo "	install -c -m 644 $(LISPS:%=lisp/%) $$""(prefix)/etc/roswell" >> $@
	echo "	install -c -m 644 $(dist_roslisppatch_DATA) $$""(prefix)/etc/roswell/patch" >> $@
	if [ 'x$(MDS)' != 'x' ]; then \
		echo "	install -c $(MDS:%.md=man/%.1) $$""(mandir)" >> $@; \
	fi
	echo "" >> $@
	echo "uninstall:" >> $@
	echo "	rm -rf $$""(prefix)/etc/roswell" >> $@
	echo "	rm -f $$""(prefix)/bin/ros$(EXEEXT)" >> $@
	echo "	cd $$""(mandir);rm -f $(MDS:%.md=%.1)" >> $@

$(PACK).tar.bz2: roswell/Makefile
	$(MKDIR_P) roswell/patch
	cd src;make ros$(EXEEXT)
	cp src/ros$(EXEEXT) roswell
	$(MKDIR_P) roswell/lisp
	cp $(LISPS:%=lisp/%) roswell/lisp
	cp $(dist_roslisppatch_DATA) roswell/patch
	if [ 'x$(MDS)' != x ]; then \
		$(MKDIR_P) roswell/man; \
		cp $(MDS:%.md=documents/man/%.1) roswell/man; \
	fi
	tar jcvf $(PACK).tar.bz2 roswell

pack: $(PACK).tar.bz2

WINPACK = roswell_$(VERSION)_$(shell if [ $$MSYSTEM = 'MINGW64' ];then echo amd64 ;else echo i686; fi)

pack.zip: $(PACK).tar.bz2
	7z a $(WINPACK).zip roswell
	make $(WINPACK).zip.hash

$(WINPACK).zip.hash:
	echo "md5sum	"`md5sum -t $(WINPACK).zip` > $(WINPACK).zip.hash
	echo "sha1sum	"`sha1sum -t $(WINPACK).zip` >> $(WINPACK).zip.hash
	echo "sha256sum	"`sha256sum -t $(WINPACK).zip` >> $(WINPACK).zip.hash
	echo "sha512sum 	"`sha512sum -t $(WINPACK).zip` >> $(WINPACK).zip.hash

FORCE:
.PHONY: release-prepare release release-push test winrelease debian pack roswell/Makefile FORCE
