SUBDIRS = lisp src

if MANUAL_INSTALL
MDS = ros.md ros-dump.md ros-init.md ros-install.md ros-list.md ros-setup.md ros-template.md \
	ros-config.md ros-delete.md ros-emacs.md ros-run.md ros-use.md ros-wait.md ros-asdf.md ros-build.md ros-update.md \
	ros-help.md
man1_MANS = $(MDS:%.md=documents/man/%.1)
endif

if MANUAL_GENERATE
SUBDIRS += documents
endif

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
.PHONY: prepare release release-2 test winrelease
