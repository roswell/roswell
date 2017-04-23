SUBDIRS = lisp src documents scripts
debian:
	ros scripts/debian.ros
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
	cp lisp/*.lisp lisp/*.ros roswell/lisp
	cd documents;MSYSTEM=MINGW64 make roswell.chm
	cp documents/roswell.chm roswell/documents
	7z a Roswell-`if [ $$MSYSTEM = 'MINGW64' ];then echo x86_64 ;else echo i686; fi`.zip roswell/ros.exe roswell/lisp/*.* roswell/documents/roswell.chm
test:
	run-prove roswell-test.asd
.PHONY: debian prepare release release-2 test winrelease
