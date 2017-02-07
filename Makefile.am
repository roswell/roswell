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
	mkdir -p roswell
	mkdir -p roswell/lisp
	mkdir -p roswell/documents/html
	cp src/ros.exe roswell
	cp lisp/*.lisp roswell/lisp
	cp lisp/*.ros  roswell/lisp
	cp documents/html/*.html roswell/documents/html
	cp scripts/chm.ros roswell/documents
	cd roswell;ros ./documents/chm.ros
	/c/Program\ Files\ \(x86\)/HTML\ Help\ Workshop/hhc.exe roswell/documents/roswell.hhp || true
	7z a Roswell-`if [ $$MSYSTEM = 'MINGW64' ];then echo x86_64 ;else echo i686; fi`.zip roswell/ros.exe roswell/lisp/*.* roswell/documents/roswell.chm
test:
	run-prove roswell-test.asd
.PHONY: debian prepare release release-2 test
