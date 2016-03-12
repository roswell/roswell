SUBDIRS = lisp src documents
debian:
	ros scripts/debian.ros
prepare-release:
	ros scripts/release.ros prepare
release:
	ros scripts/release.ros release
test:
	run-prove roswell-test.asd
.PHONY: debian prepare-release test
