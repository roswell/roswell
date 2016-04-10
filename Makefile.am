SUBDIRS = lisp src documents
debian:
	ros scripts/debian.ros
prepare:
	ros scripts/release.ros prepare
release:
	ros scripts/release.ros release
release-2:
	ros scripts/release.ros release-2
test:
	run-prove roswell-test.asd
.PHONY: debian prepare-release test
