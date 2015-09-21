SUBDIRS = src
debian:
	ros scripts/debian.ros
prepare-release:
	ros scripts/release.ros prepare
release:
	ros scripts/release.ros release
test:
	ros -L sbcl-bin t/roswell.ros
	ros -L ccl-bin t/roswell.ros
.PHONY: debian prepare-release test
