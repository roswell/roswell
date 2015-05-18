SUBDIRS = src
debian:
	ros scripts/debian.ros
prepare-release:
	ros scripts/release.ros prepare
test:
	ros t/roswell.ros
.PHONY: debian prepare-release test
