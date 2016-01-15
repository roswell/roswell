SUBDIRS = src documents
debian:
	ros scripts/debian.ros
prepare-release:
	ros scripts/release.ros prepare
release:
	ros scripts/release.ros release
test:
	ros t/roswell.ros
.PHONY: debian prepare-release test
