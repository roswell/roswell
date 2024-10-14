-include .env
export $(shell sed 's/=.*//' .env)

DOCKER_REPO ?= docker.pkg.github.com/roswell/roswell
DOCKER_PLATFORM ?= linux/amd64
DOCKER_BUILD_OPTIONS ?=
DOCKER_IMAGE_SUFFIX ?=
IMAGE ?=

all:
	echo all

latest-version:
	$(eval VERSION := $(shell grep AC_INIT configure.ac|sed 's/.*\[\(.*\)\].*/\1/'))
#'
checkout:
	if [ ! "v$(VERSION)" = "v" ];then \
		git checkout v$(VERSION); \
	fi

show:
	@echo VERSION=$(VERSION) VARIANT=$(VARIANT)

build-docker:
	docker build \
		--platform $(DOCKER_PLATFORM) \
		-t $(DOCKER_REPO)/$$(cat ./scripts/Docker/$(IMAGE)/Name)$(DOCKER_IMAGE_SUFFIX) \
		$(DOCKER_BUILD_OPTIONS) \
		./scripts/Docker/$(IMAGE)
push-docker:
	docker push $(DOCKER_REPO)/$$(cat ./scripts/Docker/$(IMAGE)/Name)$(DOCKER_IMAGE_SUFFIX);
pull-docker:
	docker pull $(DOCKER_REPO)/$$(cat ./scripts/Docker/$(IMAGE)/Name)$(DOCKER_IMAGE_SUFFIX);
docker:
	docker run \
		--rm \
		--platform $(DOCKER_PLATFORM) \
		-v `pwd`:/tmp2 \
		-e ARCH=$(ARCH) \
		-e VERSION=$(VERSION) \
		-e SUFFIX=$(SUFFIX) \
		-e CFLAGS=$(CFLAGS) \
		-e LINKFLAGS=$(LINKFLAGS) \
		-e VARIANT=$(VARIANT) \
		$(DOCKER_REPO)/$$(cat ./scripts/Docker/$(IMAGE)/Name)$(DOCKER_IMAGE_SUFFIX) \
		bash \
		-c "cd /tmp2;make -f scripts/Makefile $(MAKEOPTS)"
upload:
	echo upload!!! $(FILE)
	ros install roswell/sbcl_bin
	ros -s roswell.github.utils -e "(mapc (lambda (x) (print x) (roswell.github.utils:github x \"v$(VERSION)\" \"roswell\" \"roswell\")) (directory \"$(FILE)\"))"

musl:
	./bootstrap
	./configure --enable-maintainer-mode --with-sbcl-bin-variant=$(VARIANT) \
	LDFLAGS="--static -static-libgcc -static-libstdc++ -static" \
	LIBS="-lcurl -lnghttp2 -lssl -lcrypto -lz"
	make
	make pack

musl-upload:
	env FILE=`echo roswell*.tar.bz2` VERSION=$(VERSION) $(MAKE) -f $(realpath $(firstword $(MAKEFILE_LIST))) upload

deb:
	pwd
	ls -al
	./bootstrap
	./configure
	make
	make debian
	cp debian/debian/*.deb .

deb-upload:
	env FILE=`echo *.deb` VERSION=$(VERSION) $(MAKE) -f $(realpath $(firstword $(MAKEFILE_LIST))) upload

docs:
	rm -f documents/man/* documents/html/*
	./bootstrap
	./configure --enable-manual-generation --enable-html-generation
	make

docs-upload:
	git config --local user.email "auto@example.com"
	git config --local user.name "github actions"
	git branch
	git status
	git add -u
	git add documents/man/* documents/html/*
	(git commit -m "[ci skip] documents update" && git push origin $$(git rev-parse --abbrev-ref HEAD)) || true

upload.exe:
	curl --no-progress-bar --retry 10 -o upload.tar.bz2 -L https://github.com/roswell/roswell/releases/download/v21.06.14.110/upload-mingw64_nt-x86_64.tar.bz2
	tar xvf upload.tar.bz2

win-upload: upload.exe
	env FILE=`echo roswell*.zip` VERSION=v$(VERSION) OWNER=roswell REPO=roswell ./upload.exe
	env FILE=`echo roswell*.hash` VERSION=v$(VERSION) OWNER=roswell REPO=roswell ./upload.exe

.PHONY: all latest-version checkout show build-docker pull-docker docker upload musl
