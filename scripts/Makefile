-include .env
export $(shell sed 's/=.*//' .env)

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
	docker build -t docker.pkg.github.com/roswell/roswell/$$(cat ./scripts/Docker/$(IMAGE)/Name) ./scripts/Docker/$(IMAGE)
push-docker:
	docker push docker.pkg.github.com/roswell/roswell/$$(cat ./scripts/Docker/$(IMAGE)/Name);
pull-docker:
	docker pull docker.pkg.github.com/roswell/roswell/$$(cat ./scripts/Docker/$(IMAGE)/Name);
docker:
	docker run \
		-v `pwd`:/tmp \
		-e ARCH=$(ARCH) \
		-e VERSION=$(VERSION) \
		-e CFLAGS=$(CFLAGS) \
		-e LINKFLAGS=$(LINKFLAGS) \
		-e VARIANT=$(VARIANT) \
		docker.pkg.github.com/roswell/roswell/$$(cat ./scripts/Docker/$(IMAGE)/Name) \
		bash \
		-c "cd /tmp;make -f scripts/Makefile $(MAKEOPTS)"
upload:
	echo upload!!! $(FILE)
	ros install roswell/sbcl_bin
	ros -s roswell.github.utils -e "(mapc (lambda (x) (print x) (roswell.github.utils:github x \"v$(VERSION)\" \"roswell\" \"roswell\")) (directory \"$(FILE)\"))"

musl:
	./bootstrap
	./configure --enable-maintainer-mode --with-sbcl-bin-variant=$(VARIANT) \
	LDFLAGS="--static -static-libgcc -static-libstdc++ -static" \
	LIBS="-lcurl -lbrotlidec-static -lbrotlicommon-static -lnghttp2 -lssl -lcrypto -lz"
	make
	make pack

musl-upload:
	env FILE=`echo roswell*.tar.bz2` VERSION=$(VERSION) $(MAKE) -f $(realpath $(firstword $(MAKEFILE_LIST))) upload

.PHONY: all latest-version checkout show build-docker pull-docker docker upload musl
