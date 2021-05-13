FROM alpine:3.13
run apk add --no-cache \
	alpine-sdk automake autoconf curl-dev bash linux-headers \
	curl-static nghttp2-static openssl-libs-static brotli-static zlib-static; \
	cd /tmp/roswell;./bootstrap;./configure --with-sbcl-bin-variant='-musl';make; make install; \
	ros install sbcl-bin/2.0.5; \
	rm -rf /tmp/roswell
