FROM ghcr.io/loong64/alpine:3.21
run apk add --no-cache \
	alpine-sdk automake autoconf curl-dev bash linux-headers \
	curl-static nghttp2-static openssl-libs-static brotli-static zlib-static zstd-static c-ares-dev \
	libidn2-static libunistring-static libpsl-static;
run wget -O- https://github.com/c-ares/c-ares/releases/download/v1.34.8/c-ares-1.34.8.tar.gz | tar xz -C /tmp; \
	cd /tmp/c-ares-1.34.8; ./configure --prefix=/usr --disable-shared --enable-static --disable-tests; make install; \
	cd /tmp; rm -rf /tmp/c-ares-1.34.8
