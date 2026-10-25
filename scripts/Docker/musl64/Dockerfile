ARG BASE=alpine:3.21
FROM $BASE
ARG CURL_VERSION=8.23.0
ARG CURL_SHA256=e3de77cfa6367083c776a26647fc9f633b538c06c3baa73d931a252ac8f72b49
run apk add --no-cache \
	alpine-sdk automake autoconf bash linux-headers openssl-dev openssl-libs-static;
run set -e; wget -O /tmp/curl.tar.gz https://curl.se/download/curl-$CURL_VERSION.tar.gz; \
	echo "$CURL_SHA256  /tmp/curl.tar.gz" | sha256sum -c; \
	tar xzf /tmp/curl.tar.gz -C /tmp; cd /tmp/curl-$CURL_VERSION; \
	./configure --prefix=/usr --disable-shared --enable-static --with-openssl \
	--without-brotli --without-zstd --without-zlib --without-nghttp2 --without-libidn2 --without-libpsl \
	--without-libssh2 --disable-ldap --disable-docs; \
	make -C lib install; make -C include install; \
	cd /tmp; rm -rf /tmp/curl.tar.gz /tmp/curl-$CURL_VERSION
