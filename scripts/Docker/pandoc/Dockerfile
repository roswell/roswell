FROM ubuntu:20.04
RUN apt-get update && apt-get install -y \
    build-essential git automake libcurl4-openssl-dev curl pandoc ruby-sass \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/*
run ln -s /bin/echo /bin/run-prove
run curl -L -O https://github.com/roswell/roswell/releases/download/v21.10.14.111/roswell-21.10.14.111-linux-x86_64.tar.bz2 \
 && tar xf roswell-21.10.14.111-linux-x86_64.tar.bz2 \
 && cd roswell;make install \
 && rm -rf roswell roswell-21.10.14.111-linux-x86_64.tar.bz2
run ros install sbcl-bin/2.0.0; ros run -- --version
run ros install snmsts/sn.github
