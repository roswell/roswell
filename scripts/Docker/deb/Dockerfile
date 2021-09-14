FROM ubuntu:18.04
RUN apt-get update && apt-get install -y \
    build-essential git automake libcurl4-openssl-dev curl gcc-multilib lib32z1-dev \
    dh-autoreconf bash-completion binutils-i686-linux-gnu \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/*
run ln -s /bin/echo /bin/run-proven
run curl -L -O https://github.com/roswell/roswell/releases/download/v21.05.14.109/roswell_21.05.14.109-1_amd64.deb;dpkg -i roswell_21.05.14.109-1_amd64.deb; rm roswell_21.05.14.109-1_amd64.deb
run ros install sbcl-bin/2.0.0; ros run -- --version
run ros install snmsts/sn.github
