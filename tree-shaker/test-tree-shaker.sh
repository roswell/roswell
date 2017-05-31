#!/bin/bash

set -eu

dump (){
    name=$1
    shift
    ros dump                       $@ executable aaa.ros -o c-$name &
    ros dump --disable-compression $@ executable aaa.ros -o u-$name & # uncompressed
    # ros -L sbcl/1.1.14 dump                       $@ executable aaa.ros -o c-old-$name &
    # ros -L sbcl/1.1.14 dump --disable-compression $@ executable aaa.ros -o u-old-$name &
}

echo 
dump baseline.bin
dump docstring.bin --remove-docstrings
dump destroy-packages.bin --destroy-packages-sbcl
dump no-ql.bin --delete-package QUICKLISP
dump no-sb-posix.bin --delete-package SB-POSIX
dump no-packages.bin --delete-all-packages
dump no-packages-except-trivia.bin --delete-packages-except trivia
 
dump packages+destroyed.bin --delete-all-packages --destroy-packages-sbcl

# the ordering matters; --remove-docstrings use do-all-symbols, so it should come first
dump packages+destroyed+doc.bin --remove-docstrings --delete-all-packages --destroy-packages-sbcl

# these are the packages that must be protected.
# Protection is already performed by the default values in *package-whitelist*
# and the special treatment of the package of the main script.
# 
# dump except-doc.bin \
#      --delete-packages-except keyword \
#      --delete-packages-except roswell \
#      --delete-packages-except ros.script.dump \
#      --delete-packages-except ROS.SCRIPT.AAA.3705213720

# these packages are ok to remove
# --delete-packages-except common-lisp \
# --delete-packages-except common-lisp-user \
# --delete-packages-except roswell.util \
# --delete-packages-except roswell.dump.sbcl \


wait
echo 
ls -lah *.bin

for bin in *.bin ; do
    echo -n "$bin : "
    ./$bin
done
