#!/bin/sh
set -e

ROSWELL_RELEASE_VERSION=21.10.14.111
ROSWELL_TARBALL_PATH=$HOME/roswell.tar.gz
ROSWELL_DIR=$HOME/.roswell
ROSWELL_REPO=${ROSWELL_REPO:-https://github.com/roswell/roswell}
ROSWELL_BRANCH=${ROSWELL_BRANCH:-release}
ROSWELL_INSTALL_DIR=${ROSWELL_INSTALL_DIR:-/usr/local}
ROSWELL_PLATFORMHTML_BASE=${ROSWELL_PLATFORMHTML_BASE:-https://github.com/roswell/sbcl_bin/releases/download/files/sbcl-bin_uri.tsv}
ROSWELL_SBCL_BIN_URI=${ROSWELL_SBCL_BIN_URI:-https://github.com/roswell/sbcl_bin/releases/download/}
ROSWELL_QUICKLISP_DIST_URI=${ROSWELL_QUICKLISP_DIST_URI:-http://beta.quicklisp.org/dist/quicklisp.txt}

USE_LISP=$LISP
unset LISP

LISP_IMPLS_BIN="$ROSWELL_INSTALL_DIR/bin"
LISP_IMPLS_DIR="$ROSWELL_DIR/impls/system"


usage () {
  cat <<"EOF"
Usage:
    install-for-ci.sh

EOF
}

log () {
    echo "$ $1"
    echo `$1`
}

fetch () {
    echo "Downloading $1..."
    if curl --no-progress-bar --retry 10 -o $2 -L $1; then
        return 0;
    else
        echo "Failed to download $1."
        exit 1
    fi
}

extract () {
    opt=$1
    file=$2
    destination=$3
    echo "Extracting a tarball $file into $destination..."
    mkdir -p "$destination"
    tar -C "$destination" --strip-components=1 "$opt" -xf "$file"
}

install_script () {
    path=$1; shift
    dir=$(dirname "$path")
    tmp=$(mktemp /tmp/rosci_XXX)

    echo "#!/bin/sh" > "$tmp"
    while [ "$1" != "" ]; do
        echo "$1" >> "$tmp"
        shift
    done
    chmod 755 "$tmp"
    mkdir -p "$dir" 2>/dev/null || $SUDO mkdir -p "$dir"

    if [ -w "$dir" ]; then
        mv "$tmp" "$path"
    else
        $SUDO mv "$tmp" "$path"
    fi
}

apt_installed_p () {
    if uname -s | grep -E "MSYS|MINGW" >/dev/null; then
        true
    elif [ `uname` = "Darwin" ]; then
        if brew info "$1" |grep installed;then
            false
        else
            true
        fi
    elif [ `uname` = "FreeBSD" ]; then
        $(pkg info | grep ^$1- >/dev/null 2>&1)
    elif which dpkg >/dev/null; then
        $(dpkg -s "$1" >/dev/null 2>&1)
    else
        $(apk info |grep ^$1 >/dev/null 2>&1)
    fi
}

apt_unless_installed () {
    if ! apt_installed_p "$1"; then
        if [ `uname` = "Darwin" ]; then
            brew install "$1"
        elif [ `uname` = "FreeBSD" ]; then
            $SUDO pkg install -y "$1"
            hash -r
        elif which apt-get >/dev/null; then
            $SUDO -E apt-get -yq update
            $SUDO -E apt-get -yq --no-install-suggests --no-install-recommends --force-yes install "$1"
        elif which apk >/dev/null; then
            $SUDO apk add $1
        fi
    fi
}

install_abcl () {
    java=$(which java)
    if [ "$java" = "" ]; then
        if apt_installed_p "openjdk-7-jre"; then
            java="/usr/lib/jvm/java-7-openjdk/bin/java"
        elif apt_installed_p "openjdk-6-jre"; then
            java="/usr/lib/jvm/java-6-openjdk/bin/java"
        else
            apt_unless_installed openjdk-7-jre
            java="/usr/lib/jvm/java-7-openjdk/bin/java"
        fi
    fi
    PATH="$java:$PATH" ros install abcl-bin
    ros use abcl-bin
}

install_ecl () {
    if [ `uname` = "Darwin" ]; then
        brew install ecl
        ros use ecl/system
    else
        ros install ecl
        ros use ecl
    fi
}

install_roswell_bin () {
    if ! which ros >/dev/null; then
        if uname -s | grep -E "MSYS_NT|MINGW64|MINGW32" >/dev/null; then
            if [ $ROSWELL_BRANCH = release ]; then
                if [ "$ROSWELL_FORCE32" = "true" ];then
                     pacman -S mingw-w64-i686-roswell
                else
                     pacman -S $MINGW_PACKAGE_PREFIX-roswell
                fi
            fi
        elif uname -s |grep Linux >/dev/null && uname -m |grep x86_64 >/dev/null; then
            if [ "$ROSWELL_INSTALL_DIR" = "/usr/local" ]; then
                apt_unless_installed curl
                apt_unless_installed make
                FILE=roswell-$ROSWELL_RELEASE_VERSION-linux-`uname -m`
                if [ $ROSWELL_BRANCH = release ]; then
                    fetch "https://github.com/roswell/roswell/releases/download/v$ROSWELL_RELEASE_VERSION/$FILE.tar.bz2" /tmp/$FILE.tar.bz2
                fi
                if [ -f /tmp/$FILE.tar.bz2 ]; then
                    extract -j /tmp/$FILE.tar.bz2 /tmp/roswell;$SUDO make -C /tmp/roswell install
                    rm -rf /tmp/$FILE.tar.bz2
                    rm -rf /tmp/roswell
                fi
            fi
        elif [ `uname` = "FreeBSD" ]; then
            apt_unless_installed gmake
            GNUMAKE=/usr/local/bin/gmake
            apt_unless_installed roswell
        elif [ `uname` = "Darwin" ] && [ $ROSWELL_BRANCH = release ]; then
            apt_unless_installed roswell
        fi
    fi
}

install_roswell_src () {
    if ! which ros >/dev/null; then
        fetch "$ROSWELL_REPO/archive/$ROSWELL_BRANCH.tar.gz" "$ROSWELL_TARBALL_PATH"
        extract -z "$ROSWELL_TARBALL_PATH" "$ROSWELL_DIR"
        apt_unless_installed libcurl4-openssl-dev
        cd $ROSWELL_DIR
        sh bootstrap
        mkdir -p ~/.roswell
        echo "sbcl-bin-version-uri	0	$ROSWELL_PLATFORMHTML_BASE" >> ~/.roswell/config;
        echo "sbcl-bin-uri	0	$ROSWELL_SBCL_BIN_URI" >> ~/.roswell/config;
        ./configure --prefix=$ROSWELL_INSTALL_DIR
        make
        if [ -w "$ROSWELL_INSTALL_DIR" ]; then
            make install
        else
            $SUDO make install
        fi
    fi
}

install_roswell () {
    if ! which ros >/dev/null; then
        echo "Installing Roswell..."
        install_roswell_bin
        install_roswell_src
        echo "Roswell has been installed."
    else
        echo "Detected Roswell."
    fi
}

install_sbcl_bin () {
    true
}

install_lisp_dependency () {
    case "$USE_LISP" in
        alisp|allegro)
	      apt_unless_installed libc6-i386
          apt_unless_installed gcc-multilib
          USE_LISP=allegro
          ;;
        cmu|cmucl|cmu-bin)
          apt_unless_installed libc6-i386
          apt_unless_installed gcc-multilib
          USE_LISP=cmu-bin
          ;;
        # 'ccl' is an alias for 'ccl-bin'
        ccl)
          USE_LISP=ccl-bin
          ;;
        ccl32)
          USE_LISP=ccl-bin
          apt_unless_installed libc6-i386
          apt_unless_installed gcc-multilib
          ros config set ccl.bit 32
          ;;
        # 'sbcl-bin' is the default
        "")
          USE_LISP=sbcl-bin
          ;;
    esac
}

install_lisp () {
    echo "Installing $USE_LISP..."
    case "$USE_LISP" in
        clisp)
            if [ `uname` = "Darwin" ]; then
                apt_unless_installed clisp;
                ros use clisp/system;
            else
                ros install $USE_LISP;
            fi
            ros install asdf;
            ;;
        abcl)
            install_abcl
            ;;
        ecl)
            install_ecl
            ;;
        sbcl-bin)
            ros use $USE_LISP
            ;;
        *)
            ros install $USE_LISP;
            ros use $USE_LISP
            ;;
    esac
}

install_asdf () {
   log "ros quicklisp.dist=$ROSWELL_QUICKLISP_DIST_URI setup"
   if [ "$ROSWELL_LATEST_ASDF" ]; then
       echo "Installing the latest ASDF..."
       ros install asdf
   fi
}

show_setup () {
    log "ros version"
    ros -e '(format t "~&~A ~A up and running! (ASDF ~A)~2%"
                    (lisp-implementation-type)
                    (lisp-implementation-version)
                    #+asdf(asdf:asdf-version) #-asdf "not required")' || exit 1
}

setup_source_regisry () {
   # Setup ASDF source regisry
   if [ "$LOCALAPPDATA" ]; then
       ASDF_SR_CONF_DIR="$LOCALAPPDATA/config/common-lisp/source-registry.conf.d"
   else
       ASDF_SR_CONF_DIR="$HOME/.config/common-lisp/source-registry.conf.d"
   fi
   ASDF_SR_CONF_FILE="$ASDF_SR_CONF_DIR/ci.conf"
   LOCAL_LISP_TREE="$HOME/lisp"

   mkdir -p "$ASDF_SR_CONF_DIR"
   mkdir -p "$LOCAL_LISP_TREE"
   if [ "$TRAVIS" ]; then
       echo "(:tree \"$TRAVIS_BUILD_DIR/\")" > "$ASDF_SR_CONF_FILE"
   elif [ "$CIRCLECI" ]; then
       echo "(:tree \"$CIRCLE_WORKING_DIRECTORY/\")" > "$ASDF_SR_CONF_FILE"
   elif [ "$GITHUB_WORKSPACE" ]; then
       if uname -s | grep -E "MSYS_NT|MINGW" >/dev/null; then
           GITHUB_WORKSPACE_LISP=`echo $GITHUB_WORKSPACE | sed -e 's/\\\\/\//g'`
           echo "(:tree \"$GITHUB_WORKSPACE_LISP/\")" > "$ASDF_SR_CONF_FILE"
       else
           echo "(:tree \"$GITHUB_WORKSPACE/\")" > "$ASDF_SR_CONF_FILE"
       fi
   fi
   echo "(:tree \"$LOCAL_LISP_TREE/\")" >> "$ASDF_SR_CONF_FILE"
   echo "ASDF source registry configurations at ${ASDF_SR_CONF_FILE}."
}


install_all () {
   install_roswell
   install_sbcl_bin
   install_lisp_dependency
   install_lisp
   install_asdf
   show_setup
   setup_source_regisry
   exit 0
}


if [ x$CI = "xtrue" ]; then
   if which sudo 2>&1 >/dev/null; then
       SUDO=sudo
   fi
   install_all
fi

configure_args=

while test $# != 0
do
    case $1 in
        --*=?*)
            option=`expr "X$1" : 'X\([^=]*\)='`
            optarg=`expr "X$1" : 'X[^=]*=\(.*\)'`
            extra_shift=:
            ;;
        --*=)
            option=`expr "X$1" : 'X\([^=]*\)='`
            optarg=
            extra_shift=:
            ;;
        *)
            option=$1
            optarg=$2
            extra_shift=shift
            ;;
    esac

    case $option in
        --configure-args) configure_args=$optarg; $extra_shift ;;

        --sudo)       SUDO=sudo ;;

        *) usage; exit 1;;
    esac
    shift
done
