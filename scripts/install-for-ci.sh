#!/bin/sh

set -e

ROSWELL_TARBALL_PATH=$HOME/roswell.tar.gz
ROSWELL_DIR=$HOME/.roswell
ROSWELL_REPO=${ROSWELL_REPO:-https://github.com/roswell/roswell}
ROSWELL_BRANCH=${ROSWELL_BRANCH:-release}
ROSWELL_INSTALL_DIR=${ROSWELL_INSTALL_DIR:-/usr/local}
LISP_IMPLS_BIN="$ROSWELL_INSTALL_DIR/bin"
LISP_IMPLS_DIR="$ROSWELL_DIR/impls/system"
CMU_DIR="$LISP_IMPLS_DIR/cmucl"
ALLEGRO_DIR="$LISP_IMPLS_DIR/acl"

CMU_TARBALL_URL="https://common-lisp.net/project/cmucl/downloads/snapshots/2016/02/cmucl-2016-02-x86-linux.tar.bz2"
CMU_EXTRA_TARBALL_URL="https://common-lisp.net/project/cmucl/downloads/snapshots/2016/02/cmucl-2016-02-x86-linux.extra.tar.bz2"
ALLEGRO_TARBALL_URL="http://www.franz.com/ftp/pub/acl100express/linux86/acl100express-linux-x86.bz2"
ALLEGRO_DMG_URL="http://www.franz.com/ftp/pub/acl100express/macosx86/acl100express-macosx-x86.dmg"

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
    mkdir -p "$dir" 2>/dev/null || sudo mkdir -p "$dir"

    if [ -w "$dir" ]; then
        mv "$tmp" "$path"
    else
        sudo mv "$tmp" "$path"
    fi
}

apt_installed_p () {
    if [ `uname` = "Darwin" ]; then
        if brew info "$1" |grep installed;then
            false
        else
            true
        fi
    else
        $(dpkg -s "$1" >/dev/null 2>&1)
    fi
}
apt_unless_installed () {
    if ! apt_installed_p "$1"; then
        if [ `uname` = "Darwin" ]; then
            brew install "$1"
        else
            sudo apt-get update
            sudo apt-get install "$1"
        fi
    fi
}

install_cmucl () {
    if [ `uname` = "Darwin" ]; then
        brew install homebrew/binary/cmucl
    else
        if ! [ -f "$LISP_IMPLS_BIN/cmucl" ]; then
            apt_unless_installed libc6-i386
            fetch "$CMU_TARBALL_URL" "$HOME/cmucl.tar.bz2"
            extract -j "$HOME/cmucl.tar.bz2" "$CMU_DIR"
            fetch "$CMU_EXTRA_TARBALL_URL" "$HOME/cmucl-extra.tar.bz2"
            extract -j "$HOME/cmucl-extra.tar.bz2" "$CMU_DIR"

            export CMUCLLIB="$CMU_DIR/lib/cmucl/lib"
            install_script "$LISP_IMPLS_BIN/cmucl" \
            "export CMUCLLIB=\"$CMU_DIR/lib/cmucl/lib\"" \
            "exec \"$CMU_DIR/bin/lisp\" \"\$@\""
        fi
    fi
    PATH="$LISP_IMPLS_BIN:$PATH" ros use cmucl/system
}

install_abcl () {
    java=$(which java)
    if [ "$java" = "" ]; then
        if apt_installed_p "openjdk-7-jre"; then
            java="/usr/lib/jvm/java-7-openjdk/bin/java"
        elif apt_installed_p "openjdk-6-jre"; then
            java="/usr/lib/jvm/java-6-openjdk/bin/java"
        else
            sudo apt-get install "openjdk-7-jre"
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

install_allegro () {
    if ! [ -f "$LISP_IMPLS_BIN/alisp" ]; then
        if [ `uname` = "Darwin" ]; then
            fetch "$ALLEGRO_DMG_URL" "$HOME/acl.dmg"
            mkdir -p $ALLEGRO_DIR
            mount_dir=`hdiutil attach $HOME/acl.dmg | awk -F '\t' 'END{print $NF}'`
            cp -r $mount_dir/AllegroCLexpress.app/Contents/Resources/ $ALLEGRO_DIR/
            # mv $LISP_IMPLS_DIR/Resources $LISP_IMPLS_DIR/acl
            hdiutil detach "$mount_dir"
            install_script "$LISP_IMPLS_BIN/alisp" \
                "exec \"$ALLEGRO_DIR/alisp\" \"\$@\""
        else
            apt_unless_installed libc6-i386
            fetch "$ALLEGRO_TARBALL_URL" "$HOME/acl.bz2"
            extract -j "$HOME/acl.bz2" "$ALLEGRO_DIR"
            install_script "$LISP_IMPLS_BIN/alisp" \
                "exec \"$ALLEGRO_DIR/alisp\" \"\$@\""
        fi
    fi
    PATH="$LISP_IMPLS_BIN:$PATH" ros use alisp/system
}

if ! which ros ; then
    echo "Installing Roswell..."

    fetch "$ROSWELL_REPO/archive/$ROSWELL_BRANCH.tar.gz" "$ROSWELL_TARBALL_PATH"
    extract -z "$ROSWELL_TARBALL_PATH" "$ROSWELL_DIR"
    cd $ROSWELL_DIR
    sh bootstrap
    ./configure --prefix=$ROSWELL_INSTALL_DIR
    make
    if [ -w "$ROSWELL_INSTALL_DIR" ]; then
        make install
    else
        sudo make install
    fi
    echo "Roswell has been installed."
else
    echo "Detected Roswell."
fi

log "ros --version"
log "ros setup"

case "$LISP" in
    # 'ccl' is an alias for 'ccl-bin'
    ccl)
        LISP=ccl-bin
        ;;
    ccl32)
        LISP=ccl-bin
        apt_unless_installed libc6-i386
        ros config set ccl.bit 32
        ;;
    # 'sbcl-bin' is the default
    "")
        LISP=sbcl-bin
        ;;
esac

if [ "$ROSWELL_LATEST_ASDF" ]; then
    echo "Installing the latest ASDF..."
    ros asdf install
fi

echo "Installing $LISP..."
case "$LISP" in
    clisp)
        if [ `uname` = "Darwin" ]; then
            apt_unless_installed clisp
            ros use clisp/system
        else
            ros install $LISP
            ros use $LISP
        fi
        ;;
    cmu|cmucl)
        install_cmucl
        ;;
    abcl)
        install_abcl
        ;;
    ecl)
        install_ecl
        ;;
    allegro|alisp)
        install_allegro
        ;;
    sbcl-bin)
        ros use $LISP
        ;;
    *)
        ros install $LISP
        ros use $LISP
        ;;
esac

ros -e '(format t "~&~A ~A up and running! (ASDF ~A)~2%"
                (lisp-implementation-type)
                (lisp-implementation-version)
                (asdf:asdf-version))' || exit 1

# Setup ASDF source regisry
ASDF_SR_CONF_DIR="$HOME/.config/common-lisp/source-registry.conf.d"
ASDF_SR_CONF_FILE="$ASDF_SR_CONF_DIR/ci.conf"
LOCAL_LISP_TREE="$HOME/lisp"

mkdir -p "$ASDF_SR_CONF_DIR"
mkdir -p "$LOCAL_LISP_TREE"
if [ "$TRAVIS" ]; then
    echo "(:tree \"$TRAVIS_BUILD_DIR/\")" > "$ASDF_SR_CONF_FILE"
elif [ "$CIRCLECI" ]; then
    echo "(:tree \"$HOME/$CIRCLE_PROJECT_REPONAME/\")" > "$ASDF_SR_CONF_FILE"
fi
echo "(:tree \"$LOCAL_LISP_TREE/\")" >> "$ASDF_SR_CONF_FILE"
