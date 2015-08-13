#!/bin/sh

log () {
    echo "$ $1"
    echo `$1`
}

ROSWELL_TARBALL_PATH=$HOME/roswell.tar.gz
ROSWELL_DIR=$HOME/roswell
ROSWELL_REPO=${ROSWELL_REPO:-https://github.com/snmsts/roswell}
ROSWELL_BRANCH=${ROSWELL_BRANCH:-release}
ROSWELL_INSTALL_DIR=${ROSWELL_INSTALL_DIR:-/usr/local/}
LISP_IMPLS_DIR="$ROSWELL_DIR/impls/system"
LISP_IMPLS_BIN="$ROSWELL_DIR/bin"

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
    tmp=$(mktemp)

    echo "#!/bin/sh" > "$tmp"
    while [ "$1" != "" ]; do
        echo "$1" >> "$tmp"
        shift
    done
    chmod 755 "$tmp"

    if [ -w "$dir" ]; then
        mv "$tmp" "$path"
    else
        sudo mv "$tmp" "$path"
    fi
}

apt_unless_installed () {
    if ! [ dpkg -s "$1" >/dev/null 2>&1 ]; then
        sudo apt-get install "$1"
    fi
}

CMU_TARBALL_URL="https://common-lisp.net/project/cmucl/downloads/snapshots/2015/07/cmucl-2015-07-x86-linux.tar.bz2"
CMU_EXTRA_TARBALL_URL="https://common-lisp.net/project/cmucl/downloads/snapshots/2015/07/cmucl-2015-07-x86-linux.extra.tar.bz2"
CMU_DIR="$LISP_IMPLS_DIR/cmucl"
install_cmucl () {
    if ! [ -f "$LISP_IMPLS_BIN/cmucl" ]; then
        fetch "$CMU_TARBALL_URL" "$HOME/cmucl.tar.bz2"
        extract -j "$HOME/cmucl.tar.bz2" "$CMU_DIR"
        fetch "$CMU_EXTRA_TARBALL_URL" "$HOME/cmucl-extra.tar.bz2"
        extract -j "$HOME/cmucl-extra.tar.bz2" "$CMU_DIR"

        export CMUCLLIB="$CMU_DIR/lib/cmucl/lib"
        install_script "$LISP_IMPLS_BIN/cmucl" \
            "export CMUCLLIB=\"$CMU_DIR/lib/cmucl/lib\"" \
            "exec \"$CMU_DIR/bin/lisp\" \"\$@\""
    fi
    PATH="$LISP_IMPLS_BIN:$PATH" ros use cmucl/system
}

ABCL_TARBALL_URL="https://common-lisp.net/project/armedbear/releases/1.3.2/abcl-bin-1.3.2.tar.gz"
ABCL_DIR="$LISP_IMPLS_DIR/abcl"
install_abcl () {
    if ! [ -f "$LISP_IMPLS_BIN/abcl" ]; then
        apt_unless_installed default-jre
        fetch "$ABCL_TARBALL_URL" "$HOME/abcl.tar.gz"
        extract -z "$HOME/abcl.tar.gz" "$ABCL_DIR"
        install_script "$LISP_IMPLS_BIN/abcl" \
            "exec java -cp \"$ABCL_DIR/abcl-contrib.jar\" -jar \"$ABCL_DIR/abcl.jar\" \"\$@\""
    fi
    PATH="$LISP_IMPLS_BIN:$PATH" ros use abcl/system
}

ECL_TARBALL_URL="http://downloads.sourceforge.net/project/ecls/ecls/15.3/ecl-15.3.7.tgz"
ECL_DIR="$LISP_IMPLS_DIR/ecl"
install_ecl () {
    if ! [ -f "$LISP_IMPLS_BIN/ecl" ]; then
        fetch "$ECL_TARBALL_URL" "$HOME/ecl.tgz"
        extract -z "$HOME/ecl.tgz" "$HOME/ecl-src"
        cd $HOME/ecl-src
        ./configure --prefix="$ECL_DIR"
        make
        make install
        install_script "$LISP_IMPLS_BIN/ecl" \
            "exec \"$ECL_DIR/bin/ecl\" \"\$@\""
    fi
    PATH="$LISP_IMPLS_BIN:$PATH" ros use ecl/system
}

ALLEGRO_TARBALL_URL="http://www.franz.com/ftp/pub/acl90express/linux86/acl90express-linux-x86.bz2"
ALLEGRO_DIR="$LISP_IMPLS_DIR/acl"
install_allegro () {
    if ! [ -f "$LISP_IMPLS_BIN/alisp" ]; then
        fetch "$ALLEGRO_TARBALL_URL" "$HOME/acl.bz2"
        extract -j "$HOME/acl.bz2" "$ALLEGRO_DIR"
        install_script "$LISP_IMPLS_BIN/alisp" \
            "exec \"$ALLEGRO_DIR/alisp\" \"\$@\""
    fi
    PATH="$LISP_IMPLS_BIN:$PATH" ros use alisp/system
}

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
log "ros --version"

case "$LISP" in
    # 'ccl' is an alias for 'ccl-bin'
    ccl)
        LISP=ccl-bin
        ;;
    # 'sbcl-bin' is the default
    "")
        LISP=sbcl-bin
        ;;
esac

echo "Installing $LISP..."
case "$LISP" in
    clisp)
        apt_unless_installed clisp
        ros use clisp/system
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
    *)
        ros install $LISP
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
