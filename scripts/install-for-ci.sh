#!/bin/sh

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
    tmp=$(mktemp)

    for line; do
        echo "$line" >> "$tmp"
    done
    chmod 755 "$tmp"

    sudo mv "$tmp" "$path"
}

CMU_TARBALL_URL="https://common-lisp.net/project/cmucl/downloads/snapshots/2015/07/cmucl-2015-07-x86-linux.tar.bz2"
CMU_EXTRA_TARBALL_URL="https://common-lisp.net/project/cmucl/downloads/snapshots/2015/07/cmucl-2015-07-x86-linux.extra.tar.bz2"
install_cmucl () {
    fetch "$CMU_TARBALL_URL" "$HOME/cmucl.tar.bz2"
    extract -j "$HOME/cmucl.tar.bz2" "$HOME/cmucl"
    fetch "$CMU_EXTRA_TARBALL_URL" "$HOME/cmucl-extra.tar.bz2"
    extract -j "$HOME/cmucl-extra.tar.bz2" "$HOME/cmucl"

    export CMUCLLIB="$HOME/cmucl/lib/cmucl/lib"
    sudo ln -s "$HOME/cmucl/bin/lisp" "/usr/local/bin/cmucl"
}

ABCL_TARBALL_URL="https://common-lisp.net/project/armedbear/releases/1.3.2/abcl-bin-1.3.2.tar.gz"
install_abcl () {
    sudo apt-get install default-jre
    fetch "$ABCL_TARBALL_URL" "$HOME/abcl.tar.gz"
    extract -z "$HOME/abcl.tar.gz" "$HOME/abcl"
    install_script "#!/bin/sh" \
        "java -cp \"$HOME/abcl/abcl-contrib.jar\" -jar \"$HOME/abcl/abcl.jar\" \"\$@\""
}

ECL_TARBALL_URL="http://downloads.sourceforge.net/project/ecls/ecls/15.3/ecl-15.3.7.tgz"
install_ecl () {
    fetch "$ECL_TARBALL_URL" "$HOME/ecl.tgz"
    sudo tar -C / -xzf "$HOME/ecl.tgz"
}

ROSWELL_TARBALL_PATH=$HOME/roswell.tar.gz
ROSWELL_DIR=$HOME/roswell
ROSWELL_REPO=${ROSWELL_REPO:-https://github.com/snmsts/roswell}
ROSWELL_BRANCH=${ROSWELL_BRANCH:-release}
ROSWELL_INSTALL_DIR=${ROSWELL_INSTALL_DIR:-/usr/local/}

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
        sudo apt-get install clisp
        ros use clisp/system
        ;;
    cmu|cmucl)
        install_cmucl
        ros use cmucl/system
        ;;
    abcl)
        install_abcl
        ros use abcl/system
        ;;
    ecl)
        install_ecl
        ros use ecl/system
        ;;
    *)
        ros install $LISP
        ;;
esac

ros -e '(format t "~&~A ~A up and running! (ASDF ~A)~2%"
                (lisp-implementation-type)
                (lisp-implementation-version)
                (asdf:asdf-version))'

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
