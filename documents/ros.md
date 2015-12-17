Roswell - Common Lisp environment setup Utility

# Synopsis

* **ros** [**options**] `[command [args...]`
* **ros** [**options**] `[--]script [args...]`
* **ros** [**options**] < script.ros

# Description

Roswell is a command line tool for installing and managing Common Lisp implementations, as well as a scripting infrastructure for templating, writing, executing, compiling, distributing, downloading and installing ROSWELL-SCRIPTS written in Common Lisp.

Currently roswell supports sbcl, ccl, clisp and ecl as its supported lisp implementations. For further details see _ros-install(1)_.

Scripts installed by roswell will be system wide if appropriate directory is included in PATH.

# Subcommands

In an order of utility/frequency.

install

  : Install a given implementation (e.g. sbcl, ccl) or a system (e.g. alexandria) for roswell environment. See _ros-install(1)_.

init [name[.ros]]

  : Create a new ros script

dump [executable|output] [script]

  : Dump an image of the script for the faster startup or to make an executable.

build

  : Make an executable from the script.

run

  : Initiate REPL

use

  : Change the default implementation used by roswell.

list

  : List the various informations.

config

  : Get and set the options.

setup

  : Run the initial setup

emacs

  : Launch emacs with slime.

wait

  : Wait forever, used for daemonizing the script.

delete

  : Delete an installed implementation.

version

  : Show the roswell version information.

help

  : Show the subcommand help.

# Options

Options are processed in left-to-right order.

-w CODE     --wrap CODE

  : Run the script with a shell wrapper CODE, e.g. rlwrap

-m IMAGE    --image IMAGE        

  : Continue from Lisp image IMAGE

-L NAME     --lisp NAME          

  : Run the script with a lisp impl NAME[/VERSION] if present, e.g. sbcl-bin, sbcl/1.2.16. Fails otherwise.

-l FILE     --load FILE          

  : Load a lisp file FILE while building

-S X        --source-registry X  

  : Override the source registry of asdf systems.

-s SYSTEM   --system SYSTEM      

  : Load the asdf SYSTEM while building.

--load-system SYSTEM 

  : Same as above (buildapp compatibility)

-p PACKAGE  --package PACKAGE    

  : Change the current package to PACKAGE

-sp SP      --system-package SP  

  : Combination of -s SP and -p SP

-e FORM     --eval FORM          

  : Evaluate FORM while building

--require MODULE

  : require MODULE while building

-q          --quit               

  : quit lisp here

-r FUNC     --restart FUNC       

  : the build image restarts from calling FUNC

-E FUNC     --entry FUNC         

  : the build image restarts from calling (FUNC argv).

-i FORM     --init FORM          

  : evaluate FORM after the restart.

-ip FORM    --print FORM         

  : evaluate and princ FORM after the restart

-iw FORM    --write FORM         

  : evaluate and write FORM after the restart

-F FORM     --final FORM         

  : evaluate FORM before dumping the IMAGE

-R          --rc                 

  : try to read /etc/rosrc, ~/.roswell/init.lisp

+R          --no-rc              

  : skip /etc/rosrc, ~/.roswell/init.lisp

-Q          --quicklisp          

  : load quicklisp (default)

+Q          --no-quicklisp       

  : do not load quicklisp

-v          --verbose            

  : be quite verbose while building

--quiet

  : suppress output while building (default)

--test

  : for test purpose

dynamic-space-size=[size in MB]

  : SBCL specific. The argument is passed to SBCL by `--dynamic-space-size`




# Environmental Variables

ROSWELL_INSTALL_DIR

  : Specifies the install directory of roswell, defaulted to $HOME/.roswell . Roswell scripts are installed in $ROSWELL_INSTALL_DIR/bin .

ROSWELL_BRANCH

  : used for testing roswell itself.

ROSWELL_REPO

  : used for testing roswell itself.

LISP_IMPLS_DIR

  : used for testing roswell itself.

LISP_IMPLS_BIN

  : used for testing roswell itself.

ROSWELL_TARBALL_PATH

  : used for testing roswell itself.

ROSWELL_URI

  : used for testing roswell itself.

<!-- ROSWELL_HTML_TEST -->
<!--  -->
<!--   :  -->


# Bugs

Check out issues list (https://github.com/snmsts/roswell/issues)

# SEE ALSO
_sbcl_(1) _ros-dump_(1) _ros-init_(1) _ros-install_(1) _ros-list_(1) _ros-setup_(1)


