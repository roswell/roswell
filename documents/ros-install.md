ros-install - Install lisp implementations or quicklisp system

# Synopsis

**ros install** [name]

<!-- # subcommands -->

name

  : a name specifying a lisp implementation/version, or a quicklisp system.

# Installing a Lisp Implementation

When the `name` matches to one of the supported implementations of roswell, it fetch and download the implementation. It is not automatically set as the default implementation to use. For setting the default lisp implementation, use _ros-use(1)_.

For example, the following command download the latest sbcl binary from sbcl.com. Note that this may be different freom the default binary installed by roswell, called `sbcl-bin`. `sbcl-bin` is a stable and well-tested version of the sbcl binary which is supposed to be a little older than the latest sbcl.

    $ ros install sbcl

When invoked without a name, it prints the list of installable implementations. (Not to be confused with `ros list installed`, which shows the implementations already installed.)

    Usage: ros install impl [OPTIONS]

    For more details on impl specific options, type:
    ros help install impl

    Candidates impls for installation are:
    ecl
    sbcl
    clisp
    ccl-bin
    sbcl-bin

The name can be a name of the implementation, or a version of the implementation prepended with the name separated by a slash (/). The ones ending with `-bin` ususally do not have these variants.

    $ ros install sbcl/1.2.14

There might be some _hidden_ implementation that are not listed here --- they are in the alpha quality, but try the one you like or watch the website (https://github.com/roswell/roswell).

    $ ros install ccl
    $ ros install abcl
    $ ...

<!-- # options -->
<!--  -->
<!-- # Environmental Variables -->

# Installing a quicklisp system and the bundled roswell scripts

If the specified name does not match any of the implementations, roswell tries to find a quicklisp system of the given name.

    $ ros install alexandria

After compiling and loading the system, it funcalls `ros:*build-hook*` special variable with no argument, if some function is set during the compilation/load.

When the system comes with a **roswell script** created by _ros-init_(1) in the subdirectory `roswell`, the scripts are installed into **ROSWELL_INSTALL_DIR/bin**. Setting the path to this directory makes those scripts available from the shell command line.

    $ ros install qlot
    System 'qlot' found. Loading the system..
    Processing build-hook..
    Found 1 scripts: qlot
    Attempting to install the scripts in roswell/ subdirectory of the system...
    /home/user/.roswell/bin/qlot

    $ qlot
    Usage: qlot [install | update | bundle | exec shell-args..]



# SEE ALSO
_sbcl_(1), _ros_(1), _ros-list_(1), _ros-init_(1)

