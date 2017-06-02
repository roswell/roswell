ros-dump - make a dumped image of a roswell script

# Synopsis

**ros [OPTIONS1...]  [OPTIONS2...] MODE** args...

`OPTIONS1` specify the standard roswell options such as `-L` or `-m LISP`.
`OPTIONS2` specify the image reduction options we describe later.
`MODE` is either `output` or `executable`.

# Description

`ros-dump` creates a dumped lisp image of the state after processing all
options `OPTIONS1`.

It has two modes `output` and `executable`.
In both modes, the global environment (e.g. global binding to special
variables such as `*package*`) of the state just after the restart is
the environment which was effective when the script was dumped.

<!-- Fixme: what kind of? this is unnecessarily retracting the users from using this feature -->
<!-- There might be a limitation regarding this feature depending on the lisp -->
<!-- implementation used by roswell at the time of building.   -->

The dumped image is generally not compatible between the different
implementations, and also between the versions.

# Dump modes

## output [-f] [-o OUTPUT] NAME

It loads the roswell script `NAME` and saves the current lisp state to an image.

The image file is by default written to an internal directory of roswell in an organized manner,
i.e., somewhere under `$ROSWELL_INSTALL_DIR` deduced by the current
implementation, its version and the given NAME.

When the output file already exists, the command fails with an error code.

The image can be loaded by `ros -m IMAGE`. When restarting from the image, standard roswell
subcommands and additional arguments to the script are also available, for
example `ros run` to resume with a repl, or `--restart FUNC` to call a
specific function, leaving some flexibility.

`-o OUTPUT`

  : The image is written to `OUTPUT` instead of the default location.

`-f`

  : Force output when the output already exists.

## executable NAME [-o OUTPUT]

When a script is dumped with `executable`, the dumped image
becomes an self-contained executable binary which implies `--restart main`.

If `OUTPUT` is given, the resulting binary is written to this file.
Otherwise, the output filename is deduced from `NAME` and is written in the same directory.
On Windows and if `SCRIPT` has `.ros` extension, the filename will be `SCRIPT.exe`.
On the other systems, the result will be `SCRIPT` (without extension).

This feature is supported on SBCL, CCL, CMUCL, CLISP, ECL.

# Image Reduction Options

These options unlink some references to the runtime objects and allow gcs to
reclaim some memory, which eventually reduces the size of the dumped image.

Care should be taken to ensure the resulting program works as expected, as some
of these operation may destroy the common assumptions of the conforming
programs. For example, package-related reduction options may inhibit the runtime
calls to `read` after the restart.

Reduction options are processed in the left-to-right manner.

--disable-compression, --enable-compression, -c
    : These options disable/enable/enable the core compression feature in SBCL. Thus this option is meaningful
    only on SBCL. Compression is enabled by default.
    
--remove-docstrings
    : This option removes all docstrings from all symbols in the entire lisp image.
    
--delete-package PKG
    : This option can be specified multiple times. It uninterns the symbols in
      the given package, calls `makeunbound` and `fmakeunbound` on each symbol
      and deletes the package.
      
--delete-all-packages
    : This option applies --delete-package PKG on all packages, except some
      blacklisted packages (keyword, roswell, ROS.SCRIPT.DUMP, and the package
      of the main function symbol).

--delete-packages-except
    : This option can be specified multiple times. It is identical to
      --delete-all-packages except that it adds additional packages to
      the blacklist.

--destroy-packages-sbcl
    : This is an sbcl-specific option which is even more aggressive than the
      above methods. It destroys the packaging system by modifying the internal
      tables for packages, cleaning up the caches for package-use-list etc.
      The blacklist is shared among --delete-all-packages and
      --destroy-packages-sbcl.  However, this method does not call
      fmakunbound/makunbound, so combining the two methods can result in a more
      aggressive image size reduction.  Due to the nature of this option, it is
      desirable to specify it as the last method (i.e. rightmost).

<!-- # options -->
<!--  -->
<!-- # Environmental Variables -->

# SEE ALSO
_sbcl_(1) _ros_(1) _ros-init_(1)
