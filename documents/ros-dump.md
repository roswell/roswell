ros-dump - make a dumped image of a roswell script

# Synopsis

**ros [options] dump** `TYPE` args...

Where `TYPE` is one of:

## output [-f] [-o OUTPUT] [OPTIONS...] NAME

Specifing `output` saves the current lisp state to an image.
It loads the roswell script `NAME` and also processes other options `options...` available to roswell.

The output file is by default written to an internal directory of roswell,
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

## executable

In contrast, when a script is dumped with `executable`, the dumped image
becomes an self-contained executable binary which implies `--restart main`.
The output file is deduced from `NAME` and is written in the current directory.

This feature is SBCL and CCL only.

# Description

`ros-dump` creates a dumped lisp image of the state after processing all
arguments passed.

In both types, the global environment (e.g. global binding to a special
variable such as `*package*`) is the environment which was effective when
the script was dumped.

<!-- Fixme: what kind of? this is unnecessarily retracting the users from using this feature -->
<!-- There might be a limitation regarding this feature depending on the lisp -->
<!-- implementation used by roswell at the time of building.   -->

The dumped image is not compatible between the different
implementations and even between the same implementations with
different versions.

<!-- # options -->
<!--  -->
<!-- # Environmental Variables -->

# SEE ALSO
_sbcl_(1) _ros_(1) _ros-init_(1)
