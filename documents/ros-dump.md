# name

ros-dump - make a dumped image of a roswell script

# synopsis

**ros [options] dump** *type* args...

Where the types of subcommands are one of:

output
:Create a dump image.

executable
:Create an executable.

# description

`ros-dump` creates a dumped lisp image of the state after processing all
arguments passed.

Specifing `output` results in just an image which results in the same lisp
state when loading the image. The image can be loaded by `ros -m
IMAGE`. Standard roswell subcommands and additional arguments to the script
are also available, for example `ros run` to resume with a repl, or
`--restart FUNC` to call a specific function, leaving some flexibility.

In contrast, when a script is dumped with `executable`, the dumped image
becomes an self-contained executable binary which implies `--restart main`.

In both cases, the global environment (e.g. global binding to a special
variable such as `*package*`) is the environment which was effective when
the script was dumped.

There might be a limitation regarding this feature depending on the lisp
implementation roswell is configured to use at the time of building.  Also,
the dumped image is not compatible between the different implementations
and even between the same implementations with different versions.

<!-- # options -->
<!--  -->
<!-- # Environmental Variables -->

# SEE ALSO
_sbcl_(1) _ros_(1)

# Author

SANO Masatoshi
