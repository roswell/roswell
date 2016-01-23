

# modifying the scripts

The changes you make to the source will not be effective immediately, until you issue a command `ros setup`.

The reason behind this is as follows:

Some of the roswell scripts under this directory contains feature directives such as `#-ros.sub.use` .

These scripts are compiled and dumped into the roswell binary in .roswell/impls/.../dump/roswell.core .
During the development, 
the core may reload the source, but due to #-ros.sub.X, some definitions is not going to be redefined.

This inconveniency is sacrificed for the faster startup of roswell.
By skipping certain function definitions, rereading such files become faster.

# code structure

Control flow of running C-level subcommands (does not require lisp: e.g. `ros install sbcl`)

* ros.c
* other C source files

Control flow of running lisp-level subcommands (requires lisp)

* ros.c
* cmd-run-[IMPL].c
* init.lisp: function `run` --- The entry of all roswell commands
* init.lisp: other functions such as `load`, `system-package`
* XXX.ros --- subcommand implementations

# output style convention

When the number of arguments were insufficient and more subcommands are
expected, roswell should output the possible candidates of subcommands to
the standard output, each separated by a whitespace character. These are
interpreted by the shell as a hint to the completion, therefore no other
stuff should be printed to the stdout in such situation. When necessary,
use stderr.

Example:

the output of `ros dump` should be `output\nexecutable`. As a result,
Bash will be able to complete

`ros dump o`

to 

`ros dump output`


# exit code

Exit code 0:

Roswell processed the given operation as expected.

Exit code 1:

Roswell encountered an expected error case.
Such behavior should be explained in documents/ .

Exit code 2:

Roswell encountered an unexpected error.

