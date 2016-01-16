


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

