ros-init - Create a roswell script (optionally based on a template)

# Synopsis

**ros init [template]** name

<!-- # subcommands -->

template

  : Specify the name of a template.

# Description

Initialises [name].ros.

The default template is something like:

```
#!/bin/sh
#|-*- mode:lisp -*-|#
#|
exec ros -Q -- $0 "$@"
|#
(defun main (&rest argv)
  (declare (ignorable argv)))
```

This is basically a shell script which immediately invokes Roswell by exec (see _sh(1)_). Roswell loads the same script as an input, skips multi-line comments, reads the rest of the file as a Common Lisp program, and finally invokes a function main with command-line arguments.

<!-- # options -->
<!--  -->
<!-- # Environmental Variables -->

# SEE ALSO
_sbcl_(1) _ros_(1)

