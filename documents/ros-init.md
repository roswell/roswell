ros-init - Create a roswell script (optionally based on a template)

# Synopsis

**ros init NAME [TEMPLATE [ARGS...]]** 

TEMPLATE

  : Specifies the name of a template, defaulted to *default*
    template. However, if *TEMPLATE* is not specified and *FILENAME*
    matches one of the templates being stored, then it automatically uses
    the template. To suppress this behavior, you should explicitly
    specify *TEMPLATE* as *default*.

    Thus you are warned when you use this command from a shell script. In
    order to achieve a consistent and desired behavior, it is adviced that
    they should always explicitly specify the template name.

NAME

  : Specify the output filename, or "-" to indicate *stdout*. When *TEMPLATE* is *default*, it automatically appends a file type ".ros".


<!-- # subcommands -->

# Description

Initialises a roswell file based on a template. User-specified templates can be added by _ros-template_(1).

The default template is something like:

```
#!/bin/sh
#|-*- mode:lisp -*-|#
#| <Put a one-line description here>
exec ros -Q -- $0 "$@"
|#
(progn ;;init forms
  (ros:ensure-asdf)
  ;;#+quicklisp (ql:quickload '() :silent t)
  )

(defpackage :ros.script.test.3703600390
  (:use :cl))
(in-package :ros.script.test.3703600390)

(defun main (&rest argv)
  (declare (ignorable argv)))
;;; vim: set ft=lisp lisp:
```

This is basically a shell script which immediately invokes Roswell by exec (see _sh(1)_). Roswell loads the same script as an input, skips multi-line comments, reads the rest of the file as a Common Lisp program, and finally invokes a function main with command-line arguments.

<!-- # options -->
<!--  -->
<!-- # Environmental Variables -->

# SEE ALSO
_sbcl_(1) _ros_(1) _ros-template_(1)

