
# Synopsis

**ros template** [add|rm|list|show] [OPTIONS] [TEMPLATE [ARGS...]]

Register, manage and remove the templates.

The format of ARGS is same as the lambda list except the following translation:

* &key, &optional, &rest corresponds to --key, --optional, --rest, respectively.
* Abbreviation as -k, -o, -r are available.
* Each argument is separated by --, instead of being surrounded by parentheses.
* Optional and keyword arguments can have a default value. When no defauilt
  value is specified, it implies an empty string.

# Examples

The following example shows how to register a template file (an asdf system definition) named *tmpl.asd*, then instantiate the template with some arguments.

```
$ cat tmpl.asd
(asdf:defsystem NAME :author "AUTHOR" :depends-on DEP :short-description "library by AUTHOR.")
$ ros template add tmpl.asd NAME AUTHOR --optional DEP "(:alexandria)"
$ ros init result.asd tmpl.asd bob-utilities Bob
Successfully generated: result.asd
$ cat result.asd:
(asdf:defsystem bob-utilities :author "Bob" :depends-on (:alexandria) :short-description "library by Bob.")
```

Notice the arguments are READ. Thus you may use "#." readmacro in order to evaluate the
arguments when the template is instantiated. For example,

```
$ echo $USER
Bob
$ ros init result.asd tmpl.asd bob-utilities '#.(uiop:getenv "USER")'
$ cat result.asd:
(asdf:defsystem bob-utilities :author "Bob" :depends-on (:alexandria) :short-description "library by Bob.")
```

The results are PRINCed, so the results are not escaped with "".
If you want the results to be escaped, it should be done in the template (as in "AUTHOR").

# Subcommands

add
  : Registers a file *TEMPLATE*.

list
  : Shows the list of the registered templates.

show
  : Describes the contents of *TEMPLATE* which is already registered.

rm
  : Ensures that a template *TEMPLATE* is removed.

# Options for *ros template add*

* -f,--force : Overwrites the existing template with the same name without asking
* -r,--recursive : register the directory recursively. TEMPLATE should be a directory in order to make this option meaningful.
* --name NAME : register the template with a given name, not the original filename.


# Misc

Registered templates are stored in directory $ROSWELL_HOME/templates/ .

# SEE ALSO
_ros_(1)
_ros-init_(1)
