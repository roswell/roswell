ros-list - list the materials installed by roswell

# Synopsis

**ros list [type] [options]**

<!-- # subcommands -->

<!-- somecommand -->
<!-- :description. end with a period. -->

# Description

**ros list** shows the list of the materials installed by roswell.

installed [ [ IMPL | IMPL/VERSION ]...]

  : List the implementations that have been already installed. Optionally
  providing IMPL shows the installed versions of the implementation.

dump [IMPL]

  : List the internal dumped images that are compatible with IMPL. When
  IMPL is ommited, it is defaulted to the current lisp implementation in
  use. Images are in `$ROSWELL_INSTALL_DIR/impl/.../dump/`.

versions [IMPL]

  : List the installable versions for IMPL. When IMPL is ommited, it is
  defaulted to the current lisp implementation in use. This feature
  is experimental.

<!-- # options -->
<!--  -->
<!-- # Environmental Variables -->

# SEE ALSO
_sbcl_(1) _ros_(1)

