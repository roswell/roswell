
ros-use - sets the default implementation used by roswell
# synopsis

**ros [options] use** impl[/version]

impl
    : The name of an implementation which is already installed. New
    implementations can be downloaded by _ros-install_(1).
    The list of installed implementations is available in _ros-list_(1).

version:
    : The version specifier. The string representation of the version
    (e.g. <major>.<minor>) depends on each implementation and roswell
    generally follows the representation used by the implementation.

<!-- # subcommands -->

<!-- somecommand -->
 
<!--   : description. end with a period. -->

# description

`ros use` sets the default implementation used by roswell. It is by default
`sbcl-bin`, which is a stable and well-tested version of sbcl.

<!-- # options -->
<!--  -->
<!-- # Environmental Variables -->

# SEE ALSO
_sbcl_(1), _ros_(1), _ros-install_(1), _ros-list_(1)
