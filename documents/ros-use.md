
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
    generally follows the representation used by the implementation. In
    addition, *a special version name "system"* tells roswell to use the
    implementation that is installed in your system, which should be in PATH.

# example

ros use sbcl
    : use the latest SBCL among several versions of SBCLs installed by _ros-install_. This is compiled from the source, which may take some time to install but allows SLIME to jump to the implementation source code.

ros use sbcl-bin
    : use the latest stable SBCL binary downloaded from vendor's website. Roswell uses this by default. `sbcl-bin` tends to be an older version compared to `sbcl`, but surely newer than your _apt-get_ installed counterpart!

ros use sbcl/1.3.1
    : use the compiled SBCL 1.3.1.
    
ros use sbcl-bin/1.3.1
    : use the prebuilt SBCL 1.3.1.

ros use sbcl/system
    : use the SBCL available in the PATH.

ros use ccl
    : use the CCL compiled from the source. Same set of version specifiers as for SBCL can be applied.

Roswell also supports _abcl_, _acl_, _clisp_, _cmu_ and _ecl_.

<!-- # subcommands -->

<!-- somecommand -->
 
<!--   : description. end with a period. -->

<!-- # options -->
<!--  -->
<!-- # Environmental Variables -->

# SEE ALSO
_sbcl_(1), _ros_(1), _ros-install_(1), _ros-list_(1)
