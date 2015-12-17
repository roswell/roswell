ros-setup - setup the backend of roswell

# Synopsis

**ros setup**

<!-- # subcommands -->

<!-- somecommand -->
<!-- :description. end with a period. -->

# Description

**ros setup** initializes roswell. The process includes the following steps:

* Downloading the default stable lisp implementation (sbcl-bin) to `ROSWELL_INSTALL_DIR`.
* Installing quicklisp to `ROSWELL_INSTALL_DIR`.
* On Windows, install 7zip to `ROSWELL_INSTALL_DIR`.
* Making the core image of roswell itself.

<!-- # options -->
<!--  -->
<!-- # Environmental Variables -->

# SEE ALSO
_sbcl_(1), _ros_(1), section Environmental Variables

