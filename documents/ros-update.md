ros-update - Update system installed from vcs

# Synopsis

* **ros update** system [system ...]
* **ros update** method [params ...]

# Description

system

  : a name specifying a system.

method

  : currently supported method is `git`.

# Update system

When system are already installed and the system has `.git` in the system directory it invoke `git pull` and `ros install` the system.

# SEE ALSO
_ros_(1), _ros-install_(1)
