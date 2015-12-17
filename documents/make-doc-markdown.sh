#!/bin/bash

cat > ros-$1.md <<EOF

ros-$1 - 
# synopsis

**ros [options] $1** args...

<!-- # subcommands -->

<!-- somecommand -->
 
<!--   : description. end with a period. -->

# description

<!-- # options -->
<!--  -->
<!-- # Environmental Variables -->

# SEE ALSO
_ros_(1)
EOF

