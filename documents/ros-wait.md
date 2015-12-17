
ros-wait - 
# synopsis

**ros [options] wait**

<!-- # subcommands -->

<!-- somecommand -->
 
<!--   : description. end with a period. -->

# Description

Wait forever after processing the options.

Useful when the application should work as a background/daemonized application.

# Usecase

This loads and launches a clack application and wait for the request from the client.

    $ ros -sp clack -l app.lisp -e '(clack:clackup)' wait

<!-- # options -->
<!--  -->
<!-- # Environmental Variables -->

# SEE ALSO
_ros_(1)
