ros-client - one-shot connect to remote repl.

# synopsis

**ros client** protocol [options for the protocol ...] ...

<!-- # subcommands -->
<!-- somecommand -->
 
<!--   : description. end with a period. -->

# description

`ros-client` send message to lisp server and wait for message sent back.

# swank protocol options

`--port port`

  : set server `port` to connect. default port 4005.

`--interface interface`

  : interface for connect. default is `localhost`.

-e FORM     --eval FORM

  : Evaluate FORM while building

<!-- # options -->
<!--  -->
<!-- # Environmental Variables -->

# SEE ALSO
_ros_(1) _ros-serve_(1)
